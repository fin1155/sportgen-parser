# Crossref используется только для дополнения DOI и библиографических полей.
# Существующие значения источника не перезаписываются. Поиск по названию
# принимается только при высокой токенной схожести, чтобы не присвоить чужой DOI.

.crossref_cache <- new.env(parent = emptyenv())

crossref_authors <- function(items) {
  if (is.null(items) || length(items) == 0) return("")
  if (is.data.frame(items)) items <- lapply(seq_len(nrow(items)), function(i) as.list(items[i, , drop = FALSE]))
  values <- vapply(items, function(author) {
    if (!is.list(author)) return("")
    normalize_space(paste(scalar_text(author$family %||% ""), scalar_text(author$given %||% "")))
  }, character(1))
  paste(values[nzchar(values)], collapse = "; ")
}

crossref_year <- function(message) {
  for (field in c("published-print", "published-online", "published", "issued", "created")) {
    date_value <- message[[field]] %||% NULL
    if (!is.list(date_value)) next
    parts <- date_value[["date-parts"]] %||% NULL
    if (!is.null(parts)) {
      values <- unlist(parts, recursive = TRUE, use.names = FALSE)
      if (length(values) == 0) next
      value <- suppressWarnings(as.integer(values[1]))
      if (isTRUE(!is.na(value) && value >= 1800 && value <= 2200)) return(as.character(value))
    }
  }
  ""
}

crossref_record <- function(message) {
  if (is.null(message) || !is.list(message)) return(NULL)
  abstract <- scalar_text(message$abstract %||% "")
  abstract <- normalize_space(gsub("<[^>]+>", " ", abstract))
  list(
    doi = normalize_doi(message$DOI %||% ""),
    title = scalar_text(message$title %||% ""),
    authors = crossref_authors(message$author),
    journal = scalar_text(message[["container-title"]] %||% ""),
    year = crossref_year(message),
    abstract = abstract,
    url = scalar_text(message$URL %||% ""),
    publication_type = scalar_text(message$type %||% ""),
    publisher = scalar_text(message$publisher %||% ""),
    member = scalar_text(message$member %||% "")
  )
}

crossref_request <- function(url, config, retries = 2L) {
  mailto <- Sys.getenv("CROSSREF_MAILTO", unset = scalar_text(config$mailto %||% ""))
  if (nzchar(mailto)) {
    parsed <- httr::parse_url(url)
    parsed$query$mailto <- mailto
    url <- httr::build_url(parsed)
  }
  response <- NULL
  for (attempt in seq_len(retries + 1L)) {
    response <- httr::GET(
      url, httr::timeout(as.numeric(config$timeout_sec %||% 20)), httr::accept_json(),
      httr::user_agent(paste0("SportGenParser/1.0", if (nzchar(mailto)) paste0(" (mailto:", mailto, ")") else ""))
    )
    if (!response$status_code %in% c(429, 500, 502, 503, 504) || attempt > retries) break
    Sys.sleep(min(retry_after_seconds(response, 2^attempt), 30))
  }
  response
}

crossref_by_doi <- function(doi, config = list()) {
  doi <- normalize_doi(doi)
  if (!nzchar(doi)) return(NULL)
  cache_key <- paste0("doi:", doi)
  if (exists(cache_key, envir = .crossref_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .crossref_cache, inherits = FALSE))
  }
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE))
  response <- crossref_request(url, config)
  record <- NULL
  if (response$status_code == 200L) {
    parsed <- tryCatch(jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE
    ), error = function(e) NULL)
    record <- crossref_record(parsed$message)
  }
  assign(cache_key, record, envir = .crossref_cache)
  record
}

crossref_by_title <- function(title, year = "", config = list()) {
  if (!nzchar(normalize_title(title))) return(NULL)
  url <- httr::modify_url(
    "https://api.crossref.org/works",
    query = list(`query.bibliographic` = title, rows = 3,
                 select = "DOI,title,author,container-title,published-print,published-online,published,issued,created,abstract,URL,type,publisher,member")
  )
  response <- crossref_request(url, config)
  if (response$status_code != 200L) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE
  ), error = function(e) NULL)
  items <- parsed$message$items %||% list()
  if (length(items) == 0) return(NULL)
  candidates <- lapply(items, crossref_record)
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0) return(NULL)
  similarity <- vapply(candidates, function(item) title_similarity(title, item$title), numeric(1))
  if (nzchar(year)) {
    year_ok <- vapply(candidates, function(item) {
      !nzchar(item$year) || abs(as.integer(item$year) - as.integer(year)) <= 1L
    }, logical(1))
    similarity[!year_ok] <- 0
  }
  best <- which.max(similarity)
  threshold <- as.numeric(config$title_similarity %||% 0.85)
  if (length(best) == 0 || similarity[best] < threshold) return(NULL)
  candidates[[best]]
}

enrich_crossref <- function(df, settings = NULL) {
  source_attributes <- attributes(df)[setdiff(names(attributes(df)), c("names", "row.names", "class"))]
  df <- ensure_article_schema(df)
  if (nrow(df) == 0) return(df)
  config <- settings$crossref %||% list()
  if (identical(config$enabled, FALSE)) return(df)
  suppressMessages(library(httr))
  max_lookups <- config_limit(config$max_lookups %||% 200L, default = 200L)
  performed <- 0L
  for (i in seq_len(nrow(df))) {
    if (performed >= max_lookups) break
    needs_metadata <- any(!nzchar(c(df$doi[i], df$authors[i], df$journal[i], df$year[i],
                                    df$publication_type[i])))
    if (!needs_metadata && !isTRUE(config$fetch_abstracts %||% FALSE)) next
    record <- if (nzchar(df$doi[i])) {
      crossref_by_doi(df$doi[i], config)
    } else {
      crossref_by_title(df$title[i], df$year[i], config)
    }
    performed <- performed + 1L
    if (is.null(record)) next
    for (field in c("doi", "authors", "journal", "year", "abstract", "url", "publication_type")) {
      if (!nzchar(df[[field]][i]) && nzchar(record[[field]] %||% "")) df[[field]][i] <- record[[field]]
    }
    if (!grepl("crossref", df$retrieval_method[i], fixed = TRUE)) {
      df$retrieval_method[i] <- paste(c(df$retrieval_method[i], "crossref"), collapse = "+")
      df$retrieval_method[i] <- sub("^\\+", "", df$retrieval_method[i])
    }
    Sys.sleep(as.numeric(config$pause_sec %||% 0.12))
  }
  attr(df, "crossref_lookups") <- performed
  for (attribute_name in names(source_attributes)) {
    attr(df, attribute_name) <- source_attributes[[attribute_name]]
  }
  df
}
