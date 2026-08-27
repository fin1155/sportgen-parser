# Модуль 2: ScienceDirect Search V2.
# Если конкретному API-ключу не выдан доступ к ScienceDirect Search, режим auto
# использует официальный Scopus Search API с ограничением PUBLISHER(Elsevier).

sd_empty_df <- function() article_empty_df()

scalar_text <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.data.frame(value)) value <- unlist(value, use.names = FALSE)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0) return("")
  normalize_space(paste(value, collapse = "; "))
}

record_field <- function(record, keys) {
  for (key in keys) {
    if (!is.null(record[[key]])) {
      value <- scalar_text(record[[key]])
      if (nzchar(value)) return(value)
    }
  }
  ""
}

norm_authors <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.character(value)) return(normalize_space(paste(value, collapse = "; ")))
  if (is.data.frame(value)) {
    value <- lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE]))
  }
  if (!is.null(value$author)) return(norm_authors(value$author))
  author_keys <- c("name", "lastName", "surname", "firstName", "givenName", "$", "indexed-name")
  if (is.list(value) && !is.null(names(value)) && any(author_keys %in% names(value))) value <- list(value)
  if (!is.list(value)) return(scalar_text(value))
  names_out <- vapply(value, function(author) {
    if (!is.list(author)) return(scalar_text(author))
    full_name <- record_field(author, c("name"))
    if (nzchar(full_name)) return(full_name)
    indexed <- record_field(author, c("$", "indexed-name", "ce:indexed-name"))
    if (nzchar(indexed)) return(indexed)
    last <- record_field(author, c("lastName", "surname", "ce:surname"))
    first <- record_field(author, c("firstName", "givenName", "ce:given-name"))
    normalize_space(paste(last, first))
  }, character(1))
  paste(names_out[nzchar(names_out)], collapse = "; ")
}

norm_link <- function(value, preferred_refs = character(0)) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.character(value)) return(value[1])
  if (is.data.frame(value)) {
    if (length(preferred_refs) > 0 && "@ref" %in% names(value)) {
      for (ref in preferred_refs) {
        hit <- which(as.character(value[["@ref"]]) == ref)
        if (length(hit) > 0) {
          for (key in c("@href", "href", "url")) {
            if (key %in% names(value) && nzchar(as.character(value[[key]][hit[1]]))) {
              return(as.character(value[[key]][hit[1]]))
            }
          }
        }
      }
    }
    for (key in c("@href", "href", "url")) {
      if (key %in% names(value)) {
        links <- as.character(value[[key]])
        links <- links[!is.na(links) & nzchar(links)]
        if (length(links) > 0) return(links[1])
      }
    }
  }
  if (is.list(value) && !is.null(names(value)) && any(c("@href", "href", "url") %in% names(value))) {
    ref <- record_field(value, c("@ref", "ref"))
    if (length(preferred_refs) == 0 || !nzchar(ref) || ref %in% preferred_refs) {
      direct <- record_field(value, c("@href", "href", "url", "$"))
      if (nzchar(direct)) return(direct)
    }
  }
  if (is.list(value)) {
    for (ref in c(preferred_refs, "")) {
      for (item in value) {
        item_ref <- if (is.list(item)) record_field(item, c("@ref", "ref")) else ""
        if (nzchar(ref) && !identical(item_ref, ref)) next
        link <- norm_link(item)
        if (nzchar(link)) return(link)
      }
    }
  }
  ""
}

records_as_list <- function(records) {
  if (is.null(records) || length(records) == 0) return(list())
  if (is.data.frame(records)) {
    return(lapply(seq_len(nrow(records)), function(i) {
      record <- lapply(records, function(column) if (is.list(column)) column[[i]] else column[i])
      names(record) <- names(records)
      record
    }))
  }
  if (!is.list(records)) return(list())
  record_keys <- c("title", "dc:title", "doi", "prism:doi", "eid", "dc:identifier")
  if (!is.null(names(records)) && any(record_keys %in% names(records))) return(list(records))
  records
}

norm_sd_records <- function(records, retrieval_method = "sciencedirect_api") {
  records <- records_as_list(records)
  if (length(records) == 0) return(sd_empty_df())
  rows <- lapply(records, function(record) {
    if (!is.list(record)) return(NULL)
    source_id <- record_field(record, c("pii", "eid", "dc:identifier", "identifier", "source_id"))
    date <- record_field(record, c("prism:coverDate", "coverDate", "publicationDate", "year", "pubYear"))
    year_match <- regexpr("(19|20)[0-9]{2}", date)
    year <- if (year_match[1] > 0) regmatches(date, year_match) else ""
    doi <- normalize_doi(record_field(record, c("prism:doi", "doi", "DOI")))
    authors_value <- record$authors %||% record$author %||% record$authorList %||% record[["dc:creator"]]
    page_url <- record_field(record, c("articleUrl"))
    if (!nzchar(page_url)) page_url <- norm_link(record$link, c("scidir", "scopus"))
    if (!nzchar(page_url)) page_url <- record_field(record, c("uri", "url"))
    if (!nzchar(page_url) && nzchar(doi)) page_url <- paste0("https://doi.org/", doi)
    open_access <- tolower(record_field(record, c("openaccess", "openAccess", "isOpenAccess")))
    if (open_access %in% c("1", "true", "yes")) open_access <- "true"
    if (open_access %in% c("0", "false", "no")) open_access <- "false"
    data.frame(
      source_id = source_id,
      pmid = record_field(record, c("pmid", "pubmed-id")),
      title = record_field(record, c("dc:title", "title", "articleTitle", "titleNormalized")),
      authors = norm_authors(authors_value),
      journal = record_field(record, c("prism:publicationName", "journalTitle", "journal", "sourceTitle")),
      year = year,
      doi = doi,
      abstract = record_field(record, c("dc:description", "abstract", "abstractText", "description")),
      mesh = record_field(record, c("authkeywords", "keywords")),
      url = page_url,
      publication_type = record_field(record, c("subtypeDescription", "aggregationType", "prism:aggregationType", "type")),
      language = record_field(record, c("language", "dc:language")),
      fulltext_url = if (identical(open_access, "true")) norm_link(record$link, c("self")) else "",
      is_open_access = open_access,
      retrieval_method = retrieval_method,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(sd_empty_df())
  ensure_article_schema(do.call(rbind, rows))
}

parse_sciencedirect_envelope <- function(content, retrieval_method = "sciencedirect_api") {
  text <- if (is.raw(content)) rawToChar(content) else paste(content, collapse = "")
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed)) {
    return(list(data = sd_empty_df(), total = 0L, error = "invalid_json"))
  }
  search_results <- parsed[["search-results"]]
  entries <- if (is.list(search_results)) search_results$entry else NULL
  if (is.null(entries)) entries <- parsed$results %||% parsed$hits %||% parsed$items
  records <- records_as_list(entries)
  records <- Filter(function(record) is.list(record) && is.null(record$error), records)
  total_value <- if (is.list(search_results)) {
    search_results[["opensearch:totalResults"]] %||% search_results$totalResults
  } else {
    parsed$totalResults %||% parsed$total %||% length(records)
  }
  total <- suppressWarnings(as.integer(scalar_text(total_value)))
  if (is.na(total)) total <- length(records)
  list(data = norm_sd_records(records, retrieval_method), total = total, error = "")
}

parse_sciencedirect_payload <- function(content) {
  parse_sciencedirect_envelope(content)$data
}

load_sd_browser <- function(query, settings = NULL) {
  path <- file.path(parser_root(), "data", "sciencedirect_browser.json")
  if (!file.exists(path)) return(sd_empty_df())
  data <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(data) || !is.list(data)) return(sd_empty_df())
  cached_query <- normalize_space(data$query %||% "")
  if (nzchar(cached_query) && !identical(cached_query, normalize_space(query))) {
    warning("ScienceDirect: локальный кэш относится к другому запросу", call. = FALSE)
    return(sd_empty_df())
  }
  norm_sd_records(data$items, "local_cache")
}

elsevier_get <- function(url, key, retries = 2L) {
  response <- NULL
  for (attempt in seq_len(retries + 1L)) {
    response <- httr::GET(
      url,
      httr::timeout(30),
      httr::add_headers(`X-ELS-APIKey` = key),
      httr::accept_json(),
      httr::user_agent("SportGenParser/1.0 (research metadata client)")
    )
    if (!response$status_code %in% c(429, 500, 502, 503, 504) || attempt > retries) break
    Sys.sleep(min(retry_after_seconds(response, 2^attempt), 30))
  }
  response
}

elsevier_put <- function(query, offset, count, key, retries = 2L) {
  response <- NULL
  for (attempt in seq_len(retries + 1L)) {
    response <- httr::PUT(
      "https://api.elsevier.com/content/search/sciencedirect",
      httr::timeout(30),
      body = list(qs = query, display = list(offset = offset, show = count, sortBy = "relevance")),
      encode = "json",
      httr::add_headers(`X-ELS-APIKey` = key),
      httr::accept_json(),
      httr::user_agent("SportGenParser/1.0 (research metadata client)")
    )
    if (!response$status_code %in% c(429, 500, 502, 503, 504) || attempt > retries) break
    Sys.sleep(min(retry_after_seconds(response, 2^attempt), 30))
  }
  response
}

load_sciencedirect_api <- function(query, config, key) {
  limit <- config_limit(config$max_records %||% 200L, default = 200L, hard_cap = 6000L)
  target <- if (is.infinite(limit)) 6000L else as.integer(limit)
  page_size <- 100L
  pages <- list()
  offset <- 0L
  total <- NA_integer_
  status <- 200L
  error <- ""

  while (offset < target && (is.na(total) || offset < total)) {
    count <- min(page_size, target - offset)
    response <- elsevier_put(query, offset, count, key)
    status <- response$status_code
    if (status != 200L) {
      error <- httr::headers(response)[["x-els-status"]] %||%
        substr(httr::content(response, as = "text", encoding = "UTF-8"), 1, 500)
      break
    }
    envelope <- parse_sciencedirect_envelope(response$content, "sciencedirect_api")
    total <- envelope$total
    page <- envelope$data
    if (nrow(page) == 0) break
    pages[[length(pages) + 1L]] <- page
    offset <- offset + nrow(page)
    if (nrow(page) < count) break
    Sys.sleep(0.55)
  }

  data <- if (length(pages) > 0) ensure_article_schema(do.call(rbind, pages)) else sd_empty_df()
  if (nrow(data) > 0) data <- data[!duplicated(paste(data$source_id, data$doi, data$title)), , drop = FALSE]
  attr(data, "total_results") <- if (is.na(total)) 0L else total
  attr(data, "http_status") <- status
  attr(data, "api_error") <- error
  data
}

load_scopus_elsevier_fallback <- function(query, config, key) {
  limit <- config_limit(config$max_records %||% 200L, default = 200L, hard_cap = 5000L)
  target <- if (is.infinite(limit)) 5000L else as.integer(limit)
  page_size <- 25L
  offset <- 0L
  total <- NA_integer_
  pages <- list()
  scopus_query <- paste0("TITLE-ABS-KEY(", query, ") AND PUBLISHER(Elsevier)")

  while (offset < target && (is.na(total) || offset < total)) {
    count <- min(page_size, target - offset)
    url <- httr::modify_url(
      "https://api.elsevier.com/content/search/scopus",
      query = list(query = scopus_query, start = offset, count = count, view = "STANDARD")
    )
    response <- elsevier_get(url, key)
    if (response$status_code != 200L) break
    envelope <- parse_sciencedirect_envelope(response$content, "scopus_elsevier_fallback")
    total <- envelope$total
    page <- envelope$data
    if (nrow(page) == 0) break
    pages[[length(pages) + 1L]] <- page
    offset <- offset + nrow(page)
    if (nrow(page) < count) break
    Sys.sleep(0.55)
  }

  data <- if (length(pages) > 0) ensure_article_schema(do.call(rbind, pages)) else sd_empty_df()
  if (nrow(data) > 0) data <- data[!duplicated(paste(data$source_id, data$doi, data$title)), , drop = FALSE]
  attr(data, "total_results") <- if (is.na(total)) 0L else total
  data
}

load_sciencedirect <- function(query = NULL, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  if (is.null(query)) query <- read_query_for("sciencedirect", settings)
  config <- settings$sciencedirect %||% list()
  if (identical(config$enabled, FALSE)) return(sd_empty_df())
  mode <- tolower(config$mode %||% "auto")
  if (!mode %in% c("auto", "api", "scopus", "cache", "browser")) {
    stop("sciencedirect.mode должен быть auto, api, scopus или cache")
  }
  if (mode %in% c("cache", "browser")) return(load_sd_browser(query, settings))

  suppressMessages(library(httr))
  key <- read_secret(settings, "elsevier_api_key", "ELSEVIER_API_KEY")
  if (!nzchar(key)) {
    warning("Elsevier: задайте ELSEVIER_API_KEY", call. = FALSE)
    return(load_sd_browser(query, settings))
  }

  if (mode %in% c("auto", "api")) {
    result <- load_sciencedirect_api(query, config, key)
    if (nrow(result) > 0 || identical(mode, "api")) {
      if (nrow(result) == 0) warning("ScienceDirect API: ", attr(result, "api_error"), call. = FALSE)
      return(result)
    }
    if (identical(attr(result, "http_status"), 401L)) {
      message("ScienceDirect Search недоступен этому ключу; используется Elsevier Scopus fallback")
    }
  }

  if (mode %in% c("auto", "scopus")) {
    result <- load_scopus_elsevier_fallback(query, config, key)
    if (nrow(result) > 0) return(result)
  }
  load_sd_browser(query, settings)
}
