# Модуль 3: русскоязычные и российские публикации через OpenAlex.
# Кандидаты выбираются двумя независимыми потоками (язык RU и российская
# аффилиация), затем проходят строгий локальный фильтр: генетика AND спорт/ФА.

openalex_empty_df <- function() article_empty_df()

openalex_abstract <- function(index) {
  if (is.null(index) || length(index) == 0 || !is.list(index)) return("")
  positions <- list()
  for (word in names(index)) {
    values <- suppressWarnings(as.integer(unlist(index[[word]], use.names = FALSE)))
    values <- values[!is.na(values)]
    for (position in values) positions[[as.character(position)]] <- word
  }
  if (length(positions) == 0) return("")
  ordered <- positions[order(as.integer(names(positions)))]
  normalize_space(paste(unlist(ordered, use.names = FALSE), collapse = " "))
}

openalex_authors <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return("")
  if (is.data.frame(authorships)) {
    authorships <- lapply(seq_len(nrow(authorships)), function(i) as.list(authorships[i, , drop = FALSE]))
  }
  values <- vapply(authorships, function(item) {
    if (!is.list(item)) return("")
    author <- item$author %||% list()
    scalar_text(author$display_name %||% item$raw_author_name %||% "")
  }, character(1))
  paste(unique(values[nzchar(values)]), collapse = "; ")
}

openalex_keywords <- function(record) {
  values <- character(0)
  for (field in c("keywords", "topics")) {
    items <- record[[field]]
    if (is.null(items)) next
    if (is.data.frame(items) && "display_name" %in% names(items)) {
      values <- c(values, as.character(items$display_name))
    } else if (is.list(items)) {
      values <- c(values, vapply(items, function(x) {
        if (is.list(x)) scalar_text(x$display_name %||% "") else ""
      }, character(1)))
    }
  }
  paste(unique(values[nzchar(values)]), collapse = "; ")
}

openalex_location_field <- function(record, location_name, field) {
  location <- record[[location_name]] %||% list()
  scalar_text(location[[field]] %||% "")
}

norm_openalex_records <- function(records) {
  records <- records_as_list(records)
  if (length(records) == 0) return(openalex_empty_df())
  rows <- lapply(records, function(record) {
    if (!is.list(record)) return(NULL)
    primary <- record$primary_location %||% list()
    source <- primary$source %||% list()
    best_oa <- record$best_oa_location %||% list()
    doi <- normalize_doi(record$doi %||% record$ids$doi %||% "")
    open_access <- record$open_access %||% list()
    is_oa <- isTRUE(open_access$is_oa) || isTRUE(primary$is_oa) || isTRUE(best_oa$is_oa)
    fulltext_url <- scalar_text(best_oa$pdf_url %||% primary$pdf_url %||% "")
    if (!nzchar(fulltext_url) && is_oa) {
      fulltext_url <- scalar_text(best_oa$landing_page_url %||% primary$landing_page_url %||% "")
    }
    pmid <- scalar_text(record$ids$pmid %||% "")
    pmid <- sub("^.*/", "", sub("/$", "", pmid))
    id <- scalar_text(record$id %||% "")
    data.frame(
      source_id = sub("^.*/", "", id),
      pmid = pmid,
      title = scalar_text(record$title %||% record$display_name %||% ""),
      authors = openalex_authors(record$authorships),
      journal = scalar_text(source$display_name %||% primary$raw_source_name %||% ""),
      year = scalar_text(record$publication_year %||% ""),
      doi = doi,
      abstract = openalex_abstract(record$abstract_inverted_index),
      mesh = openalex_keywords(record),
      url = scalar_text(primary$landing_page_url %||% record$doi %||% id),
      publication_type = scalar_text(record$type_crossref %||% record$type %||% ""),
      language = scalar_text(record$language %||% ""),
      fulltext_url = fulltext_url,
      is_open_access = if (is_oa) "true" else "false",
      retrieval_method = "openalex_api",
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(openalex_empty_df())
  ensure_article_schema(do.call(rbind, rows))
}

openalex_search_text <- function(query) {
  # OpenAlex поддерживает фразы, скобки и булевы AND/OR/NOT в параметре
  # `search`. Снятие операторов превращало запрос в неявное AND по всем
  # терминам и обнуляло выдачу русскоязычных публикаций.
  query <- normalize_space(query)
  query <- gsub("[[:space:]]+(AND|OR|NOT)[[:space:]]+", " \\1 ",
                query, ignore.case = TRUE, perl = TRUE)
  query
}

openalex_get <- function(url, retries = 2L) {
  response <- NULL
  for (attempt in seq_len(retries + 1L)) {
    response <- httr::GET(url, httr::timeout(30), httr::accept_json(),
                          httr::user_agent("SportGenParser/1.0 (research metadata client)"))
    if (!response$status_code %in% c(429, 500, 502, 503, 504) || attempt > retries) break
    Sys.sleep(min(retry_after_seconds(response, 2^attempt), 30))
  }
  response
}

openalex_stream <- function(search, filter, candidate_cap, config, api_key = "") {
  cursor <- "*"
  rows <- list()
  fetched <- 0L
  total <- NA_integer_
  select <- paste(
    c("id", "ids", "doi", "title", "display_name", "publication_year", "publication_date",
      "type", "type_crossref", "language", "authorships", "primary_location",
      "best_oa_location", "open_access", "abstract_inverted_index", "keywords", "topics"),
    collapse = ","
  )
  while (!is.null(cursor) && nzchar(cursor) && fetched < candidate_cap) {
    per_page <- min(100L, candidate_cap - fetched)
    query <- list(search = search, filter = filter, per_page = per_page,
                  cursor = cursor, select = select)
    if (nzchar(api_key)) query$api_key <- api_key
    url <- httr::modify_url("https://api.openalex.org/works", query = query)
    response <- openalex_get(url)
    if (response$status_code != 200L) {
      warning("OpenAlex: HTTP ", response$status_code, call. = FALSE)
      break
    }
    parsed <- tryCatch(jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE
    ), error = function(e) NULL)
    if (is.null(parsed)) break
    if (is.na(total)) total <- suppressWarnings(as.integer(parsed$meta$count %||% 0L))
    page <- norm_openalex_records(parsed$results)
    if (nrow(page) == 0) break
    rows[[length(rows) + 1L]] <- page
    fetched <- fetched + nrow(page)
    cursor <- parsed$meta$next_cursor
    Sys.sleep(as.numeric(config$pause_sec %||% 0.12))
  }
  data <- if (length(rows) > 0) ensure_article_schema(do.call(rbind, rows)) else openalex_empty_df()
  attr(data, "total_results") <- if (is.na(total)) 0L else total
  data
}

load_openalex <- function(query = NULL, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  if (is.null(query)) query <- read_query_for("openalex", settings)
  config <- settings$openalex %||% list()
  if (identical(config$enabled, FALSE)) return(openalex_empty_df())
  suppressMessages(library(httr))
  search <- openalex_search_text(query)
  api_key <- read_secret(settings, "openalex_api_key", "OPENALEX_API_KEY")
  limit <- config_limit(config$max_records %||% 200L, default = 200L, hard_cap = 10000L)
  target <- if (is.infinite(limit)) 10000L else as.integer(limit)
  multiplier <- max(1L, as.integer(config$candidate_multiplier %||% 5L))
  candidate_cap <- min(10000L, max(100L, target * multiplier))

  language_rows <- openalex_stream(
    search, "type:article,language:ru", candidate_cap, config, api_key
  )
  affiliation_rows <- openalex_stream(
    search, "type:article,authorships.institutions.country_code:ru",
    candidate_cap, config, api_key
  )
  all <- ensure_article_schema(rbind(language_rows, affiliation_rows))
  if (nrow(all) == 0) return(openalex_empty_df())
  key <- ifelse(nzchar(all$doi), paste0("doi:", all$doi),
                ifelse(nzchar(all$source_id), paste0("oa:", all$source_id),
                       paste0("title:", normalize_title(all$title))))
  all <- all[!duplicated(key), , drop = FALSE]
  text <- paste(all$title, all$abstract, all$mesh, sep = " | ")
  keep <- vapply(text, strict_topic_match, logical(1))
  all <- all[keep, , drop = FALSE]
  if (nrow(all) > target) all <- utils::head(all, target)
  rownames(all) <- NULL
  attr(all, "total_results") <- sum(
    as.integer(attr(language_rows, "total_results") %||% 0L),
    as.integer(attr(affiliation_rows, "total_results") %||% 0L),
    na.rm = TRUE
  )
  attr(all, "candidate_results") <- length(key)
  attr(all, "strict_results") <- nrow(all)
  all
}

# Обратная совместимость со старым именем этапа. Данные берутся из OpenAlex,
# а не из HTML eLIBRARY.
load_elibrary <- function(query = NULL, settings = NULL) load_openalex(query, settings)
el_empty_df <- function() openalex_empty_df()
