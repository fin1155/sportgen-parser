# Модуль 1: PubMed через NCBI E-utilities.
# Поиск использует history server, а записи загружаются пакетами. Это позволяет
# проходить все страницы результата без одного HTTP-запроса на каждую статью.

pubmed_empty_df <- function() article_empty_df()

xml_first_text <- function(node, xpath) {
  value <- xml2::xml_find_first(node, xpath)
  if (inherits(value, "xml_missing")) return("")
  normalize_space(xml2::xml_text(value))
}

pubmed_year <- function(article) {
  value <- xml_first_text(article, ".//JournalIssue/PubDate/Year")
  if (!nzchar(value)) value <- xml_first_text(article, ".//JournalIssue/PubDate/MedlineDate")
  match <- regexpr("(19|20)[0-9]{2}", value)
  if (match[1] > 0) regmatches(value, match) else ""
}

pubmed_authors <- function(article) {
  nodes <- xml2::xml_find_all(article, ".//Article/AuthorList/Author")
  if (length(nodes) == 0) return("")
  values <- vapply(nodes, function(author) {
    collective <- xml_first_text(author, "./CollectiveName")
    if (nzchar(collective)) return(collective)
    last <- xml_first_text(author, "./LastName")
    first <- xml_first_text(author, "./ForeName")
    normalize_space(paste(last, first))
  }, character(1))
  paste(values[nzchar(values)], collapse = "; ")
}

parse_pubmed_batch <- function(content) {
  text <- if (is.raw(content)) rawToChar(content) else paste(content, collapse = "\n")
  document <- tryCatch(xml2::read_xml(text), error = function(e) NULL)
  if (is.null(document)) return(pubmed_empty_df())
  articles <- xml2::xml_find_all(document, ".//PubmedArticle")
  if (length(articles) == 0) return(pubmed_empty_df())

  rows <- lapply(articles, function(article) {
    pmid <- xml_first_text(article, ".//MedlineCitation/PMID")
    doi_node <- xml2::xml_find_first(article, ".//ArticleId[@IdType='doi']")
    doi <- if (inherits(doi_node, "xml_missing")) "" else normalize_doi(xml2::xml_text(doi_node))
    pmc_node <- xml2::xml_find_first(article, ".//ArticleId[@IdType='pmc']")
    pmcid <- if (inherits(pmc_node, "xml_missing")) "" else normalize_space(xml2::xml_text(pmc_node))
    abstract_nodes <- xml2::xml_find_all(article, ".//Article/Abstract/AbstractText")
    abstract <- normalize_space(paste(xml2::xml_text(abstract_nodes), collapse = " "))
    publication_types <- xml2::xml_text(xml2::xml_find_all(article, ".//PublicationTypeList/PublicationType"))
    mesh <- xml2::xml_text(xml2::xml_find_all(article, ".//MeshHeading/DescriptorName"))
    languages <- xml2::xml_text(xml2::xml_find_all(article, ".//Article/Language"))
    data.frame(
      source_id = pmid,
      pmid = pmid,
      title = xml_first_text(article, ".//Article/ArticleTitle"),
      authors = pubmed_authors(article),
      journal = xml_first_text(article, ".//Article/Journal/Title"),
      year = pubmed_year(article),
      doi = doi,
      abstract = abstract,
      mesh = paste(unique(mesh[nzchar(mesh)]), collapse = "; "),
      url = if (nzchar(pmid)) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/") else "",
      publication_type = paste(unique(publication_types[nzchar(publication_types)]), collapse = "; "),
      language = paste(unique(languages[nzchar(languages)]), collapse = "; "),
      fulltext_url = if (nzchar(pmcid)) paste0("https://pmc.ncbi.nlm.nih.gov/articles/", pmcid, "/") else "",
      is_open_access = if (nzchar(pmcid)) "true" else "false",
      retrieval_method = "ncbi_eutils",
      stringsAsFactors = FALSE
    )
  })
  ensure_article_schema(do.call(rbind, rows))
}

pubmed_query_with_year <- function(query, range) {
  if (!isTRUE(range$active)) return(query)
  from <- if (is.na(range$from)) "1800" else as.character(range$from)
  to <- if (is.na(range$to)) as.character(as.integer(format(Sys.Date(), "%Y")) + 1L) else as.character(range$to)
  paste0("(", query, ") AND ", from, ":", to, "[dp]")
}

load_pubmed <- function(query = NULL, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  if (is.null(query)) query <- read_query_for("pubmed", settings)
  config <- settings$pubmed %||% list()
  if (identical(config$enabled, FALSE)) return(pubmed_empty_df())
  year_range <- publication_year_range(settings, config)
  search_query <- pubmed_query_with_year(query, year_range)

  suppressMessages(library(rentrez))
  suppressMessages(library(xml2))
  key <- read_secret(settings, "ncbi_api_key", "NCBI_API_KEY")
  has_key <- nzchar(key)
  if (has_key) suppressMessages(rentrez::set_entrez_key(key))
  delay <- if (has_key) 0.12 else 0.35
  requested <- config$max_records %||% settings$retmax %||% 200
  batch_size <- max(1L, min(as.integer(config$batch_size %||% 100L), 200L))

  search <- rentrez::entrez_search(db = "pubmed", term = search_query, retmax = 0, use_history = TRUE)
  total <- as.integer(search$count %||% 0L)
  if (is.na(total) || total == 0L) {
    warning("PubMed: 0 результатов для запроса: ", search_query, call. = FALSE)
    return(pubmed_empty_df())
  }
  limit <- config_limit(requested, default = 200L)
  target <- if (is.infinite(limit)) total else min(total, as.integer(limit))
  strict_filter <- isTRUE(config$strict_filter %||% TRUE)
  multiplier <- max(1L, as.integer(config$candidate_multiplier %||% 5L))
  candidate_cap <- if (is.infinite(limit)) total else min(total, max(target, target * multiplier))
  pages <- list()
  offset <- 0L
  retained <- 0L

  while (offset < candidate_cap && retained < target) {
    count <- min(batch_size, candidate_cap - offset)
    Sys.sleep(delay)
    fetch <- function() {
      rentrez::entrez_fetch(
        db = "pubmed", web_history = search$web_history, rettype = "xml",
        retstart = offset, retmax = count
      )
    }
    payload <- tryCatch(fetch(), error = function(e) NULL)
    if (is.null(payload)) {
      Sys.sleep(2)
      payload <- tryCatch(fetch(), error = function(e) NULL)
    }
    if (is.null(payload)) {
      warning("PubMed: не удалось загрузить страницу с позиции ", offset, call. = FALSE)
      stop("PubMed: загрузка прервана после повторного сбоя страницы ", offset, call. = FALSE)
    }
    page <- parse_pubmed_batch(payload)
    page <- filter_publication_year(page, year_range)
    if (strict_filter && nrow(page) > 0) {
      text <- paste(page$title, page$abstract, page$mesh, sep = " | ")
      page <- page[vapply(text, strict_topic_match, logical(1)), , drop = FALSE]
    }
    if (nrow(page) > 0) {
      pages[[length(pages) + 1L]] <- page
      retained <- retained + nrow(page)
    }
    offset <- offset + count
  }

  pages <- pages[!vapply(pages, is.null, logical(1))]
  if (length(pages) == 0) return(pubmed_empty_df())
  result <- ensure_article_schema(do.call(rbind, pages))
  if (nrow(result) > target) result <- utils::head(result, target)
  attr(result, "total_results") <- total
  attr(result, "retrieved_results") <- nrow(result)
  attr(result, "candidate_results") <- offset
  rownames(result) <- NULL
  result
}
