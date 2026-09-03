# Общие функции и единая схема данных для всех источников.

ARTICLE_COLS <- c(
  "source_id", "pmid", "title", "authors", "journal", "year", "doi",
  "abstract", "mesh", "url", "publication_type", "language",
  "fulltext_url", "is_open_access", "retrieval_method"
)

TABLE_COLUMN_LABELS <- c(
  source = "Источник",
  source_id = "ID источника",
  pmid = "PMID",
  title = "Название статьи",
  authors = "Авторы",
  journal = "Журнал",
  year = "Год публикации",
  doi = "DOI",
  abstract = "Аннотация",
  mesh = "MeSH и темы",
  url = "Ссылка на статью",
  publication_type = "Тип публикации (метаданные)",
  language = "Язык",
  fulltext_url = "Ссылка на полный текст",
  is_open_access = "Открытый доступ",
  retrieval_method = "Способ получения",
  gene = "Исследованный ген",
  snp = "SNP (rsID)",
  pub_type = "Тип исследования",
  inherit_model = "Модель наследования",
  allele_freq = "Частота аллелей",
  hwe = "Равновесие Харди–Вайнберга",
  sample_type = "Тип выборки",
  ethnicity = "Этническая принадлежность",
  sample_size = "Размер выборки",
  sex = "Пол (M/F)",
  age = "Возраст",
  pa_level = "Уровень физической активности",
  phenotype = "Исследуемый фенотип",
  measure_method = "Метод измерения фенотипа",
  covariates = "Ковариаты",
  results = "Основные результаты",
  effect_dir = "Направление эффекта",
  effect_size = "Размер эффекта",
  p_adj = "Статистическая значимость",
  gene_env = "Взаимодействие ген × среда",
  multipletest = "Поправка на множественное тестирование",
  power = "Мощность исследования",
  data_avail = "Доступность данных",
  extraction_confidence = "Уверенность извлечения",
  extraction_evidence = "Фрагмент-основание"
)

TABLE_COLUMN_PRESETS <- list(
  core = c(
    "source", "pmid", "title", "authors", "journal", "year", "doi", "url",
    "gene", "snp", "pub_type", "results", "extraction_confidence"
  ),
  genetics = c(
    "source", "title", "year", "doi", "gene", "snp", "pub_type",
    "inherit_model", "allele_freq", "hwe", "sample_type", "ethnicity",
    "sample_size", "sex", "age", "pa_level", "phenotype", "measure_method",
    "covariates", "results", "effect_dir", "effect_size", "p_adj", "gene_env",
    "multipletest", "power", "data_avail", "extraction_confidence",
    "extraction_evidence"
  ),
  all = names(TABLE_COLUMN_LABELS)
)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.character(a) && length(a) == 1 && (is.na(a) || !nzchar(a))) return(b)
  if (!is.list(a) && !is.function(a) && length(a) == 1 && is.na(a)) return(b)
  a
}

parser_root <- function() {
  getOption("article_parser.root", getwd())
}

normalize_space <- function(x) {
  x <- gsub("[[:space:]]+", " ", as.character(x))
  trimws(x)
}

article_empty_df <- function() {
  values <- setNames(replicate(length(ARTICLE_COLS), character(0), simplify = FALSE),
                     ARTICLE_COLS)
  as.data.frame(values, stringsAsFactors = FALSE)
}

ensure_article_schema <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (column in ARTICLE_COLS) {
    if (!column %in% names(df)) df[[column]] <- rep("", nrow(df))
    df[[column]] <- as.character(df[[column]])
    df[[column]][is.na(df[[column]])] <- ""
  }
  df[, ARTICLE_COLS, drop = FALSE]
}

count_source_labels <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 0) return(0L)
  labels <- trimws(unlist(strsplit(values, ";", fixed = TRUE), use.names = FALSE))
  labels <- labels[nzchar(labels)]
  as.integer(length(unique(labels)))
}

select_output_columns <- function(df, selected, fallback = TABLE_COLUMN_PRESETS$core) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  selected <- unique(as.character(selected %||% character(0)))
  selected <- selected[selected %in% names(df)]
  if (length(selected) == 0) {
    selected <- fallback[fallback %in% names(df)]
  }
  if (length(selected) == 0 && ncol(df) > 0) selected <- names(df)[1]
  df[, selected, drop = FALSE]
}

safe_http_url <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (length(value) == 0 || is.na(value[1]) || !nzchar(value[1])) return("")
  value <- value[1]
  if (!grepl("^https?://[^[:space:]]+$", value, ignore.case = TRUE, perl = TRUE)) return("")
  value
}

article_destination <- function(url = "", doi = "", pmid = "") {
  direct <- safe_http_url(url)
  if (nzchar(direct)) return(direct)

  doi <- normalize_doi(doi)
  if (grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$", doi, perl = TRUE)) {
    return(paste0("https://doi.org/", doi))
  }

  pmid <- trimws(as.character(pmid %||% ""))[1]
  if (!is.na(pmid) && grepl("^[0-9]+$", pmid)) {
    return(paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"))
  }
  ""
}

safe_external_link <- function(label, url, fallback = label, class_name = "article-link") {
  url <- safe_http_url(url)
  label <- as.character(label %||% "")[1]
  fallback <- as.character(fallback %||% "")[1]
  if (is.na(label)) label <- ""
  if (is.na(fallback)) fallback <- ""
  if (!nzchar(label) || !nzchar(url)) return(htmltools::htmlEscape(fallback))
  sprintf(
    '<a class="%s" href="%s" target="_blank" rel="noopener noreferrer">%s</a>',
    htmltools::htmlEscape(class_name, attribute = TRUE),
    htmltools::htmlEscape(url, attribute = TRUE),
    htmltools::htmlEscape(label)
  )
}

truncate_display_text <- function(value, max_chars = 260L) {
  value <- trimws(as.character(value %||% "")[1])
  if (is.na(value) || !nzchar(value) || nchar(value, type = "chars") <= max_chars) return(value)
  preview <- substr(value, 1L, max_chars)
  preview <- sub("[[:space:]]+[^[:space:]]*$", "", preview, perl = TRUE)
  if (!nzchar(preview)) preview <- substr(value, 1L, max_chars)
  paste0(trimws(preview), "…")
}

authors_display_preview <- function(value, max_authors = 3L) {
  value <- trimws(as.character(value %||% "")[1])
  if (is.na(value) || !nzchar(value)) return("")
  authors <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  authors <- authors[nzchar(authors)]
  if (length(authors) <= max_authors) return(value)
  paste0(paste(head(authors, max_authors), collapse = "; "), "; …")
}

safe_expandable_cell <- function(value, preview, class_name) {
  value <- trimws(as.character(value %||% "")[1])
  preview <- trimws(as.character(preview %||% "")[1])
  if (is.na(value) || !nzchar(value)) return("")
  if (is.na(preview) || !nzchar(preview) || identical(preview, value)) {
    return(htmltools::htmlEscape(value))
  }
  sprintf(
    paste0(
      '<div class="expandable-cell %s">',
      '<span class="cell-preview">%s</span>',
      '<span class="cell-full" hidden>%s</span>',
      '<button type="button" class="cell-expand-button" aria-expanded="false">Показать полностью</button>',
      '</div>'
    ),
    htmltools::htmlEscape(class_name, attribute = TRUE),
    htmltools::htmlEscape(preview),
    htmltools::htmlEscape(value)
  )
}

prepare_table_display <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(list(data = df, link_columns = integer(0)))

  values <- function(name) {
    if (name %in% names(df)) as.character(df[[name]]) else rep("", nrow(df))
  }
  destinations <- mapply(
    article_destination,
    values("url"), values("doi"), values("pmid"),
    USE.NAMES = FALSE
  )
  link_names <- character(0)

  if ("authors" %in% names(df)) {
    author_values <- values("authors")
    author_previews <- vapply(author_values, authors_display_preview, character(1))
    df$authors <- mapply(
      safe_expandable_cell, author_values, author_previews,
      MoreArgs = list(class_name = "expandable-cell-authors"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "authors")
  }
  if ("abstract" %in% names(df)) {
    abstract_values <- values("abstract")
    abstract_previews <- vapply(abstract_values, truncate_display_text, character(1))
    df$abstract <- mapply(
      safe_expandable_cell, abstract_values, abstract_previews,
      MoreArgs = list(class_name = "expandable-cell-abstract"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "abstract")
  }

  if ("title" %in% names(df)) {
    df$title <- mapply(
      safe_external_link, values("title"), destinations, values("title"),
      MoreArgs = list(class_name = "article-link article-title-link"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "title")
  }
  if ("url" %in% names(df)) {
    df$url <- mapply(
      safe_external_link, rep("Открыть статью ↗", nrow(df)), destinations, values("url"),
      MoreArgs = list(class_name = "article-link article-open-link"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "url")
  }
  if ("fulltext_url" %in% names(df)) {
    fulltext_values <- values("fulltext_url")
    df$fulltext_url <- mapply(
      safe_external_link,
      rep("Открыть полный текст ↗", nrow(df)), fulltext_values, fulltext_values,
      MoreArgs = list(class_name = "article-link article-open-link"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "fulltext_url")
  }
  if ("doi" %in% names(df)) {
    doi_values <- values("doi")
    doi_urls <- vapply(doi_values, function(value) article_destination(doi = value), character(1))
    df$doi <- mapply(
      safe_external_link, doi_values, doi_urls, doi_values,
      MoreArgs = list(class_name = "article-link article-id-link"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "doi")
  }
  if ("pmid" %in% names(df)) {
    pmid_values <- values("pmid")
    pmid_urls <- vapply(pmid_values, function(value) article_destination(pmid = value), character(1))
    df$pmid <- mapply(
      safe_external_link, pmid_values, pmid_urls, pmid_values,
      MoreArgs = list(class_name = "article-link article-id-link"),
      USE.NAMES = FALSE
    )
    link_names <- c(link_names, "pmid")
  }

  list(data = df, link_columns = match(intersect(link_names, names(df)), names(df)))
}

read_query_for <- function(source, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  defaults <- c(
    pubmed = "config/query.txt",
    sciencedirect = "config/query_sciencedirect.txt",
    openalex = "config/query_openalex.txt",
    elibrary = "config/query_openalex.txt"
  )
  query_file <- NULL
  if (!is.null(settings$query_files)) query_file <- settings$query_files[[source]]
  query_file <- query_file %||% unname(defaults[source]) %||% "config/query.txt"
  path <- if (grepl("^(/|[A-Za-z]:)", query_file)) {
    query_file
  } else {
    file.path(parser_root(), query_file)
  }
  if (!file.exists(path)) {
    fallback <- file.path(parser_root(), "config", "query.txt")
    if (!file.exists(fallback)) stop("Файл запроса не найден: ", path)
    warning("Нет отдельного запроса для ", source, "; используется config/query.txt",
            call. = FALSE)
    path <- fallback
  }
  normalize_space(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " "))
}

read_secret <- function(settings, legacy_field, env_name) {
  runtime <- settings$runtime_secrets[[env_name]] %||% ""
  if (nzchar(runtime)) return(runtime)
  value <- Sys.getenv(env_name, unset = "")
  if (nzchar(value)) return(value)
  legacy <- settings[[legacy_field]] %||% ""
  if (nzchar(legacy)) {
    warning("Секрет в settings.json устарел; перенесите его в ", env_name,
            call. = FALSE)
  }
  legacy
}

normalize_doi <- function(value) {
  value <- tolower(normalize_space(value %||% ""))
  value <- sub("^doi:[[:space:]]*", "", value)
  value <- sub("^https?://(dx\\.)?doi\\.org/", "", value)
  value <- sub("[?#].*$", "", value)
  value <- sub("[[:punct:]]+$", "", value)
  value
}

normalize_title <- function(value) {
  value <- tolower(normalize_space(value %||% ""))
  value <- gsub("[^[:alnum:]а-яё]+", " ", value, perl = TRUE)
  normalize_space(value)
}

title_similarity <- function(a, b) {
  a_tokens <- unique(strsplit(normalize_title(a), " ", fixed = TRUE)[[1]])
  b_tokens <- unique(strsplit(normalize_title(b), " ", fixed = TRUE)[[1]])
  a_tokens <- a_tokens[nzchar(a_tokens)]
  b_tokens <- b_tokens[nzchar(b_tokens)]
  if (length(a_tokens) == 0 || length(b_tokens) == 0) return(0)
  length(intersect(a_tokens, b_tokens)) / length(union(a_tokens, b_tokens))
}

config_limit <- function(value, default = 200L, hard_cap = Inf) {
  parsed <- suppressWarnings(as.integer(value %||% default))
  if (is.na(parsed)) parsed <- default
  if (parsed <= 0) return(hard_cap)
  min(parsed, hard_cap)
}

normalize_publication_year <- function(value, label = "Год") {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA_integer_)
  text <- trimws(as.character(value[1]))
  if (!nzchar(text)) return(NA_integer_)
  if (!grepl("^[0-9]{4}$", text)) stop(label, " должен состоять из четырёх цифр")
  year <- suppressWarnings(as.integer(text))
  max_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
  if (is.na(year) || year < 1800L || year > max_year) {
    stop(label, " должен быть от 1800 до ", max_year)
  }
  year
}

publication_year_range <- function(settings = list(), config = list()) {
  global <- settings$publication_year %||% list()
  from <- normalize_publication_year(config$year_from %||% global$from %||% "", "Год «с»")
  to <- normalize_publication_year(config$year_to %||% global$to %||% "", "Год «по»")
  if (!is.na(from) && !is.na(to) && from > to) {
    stop("Год «с» не может быть позже года «по»")
  }
  list(from = from, to = to, active = !is.na(from) || !is.na(to))
}

filter_publication_year <- function(df, range) {
  source_attributes <- attributes(df)[setdiff(names(attributes(df)), c("names", "row.names", "class"))]
  df <- ensure_article_schema(df)
  if (nrow(df) == 0 || !isTRUE(range$active)) return(df)
  years <- suppressWarnings(as.integer(df$year))
  keep <- !is.na(years)
  if (!is.na(range$from)) keep <- keep & years >= range$from
  if (!is.na(range$to)) keep <- keep & years <= range$to
  result <- df[keep, , drop = FALSE]
  rownames(result) <- NULL
  for (attribute_name in names(source_attributes)) {
    attr(result, attribute_name) <- source_attributes[[attribute_name]]
  }
  result
}

retry_after_seconds <- function(response, default = 2) {
  value <- suppressWarnings(as.numeric(httr::headers(response)[["retry-after"]] %||% default))
  if (is.na(value) || value < 0) default else value
}

strict_topic_match <- function(text) {
  text <- normalize_space(text %||% "")
  if (!nzchar(text)) return(FALSE)
  genetics <- paste(
    c(
      "\\brs[0-9]+\\b", "\\bsnp(s)?\\b", "single nucleotide polymorphism",
      "genetic (variant|variants|polymorphism|polymorphisms)",
      "\\bgenotype(s)?\\b", "\\bpolymorphism(s)?\\b",
      "однонуклеотидн(ый|ого|ые|ых) полиморфизм",
      "генетическ(ий|ого|ие|их) (вариант|варианты|полиморфизм|полиморфизмы)",
      "\\bгенотип(а|ы|ов)?\\b", "\\bполиморфизм(а|ы|ов)?\\b"
    ),
    collapse = "|"
  )
  activity <- paste(
    c(
      "\\bathlete(s)?\\b", "\\bsport(s|ing)?\\b", "physical activity",
      "physical performance", "exercise", "endurance", "aerobic performance",
      "muscle strength", "vo2 ?max", "sedentary behavio(u)?r",
      "\\bспорт(а|е|ом|ивн|смен)?", "физическ(ая|ой|ую|ие|их) активност",
      "\\bвыносливост", "\\bупражнен", "\\bтрениров", "мышечн(ая|ой) сил"
    ),
    collapse = "|"
  )
  grepl(genetics, text, ignore.case = TRUE, perl = TRUE) &&
    grepl(activity, text, ignore.case = TRUE, perl = TRUE)
}

load_gene_symbols <- function(settings) {
  configured <- as.character(settings$gene_candidates %||% character(0))
  symbols_file <- settings$gene_symbols_file %||% "config/gene_symbols.txt"
  path <- if (grepl("^(/|[A-Za-z]:)", symbols_file)) {
    symbols_file
  } else {
    file.path(parser_root(), symbols_file)
  }
  from_file <- character(0)
  if (file.exists(path)) {
    from_file <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
    from_file <- from_file[nzchar(from_file) & !grepl("^#", from_file)]
  }
  symbols <- unique(toupper(c(configured, from_file)))
  attr(symbols, "always_match") <- unique(toupper(configured))
  symbols
}
