# Модуль 5 (ЭТАП 3): полный текст статей из PMC + извлечение полей целевой таблицы.
# Полный текст берётся через NCBI eutils: entrez_fetch(db = "pmc", rettype = "xml").
# Поля схемы (тип публикации, модель наследования, частота аллелей, HWE,
# характеристики выборки, фенотипы, результаты, качество) заполняются регулярным
# извлечением из полного текста; что не нашлось — честно пустое.
# Аккуратность: пауза pause_sec между запросами (лимит NCBI без ключа — 3 запроса/с),
# при 429 — бэкофф 30 с и один повтор.

pmc_empty_df <- function() {
  data.frame(pmid = character(0), fulltext = character(0), stringsAsFactors = FALSE)
}

section_subset <- function(text, names) {
  if (!grepl("\\[\\[SECTION:", text)) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  keep <- logical(length(lines))
  active <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^\\[\\[SECTION:", lines[i])) {
      section <- sub("^\\[\\[SECTION:[[:space:]]*", "", lines[i])
      section <- sub("\\]\\]$", "", section)
      active <- any(grepl(paste(names, collapse = "|"), section, ignore.case = TRUE, perl = TRUE)) &&
        !grepl("reference|bibliograph|литератур|introduction|discussion|background|введени|обсуждени", section, ignore.case = TRUE)
    }
    keep[i] <- active
  }
  selected <- paste(lines[keep], collapse = "\n")
  if (nzchar(normalize_space(selected))) return(selected)
  # A missing Methods/Results section is not permission to mine references or
  # a discussion of other studies. Abstract/unstructured full text is fallback.
  active <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^\\[\\[SECTION:", lines[i])) {
      active <- grepl("Abstract|Body|Open full text|Page|Аннотац|Резюме", lines[i], ignore.case = TRUE) &&
        !grepl("reference|bibliograph|литератур|introduction|discussion|background|введени|обсуждени", lines[i], ignore.case = TRUE)
    }
    keep[i] <- active
  }
  paste(lines[keep], collapse = "\n")
}

evidence_snippet <- function(pattern, text, radius = 140) {
  match <- regexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  if (match[1] == -1) return(NULL)
  start <- max(1, match[1] - radius)
  end <- min(nchar(text), match[1] + attr(match, "match.length") + radius)
  prefix <- substr(text, 1, match[1])
  section_matches <- regmatches(prefix, gregexpr("\\[\\[SECTION:[^]]+\\]\\]", prefix, perl = TRUE))[[1]]
  section <- if (length(section_matches) > 0) tail(section_matches, 1) else "unstructured"
  section <- sub("^\\[\\[SECTION:[[:space:]]*", "", section)
  section <- sub("\\]\\]$", "", section)
  snippet <- substr(text, start, end)
  snippet <- gsub("\\[\\[SECTION:[^]]+\\]\\]", " ", snippet, perl = TRUE)
  list(section = section, snippet = normalize_space(snippet))
}

fetch_pmc_text <- function(pmid, pmcid = "") {
  suppressMessages(library(rentrez))
  if (!nzchar(pmcid)) {
    # Резервный путь для старых таблиц без прямого PMCID.
    es <- tryCatch(
      rentrez::entrez_search(db = "pmc", term = paste0("PMID:", pmid), retmax = 1),
      error = function(e) NULL
    )
    if (is.null(es) || is.null(es$ids) || length(es$ids) == 0) return("")
    pmcid <- as.character(es$ids[1])
  }
  pmcid <- sub("^PMC", "", as.character(pmcid), ignore.case = TRUE)
  resp <- tryCatch(
    rentrez::entrez_fetch(db = "pmc", id = pmcid, rettype = "xml"),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    # временный сбой или рейт-лимит — пауза 2 с и один повтор (паттерн из 01)
    Sys.sleep(2)
    resp <- tryCatch(
      rentrez::entrez_fetch(db = "pmc", id = pmcid, rettype = "xml"),
      error = function(e) NULL
    )
    if (is.null(resp)) return("")
  }
  # entrez_fetch может вернуть и строку (сырой XML), и объект ответа
  raw <- if (is.character(resp)) resp else resp$content
  txt <- strip_xml(raw)
  # «Статьи нет в PMC» / пустой ответ — короткий текст
  if (nchar(txt) < 500) return("")
  out <- txt
  return(out)
}

load_pmc_fulltext <- function(pm_df, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  df <- pmc_empty_df()
  eligible <- which(
    grepl("pmc\\.ncbi\\.nlm\\.nih\\.gov/articles/PMC", as.character(pm_df$fulltext_url),
            ignore.case = TRUE)
  )
  pmc_cfg <- settings$pmc %||% list()
  if (!isTRUE(pmc_cfg$enabled %||% TRUE) || length(eligible) == 0) return(df)
  max_records <- config_limit(pmc_cfg$max_records %||% 50L, default = 50L)
  if (!is.infinite(max_records)) eligible <- utils::head(eligible, as.integer(max_records))
  pmids <- as.character(pm_df$pmid[eligible])
  pmcids <- sub(".*/articles/(PMC[0-9]+).*$", "\\1", as.character(pm_df$fulltext_url[eligible]),
                ignore.case = TRUE)
  if (length(pmids) == 0) return(df)
  key <- read_secret(settings, "ncbi_api_key", "NCBI_API_KEY")
  if (nzchar(key)) suppressMessages(rentrez::set_entrez_key(key))
  pause <- pmc_cfg$pause_sec %||% 0.35
  n <- length(pmids)
  texts <- character(n)
  for (i in seq_len(n)) {
    Sys.sleep(pause)
    texts[i] <- fetch_pmc_text(pmids[i], pmcids[i])
  }
  out_df <- data.frame(pmid = pmids, fulltext = texts, fulltext_url = pm_df$fulltext_url[eligible], stringsAsFactors = FALSE)
  return(out_df)
}

open_fulltext_empty_df <- function() {
  data.frame(doi = character(0), source_id = character(0), fulltext = character(0),
             stringsAsFactors = FALSE)
}

fetch_open_fulltext <- function(url, timeout_sec = 45) {
  url <- scalar_text(url %||% "")
  if (!nzchar(url) || !grepl("^https://", url, ignore.case = TRUE)) return("")
  response <- tryCatch(
    httr::GET(
      url, httr::timeout(timeout_sec),
      httr::user_agent("SportGenParser/1.0 (open-access research client)")
    ),
    error = function(e) NULL
  )
  if (is.null(response) || response$status_code != 200L) return("")
  content_type <- tolower(httr::headers(response)[["content-type"]] %||% "")
  is_pdf <- grepl("application/pdf", content_type) || grepl("\\.pdf([?#].*)?$", url, ignore.case = TRUE)
  if (is_pdf) {
    if (!requireNamespace("pdftools", quietly = TRUE)) return("")
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path), add = TRUE)
    writeBin(response$content, path)
    pages <- tryCatch(pdftools::pdf_text(path), error = function(e) character(0))
    return(pdf_document_text(pages))
  }
  text <- tryCatch(rawToChar(response$content), error = function(e) "")
  html_body_text(text)
}

load_open_fulltext <- function(df, settings = NULL) {
  if (is.null(settings)) {
    settings <- jsonlite::fromJSON(file.path(parser_root(), "config", "settings.json"),
                                   simplifyVector = FALSE)
  }
  config <- settings$fulltext %||% list()
  if (identical(config$enabled, FALSE) || nrow(df) == 0) return(open_fulltext_empty_df())
  eligible <- which(
    tolower(df$is_open_access) == "true" & nzchar(df$fulltext_url) &
      !grepl("pmc\\.ncbi\\.nlm\\.nih\\.gov", df$fulltext_url, ignore.case = TRUE)
  )
  if (length(eligible) == 0) return(open_fulltext_empty_df())
  max_records <- config_limit(config$max_open_records %||% 50L, default = 50L)
  if (!is.infinite(max_records)) eligible <- utils::head(eligible, as.integer(max_records))
  rows <- vector("list", length(eligible))
  for (j in seq_along(eligible)) {
    i <- eligible[j]
    text <- fetch_open_fulltext(
      df$fulltext_url[i], timeout_sec = as.numeric(config$timeout_sec %||% 45)
    )
    if (isTRUE(nchar(text) >= as.integer(config$min_chars %||% 500L))) {
      rows[[j]] <- data.frame(
        doi = normalize_doi(df$doi[i]), source_id = df$source_id[i], fulltext = text,
        stringsAsFactors = FALSE
      )
    }
    Sys.sleep(as.numeric(config$pause_sec %||% 0.4))
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(open_fulltext_empty_df())
  do.call(rbind, rows)
}

# ---- Извлечение полей из полного текста ----

# Ключевые слова: если слово есть (без учёта регистра) — кладём метку
kw_collect <- function(kws, labels, txt) {
  if (is.na(txt) || !nzchar(txt)) {
    out <- ""
    return(out)
  }
  hits <- character(0)
  for (i in seq_along(kws)) {
    if (grepl(kws[i], txt, ignore.case = TRUE)) hits <- c(hits, labels[i])
  }
  out <- paste(hits, collapse = "; ")
  return(out)
}

# Первое совпадение паттерна; затем первое значение внутри найденного фрагмента.
grab <- function(pat, txt, group_pat = "([0-9.]+)") {
  if (is.na(txt) || !nzchar(txt)) return("")
  m <- regexpr(pat, txt, perl = TRUE, ignore.case = TRUE)
  if (m[1] == -1) return("")
  full <- regmatches(txt, m)
  group_match <- regexpr(group_pat, full, perl = TRUE, ignore.case = TRUE)
  if (group_match[1] == -1) return("")
  sub("\\.$", "", regmatches(full, group_match))
}

# Пробуем список (паттерн + паттерн группы); возвращаем первое найденное значение
grab_any <- function(opts, txt) {
  if (is.na(txt) || !nzchar(txt)) return("")
  for (i in seq_along(opts)) {
    v <- grab(opts[[i]]$pat, txt, opts[[i]]$group)
    if (nzchar(v)) {
      return(v)
    }
  }
  ""
}

# p-value: первое правдоподобное значение (0 <= p <= 1); без вложенных групп (R 4.6)
grab_plausible_p <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return("")
  number <- "(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)"
  exponent <- "(?:\\s*[×x]\\s*10\\s*(?:\\^)?\\s*[-−+]\\s*[0-9]+|e[-+]?[0-9]+)?"
  pat <- paste0("\\bp\\s*[<>=]+\\s*", number, exponent)
  matches <- regmatches(txt, gregexpr(pat, txt, perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(matches) == 0) return("")
  values <- sub("^.*[<>=]\\s*", "", matches, perl = TRUE)
  normalized <- gsub("[[:space:]]+", "", values)
  normalized <- gsub("[×x]10\\^?[-−]", "e-", normalized, perl = TRUE)
  normalized <- gsub("[×x]10\\^?\\+", "e+", normalized, perl = TRUE)
  numbers <- suppressWarnings(as.numeric(normalized))
  valid <- !is.na(numbers) & numbers >= 0 & numbers <= 1
  if (!any(valid)) return("")
  normalized[which(valid)[1]]
}

# Все поля целевой таблицы из полного текста
extract_legacy_fields <- function(txt) {
  if (is.na(txt)) txt <- ""
  abstract_txt <- section_subset(txt, c("abstract"))
  methods_txt <- section_subset(
    txt,
    c("метод", "материал", "участник", "выборк", "method", "material", "participant", "subject", "population", "sample", "cohort",
      "design", "inclusion", "exclusion", "procedure", "protocol", "statistical",
      "genom", "genotyp", "quality control")
  )
  results_txt <- section_subset(txt, c("результ", "таблиц", "table", "result", "finding", "outcome", "association", "effect"))
  overview_txt <- paste(abstract_txt, methods_txt, sep = "\n")
  methods_results_txt <- paste(methods_txt, results_txt, sep = "\n")

  # Тип публикации
  pub_type <- ""
  if (grepl("meta-analysis|systematic review", overview_txt, ignore.case = TRUE)) pub_type <- "мета-анализ / обзор"
  else if (grepl("case-control|case control", overview_txt, ignore.case = TRUE)) pub_type <- "case-control"
  else if (grepl("randomi[sz]ed (controlled )?trial|random allocation", overview_txt, ignore.case = TRUE)) pub_type <- "RCT"
  else if (grepl("cohort", overview_txt, ignore.case = TRUE)) pub_type <- "когортное"
  else if (grepl("observational", overview_txt, ignore.case = TRUE)) pub_type <- "наблюдательное"
  else if (grepl("review", overview_txt, ignore.case = TRUE)) pub_type <- "обзор"

  # Модель наследования
  inherit_model <- kw_collect(
    c("additive (model|inheritance|genotyping)", "dominant (model|inheritance)", "recessive (model|inheritance)", "codominant (model|inheritance)"),
    c("аддитивная", "доминантная", "рецессивная", "кодоминантная"),
    methods_txt
  )

  # Частота аллелей: буква аллеля + численное значение
  # Буква аллеля: буква — часть совпадения до первой пробела
  # Буква аллеля: отдельное слово (1-2 буквы) перед "allele";  + s+ отсекает хвосты слов типа "optimal alleles"
  m_al <- regexpr("\\b([A-Z]{1,2})\\s+allele", methods_results_txt, perl = TRUE, ignore.case = TRUE)
  allele_letter <- ""
  if (m_al[1] > 0 && !is.na(m_al[1])) {
    al_lens <- attr(m_al, "match.length")
    if (!is.na(al_lens) && m_al[1] + al_lens - 1 <= nchar(methods_results_txt)) {
      al_full <- substr(methods_results_txt, m_al[1], m_al[1] + al_lens - 1)
      allele_letter <- sub("\\s.*$", "", al_full)
    }
  }
  if (allele_letter == "") {
    m_al2 <- regexpr("\\b([A-Z]{1,2})\\s+allele\\s+[Ff]requency", methods_results_txt, perl = TRUE, ignore.case = TRUE)
    if (m_al2[1] > 0 && !is.na(m_al2[1])) {
      al_lens2 <- attr(m_al2, "match.length")
      if (!is.na(al_lens2) && m_al2[1] + al_lens2 - 1 <= nchar(methods_results_txt)) {
        al_full2 <- substr(methods_results_txt, m_al2[1], m_al2[1] + al_lens2 - 1)
        allele_letter <- sub("\\s.*$", "", al_full2)
      }
    }
  }
  allele_freq_val <- grab_any(list(
    list(pat = "[Ff]requency of (the )?[A-Z]{1,2} allele[^0-9]{0,40}([0-9.]+)", group = "([0-9.]+)")
  ), methods_results_txt)
  allele_freq <- if (nzchar(allele_letter) && nzchar(allele_freq_val)) {
    paste(allele_letter, allele_freq_val, sep = ": ")
  } else {
    ""
  }

  # Hardy-Weinberg
  hwe <- ""
  hwe_mentioned <- grepl("\\bhardy[- ]?weinberg\\b|\\bhwe\\b", methods_results_txt, ignore.case = TRUE)
  hwe_filtered <- grepl("exclud(ed|ing).*?(deviat|hardy[- ]?weinberg|hwe)|(deviat|hardy[- ]?weinberg|hwe).*?exclud",
                        methods_results_txt, ignore.case = TRUE, perl = TRUE)
  hwe_violated <- grepl("deviat(ed|ion|es).*?(hardy[- ]?weinberg|hwe)|(hardy[- ]?weinberg|hwe).*?(violat|not in|deviat)",
                        methods_results_txt, ignore.case = TRUE, perl = TRUE)
  if (hwe_filtered) {
    hwe <- "проверено (отклонения исключены)"
  } else if (hwe_violated) {
    hwe <- "нарушено"
  } else if (hwe_mentioned) {
    hwe <- "проверено"
  }

  # Тип выборки
  sample_type <- kw_collect(
    c("athlete", "healthy (adult|subject|participant|volunteer)", "patient(s)?"),
    c("спортсмены (вид спорта)", "здоровые взрослые", "пациенты"),
    methods_txt
  )

  # Этническая принадлежность
  ethnicity <- kw_collect(
    c("caucasian|european", "asian", "african", "hispanic|latin"),
    c("европейское происхождение", "азиатское", "африканское", "латиноамериканское"),
    methods_txt
  )

  # Размер выборки (N)
  sample_patterns <- list(
    list(pat = "total of ([0-9][0-9,]*)(?: [[:alpha:]-]+){0,4} (subjects|participants|patients|students|athletes|individuals)", group = "([0-9][0-9,]*)"),
    list(pat = "total of ([0-9][0-9,]*) (subjects|participants|patients|students|athletes|individuals)", group = "([0-9][0-9,]*)"),
    list(pat = "(?:study|sample|cohort) (?:included|comprised|enrolled|consisted of) (?:a total of )?([0-9][0-9,]*)", group = "([0-9][0-9,]*)"),
    list(pat = "(?:sample|cohort) of ([0-9][0-9,]*)", group = "([0-9][0-9,]*)"),
    list(pat = "([0-9][0-9,]*) (subjects|participants|patients|students|athletes|individuals) (?:were|who were)", group = "([0-9][0-9,]*)"),
    list(pat = "(?:sample|cohort)[^.;]{0,50}\\bN\\s*=\\s*([0-9][0-9,]*)", group = "([0-9][0-9,]*)"),
    list(pat = "sample size of ([0-9][0-9,]*)", group = "([0-9][0-9,]*)")
  )
  sample_size <- grab_any(sample_patterns, abstract_txt)
  if (!nzchar(sample_size)) sample_size <- grab_any(sample_patterns, methods_txt)
  if (!nzchar(sample_size) && !grepl("обзор", pub_type, fixed = TRUE)) {
    sample_size <- grab("\\bN\\s*=\\s*([0-9][0-9,]*)", methods_txt, "([0-9][0-9,]*)")
  }
  sample_size <- gsub(",", "", sample_size, fixed = TRUE)

  # Пол (M/F)
  male_patterns <- list(
    list(pat = "\\bmales?\\s*\\(\\s*N\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "\\bM\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "men\\s*\\(n\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "([0-9]+)\\s+males?", group = "([0-9]+)")
  )
  female_patterns <- list(
    list(pat = "\\bfemales?\\s*\\(\\s*N\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "\\bF\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "women\\s*\\(n\\s*=\\s*([0-9]+)", group = "([0-9]+)"),
    list(pat = "([0-9]+)\\s+females?", group = "([0-9]+)")
  )
  m_n <- grab_any(male_patterns, abstract_txt)
  if (!nzchar(m_n)) m_n <- grab_any(male_patterns, methods_txt)
  f_n <- grab_any(female_patterns, abstract_txt)
  if (!nzchar(f_n)) f_n <- grab_any(female_patterns, methods_txt)
  parts <- character(0)
  if (nzchar(m_n)) parts <- c(parts, paste0("M=", m_n))
  if (nzchar(f_n)) parts <- c(parts, paste0("F=", f_n))
  sex <- paste(parts, collapse = ", ")

  if (!nzchar(sample_size) && nzchar(m_n) && nzchar(f_n)) {
    sample_size <- as.character(as.numeric(m_n) + as.numeric(f_n))
  }

  # Возраст (средний ± SD): пара «среднее ± SD» из той же строки
  age <- grab("mean (participant|subject|athlete|cohort)? ?age[^0-9]{0,40}([0-9.]+\\s*±\\s*[0-9.]+)", methods_txt, "([0-9.]+\\s*±\\s*[0-9.]+)")
  if (!nzchar(age)) {
    mean_age <- grab_any(list(
      list(pat = "mean (participant|subject|athlete|cohort)? ?age[^0-9]{0,40}([0-9.]+)", group = "([0-9.]+)"),
      list(pat = "average age[^0-9]{0,40}([0-9.]+)", group = "([0-9.]+)")
    ), methods_txt)
    if (nzchar(mean_age)) age <- mean_age
  }
  if (!nzchar(age)) {
    age_range <- grab(
      "ages?\\s+(?:of|between)?\\s*([0-9]+\\s*(?:and|to|[-–])\\s*[0-9]+)",
      methods_txt,
      "([0-9]+\\s*(?:and|to|[-–])\\s*[0-9]+)"
    )
    if (nzchar(age_range)) age <- gsub("\\s*(and|to|[-–])\\s*", "–", age_range)
  }

  # Уровень физической активности
  pa_level <- kw_collect(
    c("accelerometer", "questionnaire|IPAQ|self-report", "\\bgps\\b"),
    c("акселерометр", "опросник", "GPS-трекинг"),
    methods_txt
  )

  # Фенотип
  phenotype <- kw_collect(
    c("vo2 ?max", "strength", "injur", "aerobic"),
    c("выносливость", "сила", "травматизм", "аэробная выносливость"),
    paste(overview_txt, results_txt)
  )

  # Метод измерения фенотипа
  measure_method <- kw_collect(
    c("bruce protocol", "(12|six|6)-minute", "handgrip|hand grip", "treadmill|incremental|ergometer"),
    c("протокол Bruce", "6/12-минутный тест", "динамометрия", "эргометрия"),
    methods_results_txt
  )

  # Ковариаты
  covariates <- kw_collect(
    c("bmi|body mass index", "smoking|smoke|tobacco", "diet|nutrition", "training (experience|history|load|volume)"),
    c("ИМТ", "курение", "диета", "тренировочный стаж"),
    methods_txt
  )

  # Основные результаты: OR, β, p, 95% CI
  or_v <- grab_any(list(
    list(pat = "odds ratio[^0-9]{0,40}([0-9.]+)", group = "([0-9.]+)")
  ), results_txt)
  beta_v <- grab_any(list(
    list(pat = "(?:β|beta)\\s*[-=]?\\s*(-?[0-9.]+)", group = "(-?[0-9.]+)")
  ), results_txt)
  p_v <- grab_plausible_p(results_txt)
  if (!nzchar(p_v)) p_v <- grab("\\bp[- ]?value[^0-9]{0,30}([0-9.]+)", results_txt)
  ci_v <- grab("95\\s*%?\\s*[Cc][Ii][^0-9-]{0,40}([0-9.]+\\s*[-–]\\s*[0-9.]+)", results_txt, "([0-9.]+\\s*[-–]\\s*[0-9.]+)")
  parts <- character(0)
  if (nzchar(or_v)) parts <- c(parts, paste0("OR=", or_v))
  if (nzchar(beta_v)) parts <- c(parts, paste0("β=", beta_v))
  if (nzchar(p_v)) parts <- c(parts, paste0("p=", p_v))
  if (nzchar(ci_v)) parts <- c(parts, paste0("95% CI ", ci_v))
  results <- paste(parts, collapse = "; ")

  # Направление эффекта
  effect_dir <- kw_collect(
    c("protective|beneficial", "risk|adverse", "no significant|not significant|non-significant"),
    c("благотворный аллель", "аллель риска", "эффект не найден"),
    results_txt
  )

  # Размер эффекта
  cohen <- grab("cohen'?s?\\s*d[^0-9-]{0,40}(-?[0-9.]+)", results_txt, "(-?[0-9.]+)")
  eta <- grab("(η²|eta\\s*squared)[^0-9.]{0,40}([0-9.]+)", results_txt, "([0-9.]+)")
  parts <- character(0)
  if (nzchar(cohen)) parts <- c(parts, paste0("Cohen's d=", cohen))
  if (nzchar(eta)) parts <- c(parts, paste0("η²=", eta))
  effect_size <- paste(parts, collapse = "; ")

  # Статистическая значимость после коррекции
  p_adj <- kw_collect(
    c("adjusted p|p[- ]?value adjusted|post-hoc correction|multiple comparisons"),
    c("значимость после коррекции"),
    results_txt
  )

  # Взаимодействие ген × среда
  gene_env <- kw_collect(
    c("gene-environment|genetic interaction", "(gene|genetic|genotype|snp)\\s*(x|×)\\s*(environment|physical activity|exercise|training)"),
    c("SNP × ФА"),
    results_txt
  )

  # Поправка на множественное тестирование
  multipletest <- kw_collect(
    c("bonferroni|bonferroni", "fdr|false discovery rate", "holm"),
    c("Bonferroni", "FDR", "Holm"),
    methods_results_txt
  )

  # Мощности исследования
  pw <- grab("power of the study[^0-9]{0,40}([0-9.]+)", methods_results_txt, "([0-9.]+)")
  power <- ""
  if (nzchar(pw)) {
    power <- paste0("мощность ", pw)
  } else if (grepl("post-hoc power|power analysis|adequately powered", methods_results_txt, ignore.case = TRUE)) {
    power <- "анализ мощности выполнен"
  }

  # Доступность данных
  data_avail <- kw_collect(
    c("\\bdbgap\\b", "\\bgeo\\b|geo accession", "dryad|zenodo|data are available|data availability"),
    c("dbGaP", "GEO", "репозиторий (Dryad/Zenodo)"),
    methods_results_txt
  )

  out <- list(
    pub_type = pub_type, inherit_model = inherit_model, allele_freq = allele_freq, hwe = hwe,
    sample_type = sample_type, ethnicity = ethnicity, sample_size = sample_size,
    sex = sex, age = age, pa_level = pa_level, phenotype = phenotype,
    measure_method = measure_method, covariates = covariates, results = results,
    effect_dir = effect_dir, effect_size = effect_size, p_adj = p_adj,
    gene_env = gene_env, multipletest = multipletest, power = power, data_avail = data_avail
  )
  return(out)
}

# Добавляем 21 колонку целевой схемы и заполняем из PMC-текста по PMID.
ENRICH_COLS <- c("pub_type", "inherit_model", "allele_freq", "hwe", "sample_type",
                  "ethnicity", "sample_size", "sex", "age", "pa_level", "phenotype",
                  "measure_method", "covariates", "results", "effect_dir", "effect_size",
                  "p_adj", "gene_env", "multipletest", "power", "data_avail")

metadata_publication_type <- function(value) {
  value <- tolower(value %||% "")
  if (!nzchar(value)) return("")
  if (grepl("meta-analysis", value)) return("мета-анализ")
  if (grepl("systematic review", value)) return("систематический обзор")
  if (grepl("non.?random|not random", value)) return("нерандомизированное исследование")
  if (grepl("(^|; *)(randomized controlled trial|randomised controlled trial|rct)($|;)", value)) return("RCT")
  if (grepl("clinical trial protocol", value)) return("протокол клинического испытания")
  if (grepl("clinical trial|controlled clinical trial", value)) return("клиническое испытание (рандомизация не подтверждена)")
  if (grepl("case-control", value)) return("case-control")
  if (grepl("cohort", value)) return("когортное")
  if (grepl("review", value)) return("обзор")
  if (grepl("journal article|journal-article|article", value)) return("статья")
  value
}

enrich_final_table <- function(df, pmc_df = pmc_empty_df(), open_fulltext_df = open_fulltext_empty_df(), settings = NULL) {
  for (column in c(ENRICH_COLS, "article_id", "text_source", "missing_fields", "evidence_profile", "evidence_reasons")) df[[column]] <- rep("", nrow(df))
  df$extraction_confidence <- rep("не оценивалась", nrow(df))
  df$extraction_evidence <- rep("", nrow(df))
  associations <- list()
  documents <- list()
  symbols <- load_gene_symbols(settings %||% load_project_settings())
  pmc_index <- if (nrow(pmc_df) > 0) match_nonempty(as.character(df$pmid), pmc_df$pmid) else rep(NA_integer_, nrow(df))
  if ("fulltext_url" %in% names(pmc_df)) {
    by_url <- match_nonempty(df$fulltext_url, pmc_df$fulltext_url)
    pmc_index[is.na(pmc_index)] <- by_url[is.na(pmc_index)]
  }
  doi_index <- if (nrow(open_fulltext_df) > 0) {
    match_nonempty(vapply(df$doi, normalize_doi, character(1)), open_fulltext_df$doi)
  } else rep(NA_integer_, nrow(df))
  source_index <- if (nrow(open_fulltext_df) > 0) {
    match_nonempty(as.character(df$source_id), open_fulltext_df$source_id)
  } else rep(NA_integer_, nrow(df))

  for (i in seq_len(nrow(df))) {
    text_source <- "abstract"
    blocks <- c(
      "[[SECTION: Metadata]]",
      normalize_space(paste(df$title[i], df$publication_type[i], df$mesh[i], sep = ". ")),
      "[[SECTION: Abstract]]",
      df$abstract[i]
    )
    if (!is.na(pmc_index[i]) && nzchar(pmc_df$fulltext[pmc_index[i]])) {
      blocks <- c(blocks, pmc_df$fulltext[pmc_index[i]])
      text_source <- "pmc"
    } else {
      open_index <- if (!is.na(doi_index[i])) doi_index[i] else source_index[i]
      if (!is.na(open_index) && nzchar(open_fulltext_df$fulltext[open_index])) {
        blocks <- c(blocks, "[[SECTION: Open full text]]", open_fulltext_df$fulltext[open_index])
        text_source <- "open_fulltext"
      }
    }
    txt <- paste(blocks, collapse = "\n")
    f <- extract_detail_fields(txt)
    documents[[i]] <- txt
    identity_text <- paste(df$title[i], df$abstract[i], df$mesh[i], section_subset(txt, c("abstract", "method", "material", "result", "body", "open full", "table", "page", "метод", "результ")), collapse = "\n")
    df$gene[i] <- extract_gene(identity_text, symbols)
    df$snp[i] <- extract_snp(identity_text)
    df$text_source[i] <- text_source
    article_id <- article_identity(df[i, , drop = FALSE])
    df$article_id[i] <- article_id
    assoc <- extract_associations(txt, article_id, symbols, df$title[i])
    if (nrow(assoc)) associations[[length(associations) + 1L]] <- assoc
    metadata_type <- metadata_publication_type(df$publication_type[i])
    # Тип публикации из индекса журнала надёжнее упоминаний других дизайнов
    # во введении/обзоре (например, review of randomized trials != RCT).
    if ((nzchar(metadata_type) && metadata_type != "статья") || !nzchar(f$pub_type)) {
      f$pub_type <- metadata_type
      ev <- attr(f, "evidence")
      ev$pub_type <- list(value = metadata_type, section = "Metadata", snippet = df$publication_type[i], status = "метаданные источника")
      attr(f, "evidence") <- ev
    }
    ev <- attr(f, "evidence") %||% list()
    for (field in c("gene", "snp")) {
      if (nzchar(df[[field]][i])) ev[[field]] <- list(value = df[[field]][i], section = "Title / Abstract / full text", snippet = paste(unique(text_units(identity_text)$snippet[grepl(if (field == "snp") "rs[0-9]+" else "gene|genetic|genotyp|variant|polymorphism|ген|полиморф", text_units(identity_text)$snippet, ignore.case = TRUE)]), collapse = "\n"), status = "требует проверки")
    }
    attr(f, "evidence") <- ev
    for (column in ENRICH_COLS) df[[column]][i] <- f[[column]]
    df$extraction_confidence[i] <- "требует проверки"
    df$missing_fields[i] <- paste(names(f)[!vapply(f, nzchar, logical(1))], collapse = "; ")
    evidence <- attr(f, "evidence") %||% list()
    evidence <- lapply(evidence, function(item) { item$article_id <- article_id; item$text_source <- text_source; item$source_url <- if (text_source == "abstract") df$url[i] else df$fulltext_url[i]; item })
    if (length(evidence) > 0) {
      df$extraction_evidence[i] <- jsonlite::toJSON(
        evidence, auto_unbox = TRUE, null = "null", digits = NA
      )
    }
  }
  attr(df, "associations") <- if (length(associations)) do.call(rbind, associations) else empty_associations()
  attr(df, "documents") <- setNames(documents, df$article_id)
  df <- sort_articles(df)
  df
}
