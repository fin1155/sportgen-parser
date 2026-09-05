# A transparent triage worksheet, inspired by the separation of quality,
# precision and relevance in EvidenceGrade. This is NOT GRADE or EvidenceGrade.
EVIDENCE_METHOD_VERSION <- "sportgen-triage-1"
REVIEWABLE_COLS <- c("gene", "snp", ENRICH_COLS)

assess_evidence <- function(df) {
  if (!nrow(df)) {
    df$evidence_profile <- character()
    df$evidence_reasons <- character()
    return(df)
  }
  associations <- attr(df, "associations") %||% empty_associations()
  for (i in seq_len(nrow(df))) {
    a <- associations[associations$article_id == df$article_id[i], , drop = FALSE]
    reasons <- list(
      method = EVIDENCE_METHOD_VERSION,
      design = df$pub_type[i],
      quality = "Риск систематической ошибки требует экспертной проверки.",
      precision = if (nrow(a) && any(nzchar(a$ci))) "Извлечены доверительные интервалы; точность зависит от исхода и масштаба эффекта." else "Связанный с SNP доверительный интервал не извлечён.",
      relevance = "Проверить соответствие популяции, фенотипа, аллеля и модели конкретному исследовательскому вопросу.",
      replication = "Независимость выборок и воспроизведение ассоциации автоматически не установлены.",
      missing = df$missing_fields[i],
      text_source = df$text_source[i],
      flags = character()
    )
    if (identical(df$text_source[i], "abstract")) reasons$flags <- c(reasons$flags, "Доступна только аннотация/метаданные.")
    if (!nzchar(df$hwe[i])) reasons$flags <- c(reasons$flags, "HWE: сведений не извлечено; это не означает отсутствие проверки.")
    if (!nzchar(df$multipletest[i])) reasons$flags <- c(reasons$flags, "Поправка на множественное тестирование: сведений не извлечено.")
    if (nrow(a) && any(grepl("неоднозначно", a$status))) reasons$flags <- c(reasons$flags, "Есть неоднозначные связи SNP и статистических результатов.")
    df$evidence_profile[i] <- if (!nrow(a)) "Недостаточно извлечённых данных для оценки ассоциации" else "Предварительный профиль; нужна экспертная оценка"
    df$evidence_reasons[i] <- jsonlite::toJSON(reasons, auto_unbox = TRUE)
  }
  df
}

association_summary <- function(df) {
  a <- attr(df, "associations") %||% empty_associations()
  good <- nzchar(a$snp) & !grepl(",", a$snp, fixed = TRUE) & nzchar(a$phenotype) & nzchar(a$allele)
  a <- a[good & !grepl("неоднозначно", a$status), , drop = FALSE]
  empty <- data.frame(snp = character(), allele = character(), phenotype = character(), model = character(), publications = integer(), article_ids = character(), conclusion = character())
  if (!nrow(a)) return(empty)
  key <- paste(a$snp, tolower(a$allele), tolower(a$phenotype), tolower(a$model), sep = "\r")
  rows <- lapply(split(seq_len(nrow(a)), key), function(ids) {
    x <- a[ids, , drop = FALSE]
    data.frame(snp = x$snp[1], allele = x$allele[1], phenotype = x$phenotype[1], model = x$model[1],
               publications = length(unique(x$article_id)), article_ids = paste(unique(x$article_id), collapse = "; "),
               conclusion = "Число публикаций не равно числу независимых выборок. Сопоставимость и репликацию проверить вручную.")
  })
  do.call(rbind, rows)
}

bundle_object <- function(df) {
  list(schema_version = 1L, method_version = EVIDENCE_METHOD_VERSION,
       created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
       articles = as.list(df), associations = attr(df, "associations") %||% empty_associations(),
       documents = attr(df, "documents") %||% list(), report = attr(df, "report") %||% list(),
       review_log = attr(df, "review_log") %||% list())
}

write_research_archive <- function(df, path) {
  jsonlite::write_json(bundle_object(df), path, auto_unbox = FALSE, pretty = TRUE, null = "null", na = "null")
}

read_research_archive <- function(path) {
  if (file.info(path)$size > 100 * 1024^2) stop("Архив превышает 100 МБ.")
  obj <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(as.integer(unlist(obj$schema_version)), 1L)) stop("Неподдерживаемая версия архива.")
  if (!is.list(obj$articles) || !all(c(ARTICLE_COLS, "article_id") %in% names(obj$articles))) stop("В архиве отсутствуют обязательные поля статей.")
  columns <- lapply(obj$articles, function(x) {
    if (!is.list(x) || any(lengths(x) != 1L)) stop("Неверная структура столбца архива.")
    vapply(x, function(value) { if (!is.atomic(value) || length(value) != 1L) stop("Неверное значение архива."); as.character(value) }, character(1))
  })
  if (length(unique(lengths(columns))) > 1L) stop("Столбцы архива имеют разную длину.")
  df <- as.data.frame(columns, stringsAsFactors = FALSE)
  if (anyDuplicated(df$article_id) || any(!nzchar(df$article_id))) stop("ID статей должны быть непустыми и уникальными.")
  for (name in setdiff(names(TABLE_COLUMN_LABELS), names(df))) df[[name]] <- rep("", nrow(df))
  documents <- obj$documents %||% list()
  if (length(documents) && (!is.list(documents) || !all(names(documents) %in% df$article_id))) stop("Тексты не соответствуют статьям архива.")
  documents <- lapply(documents, function(x) { if (length(x) != 1L || !is.character(x[[1]])) stop("Неверная структура текста."); x[[1]] })
  a <- empty_associations()
  if (length(obj$associations)) {
    rows <- lapply(obj$associations, function(row) {
      if (!is.list(row) || !all(association_empty_cols %in% names(row))) stop("Неверная структура ассоциаций.")
      values <- vapply(row[association_empty_cols], function(x) {
        if (length(x) != 1L || !is.atomic(x)) stop("Неверное значение ассоциации.")
        as.character(x)
      }, character(1))
      as.data.frame(as.list(values), stringsAsFactors = FALSE)
    })
    a <- do.call(rbind, rows)
    if (!all(a$article_id %in% df$article_id)) stop("Ассоциация относится к отсутствующей статье.")
  }
  attr(df, "associations") <- a
  attr(df, "documents") <- documents
  attr(df, "report") <- obj$report %||% list()
  attr(df, "review_log") <- obj$review_log %||% list()
  df
}

reprocess_archive <- function(df) {
  documents <- attr(df, "documents") %||% list()
  if (!length(documents)) stop("В архиве нет сохранённых текстов для повторного извлечения.")
  original_associations <- list()
  symbols <- load_gene_symbols(load_project_settings())
  for (i in seq_len(nrow(df))) {
    text <- documents[[df$article_id[i]]] %||% ""
    if (!nzchar(text)) {
      previous <- attr(df, "associations") %||% empty_associations()
      original_associations[[i]] <- previous[previous$article_id == df$article_id[i], , drop = FALSE]
      next
    }
    fields <- extract_detail_fields(text)
    metadata <- metadata_publication_type(df$publication_type[i])
    evidence <- attr(fields, "evidence") %||% list()
    if ((nzchar(metadata) && metadata != "статья") || !nzchar(fields$pub_type)) {
      fields$pub_type <- metadata
      if (nzchar(metadata)) evidence$pub_type <- list(value = metadata, section = "Metadata", snippet = df$publication_type[i], status = "метаданные источника")
    }
    evidence <- lapply(evidence, function(item) { item$article_id <- df$article_id[i]; item$text_source <- df$text_source[i]; item$source_url <- if (df$text_source[i] == "abstract") df$url[i] else df$fulltext_url[i]; item })
    # Preserve user-verified fields and their audit trail across reprocessing.
    prior <- tryCatch(jsonlite::fromJSON(df$extraction_evidence[i], simplifyVector = FALSE), error = function(e) list())
    for (name in ENRICH_COLS) {
      if (identical(prior[[name]]$status, "проверено пользователем")) { evidence[[name]] <- prior[[name]]; next }
      df[[name]][i] <- fields[[name]]
    }
    for (name in c("gene", "snp")) {
      if (identical(prior[[name]]$status, "проверено пользователем")) evidence[[name]] <- prior[[name]]
    }
    df$missing_fields[i] <- paste(ENRICH_COLS[!vapply(df[i, ENRICH_COLS, drop = FALSE], function(x) nzchar(x[1]), logical(1))], collapse = "; ")
    df$extraction_confidence[i] <- if (any(vapply(evidence, function(x) identical(x$status, "проверено пользователем"), logical(1)))) "есть проверенные поля; остальные требуют проверки" else "требует проверки"
    if (!identical(prior$gene$status, "проверено пользователем")) df$gene[i] <- extract_gene(paste(df$title[i], section_subset(text, c("method", "result", "abstract", "body", "open full", "table", "page", "метод", "результ"))), symbols)
    if (!identical(prior$snp$status, "проверено пользователем")) df$snp[i] <- extract_snp(paste(df$title[i], section_subset(text, c("method", "result", "abstract", "body", "open full", "table", "page", "метод", "результ"))))
    for (name in c("gene", "snp")) {
      if (!identical(prior[[name]]$status, "проверено пользователем") && nzchar(df[[name]][i]))
        evidence[[name]] <- list(value = df[[name]][i], snippet = paste(df$title[i], text, sep = "\n"), section = "Сохранённый текст",
          article_id = df$article_id[i], text_source = df$text_source[i], status = "требует проверки")
    }
    df$extraction_evidence[i] <- jsonlite::toJSON(evidence, auto_unbox = TRUE)
    original_associations[[i]] <- extract_associations(text, df$article_id[i], symbols, df$title[i])
  }
  attr(df, "associations") <- if (length(original_associations)) do.call(rbind, original_associations) else empty_associations()
  assess_evidence(sort_articles(df))
}

review_article_field <- function(df, article_id, field, value, basis) {
  if (!field %in% REVIEWABLE_COLS) stop("Это поле нельзя редактировать в проверке.")
  if (!nzchar(trimws(basis))) stop("Укажите основание исправления или подтверждения.")
  i <- match(article_id, df$article_id)
  if (is.na(i)) stop("Статья не найдена.")
  before <- df[[field]][i]
  evidence <- tryCatch(jsonlite::fromJSON(df$extraction_evidence[i], simplifyVector = FALSE), error = function(e) list())
  evidence[[field]] <- list(value = value, snippet = basis, section = "Ручная проверка", article_id = article_id, status = "проверено пользователем")
  df[[field]][i] <- value
  df$extraction_evidence[i] <- jsonlite::toJSON(evidence, auto_unbox = TRUE)
  df$extraction_confidence[i] <- "есть проверенные поля; остальные требуют проверки"
  df$missing_fields[i] <- paste(ENRICH_COLS[!vapply(df[i, ENRICH_COLS, drop = FALSE], function(x) nzchar(x[1]), logical(1))], collapse = "; ")
  log <- attr(df, "review_log") %||% list()
  log[[length(log) + 1L]] <- list(article_id = article_id, field = field, before = before, after = value, basis = basis, at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  attr(df, "review_log") <- log
  assess_evidence(df)
}

export_research_bundle <- function(df, settings) {
  configured <- settings$out_dir %||% "out"
  directory <- if (grepl("^(/|[A-Za-z]:)", configured)) configured else file.path(parser_root(), configured)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  write_research_archive(df, file.path(directory, "research_archive.json"))
  readr::write_csv(attr(df, "associations") %||% empty_associations(), file.path(directory, "associations.csv"))
  readr::write_csv(association_summary(df), file.path(directory, "evidence_summary.csv"))
}

write_research_workbook <- function(df, path, selected = names(df)) {
  tables <- list(Статьи = select_output_columns(df, selected), Ассоциации = attr(df, "associations") %||% empty_associations(),
                 Сводка = association_summary(df))
  log <- attr(df, "review_log") %||% list()
  tables$Проверка <- if (length(log)) do.call(rbind, lapply(log, function(x) as.data.frame(lapply(x, function(v) paste(unlist(v), collapse = " ")), stringsAsFactors = FALSE))) else
    data.frame(Статус = "Ручная проверка не проводилась")
  openxlsx::write.xlsx(tables, path, overwrite = TRUE, firstRow = TRUE, colWidths = 30)
}


evidence_display <- function(df) {
  if (!nrow(df)) return(data.frame())
  rows <- lapply(seq_len(nrow(df)), function(i) {
    reasons <- jsonlite::fromJSON(df$evidence_reasons[i], simplifyVector = FALSE)
    data.frame(Статья = df$title[i], Дизайн = df$pub_type[i],
      Основание = switch(df$text_source[i], pmc = "Полный текст PMC", open_fulltext = "Открытый полный текст", "Аннотация / метаданные"),
      Качество = reasons$quality, Точность = reasons$precision, Применимость = reasons$relevance,
      Репликация = reasons$replication, Ограничения = paste(unlist(reasons$flags), collapse = " "), check.names = FALSE)
  })
  do.call(rbind, rows)
}
