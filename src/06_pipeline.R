load_project_settings <- function(path = file.path(parser_root(), "config", "settings.json")) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

pipeline_queries <- function(settings) {
  list(
    pubmed = read_query_for("pubmed", settings),
    sciencedirect = read_query_for("sciencedirect", settings),
    openalex = read_query_for("openalex", settings)
  )
}

pipeline_report <- function(final, sources, queries, started_at, settings = list()) {
  fill_columns <- intersect(c("gene", "snp", ENRICH_COLS), names(final))
  filled <- setNames(lapply(fill_columns, function(column) {
    sum(!is.na(final[[column]]) & nzchar(trimws(as.character(final[[column]]))))
  }), fill_columns)
  source_info <- lapply(sources, function(df) {
    list(
      retrieved = nrow(df),
      total_reported = as.integer(attr(df, "total_results") %||% nrow(df)),
      retrieval_methods = unique(as.character(df$retrieval_method[nzchar(df$retrieval_method)]))
    )
  })
  year_range <- publication_year_range(settings)
  list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    duration_seconds = round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 2),
    rows = nrow(final),
    sources = source_info,
    filled_fields = filled,
    queries = queries,
    publication_year = list(
      from = if (is.na(year_range$from)) "" else year_range$from,
      to = if (is.na(year_range$to)) "" else year_range$to
    )
  )
}

write_pipeline_report <- function(report, settings) {
  configured <- settings$out_dir %||% "out"
  out_dir <- if (grepl("^(/|[A-Za-z]:)", configured)) configured else file.path(parser_root(), configured)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(report, file.path(out_dir, "run_report.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
}

run_pipeline <- function(settings = NULL, queries = NULL, export = TRUE,
                         progress = function(stage, detail = "") invisible(NULL)) {
  started_at <- Sys.time()
  if (is.null(settings)) settings <- load_project_settings()
  if (is.null(queries)) queries <- pipeline_queries(settings)

  progress("pubmed", "Поиск PubMed")
  pm <- tryCatch(
    load_pubmed(queries$pubmed, settings),
    error = function(e) {
      warning("PubMed: ", conditionMessage(e), call. = FALSE)
      pubmed_empty_df()
    }
  )

  progress("sciencedirect", "Поиск Elsevier")
  sd <- tryCatch(
    load_sciencedirect(queries$sciencedirect, settings),
    error = function(e) {
      warning("ScienceDirect: ", conditionMessage(e), call. = FALSE)
      sd_empty_df()
    }
  )

  progress("openalex", "Поиск русскоязычных публикаций")
  oa <- tryCatch(
    load_openalex(queries$openalex, settings),
    error = function(e) {
      warning("OpenAlex: ", conditionMessage(e), call. = FALSE)
      openalex_empty_df()
    }
  )

  progress("crossref", "Дополнение библиографических данных")
  pm <- enrich_crossref(pm, settings)
  sd <- enrich_crossref(sd, settings)
  oa <- enrich_crossref(oa, settings)

  progress("merge", "Объединение и удаление дубликатов")
  final <- build_final_table(pm, sd, oa, settings)

  progress("fulltext", "Получение доступных полных текстов")
  pmc <- load_pmc_fulltext(pm, settings)
  open_text <- load_open_fulltext(final, settings)

  progress("extract", "Извлечение полей")
  final <- enrich_final_table(final, pmc, open_text)

  report <- pipeline_report(final, list(PubMed = pm, Elsevier = sd, OpenAlex = oa),
                            queries, started_at, settings)
  attr(final, "report") <- report
  if (isTRUE(export)) {
    progress("export", "Экспорт CSV и XLSX")
    export_table(final, settings)
    write_pipeline_report(report, settings)
  }
  progress("done", paste("Готово:", nrow(final), "строк"))
  final
}
