# Полный цикл автопарсинга.
# Запуск из любой директории: Rscript /путь/к/проекту/run_all.R

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
ROOT <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE))
} else {
  getwd()
}
options(article_parser.root = ROOT)

for (file in c(
  "src/00_common.R", "src/01_pubmed.R", "src/02_sciencedirect.R",
  "src/03_openalex.R", "src/03_crossref.R", "src/04_merge_export.R",
  "src/05_pmc_fulltext.R", "src/06_pipeline.R"
)) source(file.path(ROOT, file))

settings <- load_project_settings()
queries <- pipeline_queries(settings)
cat("Запрос PubMed:", queries$pubmed, "\n")
cat("Запрос ScienceDirect:", queries$sciencedirect, "\n")
cat("Запрос OpenAlex:", queries$openalex, "\n")

final <- run_pipeline(
  settings = settings,
  queries = queries,
  export = TRUE,
  progress = function(stage, detail) cat(sprintf("[%s] %s\n", stage, detail))
)
cat("Экспортировано:", nrow(final), "строк\n")
