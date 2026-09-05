script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
this_file <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[1]) else "tests/run_live_tests.R"
ROOT <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)
options(article_parser.root = ROOT)

for (file in c(
  "src/00_common.R", "src/01_pubmed.R", "src/02_sciencedirect.R",
  "src/03_openalex.R", "src/03_crossref.R", "src/04_merge_export.R",
  "src/05_pmc_fulltext.R", "src/05b_extraction.R", "src/05c_documents.R",
  "src/06_pipeline.R", "src/07_evidence_archive.R"
)) source(file.path(ROOT, file))

passed <- 0L
failed <- 0L
skipped <- 0L

check_live <- function(description, condition) {
  if (isTRUE(condition)) {
    passed <<- passed + 1L
    cat("PASS", description, "\n")
  } else {
    failed <<- failed + 1L
    cat("FAIL", description, "\n")
  }
}

skip_live <- function(description, reason) {
  skipped <<- skipped + 1L
  cat("SKIP", description, "-", reason, "\n")
}

settings <- load_project_settings()
settings$pubmed$max_records <- 3L
settings$pubmed$batch_size <- 20L
settings$pubmed$candidate_multiplier <- 10L
settings$sciencedirect$max_records <- 3L
settings$openalex$max_records <- 3L
settings$openalex$candidate_multiplier <- 20L
settings$crossref$max_lookups <- 3L
settings$pmc$enabled <- FALSE
settings$fulltext$enabled <- FALSE
queries <- pipeline_queries(settings)

pubmed <- load_pubmed(queries$pubmed, settings)
check_live("PubMed returns a bounded live result", nrow(pubmed) > 0 && nrow(pubmed) <= 3)
check_live("PubMed result has identifiers and titles",
           all(nzchar(pubmed$pmid)) && all(nzchar(pubmed$title)))

openalex <- load_openalex(queries$openalex, settings)
check_live("OpenAlex returns strictly filtered Russian/Russian-affiliated works",
           nrow(openalex) > 0 && nrow(openalex) <= 3)
if (nrow(openalex) > 0) {
  oa_text <- paste(openalex$title, openalex$abstract, openalex$mesh, sep = " | ")
  check_live("Every OpenAlex row passes genetics AND sport/activity filter",
             all(vapply(oa_text, strict_topic_match, logical(1))))
}

date_settings <- settings
date_settings$publication_year <- list(from = "2020", to = "2024")
date_settings$pubmed$max_records <- 2L
date_settings$openalex$max_records <- 2L
date_settings$sciencedirect$max_records <- 2L
dated_pubmed <- load_pubmed(queries$pubmed, date_settings)
check_live("PubMed applies the requested publication-year range",
           nrow(dated_pubmed) > 0 && all(as.integer(dated_pubmed$year) >= 2020L &
                                          as.integer(dated_pubmed$year) <= 2024L))
dated_openalex <- load_openalex(queries$openalex, date_settings)
check_live("OpenAlex applies the requested publication-year range",
           nrow(dated_openalex) > 0 && all(as.integer(dated_openalex$year) >= 2020L &
                                            as.integer(dated_openalex$year) <= 2024L))

crossref <- crossref_by_doi("10.1038/s41586-020-2649-2", settings$crossref)
check_live("Crossref resolves a known DOI",
           !is.null(crossref) && identical(crossref$doi, "10.1038/s41586-020-2649-2"))

pmc_text <- fetch_pmc_text("42074594", "PMC13116978")
check_live("PMC full text is downloaded and split into sections",
           nchar(pmc_text) > 500 && grepl("[[SECTION:", pmc_text, fixed = TRUE))
pmc_fields <- extract_detail_fields(pmc_text)
check_live("Live PMC text populates multiple target fields",
           sum(vapply(pmc_fields, nzchar, logical(1))) >= 3)

open_text <- fetch_open_fulltext(
  "https://www.smjournal.ru/jour/article/download/680/511", timeout_sec = 30
)
check_live("An OpenAlex-linked open full text is readable",
           nchar(open_text) > 500 && grepl("ACE", open_text, fixed = TRUE))

if (nzchar(Sys.getenv("ELSEVIER_API_KEY", unset = ""))) {
  elsevier <- load_sciencedirect(queries$sciencedirect, settings)
  check_live("Elsevier returns a bounded result through Search or Scopus fallback",
             nrow(elsevier) > 0 && nrow(elsevier) <= 3)
  if (nrow(elsevier) > 0) {
    check_live("Elsevier reports its retrieval route",
               all(elsevier$retrieval_method %in% c("sciencedirect_api", "scopus_elsevier_fallback")))
  }
  dated_elsevier <- load_sciencedirect(queries$sciencedirect, date_settings)
  check_live("Elsevier applies the requested publication-year range",
             nrow(dated_elsevier) > 0 && all(as.integer(dated_elsevier$year) >= 2020L &
                                               as.integer(dated_elsevier$year) <= 2024L))
} else {
  skip_live("Elsevier live API", "ELSEVIER_API_KEY is not set")
}

settings$pubmed$max_records <- 1L
settings$sciencedirect$max_records <- 1L
settings$openalex$max_records <- 1L
settings$crossref$max_lookups <- 1L
settings$out_dir <- tempfile("sportgen-live-export-")
sample <- run_pipeline(settings = settings, queries = queries, export = TRUE)
expected_sources <- c("PubMed", "OpenAlex")
if (nzchar(Sys.getenv("ELSEVIER_API_KEY", unset = ""))) expected_sources <- c(expected_sources, "ScienceDirect")
check_live("Complete pipeline merges every enabled available source",
           all(expected_sources %in% unique(sample$source)))
check_live("Pipeline writes a machine-readable run report",
           file.exists(file.path(settings$out_dir, "run_report.json")))
check_live("CSV export is readable",
           file.exists(file.path(settings$out_dir, "articles.csv")) &&
             nrow(readr::read_csv(file.path(settings$out_dir, "articles.csv"), show_col_types = FALSE)) == nrow(sample))
check_live("XLSX export is readable",
           file.exists(file.path(settings$out_dir, "articles.xlsx")) &&
             nrow(openxlsx::read.xlsx(file.path(settings$out_dir, "articles.xlsx"))) == nrow(sample))
unlink(settings$out_dir, recursive = TRUE, force = TRUE)

cat("\nLIVE RESULT", passed, "passed;", failed, "failed;", skipped, "skipped\n")
if (failed > 0) quit(status = 1)
