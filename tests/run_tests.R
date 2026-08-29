script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
this_file <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[1]) else "tests/run_tests.R"
ROOT <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)
options(article_parser.root = ROOT)

source(file.path(ROOT, "src", "00_common.R"))
source(file.path(ROOT, "src", "01_pubmed.R"))
source(file.path(ROOT, "src", "02_sciencedirect.R"))
source(file.path(ROOT, "src", "03_openalex.R"))
source(file.path(ROOT, "src", "03_crossref.R"))
source(file.path(ROOT, "src", "04_merge_export.R"))
source(file.path(ROOT, "src", "05_pmc_fulltext.R"))
source(file.path(ROOT, "src", "06_pipeline.R"))

passed <- 0L
failed <- 0L

check <- function(description, condition) {
  if (isTRUE(condition)) {
    passed <<- passed + 1L
    cat("PASS", description, "\n")
  } else {
    failed <<- failed + 1L
    cat("FAIL", description, "\n")
  }
}

check_equal <- function(description, actual, expected) {
  if (!identical(actual, expected)) {
    cat("  expected:", paste(expected, collapse = " | "), "\n")
    cat("  actual:  ", paste(actual, collapse = " | "), "\n")
  }
  check(description, identical(actual, expected))
}

check_error <- function(description, expression, pattern) {
  message <- tryCatch({
    force(expression)
    ""
  }, error = function(error) conditionMessage(error))
  check(description, nzchar(message) && grepl(pattern, message, fixed = TRUE))
}

check_equal("Blank publication year disables the filter",
            publication_year_range(list(publication_year = list(from = "", to = "")))$active,
            FALSE)
bounded_years <- publication_year_range(list(publication_year = list(from = "2020", to = "2022")))
check_equal("Publication year range is inclusive",
            unname(unlist(bounded_years[c("from", "to")])), c(2020L, 2022L))
upper_only_years <- publication_year_range(list(publication_year = list(from = "", to = "2022")))
check_equal("Publication year range accepts only an upper bound",
            c(is.na(upper_only_years$from), upper_only_years$to), c(1L, 2022L))
check_error("Publication year must contain four digits",
            publication_year_range(list(publication_year = list(from = "20", to = ""))),
            "четырёх цифр")
check_error("Publication year range rejects reversed bounds",
            publication_year_range(list(publication_year = list(from = "2022", to = "2020"))),
            "не может быть позже")

expected_table_columns <- c(
  "source", ARTICLE_COLS, "gene", "snp", ENRICH_COLS,
  "extraction_confidence", "extraction_evidence"
)
check_equal("Column customizer covers every exported field",
            names(TABLE_COLUMN_LABELS), expected_table_columns)
check_equal("All-fields preset preserves the complete export order",
            TABLE_COLUMN_PRESETS$all, expected_table_columns)
column_fixture <- data.frame(
  source = "PubMed", title = "ACE in athletes", year = "2024", doi = "10.1000/test",
  stringsAsFactors = FALSE
)
check_equal("Selected columns keep the user's requested order",
            names(select_output_columns(column_fixture, c("year", "title", "source"))),
            c("year", "title", "source"))
check_equal("Unknown column names are ignored safely",
            names(select_output_columns(column_fixture, c("doi", "not_a_column"))), "doi")
check_equal("Empty selection falls back to useful available fields",
            names(select_output_columns(column_fixture, character(0))),
            c("source", "title", "year", "doi"))

check_equal("SNP extraction and numeric sorting",
            extract_snp("RS1815739, rs2 and rs1815739"), "rs2, rs1815739")
check_equal("ACE is not found inside placebo/Paced",
            extract_gene("placebo-controlled Paced test", c("ACE")), "")
check_equal("Exact HGNC symbols are found",
            extract_gene("ACE and ACTN3 variants", c("ACE", "ACTN3")), "ACE, ACTN3")
settings_for_symbols <- jsonlite::fromJSON(file.path(ROOT, "config", "settings.json"), simplifyVector = FALSE)
secret_settings <- list(runtime_secrets = list(TEST_SECRET = "session-only"))
check_equal("Session secret takes precedence without touching the environment",
            read_secret(secret_settings, "legacy_secret", "TEST_SECRET"), "session-only")
hgnc_symbols <- load_gene_symbols(settings_for_symbols)
check("Full HGNC dictionary is loaded", length(hgnc_symbols) > 40000)
check_equal("Common uppercase words are rejected without genetic context",
            extract_gene("This WAS a CAT study", hgnc_symbols), "")
check_equal("HGNC symbol near rsID is accepted",
            extract_gene("PER3 rs228697 genotype", hgnc_symbols), "PER3")

sd_path <- file.path(ROOT, "tests", "fixtures", "sciencedirect_response.json")
sd_text <- paste(readLines(sd_path, warn = FALSE), collapse = "\n")
sd_rows <- parse_sciencedirect_payload(sd_text)
check_equal("ScienceDirect list payload keeps both records", nrow(sd_rows), 2L)
check_equal("ScienceDirect DOI normalization", sd_rows$doi[1], "10.1000/example.1")
sd_frame <- jsonlite::fromJSON(sd_text)[["search-results"]]$entry
check_equal("ScienceDirect data.frame payload keeps both records", nrow(norm_sd_records(sd_frame)), 2L)
sd_v2_path <- file.path(ROOT, "tests", "fixtures", "sciencedirect_v2_response.json")
sd_v2_text <- paste(readLines(sd_v2_path, warn = FALSE), collapse = "\n")
sd_v2 <- parse_sciencedirect_payload(sd_v2_text)
check_equal("ScienceDirect V2 payload is parsed", nrow(sd_v2), 1L)
check_equal("ScienceDirect V2 authors are normalized",
            sd_v2$authors[1], "Ivan Researcher; Anna Scientist")
check_equal("ScienceDirect V2 URI is retained",
            sd_v2$url[1], "https://www.sciencedirect.com/science/article/pii/S123456789")
check_equal("ScienceDirect receives an inclusive year range",
            elsevier_date_param(bounded_years), "2020-2022")
check_equal("ScienceDirect accepts only an upper year bound",
            elsevier_date_param(upper_only_years), "1800-2022")
check_equal("Scopus fallback receives inclusive publication-year clauses",
            scopus_year_clause(bounded_years), "PUBYEAR > 2019 AND PUBYEAR < 2023")

oa_path <- file.path(ROOT, "tests", "fixtures", "openalex_response.json")
oa_payload <- jsonlite::fromJSON(oa_path, simplifyVector = FALSE)
oa_rows <- norm_openalex_records(oa_payload$results)
check_equal("OpenAlex fixture produces two records", nrow(oa_rows), 2L)
check_equal("OpenAlex abstract is reconstructed in word order",
            oa_rows$abstract[1], "ACE polymorphism is associated with physical activity")
oa_keep <- vapply(paste(oa_rows$title, oa_rows$abstract), strict_topic_match, logical(1))
check_equal("OpenAlex strict topic filter rejects irrelevant work", unname(oa_keep), c(TRUE, FALSE))
check_equal("OpenAlex DOI is normalized", oa_rows$doi[1], "10.1000/openalex.1")
check_equal("OpenAlex receives publication-date filters",
            openalex_filter_with_year("type:article", bounded_years),
            "type:article,from_publication_date:2020-01-01,to_publication_date:2022-12-31")
check_equal("OpenAlex accepts only an upper publication-date bound",
            openalex_filter_with_year("type:article", upper_only_years),
            "type:article,to_publication_date:2022-12-31")
check_equal("Missing HTML body is treated as empty full text",
            html_body_text("<html><head><title>Empty</title></head></html>"), "")
check_equal("HTML body text is normalized",
            html_body_text("<html><body><p> usable   text </p></body></html>"), "usable text")

crossref_path <- file.path(ROOT, "tests", "fixtures", "crossref_work.json")
crossref_payload <- jsonlite::fromJSON(crossref_path, simplifyVector = FALSE)
crossref_item <- crossref_record(crossref_payload$message)
check_equal("Crossref DOI is parsed", crossref_item$doi, "10.1000/crossref.1")
check_equal("Crossref record without date parts has empty year",
            crossref_year(list(issued = list(`date-parts` = list()))), "")
check_equal("Crossref malformed date has empty year",
            crossref_year(list(issued = "unknown")), "")
check("Crossref strict title similarity accepts matching title",
      title_similarity("ACE polymorphism in athletes", crossref_item$title) >= 0.85)

pmc_path <- file.path(ROOT, "tests", "fixtures", "pmc_article.xml")
pmc_text <- strip_xml(paste(readLines(pmc_path, warn = FALSE), collapse = "\n"))
check("PMC parser excludes bibliography", !grepl("N=999", pmc_text, fixed = TRUE))
fields <- extract_detail_fields(pmc_text)
check_equal("Systematic review is not classified as RCT", fields$pub_type, "мета-анализ / обзор")
check_equal("Sample size extraction", fields$sample_size, "196")
check_equal("Sex extraction", fields$sex, "M=143, F=53")
check_equal("Mean age extraction", fields$age, "42.5 ± 11.4")
check_equal("HWE violation is distinguished", fields$hwe, "нарушено")
check("Results come from Results section", grepl("β=-47.7", fields$results, fixed = TRUE))
check("Structured evidence has confidence", identical(attr(fields, "confidence"), 0.8))
hwe_qc <- extract_detail_fields("SNPs were excluded if they deviated from Hardy-Weinberg equilibrium.")
check_equal("HWE quality-control exclusion is not reported as a sample violation",
            hwe_qc$hwe, "проверено (отклонения исключены)")
alternate_sample <- extract_detail_fields("A total of 249 students included 115 males and 134 females.")
check_equal("Alternative sample-size wording", alternate_sample$sample_size, "249")
check_equal("Alternative sex wording", alternate_sample$sex, "M=115, F=134")
described_sample <- extract_detail_fields("A total of 249 Han college students were enrolled.")
check_equal("Sample size allows demographic descriptors", described_sample$sample_size, "249")
check_equal("Unicode scientific p-value is normalized",
            grab_plausible_p("The threshold was p < 1.00 × 10−5."), "1.00e-5")

settings <- settings_for_symbols
make_article <- function(source_id, pmid = "", title = "", abstract = "", doi = "",
                         publication_type = "", retrieval_method = "fixture") {
  row <- as.list(setNames(rep("", length(ARTICLE_COLS)), ARTICLE_COLS))
  row$source_id <- source_id
  row$pmid <- pmid
  row$title <- title
  row$abstract <- abstract
  row$doi <- doi
  row$publication_type <- publication_type
  row$retrieval_method <- retrieval_method
  ensure_article_schema(as.data.frame(row, stringsAsFactors = FALSE))
}

check_equal("PubMed receives a publication-date filter",
            pubmed_query_with_year("ACE AND athlete", bounded_years),
            "(ACE AND athlete) AND 2020:2022[dp]")
year_rows <- rbind(
  make_article("Y1", title = "2020 article"),
  make_article("Y2", title = "2022 article"),
  make_article("Y3", title = "2023 article"),
  make_article("Y4", title = "Unknown year")
)
year_rows$year <- c("2020", "2022", "2023", "")
attr(year_rows, "total_results") <- 4L
filtered_year_rows <- filter_publication_year(year_rows, bounded_years)
check_equal("Local publication-year safety filter keeps both inclusive bounds",
            filtered_year_rows$source_id, c("Y1", "Y2"))
check_equal("Publication-year filtering preserves source metadata",
            attr(filtered_year_rows, "total_results"), 4L)

pm <- make_article("100", "100", "ACE rs1234 in athletes", "ACE rs1234", "10.1000/duplicate")
sd <- make_article("SCIDIR:100", "", "ACE rs1234 in athletes", "ACE rs1234", "10.1000/duplicate")
combined <- build_final_table(pm, sd, el_empty_df(), settings)
check_equal("Duplicate DOI is merged", nrow(combined), 1L)
check("Merged source provenance is retained", grepl("PubMed", combined$source) && grepl("ScienceDirect", combined$source))
check_equal("Merged provenance counts physical sources once",
            count_source_labels(c("PubMed; ScienceDirect", "OpenAlex", "PubMed")), 3L)
check_equal("Integrated SNP extraction", combined$snp[1], "rs1234")
check_equal("Integrated gene extraction", combined$gene[1], "ACE")

enriched <- enrich_final_table(combined, data.frame(pmid = "100", fulltext = pmc_text,
                                                     stringsAsFactors = FALSE))
check_equal("Only PubMed-linked row is enriched", enriched$sample_size[1], "196")
check("Evidence is exported as JSON", nzchar(enriched$extraction_evidence[1]))

sort_a <- make_article("1", title = "ACE rs2 in athletes")
sort_b <- make_article("2", title = "ACE rs10 in athletes")
sorted <- build_final_table(rbind(sort_b, sort_a), sd_empty_df(), openalex_empty_df(), settings)
check_equal("Rows use numeric SNP sorting", sorted$snp, c("rs2", "rs10"))

abstract_only <- make_article(
  "OA1", title = "ACTN3 rs1815739 in athletes",
  abstract = "A total of 120 athletes were evaluated. The ACTN3 rs1815739 variant was associated with strength (p=0.02).",
  publication_type = "journal-article", retrieval_method = "openalex_api"
)
abstract_combined <- build_final_table(pubmed_empty_df(), sd_empty_df(), abstract_only, settings)
abstract_enriched <- enrich_final_table(abstract_combined)
check_equal("Non-PubMed abstract is enriched", abstract_enriched$sample_size[1], "120")
check_equal("Publication type metadata is retained", abstract_enriched$pub_type[1], "статья")

review_article <- make_article(
  "REV1", title = "Review of randomized trials in athletes",
  abstract = "This review discusses randomized controlled trials of exercise.",
  publication_type = "Journal Article; Review"
)
review_final <- enrich_final_table(build_final_table(review_article, sd_empty_df(), openalex_empty_df(), settings))
check_equal("Authoritative review metadata prevents false RCT classification",
            review_final$pub_type[1], "обзор")

app_text <- paste(readLines(file.path(ROOT, "app.R"), warn = FALSE), collapse = "\n")
check("Web sessions do not write shared result files",
      grepl("settings = settings, queries = queries, export = FALSE", app_text, fixed = TRUE))
check("Results table uses client-side data and cannot time out on a DT Ajax request",
      grepl("}, server = FALSE)", app_text, fixed = TRUE))
check("Query editor explains that changes are optional",
      grepl("Запросы · необязательно", app_text, fixed = TRUE))
check("Query editor can restore source defaults",
      grepl("observeEvent(input$reset_queries", app_text, fixed = TRUE))
year_ui_markers <- c('textInput("year_from"', 'textInput("year_to"',
                     "Год публикации (необязательно)")
check("Web interface exposes optional publication-year bounds",
      all(vapply(year_ui_markers, function(marker) grepl(marker, app_text, fixed = TRUE), logical(1))))
check("Web search stores validated publication-year bounds in session settings",
      grepl("settings$publication_year <- list", app_text, fixed = TRUE))
column_ui_markers <- c(
  'nav_panel("Столбцы"', 'actionButton("columns_core"',
  'actionButton("columns_genetics"', 'actionButton("columns_all"', 'selectizeInput('
)
check("Web interface exposes the column customizer and presets",
      all(vapply(column_ui_markers,
                 function(marker) grepl(marker, app_text, fixed = TRUE), logical(1))))
check("Results table and both downloads use the selected columns",
      length(gregexpr("visible_result_data()", app_text, fixed = TRUE)[[1]]) >= 3)
check("Column order can be changed by dragging selected fields",
      grepl('plugins = list("remove_button", "drag_drop")', app_text, fixed = TRUE))
check("Removing every field restores the core preset",
      grepl("ignoreInit = TRUE, ignoreNULL = FALSE", app_text, fixed = TRUE))
manifest <- jsonlite::read_json(file.path(ROOT, "manifest.json"), simplifyVector = FALSE)
check("Connect Cloud manifest targets a supported Shiny runtime",
      identical(manifest$platform, "4.6.0") && identical(manifest$metadata$appmode, "shiny"))

cat("\nRESULT", passed, "passed;", failed, "failed\n")
if (failed > 0) quit(status = 1)
