# Exercise server reactives with a deterministic in-memory pipeline; no network.
args <- grep('^--file=', commandArgs(FALSE), value=TRUE)
ROOT <- normalizePath(file.path(dirname(sub('^--file=', '', args[1])), '..'))
setwd(ROOT)
app_env <- new.env(parent = globalenv())
app_env$commandArgs <- function(...) character()
sys.source('app.R', envir = app_env)
settings <- load_project_settings()
record <- as.list(setNames(rep('', length(ARTICLE_COLS)), ARTICLE_COLS))
record$source_id <- 'test-server'; record$pmid <- '999'; record$title <- 'ACTN3 rs1815739 in athletes'
record$abstract <- 'A total of 100 athletes were enrolled. ACTN3 rs1815739 R allele was associated with strength (p < 0.01).'
fixture <- assess_evidence(enrich_final_table(build_final_table(as.data.frame(record), sd_empty_df(), openalex_empty_df(), settings)))
archive <- tempfile(fileext='.json')
write_research_archive(fixture, archive)
calls <- list()
run_pipeline <- function(settings, queries, export, progress) {
  calls[[length(calls)+1L]] <<- settings$pubmed$max_records
  fixture
}
checks <- 0L
verify <- function(value) { stopifnot(isTRUE(value)); checks <<- checks+1L }
shiny::testServer(app_env$server, {
  session$setInputs(sources='pubmed', max_records=2, year_from='', year_to='', elsevier_key='',
                   fulltext=FALSE, query_pubmed='test', query_sciencedirect='', query_openalex='',
                   table_columns=TABLE_COLUMN_PRESETS$core, run=NULL)
  session$setInputs(run=1)
  verify(identical(tail(calls,1)[[1]],2L))
  verify(nrow(result_data()) == 1L)
  verify(run_state()$kind == 'success')
  session$setInputs(review_article=fixture$article_id[1],review_field='sample_size')
  session$setInputs(review_value='120', review_basis='Synthetic review fixture.', save_review=1)
  verify(result_data()$sample_size == '120')
  verify(length(attr(result_data(),'review_log')) == 1L)
  session$setInputs(undo_review=1)
  verify(result_data()$sample_size == '100')
  verify(length(attr(result_data(),'review_log')) == 0L)
  session$setInputs(import_archive=list(datapath=archive))
  verify(result_data()$article_id == fixture$article_id)
  session$setInputs(review_value='120', review_basis='Synthetic review fixture.', save_review=2)
  session$setInputs(reprocess_archive=1)
  verify(result_data()$sample_size == '120')
  verify(length(calls) == 1L)
  session$setInputs(sources=character(),run=2)
  verify(run_state()$kind == 'error')
  verify(length(calls) == 1L)
  session$setInputs(sources='pubmed',max_records=-1,run=3)
  verify(run_state()$kind == 'error')
  session$setInputs(max_records=2,year_from='2025',year_to='2020',run=4)
  verify(run_state()$kind == 'error')
  session$setInputs(year_from='',year_to='',run=5)
  verify(run_state()$kind == 'success')
  verify(length(calls) == 2L)
})
unlink(archive)
cat('SERVER', checks, 'checks passed\n')
