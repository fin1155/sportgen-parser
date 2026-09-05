# Source-backed development corpus. Metrics apply only to annotated fields.
script_arg <- grep('^--file=', commandArgs(FALSE), value=TRUE)
ROOT <- normalizePath(file.path(dirname(sub('^--file=', '', script_arg[1])), '..'))
options(article_parser.root=ROOT)
for (name in c('00_common.R','01_pubmed.R','02_sciencedirect.R','03_openalex.R','03_crossref.R','04_merge_export.R','05_pmc_fulltext.R','05b_extraction.R','05c_documents.R','06_pipeline.R','07_evidence_archive.R')) source(file.path(ROOT,'src',name))
corpus <- jsonlite::read_json(file.path(ROOT,'tests/fixtures/real/pubmed_30.json'), simplifyVector=FALSE)
rows <- list()
for (item in corpus$records) {
  f <- extract_detail_fields(paste('[[SECTION: Abstract]]', item$excerpt, sep='\n'))
  actual_topic <- strict_topic_match(paste(item$title,item$excerpt))
  rows[[length(rows)+1L]] <- data.frame(pmid=item$pmid, field='topic', expected=as.character(item$topic_expected), actual=as.character(actual_topic), present_expected=item$topic_expected, present_actual=actual_topic, correct=identical(actual_topic,item$topic_expected))
  for (field in names(item$expect)) {
    expected <- unlist(item$expect[[field]], use.names=FALSE)
    actual <- f[[field]]
    correct <- if (!length(expected)) !nzchar(actual) else all(vapply(expected, function(x) grepl(x, actual, fixed=TRUE), logical(1)))
    rows[[length(rows)+1L]] <- data.frame(pmid=item$pmid, field=field, expected=paste(expected,collapse=' | '),actual=actual,present_expected=length(expected)>0,present_actual=nzchar(actual),correct=correct)
  }
}
result <- do.call(rbind, rows)
metrics <- do.call(rbind,lapply(split(result,result$field),function(x) {
  tp <- sum(x$present_expected & x$present_actual & x$correct)
  fp <- sum(x$present_actual & !x$correct)
  fn <- sum(x$present_expected & !x$correct)
  data.frame(field=x$field[1], checks=nrow(x), correct=sum(x$correct), tp=tp, fp=fp, fn=fn,
    precision=if(tp+fp) tp/(tp+fp) else NA_real_, recall=if(tp+fn) tp/(tp+fn) else NA_real_)
}))
readr::write_csv(result,file.path(ROOT,'docs/qa/corpus_results.csv'))
readr::write_csv(metrics,file.path(ROOT,'docs/qa/corpus_metrics.csv'))
print(metrics, row.names=FALSE)
if(any(!result$correct)) print(result[!result$correct,c('pmid','field','expected','actual')],row.names=FALSE)
cat('CORPUS',sum(result$correct),'/',nrow(result),'assertions on',length(corpus$records),'real source excerpts\n')
if(any(!result$correct)) quit(status=1)
