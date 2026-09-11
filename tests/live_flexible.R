# Live checks for non-sports search; never used as scientific validation.
args <- grep('^--file=', commandArgs(FALSE), value=TRUE)
ROOT <- normalizePath(file.path(dirname(sub('^--file=', '', args[1])), '..'))
options(article_parser.root=ROOT)
for (name in c('00_common.R','01_pubmed.R','02_sciencedirect.R','03_openalex.R','03_crossref.R','04_merge_export.R','05_pmc_fulltext.R','05b_extraction.R','05c_documents.R','05d_biomedical.R','06_pipeline.R','07_evidence_archive.R')) source(file.path(ROOT,'src',name))
s <- load_project_settings()
s$pubmed$max_records <- 2L; s$pubmed$batch_size <- 5L
s$openalex$max_records <- 2L; s$openalex$scope <- 'global'
s$crossref$enabled <- FALSE; s$pmc$enabled <- FALSE; s$fulltext$enabled <- FALSE
for (profile in c('rna','cells','diseases')) {
 s$research_profile <- profile
 query <- switch(profile,rna='miR-21 AND breast cancer',cells='HeLa AND cell line',diseases='diabetes AND biomarker')
 records <- load_pubmed(query,s)
 stopifnot(nrow(records)>0L,nrow(records)<=2L)
 df <- enrich_final_table(build_final_table(records,sd_empty_df(),openalex_empty_df(),s),settings=s)
 field <- switch(profile,rna='mirna',cells='cell_line',diseases='disease')
 stopifnot(any(nzchar(df[[field]])))
 cat('PASS',profile,'PubMed',nrow(df),'rows;',field,sum(nzchar(df[[field]])),'filled\n')
}
s$research_profile <- 'rna'
oa <- load_openalex('miR-21 AND breast cancer',s)
stopifnot(nrow(oa)>0L,nrow(oa)<=2L)
cat('PASS OpenAlex global non-sports search',nrow(oa),'rows\n')
cat('LIVE FLEXIBLE 7 checks passed\n')
