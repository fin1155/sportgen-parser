# Synthetic browser fixture. Run from the project root.
options(article_parser.root=getwd())
for (f in c('00_common.R','01_pubmed.R','02_sciencedirect.R','03_openalex.R','03_crossref.R','04_merge_export.R','05_pmc_fulltext.R','05b_extraction.R','05c_documents.R','05d_biomedical.R','06_pipeline.R','07_evidence_archive.R')) source(file.path('src',f))
settings <- load_project_settings()
records <- lapply(seq_len(30), function(i) {
 row <- as.list(setNames(rep('',length(ARTICLE_COLS)),ARTICLE_COLS))
 row$source_id <- paste0('qa-',i)
 row$title <- paste('Synthetic QA',i,if(i %% 2) 'miR-21 in MCF-7 breast cancer cells' else 'miR-34a in A549 lung cancer cells')
 row$year <- as.character(2020 + i %% 6)
 row$pmid <- as.character(90000000+i)
 row$abstract <- if(i %% 2) 'MCF-7 breast cancer cells were treated with a miR-21 inhibitor at 5 µM for 48 hours. We used a scrambled control. miR-21 expression decreased. RT-qPCR was performed.' else 'A549 lung cancer cells were transfected with miR-34a mimic. miR-34a directly targeted a putative gene in this synthetic fixture. Western blotting was performed.'
 row$publication_type <- 'Journal Article'
 as.data.frame(row)
})
df <- assess_evidence(enrich_final_table(build_final_table(do.call(rbind,records),sd_empty_df(),openalex_empty_df(),settings)))
attr(df,'table_settings') <- list(columns=c('title','year','disease','cell_line','mirna','assay_method'))
args <- commandArgs(trailingOnly=TRUE)
path <- if(length(args)) args[1] else 'out/browser_fixture.json'
dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
write_research_archive(df,path)
cat(normalizePath(path),'\n')
