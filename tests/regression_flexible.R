# Synthetic regression fixtures, explicitly not independent scientific validation.
biotext <- paste(
  '[[SECTION: Methods]]',
  'Human MCF-7 and MDA-MB-231 breast cancer cells were treated with doxorubicin at a concentration of 5 µM for 48 hours.',
  'We used a scrambled negative control and measured expression by RT-qPCR and Western blotting.',
  'Patients with stage II breast cancer were included.',
  '[[SECTION: Results]]',
  'hsa-miR-21-5p expression was increased, while miR-34a expression was unchanged.',
  'miR-21 directly targeted PTEN according to a dual-luciferase assay.',
  'The lncRNA MALAT1 and circRNA hsa_circ_0001234 were detected in an in vitro model.',
  '[[SECTION: References]]',
  'HeLa cells and miR-999 were discussed in another publication.', sep = '\n')
bio <- extract_detail_fields(biotext)
check('Cell lines are extracted without a SNP', grepl('MCF-7',bio$cell_line,fixed=TRUE) && grepl('MDA-MB-231',bio$cell_line,fixed=TRUE))
check('Bibliographic cell lines are excluded', !grepl('HeLa',bio$cell_line,fixed=TRUE))
check('RNA identifiers preserve species and arm', grepl('hsa-miR-21-5p',bio$mirna,fixed=TRUE))
check('Bibliographic RNA identifiers are excluded', !grepl('999',bio$mirna,fixed=TRUE))
check('Disease retains its supporting statement', grepl('breast cancer',bio$disease,fixed=TRUE))
check('Disease stage retains attribution', grepl('stage II breast cancer',bio$disease_stage,fixed=TRUE))
check('RNA target is a complete source statement', grepl('miR-21 directly targeted PTEN',bio$target_gene,fixed=TRUE))
check('Unchanged expression is not reclassified as decreased', grepl('miR-34a expression was unchanged',bio$expression_change,fixed=TRUE))
check('Dose preserves units and intervention', grepl('5 µM',bio$dose,fixed=TRUE) && grepl('doxorubicin',bio$dose,fixed=TRUE))
check('Exposure time is retained with context', grepl('48 hours',bio$exposure_duration,fixed=TRUE))
check('Control condition retains its label', grepl('scrambled negative control',bio$control_group,fixed=TRUE))
check('Assay names are retained', grepl('RT-qPCR',bio$assay_method,fixed=TRUE) && grepl('Western blotting',bio$assay_method,fixed=TRUE))
check_equal('Long noncoding RNA is found with context', bio$lncrna, 'MALAT1')
check_equal('Circular RNA identifier is retained', bio$circrna, 'hsa_circ_0001234')
check_equal('Experimental model is explicit', bio$experimental_model, 'in vitro')
check_equal('Dose is not invented from a sample count', extract_detail_fields('There were 48 patients with diabetes.')$dose, '')
check_equal('Follow-up time is not exposure time', extract_detail_fields('The follow-up lasted 48 hours.')$exposure_duration, '')
check_equal('Cell stage is not disease stage', extract_detail_fields('The second stage of cell culture used 5 plates.')$disease_stage, '')
check_equal('Gene name alone does not imply lncRNA', extract_detail_fields('MALAT1 was listed in the appendix.')$lncrna, '')
check_equal('A generic RNA mention does not invent an identifier', extract_detail_fields('MicroRNA expression was measured.')$mirna, '')
check_equal('Gene co-occurrence does not imply an RNA target', extract_detail_fields('miR-21 and PTEN were measured.')$target_gene, '')
check_equal('Unstudied background RNA is excluded', extract_detail_fields('[[SECTION: Introduction]]\nmiR-888 is a known regulator.')$mirna, '')
check('Every biomedical value has source evidence', all(BIOMEDICAL_COLS[vapply(bio[BIOMEDICAL_COLS],nzchar,logical(1))] %in% names(attr(bio,'evidence'))))
check('Biomedical evidence preserves source text', grepl('5 µM for 48 hours',attr(bio,'evidence')$dose$snippet,fixed=TRUE))
russian_bio <- extract_detail_fields('Клетки MCF-7 рака молочной железы обработаны препаратом. Экспрессия miR-21 повышена. Метод ПЦР использован для оценки.')
check('Russian disease and RNA statements are found', nzchar(russian_bio$disease) && nzchar(russian_bio$mirna) && nzchar(russian_bio$expression_change))
check_equal('Russian assay label is retained', russian_bio$assay_method, 'ПЦР')
for (profile in c('rna','cells','diseases','general')) {
  check(paste('Non-sports article is not rejected for profile', profile), topic_match('HeLa cancer cells express miR-21.',list(research_profile=profile)))
}
check('Sports filter remains active in its profile', !topic_match('HeLa cancer cells express miR-21.',list(research_profile='sports')))
check('Sports filter can be disabled explicitly', topic_match('Unrestricted topic.',list(research_profile='sports',topic_filter=FALSE)))
check_error('Unknown profile is rejected', research_profile(list(research_profile='invalid')), 'Неизвестная')
check('MicroRNA template contains no sports requirement', !grepl('athlete|sport',profile_queries('rna')$pubmed))
check_equal('Free search starts with an empty query', profile_queries('general')$pubmed, '')

biorecord <- make_article('BIO1', title='Cell line study of miR-21', abstract=biotext)
biofinal <- assess_evidence(enrich_final_table(build_final_table(biorecord,sd_empty_df(),openalex_empty_df(),settings)))
check_equal('Final table contains the expanded schema', ncol(biofinal), length(TABLE_COLUMN_LABELS))
check('RNA identifiers are extracted from a title without an abstract', nzchar(enrich_final_table(build_final_table(make_article('TITLEBIO',title='miR-21 in MCF-7 cells'),sd_empty_df(),openalex_empty_df(),settings))$mirna))
biofinal <- review_article_field(biofinal,biofinal$article_id[1],'cell_line','MCF-7 (reviewed)','Synthetic review fixture')
attr(biofinal,'table_settings') <- list(columns=c('mirna','cell_line','title'))
bioarchive <- tempfile(fileext='.json')
write_research_archive(biofinal,bioarchive)
biorestored <- reprocess_archive(read_research_archive(bioarchive))
check_equal('Reviewed new fields survive archive reprocessing',biorestored$cell_line,'MCF-7 (reviewed)')
check_equal('Selected column order survives archive roundtrip',attr(biorestored,'table_settings')$columns,c('mirna','cell_line','title'))
old <- bundle_object(biofinal)
old$articles[BIOMEDICAL_COLS] <- NULL
old$table_settings <- NULL
jsonlite::write_json(old,bioarchive,auto_unbox=FALSE,null='null')
old_restored <- read_research_archive(bioarchive)
check('Old archives acquire empty new columns', all(vapply(old_restored[BIOMEDICAL_COLS],function(x) identical(x,''),logical(1))))
check('Old archived text can populate new fields offline', nzchar(reprocess_archive(old_restored)$mirna))
unlink(bioarchive)
minimal_display <- prepare_table_display(biofinal['title'], links_from=transform(biofinal,pmid='123456'))
check('Title-only table still links to the original article',grepl('pubmed.ncbi.nlm.nih.gov/123456',minimal_display$data$title,fixed=TRUE))
subset_empty <- subset_research_rows(biofinal,integer())
check_equal('No matching export rows remains a valid empty table', nrow(subset_empty), 0L)
check_equal('Empty filtered export excludes other article review history',length(attr(subset_empty,'review_log')),0L)
book <- tempfile(fileext='.xlsx')
write_research_workbook(biofinal,book,c('mirna','cell_line','title'))
book_data <- openxlsx::read.xlsx(book,sheet=1,check.names=FALSE)
check_equal('XLSX new fields follow exactly the chosen order', names(book_data),c('mirna','cell_line','title'))
check_equal('XLSX retains reviewed new values',book_data$cell_line,'MCF-7 (reviewed)')
unlink(book)

original_crossref_request <- crossref_request
crossref_request <- function(...) NULL
check_equal('Missing Crossref response does not abort DOI enrichment', crossref_by_doi('10.1000/transport-fixture'), NULL)
check_equal('Missing Crossref response does not abort title enrichment', crossref_by_title('Synthetic transport fixture'), NULL)
crossref_request <- original_crossref_request
