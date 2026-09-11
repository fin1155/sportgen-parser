# Additional research fields are source-backed candidates, not clinical assertions.
# Entity dictionaries are deliberately bounded; unsupported names remain reviewable.
BIOMEDICAL_COLS <- c("disease", "disease_subtype", "disease_stage", "cell_line", "cell_type", "tissue", "organism", "mirna", "lncrna", "circrna", "target_gene", "expression_change", "intervention", "dose", "exposure_duration", "control_group", "assay_method", "experimental_model")
ENRICH_COLS <- c(ENRICH_COLS, BIOMEDICAL_COLS)

extract_biomedical_fields <- function(txt) {
  out <- as.list(setNames(rep("", length(BIOMEDICAL_COLS)), BIOMEDICAL_COLS))
  evidence <- list()
  units <- text_units(txt)
  units <- units[!grepl("introduction|discussion|background|reference|bibliograph|Metadata|введени|обсуждени|литератур", units$section, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(units)) { attr(out, "evidence") <- evidence; return(out) }
  put <- function(field, pattern, entity = FALSE, context = NULL) {
    selected <- units[grepl(pattern, units$snippet, perl = TRUE, ignore.case = TRUE), , drop = FALSE]
    if (!is.null(context)) selected <- selected[grepl(context, selected$snippet, perl = TRUE, ignore.case = TRUE), , drop = FALSE]
    if (!nrow(selected)) return(invisible(NULL))
    # Statements retain attribution, negation and multiple comparisons. Only
    # explicitly named identifiers are reduced to a list of entity strings.
    values <- if (entity) unlist(lapply(selected$snippet, function(text) all_matches(pattern, text)), use.names = FALSE) else selected$snippet
    out[[field]] <<- paste(unique(values[nzchar(values)]), collapse = " | ")
    evidence[[field]] <<- list(value = out[[field]], section = paste(unique(selected$section), collapse = "; "),
      snippet = paste(unique(selected$snippet), collapse = "\n"), status = "требует проверки")
  }
  disease_pattern <- paste0("\\b(?:[[:alpha:]-]+[ -]){0,3}(?:cancer|carcinoma|melanoma|glioma|glioblastoma|leukemia|leukaemia|lymphoma|sarcoma|diabetes|asthma|arthritis|atherosclerosis|osteoporosis|obesity|fibrosis|Alzheimer['’]?s?|Parkinson['’]?s?|COVID-19|disease|disorder)\\b|онколог|карцином|меланом|глиом|глиобластом|лейк[её]ми|лимфом|сарком|диабет|астм|артрит|атеросклероз|остеопороз|ожирени|фиброз|болезн|заболеван|(?:^|[[:space:][:punct:]])рак[а-яё]*(?:$|[[:space:][:punct:]])")
  put("disease", disease_pattern)
  put("disease_subtype", "triple[- ]negative|HER2[- ](?:positive|negative)|ER[- ](?:positive|negative)|non[- ]small[- ]cell|small[- ]cell lung|adenocarcinoma|squamous cell|molecular subtype|disease subtype|трижды негатив|аденокарцином|плоскоклеточ|немелкоклеточ|подтип.{0,30}(?:опухол|заболеван)")
  put("disease_stage", "\\bstage\\s+(?:[IVX]{1,4}|[0-4])(?:[ABC])?\\b|\\bT[0-4][abc]?N[0-3][abc]?M[01x]\\b|стади[яи]\\s+(?:[IVX]{1,4}|[0-4])", context = disease_pattern)
  cell_lines <- "\\b(?:HeLa|HEK[- ]?293T?|293T|MCF[- ]?7|MDA[- ]MB[- ](?:231|468)|A549|HCT[- ]?116|HepG2|Huh[- ]?7|U[- ]?87(?:MG)?|U[- ]?251|SH[- ]SY5Y|PC[- ]?3|DU[- ]?145|LNCaP|K[- ]?562|Jurkat|THP[- ]?1|HL[- ]?60|NIH[- /]?3T3|C2C12|CHO[- ]?K1|RAW[ .]?264[.]?7|HT[- ]?29|SW[- ]?480|SK[- ]BR[- ]3|BT[- ]?474|SK[- ]OV[- ]3|OVCAR[- ]?3|BEAS[- ]?2B)\\b"
  put("cell_line", paste0(cell_lines, "|\\bCVCL_[A-Z0-9]{4}\\b"), entity = TRUE)
  put("cell_type", "\\b(?:epithelial|endothelial|stem|immune|tumou?r|cancer|primary|T|B|NK) cells?\\b|\\b(?:fibroblasts?|neurons?|astrocytes?|hepatocytes?|cardiomyocytes?|myocytes?|macrophages?|monocytes?|lymphocytes?)\\b|эпителиальн[а-яё ]+клет|эндотелиальн[а-яё ]+клет|стволов[а-яё ]+клет|фибробласт|нейрон|гепатоцит|кардиомиоцит|макрофаг|лимфоцит")
  put("tissue", "\\b(?:breast|lung|liver|kidney|brain|heart|colon|colorectal|prostate|ovarian|ovary|pancreatic|pancreas|skeletal muscle|blood|serum|plasma|bone marrow|skin|tissue|biops(?:y|ies))\\b|ткан[ьи]|биопси|печен|печён|л[её]гк|почек|почеч|головного мозга|молочн[а-яё ]+желез|скелетн[а-яё ]+мышц|сыворотк|плазм[аы]|костн[а-яё ]+мозг")
  put("organism", "\\b(?:Homo sapiens|Mus musculus|Rattus norvegicus|Danio rerio|Drosophila melanogaster|Caenorhabditis elegans|human|humans|mice|mouse|rats?|zebrafish)\\b|человек|пациент|мыш[ьи]|крыс|данио", entity = TRUE)
  put("mirna", "\\b(?:(?:hsa|mmu|rno|dre|dme|cel)[- ])?(?:miR(?:NA)?|microRNA)[- ]?[0-9]+[a-z]?(?:[- ][0-9]+)?(?:[- ][35]p)?\\b|\\blet[- ]7[a-z]?(?:[- ][0-9]+)?(?:[- ][35]p)?\\b", entity = TRUE)
  put("lncrna", "\\b(?:MALAT1|NEAT1|HOTAIR|H19|XIST|MEG3|GAS5|PVT1|TUG1|HOTTIP|LINC[0-9]{4,6})\\b", entity = TRUE, context = "lncRNA|long non[- ]?coding|некодирующ|РНК|RNA|transcript")
  put("circrna", "\\b(?:hsa_|mmu_)?circ(?:RNA)?[_-][A-Za-z0-9]+\\b|\\b(?:CDR1as|ciRS[- ]7)\\b", entity = TRUE, context = "circ|circular|кольцев|циркуляр")
  put("target_gene", "(?:direct(?:ly)?|downstream|validated|putative|predicted)?\\s*target(?:ed|s|ing)?\\b|ген.{0,8}мишен|мишен[ьию]", context = "miR|microRNA|lncRNA|circRNA|let[- ]7|микроРНК|некодирующ")
  put("expression_change", "up[- ]regulat|down[- ]regulat|overexpress|underexpress|(?:increas|decreas|elevat|reduc|higher|lower|unchanged|no change|no significant change).{0,60}(?:expression|miR|RNA)|(?:expression|miR|RNA).{0,60}(?:increas|decreas|elevat|reduc|higher|lower|unchanged)|экспресси.{0,60}(?:повыш|сниж|увелич|уменьш|не измен)|(?:повыш|сниж|увелич|уменьш).{0,60}экспресси")
  put("intervention", "\\b(?:treated|treatment|exposed|exposure|transfected|transfection|knockdown|knockout|silencing|inhibitor|mimic|agonist|antagonist|irradiation|CRISPR|siRNA|shRNA)\\b|обработан|воздейств|трансфек|нокдаун|нокаут|ингибитор|облучен|препарат")
  put("dose", "[0-9]+(?:[.,][0-9]+)?\\s*(?:[nµμumk]?M\\b|[nµμumk]?g\\s*/\\s*(?:m[lL]|[lL]|kg)|[nмкµμu]?моль\\s*/\\s*л|м[к]?г\\s*/\\s*(?:мл|кг)|Gy\\b|Гр\\b|%\\s*(?:DMSO|FBS))", context = "treat|expos|dose|concentration|incubat|transfect|medium|media|доз|концентрац|обработ|инкубир|воздейств")
  put("exposure_duration", "\\b[0-9]+(?:[.,][0-9]+)?(?:[-– ][0-9]+)?\\s*(?:h(?:ours?)?|min(?:utes?)?|days?|weeks?|ч(?:ас(?:а|ов)?)?|минут[а-яё]*|сут(?:ок|ки)?|дн(?:ей|я)|недел[а-яё]*)\\b", context = "treat|expos|incubat|transfect|cultured|обработ|воздейств|инкубир|трансфек|культивир")
  put("control_group", "negative control|positive control|control group|untreated|vehicle[- ](?:treated|control)|scrambled|mock[- ]transfect|healthy controls?|контрольн[а-яё ]+(?:групп|клет|образ)|негативн[а-яё ]+контрол|положительн[а-яё ]+контрол")
  put("assay_method", "\\b(?:RT[- ]qPCR|qRT[- ]PCR|RT[- ]PCR|qPCR|RNA[- ]seq|scRNA[- ]seq|Western blot(?:ting)?|ELISA|flow cytometry|luciferase(?: reporter)? assay|dual[- ]luciferase|immunohistochemistry|immunofluorescence|MTT assay|CCK[- ]8|Transwell|wound healing assay|ChIP[- ]seq|microarray)\\b|ПЦР|вестерн[- ]блот|проточн[а-яё ]+цитометр|иммуногистохим|иммунофлуоресцен|люцифераз", entity = TRUE)
  put("experimental_model", "\\bin vitro\\b|\\bin vivo\\b|\\bex vivo\\b|\\borganoids?\\b|\\bxenografts?\\b|\\b(?:animal|murine|mouse|rat) model\\b|органоид|ксенографт|животн[а-яё ]+модел", entity = TRUE)
  attr(out, "evidence") <- evidence
  out
}

extract_detail_fields <- function(txt) {
  original <- extract_sport_detail_fields(txt)
  biomedical <- extract_biomedical_fields(txt)
  for (field in BIOMEDICAL_COLS) original[[field]] <- biomedical[[field]]
  attr(original, "evidence") <- c(attr(original, "evidence") %||% list(), attr(biomedical, "evidence") %||% list())
  original
}
