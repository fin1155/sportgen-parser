# Conservative, source-grounded extraction. Values are candidates for review,
# never calibrated probabilities or a formal evidence grade.
EXTRA_DETAIL_COLS <- c("study_period", "sample_groups", "sport", "effect_allele", "data_links")
ENRICH_COLS <- c(ENRICH_COLS, EXTRA_DETAIL_COLS)

match_nonempty <- function(x, table) {
  result <- match(x, table)
  result[is.na(x) | !nzchar(trimws(x))] <- NA_integer_
  result
}

article_identity <- function(row) {
  if (nzchar(row$doi)) return(paste0("doi:", normalize_doi(row$doi)))
  if (nzchar(row$pmid)) return(paste0("pmid:", row$pmid))
  if (nzchar(row$source_id)) return(paste0("source:", row$source_id))
  paste0("title:", normalize_title(row$title))
}

sort_articles <- function(df) {
  if (!nrow(df)) return(df)
  snp <- suppressWarnings(as.numeric(sub("^rs", "", sub(",.*", "", df$snp))))
  ord <- order(ifelse(nzchar(df$gene), df$gene, NA_character_), snp, na.last = TRUE)
  # Preserve auxiliary data.frames/lists attached by the pipeline.
  extras <- attributes(df)[setdiff(names(attributes(df)), c("names", "row.names", "class"))]
  df <- df[ord, , drop = FALSE]
  rownames(df) <- NULL
  for (name in names(extras)) attr(df, name) <- extras[[name]]
  df
}

text_units <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  section <- "unstructured"
  units <- list()
  for (line in lines) {
    if (grepl("^\\[\\[SECTION:", line)) {
      section <- sub("^\\[\\[SECTION: *|\\]\\]$", "", line)
      section <- sub("\\]\\]$", "", section)
      next
    }
    if (grepl("reference|bibliograph|литератур|список источников", section, ignore.case = TRUE)) next
    pieces <- if (grepl("\\[\\[TABLE_ROW\\]\\]", line)) line else
      strsplit(line, "(?<=[.!?])\\s+(?=[A-ZА-ЯЁ])", perl = TRUE)[[1]]
    for (piece in pieces) {
      piece <- normalize_space(piece)
      if (nzchar(piece)) units[[length(units) + 1L]] <- data.frame(section = section, snippet = piece)
    }
  }
  if (!length(units)) return(data.frame(section = character(), snippet = character()))
  unique(do.call(rbind, units))
}

all_matches <- function(pattern, text) {
  found <- regmatches(text, gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE))[[1]]
  unique(found[nzchar(found)])
}

NUM <- "[-−+]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)(?:[eE][-+]?[0-9]+)?"
P_PATTERN <- paste0("\\bp(?:[- ]?(?:value|adj(?:usted)?))?\\s*[:=<>≤≥]+\\s*(?:", NUM, ")(?:\\s*[×x]\\s*10\\s*\\^?\\s*[-−+]\\s*[0-9]+)?")

p_expressions <- function(text) {
  hits <- all_matches(P_PATTERN, text)
  if (!length(hits)) return(character())
  valid <- vapply(hits, function(hit) {
    value <- sub("^.*?[:=<>≤≥]+\\s*", "", hit, perl = TRUE)
    value <- gsub("\\s+", "", value, perl = TRUE)
    value <- gsub("[×x]10\\^?[-−]", "e-", value, perl = TRUE)
    value <- gsub("[×x]10\\^?\\+", "e+", value, perl = TRUE)
    n <- suppressWarnings(as.numeric(chartr(",−", ".-", value)))
    !is.na(n) && n >= 0 && n <= 1
  }, logical(1))
  hits[valid]
}

phenotype_terms <- function(text) {
  all_matches("\\bVO2\\s*max\\b|\\bVO₂max\\b|\\b(?:muscle )?strength\\b|\\bendurance\\b|\\b(?:physical|aerobic|exercise|sports?) performance\\b|\\binjur(?:y|ies)\\b|\\bphysical activity\\b|выносливост[[:alpha:]]*|сил[аы] мышц|травматизм|физическ[[:alpha:]]* активност[[:alpha:]]*", text)
}

association_empty_cols <- c("article_id", "association_id", "gene", "snp", "allele", "group", "phenotype", "model", "effect_metric", "effect_value", "ci", "p_value", "p_adjusted", "correction", "status", "section", "snippet", "link_method", "link_evidence")
empty_associations <- function() as.data.frame(setNames(rep(list(character()), length(association_empty_cols)), association_empty_cols))

allele_terms <- function(text) {
  hits <- all_matches("\\b[ACGTIDRX]{1,3}(?:/[ACGTIDRX]{1,3})?[- ]+(?:allele|genotype)\\b|(?:аллель|аллеля|генотип)\\s+[ACGTIDRX]{1,3}\\b|(?:effect|risk|reference) allele\\s*[:=]\\s*[ACGTIDRX]{1,3}\\b", text)
  hits
}

extract_associations <- function(txt, article_id = "", symbols = character(), title = "") {
  title_snp <- extract_snp(title)
  title_genes <- extract_gene(title, symbols)
  units <- text_units(txt)
  units <- units[!grepl("introduction|discussion|background|введени|обсуждени|Metadata", units$section, ignore.case = TRUE), , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(units))) {
    text <- units$snippet[i]
    if (grepl("significance (?:threshold|level)|(?:statistical )?significance was (?:set|defined)|p.{0,20}(?:was|were) considered significant|уровень значимости.{0,15}(?:принят|установлен)", text, ignore.case = TRUE, perl = TRUE)) next
    if (grepl("Hardy[- ]?Weinberg|\\bHWE\\b|Харди", text, ignore.case = TRUE, perl = TRUE) && !length(phenotype_terms(text))) next
    snps <- extract_snp(text)
    link_method <- "same_source_fragment"
    link_evidence <- ""
    if (!nzchar(snps) && nzchar(title_snp) && !grepl(",", title_snp, fixed = TRUE) &&
        length(allele_terms(text)) && length(phenotype_terms(text))) {
      genes <- extract_gene(text, symbols)
      if (!nzchar(genes) || identical(genes, title_genes)) {
        snps <- title_snp
        link_method <- "single_snp_title_requires_review"
        link_evidence <- title
      }
    }
    ps <- p_expressions(text)
    effects <- all_matches(paste0("(?:\\bOR\\b|odds ratio|\\bHR\\b|hazard ratio|\\bRR\\b|relative risk|β|\\bbeta\\b|Cohen'?s?\\s*d|η²|eta squared)\\s*[:=]?\\s*", NUM), text)
    cis <- all_matches(paste0("95\\s*%\\s*(?:CI|confidence interval|ДИ)\\s*[:=]?\\s*\\(?\\s*", NUM, "\\s*(?:[-–;,]|to)\\s*", NUM, "\\s*\\)?"), text)
    if (!nzchar(snps) || (!length(ps) && !length(effects) && !length(cis))) next
    row <- as.list(setNames(rep("", length(association_empty_cols)), association_empty_cols))
    row$link_method <- link_method
    row$link_evidence <- link_evidence
    row$article_id <- article_id
    row$association_id <- paste0(article_id, "#", length(rows) + 1L)
    row$gene <- extract_gene(text, symbols)
    row$snp <- snps
    row$allele <- paste(allele_terms(text), collapse = "; ")
    row$phenotype <- paste(phenotype_terms(text), collapse = "; ")
    row$group <- paste(all_matches("\\b(?:male|female|elite|control|power|endurance|healthy|trained|untrained)[ -]+(?:athletes?|participants?|subjects?|controls?|men|women)\\b|\\b(?:men|women|controls?)\\b|мужчин[[:alpha:]]*|женщин[[:alpha:]]*|контрольн[[:alpha:]]* групп[[:alpha:]]*", text), collapse = "; ")
    row$model <- paste(all_matches("\\b(?:additive|dominant|recessive|codominant) model\\b|аддитивн[[:alpha:]]* модел[[:alpha:]]*|рецессивн[[:alpha:]]* модел[[:alpha:]]*", text), collapse = "; ")
    row$correction <- paste(all_matches("Bonferroni|\\bFDR\\b|false discovery rate|\\bHolm\\b", text), collapse = "; ")
    # A sentence with multiple SNPs/comparisons is retained for review, but we do
    # not manufacture a cross product between identifiers, alleles and numbers.
    ambiguous <- grepl("AMBIGUOUS_TABLE", text, fixed = TRUE) || grepl(",", snps, fixed = TRUE) || length(effects) > 1L || length(cis) > 1L || length(ps) > 2L ||
      (length(ps) > 1L && !grepl("adjust|correct|коррек", text, ignore.case = TRUE)) ||
      length(phenotype_terms(text)) > 1L || length(allele_terms(text)) > 1L
    row$status <- if (ambiguous) "неоднозначно: сопоставить по оригиналу" else if (link_method != "same_source_fragment") "контекстная связь по заголовку; требует проверки" else "требует проверки"
    if (!ambiguous) {
      if (length(effects)) {
        row$effect_metric <- sub(paste0("\\s*[:=]?\\s*", NUM, "$"), "", effects[1], perl = TRUE)
        row$effect_value <- regmatches(effects[1], regexpr(paste0(NUM, "$"), effects[1], perl = TRUE))
      }
      row$ci <- paste(cis, collapse = "; ")
      if (length(ps)) {
        adjusted <- grepl("adjusted p|corrected p|p[- ]?adj|Bonferroni|FDR|Holm|multiple.{0,20}(?:test|compar)|поправ.{0,20}множеств|скорректирован.{0,5}p", text, ignore.case = TRUE, perl = TRUE)
        row$p_value <- if (length(ps) > 1L || !adjusted) ps[1] else ""
        row$p_adjusted <- if (adjusted) tail(ps, 1) else ""
      }
    }
    row$section <- units$section[i]
    row$snippet <- text
    rows[[length(rows) + 1L]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(empty_associations())
  do.call(rbind, rows)
}

extract_detail_fields <- function(txt) {
  txt <- if (is.na(txt)) "" else txt
  out <- extract_legacy_fields(txt)
  for (name in EXTRA_DETAIL_COLS) out[[name]] <- ""
  units <- text_units(txt)
  methods <- units[!grepl("introduction|discussion|background|reference|Metadata|введени|обсуждени", units$section, ignore.case = TRUE), , drop = FALSE]
  result_units <- methods[grepl("result|table|результ|таблиц", methods$section, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(result_units)) result_units <- methods
  evidence <- list()
  # Each selected sentence is stored verbatim. No numeric confidence is claimed.
  set_field <- function(field, selected, values = selected$snippet) {
    out[[field]] <<- paste(unique(values[nzchar(values)]), collapse = " | ")
    if (nzchar(out[[field]]) && nrow(selected)) evidence[[field]] <<- list(
      value = out[[field]], section = paste(unique(selected$section), collapse = "; "),
      snippet = paste(unique(selected$snippet), collapse = "\n"), status = "требует проверки"
    )
  }
  choose <- function(pattern, pool = methods) pool[grepl(pattern, pool$snippet, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
  # Explicit facts rather than inferred labels; multi-group values remain together.
  rules <- c(
    inherit_model = "\\b(?:additive|dominant|recessive|codominant) (?:model|inheritance)\\b|(?:аддитивн|доминантн|рецессивн|кодоминантн)[[:alpha:]]* модел",
    allele_freq = "(?:allele.{0,40}frequenc|frequenc.{0,40}allele|частот.{0,40}аллел).{0,80}[0-9]|[0-9].{0,40}(?:allele frequency|частот.{0,15}аллел)",
    ethnicity = "\\b(?:Caucasian|European|Asian|African|Hispanic|Han Chinese|Chinese|Japanese|Korean|Russian|Brazilian|Polish|Turkish|Israeli|Italian)\\b|европейск|азиатск|африканск|этническ|происхождени",
    covariates = "(?:adjust(?:ed|ment)? for|covariates?|controlled for|скорректир|ковариат|поправк[аи] на).{0,150}(?:age|sex|BMI|smok|diet|train|ancestry|principal component|возраст|пол|ИМТ|компонент)|(?:age|sex|BMI|возраст|ИМТ).{0,100}(?:covariate|ковариат)",
    pa_level = "accelerometer|questionnaire|IPAQ|self-report|\\bGPS\\b|акселерометр|опросник",
    measure_method = "bruce protocol|(?:12|six|6)-minute|handgrip|hand grip|treadmill|ergometer|протокол Брюса|динамометр|эргометр",
    gene_env = "gene[- ×x]environment|genetic interaction|(?:gene|genotype|SNP)\\s*[×x]\\s*(?:environment|physical activity|exercise|training)|ген.{0,10}сред",
    multipletest = "Bonferroni|\\bFDR\\b|false discovery rate|\\bHolm\\b|Бонферрони",
    power = "power analysis|post-hoc power|power of (?:the )?study|statistical power|мощност.{0,30}исследован",
    data_avail = "data availability|data (?:are|is|were) (?:not )?available|dbGaP|GEO accession|Dryad|Zenodo|доступност.{0,20}данных|данные.{0,30}доступн",
    study_period = "(?:recruit|enrol|collect|conduct|examin|assess|наб[ои]р|провод|обслед|сбор).{0,100}\\b(?:19|20)[0-9]{2}\\b|\\b(?:19|20)[0-9]{2}\\b.{0,100}(?:recruit|enrol|collect|conduct|наб[ои]р|провод|обслед)",
    sample_groups = "[0-9][0-9, ]*.{0,45}(?:athletes?|controls?|participants?|patients?|subjects?|individuals?|players?|footballers?|mice|rats|men|women|мужчин|женщин|спортсмен|контрол)|(?:group|групп|athlete|control).{0,50}\\bn\\s*=\\s*[0-9]+",
    sport = "\\b(?:soccer|football|rugby|swimm(?:er|ing)|sprint(?:er|ing)|marathon|cycl(?:ist|ing)|row(?:er|ing)|wrestl(?:er|ing)|weightlift(?:er|ing)|basketball|volleyball|ski(?:er|ing)|gymnast|judo|tennis)[[:alpha:]]*\\b|футбол|плаван|спринт|марафон|велоспорт|гребл|борц|тяж[её]л.{0,5}атлет|лыж|гимнаст|дзюдо",
    effect_dir = "protective|beneficial|risk|adverse|no significant|not significant|non-significant|благоприят|риск|не выявлен|не обнаружен",
    effect_allele = "(?:allele|genotype|аллел|генотип).{0,120}(?:protect|beneficial|risk|associat|higher|lower|благоприят|риск|ассоци|выше|ниже)|(?:protect|beneficial|risk|ассоци).{0,100}(?:allele|аллел)"
  )
  for (field in names(rules)) set_field(field, choose(rules[[field]], if (field %in% c("effect_dir", "effect_allele", "gene_env")) result_units else methods))
  selected <- choose("Hardy[-– ]?Weinberg|\\bHWE\\b|Харди.{0,3}Вайнберг")
  set_field("hwe", selected) # preserve negation, thresholds, and group attribution
  if (nrow(selected)) {
    labels <- vapply(selected$snippet, function(text) {
      if (grepl("not (?:test|assess)|не провер|не оцен", text, ignore.case = TRUE)) return("не проверено")
      if (grepl("no (?:significant )?deviation|did not deviate|not deviate|не обнаружен|не установл|in (?:Hardy[- ]Weinberg )?equilibrium|отклонен.{0,20}не|не выявлен", text, ignore.case = TRUE, perl = TRUE)) return("отклонений не обнаружено")
      if (grepl("exclud|исключ", text, ignore.case = TRUE) && grepl("deviat|отклон", text, ignore.case = TRUE)) return("проверено (отклонения исключены)")
      if (grepl("deviat|violat|наруш|отклон", text, ignore.case = TRUE)) return("нарушено")
      "упомянуто; результат уточнить"
    }, character(1))
    set_field("hwe", selected, labels)
  }
  set_field("phenotype", choose("VO2|VO₂|strength|endurance|performance|injur|physical activity|вынослив|травматизм|физическ"))
  # Statistics stay in their original sentence; never concatenate first numbers
  # taken from unrelated SNPs into a synthetic result.
  has_stat <- vapply(result_units$snippet, function(text) length(p_expressions(text)) > 0 || grepl("odds ratio|\\bOR\\s*=|β\\s*=|beta\\s*=|95\\s*%\\s*CI", text, perl = TRUE, ignore.case = TRUE), logical(1))
  set_field("results", result_units[has_stat, , drop = FALSE])
  set_field("effect_size", choose("Cohen'?s?\\s*d|η²|eta squared", result_units))
  adjusted <- choose("adjust|correct|коррек", result_units)
  adjusted <- adjusted[vapply(adjusted$snippet, function(text) length(p_expressions(text)) > 0L || grepl("(?:not |non[- ])?significant|значим", text, ignore.case = TRUE), logical(1)), , drop = FALSE]
  set_field("p_adj", adjusted)
  links <- choose("https?://[^ ]*(?:ncbi|dbgap|geo|zenodo|dryad|figshare|ebi\\.ac|osf\\.io|github)")
  values <- unlist(lapply(links$snippet, function(text) all_matches("https?://[^ <>\\\"\\)]+", text)), use.names = FALSE)
  values <- sub("[.;,]+$", "", values)
  values <- values[grepl("^https?://(?:www\\.)?(?:zenodo\\.org|(?:datadryad|dryad)\\.org|(?:[^/]+\\.)?figshare\\.com|osf\\.io|github\\.com|(?:www\\.)?ebi\\.ac\\.uk|(?:www\\.)?ncbi\\.nlm\\.nih\\.gov/(?:geo|projects/gap)|dbgap\\.ncbi\\.nlm\\.nih\\.gov)(?:[/#?]|$)", values, perl = TRUE, ignore.case = TRUE)]
  set_field("data_links", links, values)
  sample_candidates <- choose("(?:study|sample|cohort) (?:included|comprised|enrolled)|total of|data from [0-9]|from [0-9]+ (?:footballer|player)|(?:всего|обследован|исследован.{0,15}участвовал)")
  if (!nzchar(out$sample_size) && nrow(sample_candidates)) {
    for (sample_text in sample_candidates$snippet) {
      n <- grab("(?:total of|included|comprised|enrolled|[Dd]ata from|from) [0-9][0-9,]*", sample_text, "[0-9][0-9,]*")
      if (nzchar(n)) { out$sample_size <- gsub(",", "", n, fixed = TRUE); break }
    }
  }
  if (!nzchar(out$sample_size)) {
    enrollment <- choose("[0-9]+[ ]+(?:[[:alpha:]-]+[ ]+){0,5}(?:players|footballers|athletes|participants|subjects)[ ]+(?:were[ ]+)?(?:enrol|recruit)")
    if (nrow(enrollment)) {
      match <- all_matches("[0-9]+[ ]+(?:[[:alpha:]-]+[ ]+){0,5}(?:players|footballers|athletes|participants|subjects)[ ]+(?:were[ ]+)?(?:enrol|recruit)", enrollment$snippet[1])
      if (length(match)) out$sample_size <- sub(" .*", "", match[1])
    }
  }
  # Keep legacy numeric demographics only with an exact supporting source unit.
  patterns <- c(pub_type = "meta-analysis|systematic review|case.control|randomi[sz]ed|cohort|observational|review|обзор|когорт|рандомиз", sample_type = "athlete|healthy|patient|спортсмен|здоров|пациент", sample_size = "sample|cohort|total|footballer|player|[0-9]+ (?:athlete|subject|participant|student)|\\bN\\s*=|выборк|исследован|обследован|спортсмен", sex = "male|female|\\bmen\\b|\\bwomen\\b|\\b[MF]\\s*=|мужчин|женщин", age = "\\bage\\b|\\bages\\b|возраст")
  # Russian extraction preserves complete statements when no numeric EN match.
  for (field in names(patterns)) {
    selected <- choose(patterns[[field]])
    if (field == "sample_type" || (field %in% c("sex", "age") && !nzchar(out[[field]]))) {
      set_field(field, selected)
    } else if (nzchar(out[[field]]) && nrow(selected)) {
      set_field(field, selected, out[[field]])
    } else if (field == "sample_size") {
      total <- choose("(?:всего|общ.{0,10}выборк|обследован|исследован.{0,15}участвовал).{0,40}[0-9]+")
      set_field(field, total)
    } else out[[field]] <- ""
  }
  grouped <- choose("(?:total of [0-9]+|included).{0,120}(?:athletes?|players?|спортсмен).{0,120}(?:and|и) [0-9]+.{0,50}(?:controls?|контрол)")
  if (nrow(grouped)) set_field("sample_size", grouped) # do not call the first group's N the total
  if (grepl("обзор|мета-анализ", out$pub_type)) {
    period <- choose(rules[["study_period"]])
    period <- period[!grepl("literature|review|database|published|search|обзор|публикац|поиск", period$snippet, ignore.case = TRUE), , drop = FALSE]
    set_field("study_period", period)
  }
  # Publication type must describe this study, not a background citation.
  design <- paste(methods$snippet, collapse = " ")
  if (grepl("(?:non[- ]randomi[sz]ed|not randomi[sz]ed|нерандомиз)", design, ignore.case = TRUE)) {
    set_field("pub_type", choose("non[- ]randomi[sz]ed|not randomi[sz]ed|нерандомиз"), "нерандомизированное исследование")
  }
  attr(out, "evidence") <- evidence
  attr(out, "confidence") <- "требует проверки"
  out
}
