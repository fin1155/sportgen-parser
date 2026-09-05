# Модуль 4: объединение таблиц, извлечение гена/SNP, сортировка.
# Экспорт вынесен в export_table (вызывается из run_all после enrich-а).

extract_snp <- function(text) {
  if (is.na(text) || !nzchar(text)) return("")
  m <- gregexpr("\\brs[0-9]+\\b", text, perl = TRUE, ignore.case = TRUE)
  hits <- regmatches(text, m)[[1]]
  if (length(hits) == 0) return("")
  hits <- unique(tolower(hits))
  hits <- hits[order(suppressWarnings(as.numeric(sub("^rs", "", hits))))]
  paste(hits, collapse = ", ")
}

extract_gene <- function(text, candidates) {
  if (is.na(text) || !nzchar(text)) return("")
  if (is.null(candidates) || length(candidates) == 0) return("")
  text <- gsub("\\bACTN-3\\b", "ACTN3", text, perl = TRUE)
  token_matches <- gregexpr("[A-Za-z][A-Za-z0-9-]*", text, perl = TRUE)
  tokens <- regmatches(text, token_matches)[[1]]
  if (length(tokens) == 0) return("")
  starts <- token_matches[[1]]
  lengths <- attr(starts, "match.length")
  candidates <- unique(toupper(as.character(candidates)))
  # HGNC-символы в научных текстах регистрозависимы. Это защищает от
  # ложных совпадений с обычными словами вроде WAS, CAT или MET.
  possible <- candidates[candidates %in% unique(tokens)]
  context_pattern <- "\\b(gene|genes|genetic|genotype|genotypes|allele|alleles|variant|variants|polymorphism|polymorphisms|SNP|rs[0-9]+|ген[[:alpha:]]*|аллел[[:alpha:]]*|полиморф[[:alpha:]]*)\\b"
  keep <- vapply(possible, function(symbol) {
    positions <- which(tokens == symbol)
    any(vapply(positions, function(index) {
      left <- max(1, starts[index] - 50)
      right <- min(nchar(text), starts[index] + lengths[index] + 50)
      grepl(context_pattern, substr(text, left, right), ignore.case = TRUE, perl = TRUE)
    }, logical(1)))
  }, logical(1))
  hits <- sort(possible[keep])
  paste(hits, collapse = ", ")
}

deduplicate_articles <- function(df) {
  if (nrow(df) < 2) return(df)
  doi <- vapply(ifelse(is.na(df$doi), "", df$doi), normalize_doi, character(1))
  pmid <- trimws(ifelse(is.na(df$pmid), "", df$pmid))
  title <- tolower(trimws(ifelse(is.na(df$title), "", df$title)))
  title <- gsub("[^[:alnum:]]+", " ", title)
  title <- normalize_space(title)
  # Strong identifiers first, then unambiguous title matches. Never merge two
  # different nonempty DOI/PMID sets through a weak title-only bridge.
  parent <- seq_len(nrow(df))
  root <- function(i) { while (parent[i] != i) i <- parent[i]; i }
  for (values in list(doi, pmid, title)) {
    for (value in unique(values[nzchar(values)])) {
      indices <- which(values == value)
      roots <- unique(vapply(indices, root, integer(1)))
      if (length(roots) < 2) next
      members <- which(vapply(seq_len(nrow(df)), root, integer(1)) %in% roots)
      if (length(unique(doi[members][nzchar(doi[members])])) > 1L ||
          length(unique(pmid[members][nzchar(pmid[members])])) > 1L) next
      parent[roots] <- roots[1]
    }
  }
  groups <- split(seq_len(nrow(df)), vapply(seq_len(nrow(df)), root, integer(1)))
  if (length(groups) == nrow(df)) return(df)
  rows <- lapply(groups, function(index) {
    group <- df[index, , drop = FALSE]
    row <- group[1, , drop = FALSE]
    row$source <- paste(unique(group$source[nzchar(group$source)]), collapse = "; ")
    row$source_id <- paste(unique(group$source_id[nzchar(group$source_id)]), collapse = "; ")
    for (column in setdiff(names(group), c("source", "source_id", "retrieval_method"))) {
      values <- as.character(group[[column]])
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0) {
        row[[column]] <- ""
      } else if (column == "fulltext_url") {
        oa <- which(tolower(group$is_open_access) == "true" & nzchar(group$fulltext_url))
        row[[column]] <- if (length(oa)) group$fulltext_url[oa[1]] else values[1]
      } else if (column %in% c("title", "abstract")) {
        row[[column]] <- values[which.max(nchar(values))]
      } else {
        row[[column]] <- values[1]
      }
    }
    if ("retrieval_method" %in% names(group)) {
      row$retrieval_method <- paste(unique(group$retrieval_method[nzchar(group$retrieval_method)]), collapse = "; ")
    }
    if ("is_open_access" %in% names(group)) {
      row$is_open_access <- if (any(tolower(group$is_open_access) == "true")) "true" else "false"
    }
    row
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

build_final_table <- function(pm_df, sd_df, oa_df, settings) {
  std_cols <- c("source", ARTICLE_COLS)
  norm <- function(df, src) {
    df <- ensure_article_schema(df)
    df$source <- rep(src, nrow(df))
    return(df[, std_cols])
  }
  all <- rbind(
    norm(as.data.frame(pm_df), "PubMed"),
    norm(as.data.frame(sd_df), "ScienceDirect"),
    norm(as.data.frame(oa_df), "OpenAlex")
  )
  if (nrow(all) == 0) {
    all <- article_empty_df()
    all$source <- character(0)
    all <- all[, std_cols, drop = FALSE]
  }
  all <- deduplicate_articles(all)

  t <- ifelse(is.na(all$title), "", all$title)
  a <- ifelse(is.na(all$abstract), "", all$abstract)
  m <- ifelse(is.na(all$mesh), "", all$mesh)
  text_col <- paste(t, a, m, sep = " | ")
  cands <- load_gene_symbols(settings)
  all$gene <- vapply(text_col, function(txt) extract_gene(txt, cands), character(1))
  all$snp <- vapply(text_col, extract_snp, character(1))

  # Сортировка: первично по названию гена, вторично по идентификатору SNP; пустые — в конце
  gene_na <- ifelse(all$gene %in% c("", NA), NA_character_, all$gene)
  first_snp <- sub(",.*$", "", all$snp)
  snp_number <- suppressWarnings(as.numeric(sub("^rs", "", first_snp, ignore.case = TRUE)))
  ord <- order(gene_na, snp_number, first_snp, na.last = TRUE)
  all <- all[ord, , drop = FALSE]
  rownames(all) <- NULL
  return(all)
}

# Экспорт в XLSX + CSV (доступная таблица)
export_table <- function(df, settings) {
  out_dir <- settings$out_dir %||% "out"
  out_path <- if (grepl("^(/|[A-Za-z]:)", out_dir)) out_dir else file.path(parser_root(), out_dir)
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  suppressMessages(library(openxlsx))
  suppressMessages(library(readr))
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Статьи", gridLines = FALSE)
  openxlsx::writeData(workbook, "Статьи", df, withFilter = TRUE)
  header_style <- openxlsx::createStyle(
    fgFill = "#173B3F", fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "center", valign = "center", wrapText = TRUE
  )
  openxlsx::addStyle(workbook, "Статьи", header_style, rows = 1,
                     cols = seq_len(ncol(df)), gridExpand = TRUE)
  openxlsx::freezePane(workbook, "Статьи", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(workbook, "Статьи", cols = seq_len(ncol(df)), widths = "auto")
  wide <- intersect(c("title", "abstract", "results", "extraction_evidence", "url", "fulltext_url"), names(df))
  if (length(wide) > 0) openxlsx::setColWidths(workbook, "Статьи", cols = match(wide, names(df)), widths = 45)
  openxlsx::setRowHeights(workbook, "Статьи", rows = 1, heights = 32)
  openxlsx::saveWorkbook(workbook, file.path(out_path, "articles.xlsx"), overwrite = TRUE)
  readr::write_csv(df, file.path(out_path, "articles.csv"))
  invisible(df)
}
