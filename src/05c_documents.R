# Preserve table headers, rows, nested sections and data-availability statements.
# Tables with merged cells retain raw cells and an ambiguity marker.
xml_table_lines <- function(table) {
  rows <- xml2::xml_find_all(table, ".//tr")
  headers <- character()
  table_ambiguous <- length(xml2::xml_find_all(table, ".//*[@colspan and @colspan!='1']|.//*[@rowspan and @rowspan!='1']")) > 0L
  first_row <- TRUE
  lines <- character()
  for (row in rows) {
    cells <- xml2::xml_find_all(row, "./th|./td")
    values <- normalize_space(xml2::xml_text(cells))
    if (!length(values)) next
    is_header <- length(xml2::xml_find_all(row, "./th")) > 0L || first_row
    first_row <- FALSE
    spans <- xml2::xml_attr(cells, "colspan")
    rowspans <- xml2::xml_attr(cells, "rowspan")
    merged <- any(!is.na(spans) & spans != "1") || any(!is.na(rowspans) & rowspans != "1")
    if (is_header) {
      headers <- values
      if (merged) headers <- character()
      lines <- c(lines, paste("[[TABLE_HEADER]]", paste(values, collapse = " | ")))
    } else {
      mapped <- length(headers) == length(values) && !merged && !table_ambiguous
      text <- if (mapped) paste(paste(headers, values, sep = ": "), collapse = " | ") else
        paste("[[AMBIGUOUS_TABLE]]", paste(values, collapse = " | "))
      lines <- c(lines, paste("[[TABLE_ROW]]", text))
    }
  }
  lines
}

strip_xml <- function(raw) {
  text <- if (is.raw(raw)) rawToChar(raw) else paste(raw, collapse = "\n")
  document <- tryCatch(xml2::read_xml(text), error = function(e) NULL)
  if (is.null(document)) return("")
  blocks <- character()
  abstract <- xml2::xml_find_all(document, ".//abstract//p")
  if (length(abstract)) blocks <- c("[[SECTION: Abstract]]", normalize_space(xml2::xml_text(abstract)))
  nodes <- xml2::xml_find_all(document, ".//body//p[not(ancestor::table-wrap) and not(ancestor::ref-list)]|.//body//table-wrap|.//back//sec[not(ancestor::ref-list)]//p")
  for (node in nodes) {
    titles <- xml2::xml_text(xml2::xml_find_all(node, "ancestor::sec/title"))
    section <- if (length(titles)) paste(titles, collapse = " / ") else "Body"
    if (xml2::xml_name(node) == "table-wrap") {
      label <- normalize_space(paste(xml2::xml_text(xml2::xml_find_all(node, "./label|./caption")), collapse = " "))
      blocks <- c(blocks, paste0("[[SECTION: Table / ", section, " / ", label, "]]"), xml_table_lines(node))
    } else {
      blocks <- c(blocks, paste0("[[SECTION: ", section, "]]"), normalize_space(xml2::xml_text(node)))
    }
  }
  paste(blocks, collapse = "\n")
}

html_body_text <- function(text) {
  document <- tryCatch(xml2::read_html(scalar_text(text)), error = function(e) NULL)
  if (is.null(document)) return("")
  xml2::xml_remove(xml2::xml_find_all(document, ".//script|.//style|.//nav|.//footer|.//header|.//*[contains(@class,'references') or @id='references']"))
  body <- xml2::xml_find_first(document, ".//article")
  if (inherits(body, "xml_missing")) body <- xml2::xml_find_first(document, ".//body")
  if (inherits(body, "xml_missing")) return("")
  nodes <- xml2::xml_find_all(body, ".//h1|.//h2|.//h3|.//h4|.//p[not(ancestor::table)]|.//table")
  if (!length(nodes)) return(normalize_space(xml2::xml_text(body)))
  lines <- character()
  for (node in nodes) {
    type <- xml2::xml_name(node)
    value <- normalize_space(xml2::xml_text(node))
    if (grepl("^h[1-4]$", type)) lines <- c(lines, paste0("[[SECTION: ", value, "]]"))
    else if (type == "table") lines <- c(lines, "[[SECTION: Table]]", xml_table_lines(node))
    else lines <- c(lines, value)
  }
  paste(lines, collapse = "\n")
}

# Retain pages and explicit headings; PDF tables/columns are not reconstructed.
pdf_document_text <- function(pages) {
  blocks <- character()
  section <- "Unstructured"
  for (i in seq_along(pages)) {
    blocks <- c(blocks, paste0("[[SECTION: Page ", i, " / ", section, "]]"))
    for (line in strsplit(pages[i], "\n", fixed = TRUE)[[1]]) {
      heading <- trimws(line)
      if (grepl("^(?:[0-9.]+[ ]*)?(?:abstract|introduction|background|materials and methods|methods|results|discussion|conclusions?|references|bibliography|data availability|введение|материалы и методы|методы|результаты|обсуждение|заключение|список литературы|литература|доступность данных)[:.]?$", heading, ignore.case = TRUE, perl = TRUE)) {
        section <- heading
        blocks <- c(blocks, paste0("[[SECTION: Page ", i, " / ", section, "]]"))
      } else blocks <- c(blocks, line)
    }
  }
  paste(blocks, collapse = "\n")
}
