script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
this_file <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[1]) else "tools/update_hgnc_symbols.R"
ROOT <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)

source_url <- "https://storage.googleapis.com/public-download-files/hgnc/tsv/tsv/hgnc_complete_set.txt"
temporary <- tempfile(fileext = ".tsv")
on.exit(unlink(temporary), add = TRUE)
download.file(source_url, temporary, mode = "wb", quiet = FALSE)

data <- read.delim(temporary, sep = "\t", quote = "", check.names = FALSE,
                   stringsAsFactors = FALSE, encoding = "UTF-8")
if (!all(c("symbol", "status") %in% names(data))) {
  stop("HGNC-файл не содержит ожидаемые столбцы symbol/status")
}
symbols <- sort(unique(data$symbol[data$status == "Approved" & nzchar(data$symbol)]))
if (length(symbols) < 40000) stop("HGNC-файл подозрительно мал: ", length(symbols), " символов")

header <- c(
  "# HGNC-approved human gene symbols, one per line.",
  paste0("# Source: ", source_url),
  paste0("# Updated: ", Sys.Date())
)
writeLines(c(header, symbols), file.path(ROOT, "config", "hgnc_symbols.txt"), useBytes = TRUE)
cat("Сохранено HGNC-символов:", length(symbols), "\n")
