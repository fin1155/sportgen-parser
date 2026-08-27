script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- if (length(script_arg) > 0) dirname(normalizePath(sub("^--file=", "", script_arg[1]))) else getwd()
port <- suppressWarnings(as.integer(Sys.getenv("SPORTGEN_PORT", unset = "3838")))
if (is.na(port)) port <- 3838L
shiny::runApp(file.path(root, "app.R"), host = "127.0.0.1", port = port, launch.browser = TRUE)
