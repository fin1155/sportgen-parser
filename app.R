script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
ROOT <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE))
} else normalizePath(getwd(), mustWork = TRUE)
options(article_parser.root = ROOT)
for (file in c(
  "src/00_common.R", "src/01_pubmed.R", "src/02_sciencedirect.R", "src/03_openalex.R", "src/03_crossref.R",
  "src/04_merge_export.R", "src/05_pmc_fulltext.R", "src/05b_extraction.R", "src/05c_documents.R",
  "src/05d_biomedical.R", "src/06_pipeline.R", "src/07_evidence_archive.R", "src/08_ui.R"
)) source(file.path(ROOT, file))
options(shiny.maxRequestSize = 100 * 1024^2)
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bslib))
settings_initial <- load_project_settings()
queries_initial <- pipeline_queries(settings_initial)
ui <- research_ui(queries_initial)

server <- function(input, output, session) {
  result_data <- reactiveVal(assess_evidence(enrich_final_table(build_final_table(pubmed_empty_df(), sd_empty_df(), openalex_empty_df(), settings_initial), settings = settings_initial)))
  run_state <- reactiveVal(list(kind = "idle", text = "Готово к запуску. Запросы уже настроены."))
  log_lines <- reactiveVal("Приложение готово. Запросы уже настроены; нажмите «Запустить поиск».")
  table_version <- reactiveVal(0L)
  review_undo <- reactiveVal(NULL)
  append_log <- function(text) {
    stamp <- format(Sys.time(), "%H:%M:%S")
    log_lines(paste(log_lines(), paste0("[", stamp, "] ", text), sep = "\n"))
  }

  output$log <- renderText(log_lines())

  output$run_status <- renderUI({
    state <- run_state()
    div(class = paste("run-status", state$kind), state$text)
  })

  selected_columns <- reactive(valid_table_columns(input$table_columns))
  observeEvent(list(result_data(), selected_columns()), {
    table_version(isolate(table_version()) + 1L)
  }, priority = 100)

  set_columns <- function(columns) {
    session$sendCustomMessage("column-preset", list(columns = columns, apply = TRUE))
  }
  observeEvent(input$show_results, updateTabsetPanel(session, "workspace_tab", selected = "results"))

  export_data <- reactive({
    df <- result_data()
    view <- input$results_view
    if (!isTRUE(input$export_filtered %||% TRUE) || is.null(view)) return(df)
    # A client view belongs to one exact render, never a previous search or schema.
    if (!identical(as.integer(view$version), as.integer(table_version()))) return(df)
    rows <- suppressWarnings(as.integer(unlist(view$rows, use.names = FALSE)))
    if (anyNA(rows) || anyDuplicated(rows) || any(rows < 1L | rows > nrow(df))) return(df)
    subset_research_rows(df, rows)
  })

  visible_result_data <- reactive({
    df <- result_data()
    if (ncol(df) == 0) return(df)
    select_output_columns(df, selected_columns())
  })

  observeEvent(result_data(), {
    df <- result_data()
    if (!nrow(df)) return()
    selected <- isolate(input$review_article)
    if (is.null(selected) || !selected %in% df$article_id) selected <- df$article_id[1]
    updateSelectInput(session, "review_article", choices = setNames(df$article_id, paste(df$year, df$title)), selected = selected)
  })
  review_index <- reactive({ req(nrow(result_data()), input$review_article); match(input$review_article, result_data()$article_id) })
  observeEvent(list(input$review_article, input$review_field, result_data()), {
    req(input$review_field, input$review_article)
    i <- review_index(); req(!is.na(i))
    updateTextAreaInput(session, "review_value", value = result_data()[[input$review_field]][i])
    updateTextAreaInput(session, "review_basis", value = "")
  })
  output$review_evidence <- renderText({
    i <- review_index(); req(!is.na(i), input$review_field)
    evidence <- tryCatch(jsonlite::fromJSON(result_data()$extraction_evidence[i], simplifyVector = FALSE), error = function(e) list())
    item <- evidence[[input$review_field]]
    if (is.null(item)) "Основание не извлечено." else paste(item$section, item$snippet, sep = "\n")
  })
  output$review_document <- renderText({
    req(input$review_article)
    (attr(result_data(), "documents") %||% list())[[input$review_article]] %||% "Текст не сохранён."
  })
  observeEvent(input$save_review, {
    tryCatch({
      next_data <- review_article_field(result_data(), input$review_article, input$review_field, input$review_value, input$review_basis)
      review_undo(result_data()); result_data(next_data)
      showNotification("Поле подтверждено. Скачайте архив, чтобы сохранить проверку.", type = "message", id = "review-state", duration = 3)
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$undo_review, {
    req(!is.null(review_undo()))
    result_data(review_undo()); review_undo(NULL)
    showNotification("Последняя проверка отменена.", type = "message", id = "review-state", duration = 3)
  })
  observeEvent(input$import_archive, {
    req(input$import_archive$datapath)
    tryCatch({
      imported <- read_research_archive(input$import_archive$datapath)
      result_data(imported); review_undo(NULL)
      set_columns(table_preferences(attr(imported, "table_settings"))$columns)
      run_state(list(kind = "success", text = paste("Архив открыт:", nrow(imported), "статей. Доступна локальная проверка.")))
      append_log(paste("Открыт архив:", nrow(imported), "статей. Сеть не использовалась."))
      showNotification("Архив открыт.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$reprocess_archive, {
    tryCatch({
      next_data <- reprocess_archive(result_data())
      review_undo(result_data()); result_data(next_data)
      showNotification("Извлечение повторено без сети. Подтверждённые поля сохранены.", type = "message", id = "review-state", duration = 3)
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$associations <- renderDT({ datatable(attr(result_data(), "associations") %||% empty_associations(), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE, pageLength = 10)) }, server = FALSE)
  output$evidence_summary <- renderDT({ datatable(association_summary(result_data()), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE)) }, server = FALSE)
  output$evidence_profiles <- renderDT({
    df <- result_data(); req(nrow(df))
    datatable(evidence_display(df), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE, pageLength = 10))
  }, server = FALSE)

  output$column_summary <- renderUI({
    p(class = "column-summary", paste("В таблице:", length(selected_columns()), "из", length(TABLE_COLUMN_LABELS), "колонок"))
  })
  output$preview_note <- renderUI({
    p(class = "field-help", if (nrow(result_data())) "Первые 3 статьи текущего поиска" else "Пример структуры · условные данные")
  })
  output$column_preview <- renderUI({
    columns <- valid_table_columns(input$column_draft, fallback = character())
    if (!length(columns)) return(p("Выберите колонки галочками — здесь появится пример таблицы."))
    df <- result_data()
    if (nrow(df)) {
      values <- utils::head(df[, columns, drop = FALSE], 3)
    } else {
      values <- as.data.frame(setNames(lapply(columns, function(field) rep("—", 2)), columns), stringsAsFactors = FALSE)
      samples <- list(title = c("Исследование 1", "Исследование 2"), year = c("2025", "2024"),
        disease = c("Заболевание A", "Заболевание B"), cell_line = c("Линия A", "Линия B"),
        mirna = c("miRNA A", "miRNA B"), target_gene = c("Ген A", "Ген B"), expression_change = c("Повышена", "Снижена"))
      for (field in intersect(columns, names(samples))) values[[field]] <- samples[[field]]
    }
    tags$table(class = "preview-table",
      tags$thead(tags$tr(lapply(columns, function(field) tags$th(unname(TABLE_COLUMN_LABELS[[field]]))))),
      tags$tbody(lapply(seq_len(nrow(values)), function(i) tags$tr(lapply(columns, function(field) tags$td(truncate_display_text(values[[field]][i], 120L)))))))
  })
  observeEvent(input$research_profile, {
    profile <- input$research_profile
    req(profile %in% names(RESEARCH_PROFILES))
    queries <- profile_queries(profile, settings_initial)
    for (source in names(queries)) updateTextAreaInput(session, paste0("query_", source), value = queries[[source]])
    updateSelectInput(session, "openalex_scope", selected = if (profile == "sports") "russian" else "global")
    set_columns(TABLE_COLUMN_PRESETS[[RESEARCH_PROFILES[[profile]]$preset]])
  }, ignoreInit = TRUE)
  observeEvent(input$reset_queries, {
    queries <- profile_queries(input$research_profile %||% "sports", settings_initial)
    for (source in names(queries)) updateTextAreaInput(session, paste0("query_", source), value = queries[[source]])
    showNotification("Запросы выбранной темы восстановлены.", type = "message")
  })
  output$metrics <- renderUI({
    df <- result_data()
    div(class = "result-metrics",
      span(strong(nrow(df)), " статей"),
      span(strong(count_source_labels(df$source)), " источников"),
      span(strong(length(selected_columns())), " колонок"))
  })
  output$export_summary <- renderUI({
    p(paste("В выгрузке:", nrow(export_data()), "строк ·", length(selected_columns()), "колонок"))
  })
  output$results <- renderDT({
    df <- visible_result_data()
    version <- table_version()
    if (!nrow(df)) return(datatable(data.frame(Статус = "Результатов пока нет. Настройте тему и запустите поиск или откройте архив."), rownames = FALSE, options = list(dom = "t")))
    display <- prepare_table_display(df, links_from = result_data())
    escape_columns <- setdiff(seq_along(display$data), display$link_columns)
    datatable(display$data, colnames = unname(TABLE_COLUMN_LABELS[names(df)]),
      filter = "top", rownames = FALSE, selection = "none", escape = escape_columns,
      callback = JS(sprintf("function publishView() { Shiny.setInputValue('results_view', {version: %d, rows: table.rows({search:'applied',order:'applied'}).indexes().toArray().map(function(i){return i+1;})}, {priority:'event'}); } table.on('draw.dt', publishView); publishView();", version)),
      options = list(pageLength = 25, scrollX = TRUE, scrollY = "65vh", scrollCollapse = FALSE,
        deferRender = TRUE, order = list(), search = list(regex = FALSE), lengthMenu = c(10, 25, 50, 100),
        language = list(search = "Поиск в таблице:", lengthMenu = "Строк на странице: _MENU_",
          info = "_START_–_END_ из _TOTAL_", infoEmpty = "Нет строк", infoFiltered = "(всего _MAX_)",
          zeroRecords = "По текущим фильтрам ничего не найдено.", emptyTable = "Статей пока нет.",
          paginate = list(first = "Первая", previous = "Назад", "next" = "Далее", last = "Последняя"))))
  }, server = FALSE)

  observeEvent(input$run, {
    validation_error <- if (!length(input$sources)) "Выберите хотя бы один источник." else
      if (length(input$max_records) != 1L || is.na(input$max_records) || !is.finite(input$max_records) ||
          input$max_records < 0 || input$max_records != floor(input$max_records) || input$max_records > .Machine$integer.max)
        "Количество статей должно быть целым неотрицательным числом." else NULL
    if (!is.null(validation_error)) {
      run_state(list(kind = "error", text = validation_error))
      session$sendCustomMessage("search-running", list(running = FALSE, kind = "error", text = validation_error))
      showNotification(validation_error, type = "error")
      return()
    }
    settings <- load_project_settings()
    settings$research_profile <- input$research_profile %||% "sports"
    settings$topic_filter <- isTRUE(input$topic_filter %||% TRUE)
    settings$openalex$scope <- input$openalex_scope %||% if (settings$research_profile == "sports") "russian" else "global"
    year_range <- tryCatch(
      publication_year_range(list(publication_year = list(
        from = input$year_from %||% "", to = input$year_to %||% ""
      ))),
      error = function(e) {
        message <- conditionMessage(e)
        run_state(list(kind = "error", text = message))
        session$sendCustomMessage("search-running", list(
          running = FALSE, kind = "error", text = message
        ))
        showNotification(message, type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(year_range)) return()
    settings$publication_year <- list(
      from = if (is.na(year_range$from)) "" else as.character(year_range$from),
      to = if (is.na(year_range$to)) "" else as.character(year_range$to)
    )
    if (nzchar(input$elsevier_key)) {
      settings$runtime_secrets <- list(ELSEVIER_API_KEY = input$elsevier_key)
    }
    settings$pubmed$enabled <- "pubmed" %in% input$sources
    settings$sciencedirect$enabled <- "sciencedirect" %in% input$sources
    settings$openalex$enabled <- "openalex" %in% input$sources
    settings$pubmed$max_records <- as.integer(input$max_records)
    settings$sciencedirect$max_records <- as.integer(input$max_records)
    settings$openalex$max_records <- as.integer(input$max_records)
    settings$pmc$enabled <- isTRUE(input$fulltext)
    settings$fulltext$enabled <- isTRUE(input$fulltext)
    queries <- list(pubmed = input$query_pubmed,
                    sciencedirect = input$query_sciencedirect,
                    openalex = input$query_openalex)
    enabled_queries <- unlist(queries[input$sources], use.names = FALSE)
    if (any(!nzchar(trimws(enabled_queries)))) {
      message <- "Заполните запрос для каждого выбранного источника в разделе «Поиск и фильтры»."
      run_state(list(kind = "error", text = message))
      session$sendCustomMessage("search-running", list(running = FALSE, kind = "error", text = message))
      showNotification(message, type = "error")
      return()
    }
    log_lines(paste("Запуск начат. Тема:", RESEARCH_PROFILES[[research_profile(settings)]]$label))
    if (isTRUE(year_range$active)) {
      append_log(paste0(
        "Диапазон публикации: ",
        if (is.na(year_range$from)) "без нижней границы" else year_range$from,
        " — ",
        if (is.na(year_range$to)) "без верхней границы" else year_range$to
      ))
    }
    run_state(list(kind = "running", text = "Поиск выполняется. Не закрывайте вкладку; полный запуск может занять несколько минут."))
    stages <- c(pubmed = 0.10, sciencedirect = 0.25, openalex = 0.40, crossref = 0.55,
                merge = 0.65, fulltext = 0.75, extract = 0.88, export = 0.96, done = 1)
    tryCatch(
      withProgress(message = "Поиск и обработка статей", value = 0, {
        warnings <- character(0)
        result <- withCallingHandlers(
          run_pipeline(
            # Веб-сеансы не пишут в общий out/: каждый пользователь скачивает
            # собственный reactive-результат через downloadHandler ниже.
            settings = settings, queries = queries, export = FALSE,
            progress = function(stage, detail) {
              setProgress(value = unname(stages[[stage]] %||% 0), detail = detail)
              append_log(detail)
            }
          ),
          warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        if (length(warnings) > 0) for (warning in unique(warnings)) append_log(paste("Предупреждение:", warning))
        result_data(result)
        review_undo(NULL)
        finished_kind <- if (length(warnings)) "warning" else "success"
        finished_text <- paste(if (length(warnings)) "Готово с ограничениями:" else "Готово:", nrow(result), "строк.", if (length(warnings)) "Проверьте журнал источников." else "Таблицу можно проверить и скачать.")
        run_state(list(kind = finished_kind, text = finished_text))
        session$sendCustomMessage("search-running", list(
          running = FALSE, kind = finished_kind, text = finished_text
        ))
      }),
      error = function(e) {
        append_log(paste("Ошибка:", conditionMessage(e)))
        run_state(list(kind = "error", text = "Поиск не завершён. Подробности записаны во вкладке «Журнал»."))
        session$sendCustomMessage("search-running", list(
          running = FALSE, kind = "error",
          text = "Поиск не завершён. Подробности записаны во вкладке «Журнал»."
        ))
        showNotification("Поиск не завершён. Откройте вкладку «Журнал».", type = "error", duration = NULL)
      }
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("sportgen_articles_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(select_output_columns(export_data(), selected_columns()), file)
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("sportgen_articles_", Sys.Date(), ".xlsx"),
    content = function(file) write_research_workbook(export_data(), file, selected_columns())
  )
  output$download_archive <- downloadHandler(
    filename = function() paste0("sportgen_archive_", Sys.Date(), ".json"),
    content = function(file) {
      df <- result_data()
      attr(df, "table_settings") <- list(columns = selected_columns())
      write_research_archive(df, file)
    }
  )
  output$download_associations <- downloadHandler(
    filename = function() paste0("sportgen_associations_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(attr(result_data(), "associations") %||% empty_associations(), file)
  )
}

shinyApp(ui, server)
