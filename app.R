script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
ROOT <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE))
} else normalizePath(getwd(), mustWork = TRUE)
options(article_parser.root = ROOT)

for (file in c(
  "src/00_common.R", "src/01_pubmed.R", "src/02_sciencedirect.R",
  "src/03_openalex.R", "src/03_crossref.R", "src/04_merge_export.R",
  "src/05_pmc_fulltext.R", "src/06_pipeline.R"
)) source(file.path(ROOT, file))

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bslib))

settings_initial <- load_project_settings()
queries_initial <- pipeline_queries(settings_initial)

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap');
:root { --ink:#102a2d; --paper:#f4f1e9; --panel:#fffdf7; --line:#d9d3c5; --accent:#d95d39; --good:#227c65; --muted:#6a716f; }
body { font-family:'IBM Plex Sans','Segoe UI',sans-serif; background:var(--paper); color:var(--ink); }
.navbar { background:var(--ink)!important; border:0; box-shadow:none; }
.navbar-brand { font-weight:700; letter-spacing:-.02em; }
.app-shell { max-width:1600px; margin:0 auto; padding:22px 22px 44px; }
.intro { display:flex; justify-content:space-between; gap:20px; align-items:end; margin-bottom:18px; }
.intro h1 { font-size:clamp(28px,4vw,48px); line-height:1; letter-spacing:-.045em; margin:0 0 8px; }
.intro p { color:var(--muted); max-width:760px; margin:0; }
.eyebrow { font-family:'IBM Plex Mono',monospace; text-transform:uppercase; letter-spacing:.08em; font-size:12px; color:var(--accent); }
.usage-strip { display:grid; grid-template-columns:repeat(3,1fr); gap:1px; margin:0 0 18px; border:1px solid var(--line); background:var(--line); }
.usage-step { background:var(--panel); padding:12px 14px; font-size:14px; }
.usage-step strong { font-family:'IBM Plex Mono',monospace; color:var(--accent); margin-right:6px; }
.control-panel,.content-panel,.metric { background:var(--panel); border:1px solid var(--line); border-radius:3px; }
.control-panel { padding:18px; position:sticky; top:18px; }
.content-panel { padding:18px; min-height:400px; }
.metric-grid { display:grid; grid-template-columns:repeat(4,minmax(150px,1fr)); gap:10px; margin-bottom:12px; }
.metric { padding:14px; }
.metric strong { display:block; font-size:26px; line-height:1; margin-bottom:7px; }
.metric span { color:var(--muted); font-size:13px; }
.btn-primary { background:var(--accent); border-color:var(--accent); border-radius:2px; font-weight:700; }
.btn-primary:hover,.btn-primary:focus { background:#bd492b; border-color:#bd492b; }
.btn-default { border-radius:2px; }
.form-control,.selectize-input { border-radius:2px; border-color:var(--line); background:#fff; }
.form-control:focus { border-color:var(--good); box-shadow:0 0 0 2px rgba(34,124,101,.14); }
textarea.form-control { font-family:'IBM Plex Mono',monospace; font-size:12px; line-height:1.45; }
.source-note { border-left:3px solid var(--good); padding:9px 11px; background:#edf5f1; font-size:13px; margin:10px 0 14px; }
.warning-note { border-left-color:var(--accent); background:#fbede7; }
.field-help,.query-help { color:var(--muted); font-size:13px; line-height:1.45; }
.field-help { margin:-8px 0 12px; }
.query-intro { display:flex; justify-content:space-between; align-items:start; gap:16px; padding:14px; background:#edf5f1; margin-bottom:12px; }
.query-intro p { margin:0; max-width:760px; }
.query-editor { border-top:1px solid var(--line); padding:12px 0; }
.query-editor summary { cursor:pointer; font-weight:600; font-size:16px; }
.query-editor .query-help { margin:8px 0; }
.run-status { margin-top:12px; padding:10px 12px; font-size:13px; border:1px solid var(--line); background:#f8f6ef; }
.run-status.running { background:#fff4d6; border-color:#d9b45d; }
.run-status.success { background:#edf5f1; border-color:#8fbaaa; }
.run-status.error { background:#fbede7; border-color:#df9b87; }
.status-log { background:#102a2d; color:#dce9e5; font-family:'IBM Plex Mono',monospace; font-size:12px; padding:14px; min-height:150px; white-space:pre-wrap; }
.nav-tabs { border-bottom-color:var(--line); }
.nav-tabs>li>a { color:var(--ink); border-radius:2px 2px 0 0; }
.nav-tabs>li.active>a { background:var(--panel); border-color:var(--line); border-bottom-color:var(--panel); }
.dataTables_wrapper { font-size:12px; }
table.dataTable thead th { background:var(--ink); color:white; }
@media(max-width:1000px){ .metric-grid{grid-template-columns:repeat(2,1fr)} .usage-strip{grid-template-columns:1fr} .control-panel{position:static} }
"

ui <- page_navbar(
  title = "SportGen Parser",
  theme = bs_theme(version = 5, bg = "#f4f1e9", fg = "#102a2d", primary = "#d95d39"),
  header = tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('search-running', function(state) {
        var button = document.getElementById('run');
        if (button) {
          button.disabled = state.running;
          button.textContent = state.running ? 'Поиск выполняется…' : 'Запустить поиск';
        }
        var status = document.querySelector('#run_status .run-status');
        if (status) {
          status.className = 'run-status ' + state.kind;
          status.textContent = state.text;
        }
      });
    "))
  ),
  nav_panel(
    "Поиск",
    div(class = "app-shell",
      div(class = "intro",
        div(
          div(class = "eyebrow", "Литературный поиск · спортивная генетика"),
          h1("Из запроса — в проверяемую таблицу"),
          p("PubMed, Elsevier и русскоязычные публикации OpenAlex. Каждое извлечённое поле сопровождается уверенностью и фрагментом-основанием.")
        ),
        div(downloadButton("download_csv", "CSV"), downloadButton("download_xlsx", "XLSX"))
      ),
      div(class = "usage-strip",
        div(class = "usage-step", strong("1"), "Выберите источники и количество статей"),
        div(class = "usage-step", strong("2"), "Нажмите «Запустить поиск» и дождитесь завершения"),
        div(class = "usage-step", strong("3"), "Проверьте результат и скачайте CSV или XLSX")
      ),
      fluidRow(
        column(3,
          div(class = "control-panel",
            h4("Параметры запуска"),
            p(class = "field-help", "Для первого запуска ничего изменять не нужно."),
            checkboxGroupInput(
              "sources", "Источники",
              choices = c("PubMed" = "pubmed", "Elsevier" = "sciencedirect",
                          "Русские публикации · OpenAlex" = "openalex"),
              selected = c("pubmed", "sciencedirect", "openalex")
            ),
            numericInput("max_records", "Статей на каждый источник",
                         value = 200, min = 0, step = 25),
            p(class = "field-help", "200 — обычный запуск. 0 — загрузить максимум, разрешённый источником."),
            checkboxInput("fulltext", "Извлекать доступные полные тексты", TRUE),
            p(class = "field-help", "Это улучшает извлечение полей, но увеличивает время обработки."),
            tags$details(
              class = "query-editor",
              tags$summary("Свой ключ Elsevier (необязательно)"),
              passwordInput("elsevier_key", NULL, value = "",
                            placeholder = "Оставьте пустым: серверный ключ уже подключён"),
              p(class = "field-help", "Введённый ключ действует только в текущем сеансе.")
            ),
            div(class = "source-note",
                "Elsevier уже подключён. Если ScienceDirect Search недоступен, используется официальный Scopus API с фильтром Elsevier."),
            actionButton(
              "run", "Запустить поиск", class = "btn-primary", width = "100%",
              onclick = "this.disabled=true; this.textContent='Поиск выполняется…'; var s=document.querySelector('#run_status .run-status'); if(s){s.className='run-status running'; s.textContent='Поиск выполняется. Не закрывайте вкладку; полный запуск может занять несколько минут.';}"
            ),
            uiOutput("run_status")
          )
        ),
        column(9,
          uiOutput("metrics"),
          div(class = "content-panel",
            navset_tab(
              nav_panel("Результаты", DTOutput("results")),
              nav_panel("Запросы · необязательно",
                div(class = "query-intro",
                  p(tags$strong("Можно ничего не менять."),
                    " Готовые запросы уже настроены отдельно для каждого источника. Этот раздел нужен только для изменения темы поиска."),
                  actionButton("reset_queries", "Вернуть исходные", class = "btn-default")
                ),
                tags$details(class = "query-editor",
                  tags$summary("PubMed — английский запрос"),
                  p(class = "query-help", "Добавляйте или удаляйте термины. AND означает «и», OR означает «или». Не удаляйте внешние скобки."),
                  textAreaInput("query_pubmed", NULL, queries_initial$pubmed, rows = 7, width = "100%")
                ),
                tags$details(class = "query-editor",
                  tags$summary("Elsevier — английский запрос"),
                  p(class = "query-help", "Запрос отдельный, потому что синтаксис Elsevier отличается от PubMed."),
                  textAreaInput("query_sciencedirect", NULL, queries_initial$sciencedirect, rows = 7, width = "100%")
                ),
                tags$details(class = "query-editor",
                  tags$summary("OpenAlex — запрос для русскоязычных публикаций"),
                  p(class = "query-help", "Используйте русские термины. Результаты дополнительно фильтруются по генетике и физической активности."),
                  textAreaInput("query_openalex", NULL, queries_initial$openalex, rows = 6, width = "100%")
                )
              ),
              nav_panel("Журнал", verbatimTextOutput("log", placeholder = TRUE)),
              nav_panel("Ограничения",
                div(class = "source-note warning-note",
                    "Платные полные тексты не обходятся. Поля заполняются из метаданных, аннотаций, PMC и легально доступных открытых текстов. Пустое поле означает, что подтверждаемое значение не найдено."),
                tags$ul(
                  tags$li("OpenAlex заменяет нестабильный веб-парсинг eLIBRARY и проходит строгую локальную фильтрацию."),
                  tags$li("Crossref только дополняет DOI и библиографию при высоком совпадении названия."),
                  tags$li("extraction_evidence содержит раздел и исходный фрагмент для ручной проверки.")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  result_data <- reactiveVal(data.frame())
  run_state <- reactiveVal(list(kind = "idle", text = "Готово к запуску. Запросы уже настроены."))
  log_lines <- reactiveVal("Приложение готово. Запросы уже настроены; нажмите «Запустить поиск».")
  append_log <- function(text) {
    stamp <- format(Sys.time(), "%H:%M:%S")
    log_lines(paste(log_lines(), paste0("[", stamp, "] ", text), sep = "\n"))
  }

  output$log <- renderText(log_lines())

  output$run_status <- renderUI({
    state <- run_state()
    div(class = paste("run-status", state$kind), state$text)
  })

  observeEvent(input$reset_queries, {
    updateTextAreaInput(session, "query_pubmed", value = queries_initial$pubmed)
    updateTextAreaInput(session, "query_sciencedirect", value = queries_initial$sciencedirect)
    updateTextAreaInput(session, "query_openalex", value = queries_initial$openalex)
    showNotification("Исходные запросы восстановлены.", type = "message")
  })

  output$metrics <- renderUI({
    df <- result_data()
    sources <- count_source_labels(df$source)
    genes <- if (nrow(df) > 0) sum(nzchar(df$gene)) else 0
    evidence <- if (nrow(df) > 0) sum(nzchar(df$extraction_evidence)) else 0
    div(class = "metric-grid",
      div(class = "metric", strong(nrow(df)), span("строк после дедупликации")),
      div(class = "metric", strong(sources), span("источников в выдаче")),
      div(class = "metric", strong(genes), span("строк с найденным геном")),
      div(class = "metric", strong(evidence), span("строк с доказательствами"))
    )
  })

  output$results <- renderDT({
    df <- result_data()
    if (nrow(df) == 0) return(datatable(data.frame(Статус = "Результатов пока нет"), options = list(dom = "t")))
    datatable(
      df, filter = "top", rownames = FALSE, escape = TRUE,
      extensions = c("Scroller", "FixedColumns"),
      options = list(
        pageLength = 25, scrollX = TRUE, scrollY = 620, deferRender = TRUE,
        scroller = TRUE, fixedColumns = list(leftColumns = 3),
        search = list(regex = FALSE), lengthMenu = c(10, 25, 50, 100)
      )
    )
  }, server = FALSE)

  observeEvent(input$run, {
    req(length(input$sources) > 0)
    settings <- load_project_settings()
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
    log_lines("Запуск начат.")
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
        run_state(list(kind = "success", text = paste("Готово:", nrow(result), "строк. Таблицу можно проверить и скачать.")))
        session$sendCustomMessage("search-running", list(
          running = FALSE, kind = "success",
          text = paste("Готово:", nrow(result), "строк. Таблицу можно проверить и скачать.")
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
    content = function(file) readr::write_csv(result_data(), file)
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("sportgen_articles_", Sys.Date(), ".xlsx"),
    content = function(file) openxlsx::write.xlsx(result_data(), file, overwrite = TRUE)
  )
}

shinyApp(ui, server)
