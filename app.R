script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
ROOT <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE))
} else normalizePath(getwd(), mustWork = TRUE)
options(article_parser.root = ROOT)

for (file in c(
  "src/00_common.R", "src/01_pubmed.R", "src/02_sciencedirect.R",
  "src/03_openalex.R", "src/03_crossref.R", "src/04_merge_export.R",
  "src/05_pmc_fulltext.R", "src/05b_extraction.R", "src/05c_documents.R",
  "src/06_pipeline.R", "src/07_evidence_archive.R"
)) source(file.path(ROOT, file))

options(shiny.maxRequestSize = 100 * 1024^2)
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bslib))

settings_initial <- load_project_settings()
queries_initial <- pipeline_queries(settings_initial)

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap');
:root { --ink:#102a2d; --paper:#f4f1e9; --panel:#fffdf7; --line:#d9d3c5; --accent:#d95d39; --good:#227c65; --muted:#6a716f; }
html,body { max-width:100%; overflow-x:hidden; }
body { font-family:'IBM Plex Sans','Segoe UI',sans-serif; background:var(--paper); color:var(--ink); }
.navbar { background:var(--ink)!important; border:0; box-shadow:none; }
.navbar-brand { font-weight:700; letter-spacing:-.02em; }
.app-shell,.app-shell *,.app-shell *::before,.app-shell *::after { box-sizing:border-box; }
.app-shell { width:100%; max-width:none; margin:0 auto; padding:18px 18px 40px; }
.intro { display:flex; justify-content:space-between; gap:20px; align-items:end; margin-bottom:18px; }
.intro-copy { min-width:0; }
.intro h1 { font-size:clamp(28px,4vw,48px); line-height:1; letter-spacing:-.045em; margin:0 0 8px; }
.intro p { color:var(--muted); max-width:760px; margin:0; }
.export-actions { display:flex; flex:0 0 auto; gap:8px; }
.export-actions .btn { min-width:104px; margin:0; white-space:nowrap; }
.eyebrow { font-family:'IBM Plex Mono',monospace; text-transform:uppercase; letter-spacing:.08em; font-size:12px; color:var(--accent); }
.usage-strip { display:grid; grid-template-columns:repeat(3,1fr); gap:1px; margin:0 0 18px; border:1px solid var(--line); background:var(--line); }
.usage-step { min-width:0; overflow-wrap:anywhere; background:var(--panel); padding:12px 14px; font-size:14px; }
.usage-step strong { font-family:'IBM Plex Mono',monospace; color:var(--accent); margin-right:6px; }
.workspace-row { display:grid; grid-template-columns:minmax(260px,290px) minmax(0,1fr); gap:16px; margin:0; }
.workspace-row::before,.workspace-row::after { display:none; }
.workspace-row > .sidebar-column,.workspace-row > .main-column { float:none; width:auto; min-width:0; padding:0; }
.control-panel,.content-panel,.metric { background:var(--panel); border:1px solid var(--line); border-radius:3px; }
.control-panel { padding:18px; position:sticky; top:18px; }
.content-panel { min-width:0; padding:14px; min-height:calc(100vh - 310px); overflow:hidden; }
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
.filter-label { font-weight:600; margin:4px 0 8px; }
.year-range { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:10px; }
.year-range .form-group { margin-bottom:10px; }
.year-range .shiny-input-container,.year-range .form-control { width:100%!important; min-width:0; }
.column-config { max-width:980px; }
.column-config h4 { margin:2px 0 8px; }
.column-presets { display:flex; flex-wrap:wrap; gap:8px; margin:14px 0 16px; }
.column-presets .btn { margin:0; }
.column-summary { color:var(--muted); font-family:'IBM Plex Mono',monospace; font-size:12px; margin:8px 0 0; }
.column-config .selectize-input { min-height:94px; max-height:220px; overflow-y:auto; padding:8px; }
.column-config .selectize-control.multi .selectize-input > div { background:#edf5f1; border:1px solid #b8cec5; border-radius:2px; color:var(--ink); }
.query-intro { display:flex; justify-content:space-between; align-items:start; gap:16px; padding:14px; background:#edf5f1; margin-bottom:12px; }
.query-intro p { margin:0; max-width:760px; }
.query-editor { border-top:1px solid var(--line); padding:12px 0; }
.query-editor summary { cursor:pointer; font-weight:600; font-size:16px; }
.query-editor .query-help { margin:8px 0; }
.run-status { margin-top:12px; padding:10px 12px; font-size:13px; border:1px solid var(--line); background:#f8f6ef; }
.run-status.running { background:#fff4d6; border-color:#d9b45d; }
.run-status.success { background:#edf5f1; border-color:#8fbaaa; }
.run-status.warning { background:#fff4d6; border-color:#d9b45d; }
.run-status.error { background:#fbede7; border-color:#df9b87; }
.status-log { background:#102a2d; color:#dce9e5; font-family:'IBM Plex Mono',monospace; font-size:12px; padding:14px; min-height:150px; white-space:pre-wrap; }
.nav-tabs { border-bottom-color:var(--line); }
.content-panel .tabbable { min-width:0; }
.content-panel .tab-content { min-width:0; overflow-x:auto; }
.result-help { color:var(--muted); font-size:13px; margin:10px 0 12px; }
.nav-tabs>li>a { color:var(--ink); border-radius:2px 2px 0 0; }
.nav-tabs>li.active>a { background:var(--panel); border-color:var(--line); border-bottom-color:var(--panel); }
.dataTables_wrapper { width:100%; max-width:100%; font-size:13px; }
.dataTables_wrapper .dataTables_scroll { max-width:100%; }
.dataTables_wrapper .dataTables_scrollBody { overflow:auto!important; }
table.dataTable { width:100%!important; }
table.dataTable thead th { background:var(--ink); color:white; white-space:nowrap; }
table.dataTable tbody td { max-width:360px; vertical-align:top; white-space:normal!important; overflow-wrap:anywhere; word-break:normal; }
.article-link { color:var(--good); font-weight:600; text-decoration:underline; text-decoration-thickness:1px; text-underline-offset:2px; }
.article-link:hover,.article-link:focus { color:var(--accent); }
.article-title-link { display:block; min-width:300px; max-width:520px; white-space:normal!important; overflow-wrap:anywhere; }
.article-id-link { white-space:nowrap; }
.article-open-link { white-space:nowrap; }
.expandable-cell { line-height:1.45; }
.expandable-cell-authors { min-width:230px; max-width:340px; }
.expandable-cell-abstract { min-width:340px; max-width:540px; }
.cell-preview,.cell-full { display:block; }
.cell-preview[hidden],.cell-full[hidden] { display:none!important; }
.cell-expand-button { display:inline-flex; align-items:center; min-height:30px; margin:6px 0 0; padding:0; border:0; background:transparent; color:var(--good); font:600 12px/1.2 'IBM Plex Sans','Segoe UI',sans-serif; text-decoration:underline; text-underline-offset:2px; cursor:pointer; }
.cell-expand-button:hover,.cell-expand-button:focus { color:var(--accent); outline:none; }
.cell-expand-button:focus-visible { outline:2px solid var(--good); outline-offset:2px; }
@media(max-width:1100px){
  .workspace-row{grid-template-columns:1fr}
  .control-panel{position:static}
  .control-panel .year-range{max-width:520px}
}
@media(max-width:900px){
  .app-shell{padding:18px 16px 36px}
  .intro{align-items:flex-start; flex-direction:column}
  .export-actions{width:100%}
  .export-actions .btn{flex:1 1 0}
  .metric-grid{grid-template-columns:repeat(2,minmax(0,1fr))}
  .usage-strip{grid-template-columns:1fr}
  .nav-tabs{display:flex; flex-wrap:wrap}
  .nav-tabs>li{float:none}
}
@media(max-width:540px){
  .app-shell{padding:14px 12px 28px}
  .intro h1{font-size:34px}
  .export-actions{flex-direction:column}
  .year-range{grid-template-columns:1fr}
  .metric-grid{grid-template-columns:1fr 1fr}
  .control-panel,.content-panel{padding:14px}
  .column-presets{display:grid; grid-template-columns:1fr}
  .column-presets .btn{width:100%}
}
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
      document.addEventListener('click', function(event) {
        var button = event.target.closest('.cell-expand-button');
        if (!button) return;
        var cell = button.closest('.expandable-cell');
        if (!cell) return;
        var preview = cell.querySelector('.cell-preview');
        var full = cell.querySelector('.cell-full');
        var expanded = button.getAttribute('aria-expanded') === 'true';
        button.setAttribute('aria-expanded', expanded ? 'false' : 'true');
        button.textContent = expanded ? 'Показать полностью' : 'Свернуть';
        if (preview) preview.hidden = !expanded;
        if (full) full.hidden = expanded;
      });
    "))
  ),
  nav_panel(
    "Поиск",
    div(class = "app-shell",
      div(class = "intro",
        div(class = "intro-copy",
          div(class = "eyebrow", "Литературный поиск · спортивная генетика"),
          h1("Из запроса — в проверяемую таблицу"),
          p("PubMed, Elsevier и русскоязычные публикации OpenAlex. Извлечённые данные сопровождаются исходными фрагментами и статусом проверки.")
        ),
        div(class = "export-actions",
            downloadButton("download_csv", "CSV"),
            downloadButton("download_xlsx", "XLSX"))
      ),
      div(class = "usage-strip",
        div(class = "usage-step", strong("1"), "Выберите источники и количество статей"),
        div(class = "usage-step", strong("2"), "Нажмите «Запустить поиск» и дождитесь завершения"),
        div(class = "usage-step", strong("3"), "Проверьте результат и скачайте CSV или XLSX")
      ),
      fluidRow(class = "workspace-row",
        column(3, class = "sidebar-column",
          div(class = "control-panel",
            h4("Параметры запуска"),
            p(class = "field-help", "Для первого запуска ничего изменять не нужно."),
            checkboxGroupInput(
              "sources", "Источники",
              choices = c("PubMed" = "pubmed", "Elsevier" = "sciencedirect",
                          "Русские публикации · OpenAlex" = "openalex"),
              selected = c("pubmed", "sciencedirect", "openalex")
            ),
            div(class = "filter-label", "Год публикации (необязательно)"),
            div(class = "year-range",
              textInput("year_from", "С", value = "", placeholder = "2020"),
              textInput("year_to", "По", value = "", placeholder = "2025")
            ),
            p(class = "field-help", "Оставьте оба поля пустыми, чтобы искать за всё время. Можно заполнить только одно поле."),
            numericInput("max_records", "Статей на каждый источник",
                         value = 200, min = 0, step = 1),
            p(class = "field-help", "200 — обычный запуск. 0 — загрузить максимум, разрешённый источником."),
            checkboxInput("fulltext", "Извлекать доступные полные тексты", TRUE),
            p(class = "field-help", "Это улучшает извлечение полей, но увеличивает время обработки."),
            tags$details(
              class = "query-editor",
              tags$summary("Свой ключ Elsevier (необязательно)"),
              passwordInput("elsevier_key", NULL, value = "",
                            placeholder = "Серверный ключ или ключ текущего сеанса"),
              p(class = "field-help", "Введённый ключ действует только в текущем сеансе.")
            ),
            div(class = "source-note",
                "Для Elsevier нужен ключ. Если ScienceDirect Search недоступен, используется Scopus с фильтром Elsevier; маршрут показан в таблице и журнале."),
            actionButton(
              "run", "Запустить поиск", class = "btn-primary", width = "100%",
              onclick = "this.disabled=true; this.textContent='Поиск выполняется…'; var s=document.querySelector('#run_status .run-status'); if(s){s.className='run-status running'; s.textContent='Поиск выполняется. Не закрывайте вкладку; полный запуск может занять несколько минут.';}"
            ),
            uiOutput("run_status")
          )
        ),
        column(9, class = "main-column",
          uiOutput("metrics"),
          div(class = "content-panel",
            navset_tab(
              nav_panel("Результаты",
                p(class = "result-help",
                  "Нажмите на название статьи, PMID, DOI или «Открыть статью ↗», чтобы перейти к публикации в новой вкладке."),
                DTOutput("results")
              ),
              nav_panel("Ассоциации",
                p(class = "result-help", "Одна запись — один фрагмент с SNP и результатом. Неоднозначные сравнения сохранены без автоматического присвоения чисел."),
                downloadButton("download_associations", "Ассоциации CSV"),
                DTOutput("associations")
              ),
              nav_panel("Проверка",
                p(class = "result-help", "Выберите статью и поле. Сверьте значение с сохранённым текстом или оригиналом. Подтверждение сохраняется вместе с основанием; его можно отменить."),
                selectInput("review_article", "Статья", choices = character(), width = "100%"),
                selectInput("review_field", "Поле", choices = setNames(REVIEWABLE_COLS, unname(TABLE_COLUMN_LABELS[REVIEWABLE_COLS]))),
                textAreaInput("review_value", "Значение", rows = 3, width = "100%"),
                textAreaInput("review_basis", "Основание: цитата, раздел / страница или пояснение", rows = 3, width = "100%"),
                actionButton("save_review", "Подтвердить поле"),
                actionButton("undo_review", "Отменить последнюю проверку"),
                tags$details(tags$summary("Основание текущего значения"), verbatimTextOutput("review_evidence")),
                tags$details(tags$summary("Сохранённый текст статьи"), verbatimTextOutput("review_document"))
              ),
              nav_panel("Доказательства",
                p(class = "result-help", "Предварительный профиль: дизайн, точность и применимость. Это не GRADE и не оценка OpenEvidence. Число публикаций не доказывает независимую репликацию; размер выборки и название журнала не дают автоматического рейтинга."),
                DTOutput("evidence_profiles"),
                h4("Публикации по сопоставляемым признакам"),
                DTOutput("evidence_summary")
              ),
              nav_panel("Архив",
                p(class = "result-help", "Архив содержит таблицу, доступные тексты, ассоциации, запросы и историю ручной проверки. Скачайте его для продолжения работы без повторного поиска. Ключи в архив не входят."),
                downloadButton("download_archive", "Скачать архив JSON"),
                fileInput("import_archive", "Открыть сохранённый архив", accept = ".json"),
                actionButton("reprocess_archive", "Повторить извлечение из архива"),
                p(class = "field-help", "Повторное извлечение выполняется без сети и сохраняет подтверждённые пользователем поля.")
              ),
              nav_panel("Столбцы",
                div(class = "column-config",
                  h4("Настройка таблицы и экспорта"),
                  p(class = "query-help",
                    "Выберите нужные поля. Порядок плашек совпадает с порядком столбцов; плашки можно перетаскивать. CSV и XLSX используют этот же набор."),
                  div(class = "column-presets",
                    actionButton("columns_core", "Основные", class = "btn-default"),
                    actionButton("columns_genetics", "Генетика и результаты", class = "btn-default"),
                    actionButton("columns_all", "Все поля", class = "btn-default")
                  ),
                  selectizeInput(
                    "table_columns", "Показываемые столбцы и их порядок",
                    choices = setNames(names(TABLE_COLUMN_LABELS), unname(TABLE_COLUMN_LABELS)),
                    selected = TABLE_COLUMN_PRESETS$all, multiple = TRUE,
                    options = list(plugins = list("remove_button", "drag_drop"),
                                   placeholder = "Начните вводить название столбца")
                  ),
                  uiOutput("column_summary")
                )
              ),
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
                  tags$li("OpenAlex ищет русскоязычные и российские публикации; его охват не совпадает с eLIBRARY. Прямой поиск eLIBRARY не подключён."),
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
  result_data <- reactiveVal(assess_evidence(enrich_final_table(build_final_table(pubmed_empty_df(), sd_empty_df(), openalex_empty_df(), settings_initial), settings = settings_initial)))
  run_state <- reactiveVal(list(kind = "idle", text = "Готово к запуску. Запросы уже настроены."))
  log_lines <- reactiveVal("Приложение готово. Запросы уже настроены; нажмите «Запустить поиск».")
  empty_columns_reset <- reactiveVal(FALSE)
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

  selected_columns <- reactive({
    requested <- input$table_columns
    if (is.null(requested)) return(TABLE_COLUMN_PRESETS$core)
    valid <- unique(as.character(requested))
    valid <- valid[valid %in% names(TABLE_COLUMN_LABELS)]
    if (length(valid) == 0) TABLE_COLUMN_PRESETS$core else valid
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
      showNotification("Поле подтверждено. Скачайте архив, чтобы сохранить проверку.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$undo_review, {
    req(!is.null(review_undo()))
    result_data(review_undo()); review_undo(NULL)
    showNotification("Последняя проверка отменена.", type = "message")
  })
  observeEvent(input$import_archive, {
    req(input$import_archive$datapath)
    tryCatch({
      imported <- read_research_archive(input$import_archive$datapath)
      result_data(imported); review_undo(NULL)
      run_state(list(kind = "success", text = paste("Архив открыт:", nrow(imported), "статей. Доступна локальная проверка.")))
      append_log(paste("Открыт архив:", nrow(imported), "статей. Сеть не использовалась."))
      showNotification("Архив открыт.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$reprocess_archive, {
    tryCatch({
      next_data <- reprocess_archive(result_data())
      review_undo(result_data()); result_data(next_data)
      showNotification("Извлечение повторено без сети. Подтверждённые поля сохранены.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$associations <- renderDT({ datatable(attr(result_data(), "associations") %||% empty_associations(), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE, pageLength = 10)) }, server = FALSE)
  output$evidence_summary <- renderDT({ datatable(association_summary(result_data()), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE)) }, server = FALSE)
  output$evidence_profiles <- renderDT({
    df <- result_data(); req(nrow(df))
    datatable(evidence_display(df), rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE, pageLength = 10))
  }, server = FALSE)

  output$column_summary <- renderUI({
    count <- length(selected_columns())
    p(class = "column-summary", paste("Выбрано столбцов:", count, "из", length(TABLE_COLUMN_LABELS)))
  })

  apply_column_preset <- function(columns, label) {
    updateSelectizeInput(session, "table_columns", selected = columns, server = FALSE)
    showNotification(paste("Выбран набор:", label), type = "message", duration = 3,
                     id = "column-preset")
  }

  observeEvent(input$columns_core, {
    apply_column_preset(TABLE_COLUMN_PRESETS$core, "Основные")
  })
  observeEvent(input$columns_genetics, {
    apply_column_preset(TABLE_COLUMN_PRESETS$genetics, "Генетика и результаты")
  })
  observeEvent(input$columns_all, {
    apply_column_preset(TABLE_COLUMN_PRESETS$all, "Все поля")
  })
  observeEvent(input$table_columns, {
    is_empty <- length(input$table_columns %||% character(0)) == 0
    if (is_empty && !isTRUE(empty_columns_reset())) {
      empty_columns_reset(TRUE)
      updateSelectizeInput(session, "table_columns", selected = TABLE_COLUMN_PRESETS$core,
                           server = FALSE)
      showNotification("В таблице должен остаться хотя бы один столбец. Восстановлен набор «Основные».",
                       type = "warning", duration = 5)
    } else if (!is_empty) {
      empty_columns_reset(FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

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
      div(class = "metric", strong(evidence), span("строк с фрагментами"))
    )
  })

  output$results <- renderDT({
    df <- visible_result_data()
    if (nrow(df) == 0) return(datatable(data.frame(Статус = "Результатов пока нет"), options = list(dom = "t")))
    display <- prepare_table_display(df)
    escape_columns <- setdiff(seq_along(display$data), display$link_columns)
    datatable(
      display$data, filter = "top", rownames = FALSE, selection = "none",
      escape = escape_columns,
      options = list(
        pageLength = 25, scrollX = TRUE, scrollY = "65vh", deferRender = TRUE,
        scrollCollapse = TRUE,
        search = list(regex = FALSE), lengthMenu = c(10, 25, 50, 100)
      )
    )
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
    log_lines("Запуск начат.")
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
    content = function(file) readr::write_csv(visible_result_data(), file)
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("sportgen_articles_", Sys.Date(), ".xlsx"),
    content = function(file) write_research_workbook(result_data(), file, selected_columns())
  )
  output$download_archive <- downloadHandler(
    filename = function() paste0("sportgen_archive_", Sys.Date(), ".json"),
    content = function(file) write_research_archive(result_data(), file)
  )
  output$download_associations <- downloadHandler(
    filename = function() paste0("sportgen_associations_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(attr(result_data(), "associations") %||% empty_associations(), file)
  )
}

shinyApp(ui, server)
