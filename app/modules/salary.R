box::use(
  shiny[...],
  lubridate[...],
  stats[runif]
)

box::use(
  .. / utils / constants[...],
  .. / logic / save_files[...],
  .. / logic / input_fun[...],
  .. / logic / update_fun[...],
  .. / utils / continue_sequence[...]
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      3,
      uiOutput(ns("salary_dates_panel")),
      uiOutput(ns("salary_single_panel")),
      uiOutput(ns("salary_period_panel"))
    ),
    column(
      6,
      div(
        class = "salary_and_days_container",
        div(
          class = "salary",
          uiOutput(ns("salary_box"))
        ),
        div(
          class = "modified",
          uiOutput(ns("modified_days_box"))
        ),
        div(
          class = "non_working_days",
          uiOutput(ns("non_working_days_box"))
        )
      ),
      uiOutput(ns("save_salary_box"))
    )
  )
}

server <- function(id, rv_jsons, sublist, file_reac, currency_date_vars, temp_folder_session, bump_month_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$non_working_days_box <- renderUI({
      salary_list <- rv_jsons[[sublist]]
      num_nwd <- names(which(sapply(salary_list$non_working_days, function(x) is.numeric(x))))
      logic_nwd <- names(which(sapply(salary_list$non_working_days, function(x) is.logical(x))))
      child_namespace <- "non_working_days"
      tagList(
        wellPanel(
          h4(strong("non-working Days")),
          div(
            class = "nwd-grid",
            create_numeric_input(num_nwd, salary_list$non_working_days, ns,
              idx = 50, useChildNS = TRUE, child_namespace = child_namespace
            ),
            div(),
            div(
              class = "go-bottom",
              create_check_box_input(logic_nwd, salary_list$non_working_days,
                ns,
                useChildNS = TRUE, child_namespace = child_namespace
              )
            )
          )
        )
      )
    })

    output$modified_days_box <- renderUI({
      salary_list <- rv_jsons[[sublist]]

      char_modified <- names(which(sapply(salary_list$modified_days, function(x) is.character(x))))
      num_modified <- names(which(sapply(salary_list$modified_days, function(x) is.numeric(x))))
      logic_modified <- names(which(sapply(salary_list$modified_days, function(x) is.logical(x))))
      child_namespace <- "modified_days"
      tagList(
        wellPanel(
          h4(strong("Modified Pay Days")),
          div(
            class = "four_items_grid",
            create_text_input_nc_simple(
              char_modified, salary_list$modified_days, ns,
              useChildNS = TRUE,
              child_namespace = child_namespace
            ),
            create_numeric_input_nc(
              num_modified, salary_list$modified_days, ns,
              idx = 50,
              useChildNS = TRUE, child_namespace = child_namespace
            ),
            div(
              class = "go-bottom",
              create_check_box_input(logic_modified, salary_list$modified_days, ns,
                useChildNS = TRUE, child_namespace = child_namespace
              )
            )
          )
        )
      )
    })

    output$salary_box <- renderUI({
      salary_list <- rv_jsons[[sublist]]

      char_names <- names(which(sapply(salary_list$main, function(x) is.character(x))))
      num_names <- names(which(sapply(salary_list$main, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(salary_list$main, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names, value = TRUE)
      num_names_currency <- grep("currency", num_names, value = TRUE)

      char_names_not_currency <- grep("currency", char_names, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names, value = TRUE, invert = TRUE)

      child_namespace <- "main"
      wellPanel(
        h4(strong("Details")),
        {
          char_names_currency_list <- create_text_input_wrap(
            char_names_currency,
            salary_list$main, ns,
            useChildNS = TRUE, child_namespace = child_namespace
          )

          num_names_currency_list <- create_numeric_input_wrap(
            num_names_currency,
            salary_list$main, ns,
            useChildNS = TRUE, child_namespace = child_namespace
          )
          char_list <- create_text_input_wrap(
            char_names_not_currency, salary_list$main, ns,
            useChildNS = TRUE, child_namespace = child_namespace
          )

          num_list <- create_numeric_input_nc_child_bottom(
            num_names_not_currency, salary_list$main, ns,
            child_namespace = child_namespace
          )

          logic_list <- create_check_box_input_child(logic_names, salary_list$main, ns, child_namespace)

          div(
            div(
              class = "four_column_grid",
              div(
                class = "go-bottom",
                char_names_currency_list
              ),
              div(
                class = "go-bottom",
                num_names_currency_list
              ),
              num_list,
            ),
            div(
              class = "three_column_grid_left_big",
              div(
                class = "go-bottom",
                char_list[1]
              ),
              div(
                class = "go-bottom",
                char_list[2]
              ),
              div(
                class = "go-bottom",
                logic_list
              )
            )
          )
        }
      )
    })

    output$save_salary_box <- renderUI({
      div(
        class = "fit-content",
        wellPanel(
          div(
            helpText("Go to Main tab to save all .json files"),
            downloadButton(ns("save_download_salary"),
              strong(
                "Save and Download", code("salary.json")
              ),
              style = "white-space: normal;
                           word-wrap: break-word;"
            )
          )
        )
      )
    })

    output$salary_period_panel <- renderUI({
      salary_list <- rv_jsons[[sublist]]

      char_period <- names(which(sapply(salary_list$period, function(x) is.character(x))))
      num_period <- names(which(sapply(salary_list$period, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(salary_list$period, function(x) is.logical(x))))
      child_namespace <- "period"
      wellPanel(
        h4(strong("Period(s)")),
        div(
          class = "three_column_grid_equal",
          create_numeric_input_child_pattern(
            num_period, salary_list$period, ns,
            child_namespace = child_namespace
          ),
          create_text_input_with_child_patterns(
            char_period, salary_list$period, ns, child_namespace
          ),
          div(
            class = "go-bottom",
            create_check_box_input(logic_period, salary_list$period, ns,
              useChildNS = TRUE, child_namespace = child_namespace
            )
          )
        )
      )
    })

    output$salary_dates_panel <- renderUI({
      salary_list <- rv_jsons[[sublist]]

      wellPanel(
        h4(strong("Date Range")),
        splitLayout(
          div(
            style = "display: flex;
               flex-direction: column;
               justify-content: space-between;
               max-width:150px;
               align-items:center;
               gap: 10px",
            br(),
            checkboxInput(ns(paste0("dates", "-", "use")), "Show", salary_list$dates$use),
            actionButton(ns("increaseDate"), ""),
            span("1 Month"),
            actionButton(ns("decreaseDate"), "")
          ),
          tagList(
            dateInput(ns(paste0("dates", "-", "start")), "Start Date: ", value = as.Date(salary_list$dates$start)),
            textInput(ns(paste0("dates", "-", "date_connector")), "date connector", salary_list$dates$date_connector),
            dateInput(ns(paste0("dates", "-", "end")), "End Date: ", value = as.Date(salary_list$dates$end))
          )
        )
      )
    })

    output$salary_single_panel <- renderUI({
      salary_list <- rv_jsons[[sublist]]
      single_ns <- "single"

      wellPanel(
        h4(strong("Single date/month")),
        splitLayout(
          div(
            style = "display: flex;
               flex-direction: column;
               justify-content: space-between;
               max-width:150px;
               align-items:center;
               gap: 10px",
            checkboxInput(ns(paste0(single_ns, "-", "use")), "Show", salary_list$single$use),
            actionButton(ns("increaseSingleDate"), ""),
            span("1 Month"),
            actionButton(ns("decreaseSingleDate"), "")
          ),
          tagList(
            checkboxInput(
              ns(paste0(single_ns, "-", "show_month/year_only")),
              div(
                class = "wrap",
                "Show Month/Year Only"
              ),
              salary_list$single$`show_month/year_only`
            ),
            dateInput(ns(paste0(single_ns, "-", "date")), "Date: ", value = as.Date(salary_list$single$date)),
            textInput(ns(paste0(single_ns, "-", "text")), "Title", salary_list$single$text)
          )
        )
      )
    })

    decreaseDate_rv <- eventReactive(c(
      input$decreaseDate,
      input$decreaseSingleDate
    ), ignoreInit = TRUE, {
      runif(1)
    })

    increaseDate_rv <- eventReactive(c(
      input$increaseDate,
      input$increaseSingleDate
    ), ignoreInit = TRUE, {
      runif(1)
    })


    observeEvent(c(increaseDate_rv(), bump_month_vars$increaseEverything()), ignoreInit = TRUE, {
      sdate <- input$`dates-start`
      edate <- input$`dates-end`
      single_date <- input$`single-date`

      new_date_s <- date_bump_month(sdate)
      new_date_e <- date_bump_month(edate)
      new_date_single <- date_bump_month(single_date)

      updateDateInput(session, "dates-start", value = new_date_s)
      updateDateInput(session, "dates-end", value = new_date_e)
      updateDateInput(session, "single-date", value = new_date_single)
    })

    observeEvent(c(decreaseDate_rv(), bump_month_vars$decreaseEverything()), ignoreInit = TRUE, {
      sdate <- input$`dates-start`
      edate <- input$`dates-end`
      single_date <- input$`single-date`

      new_date_s <- date_bump_month(sdate, decrease = TRUE)
      new_date_e <- date_bump_month(edate, decrease = TRUE)
      new_date_single <- date_bump_month(single_date, decrease = TRUE)

      updateDateInput(session, "dates-start", value = new_date_s)
      updateDateInput(session, "dates-end", value = new_date_e)
      updateDateInput(session, "single-date", value = new_date_single)
    })

    observeEvent(bump_month_vars$update_everything(), ignoreInit = TRUE, {
      year_month_vec <- get_current_month_year()
      year_int <- year_month_vec[1]
      month_int <- year_month_vec[2]

      new_start_date <- paste0(c(year_int, month_int, "1"), collapse = "-") |> as.Date()

      updateDateInput(session, "dates-start", value = new_start_date)

      new_end_date <- get_new_date(input$`dates-end`, year_int, month_int, mon_span)

      updateDateInput(session, "dates-end", value = new_end_date)

      new_single_date <- get_new_date(input$`single-date`, year_int, month_int, mon_span)

      updateDateInput(session, "single-date", value = new_single_date)
    })

    observeEvent(currency_date_vars$exchange_salary(), ignoreInit = TRUE, {
      updateNumericInput(session, paste0("main", "-", "currency_exchange_to_Final_Currency"),
        value = currency_date_vars$exchange_salary() |> as.numeric()
      )
    })


    output$save_download_salary <- downloadHandler(
      filename = function() {
        "salary.json"
      },
      content = function(file) {
        file_name <- "salary.json"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)
        salary_list <- rv_jsons[[sublist]]

        nested_json_save(
          input,
          nested_list = salary_list,
          folders = c(folder, file.path(temp_folder_session(), "json")),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), ignoreInit = TRUE, {
      salary_list <- rv_jsons[[sublist]]

      updateCheckboxInput(
        session,
        paste0("dates", "-", "use"),
        value = salary_list$dates$use
      )

      updateDateInput(session,
        paste0("dates", "-", "start"),
        value = as.Date(salary_list$dates$start)
      )

      updateTextInput(
        session, paste0("dates", "-", "date_connector"),
        value = salary_list$dates$date_connector
      )

      updateDateInput(session, paste0("dates", "-", "end"),
        value = as.Date(salary_list$dates$end)
      )

      updateTextInput(
        session, paste0("single", "-", "text"),
        value = salary_list$single$text
      )

      updateDateInput(session, paste0("single", "-", "date"),
        value = as.Date(salary_list$single$date)
      )

      updateCheckboxInput(
        session, paste0("single", "-", "use"),
        value = salary_list$single$use
      )

      updateCheckboxInput(
        session, paste0("single", "-", "show_month/year_only"),
        value = salary_list$single$`show_month/year_only`
      )

      json_salary_list_main <- salary_list$main
      char_names <- names(which(sapply(json_salary_list_main, function(x) is.character(x))))
      num_names <- names(which(sapply(json_salary_list_main, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(json_salary_list_main, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names, value = TRUE)
      num_names_currency <- grep("currency", num_names, value = TRUE)

      char_names_not_currency <- grep("currency", char_names, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names, value = TRUE, invert = TRUE)
      child_namespace <- "main"

      update_text_input_list(
        session, char_names_currency, json_salary_list_main,
        useChildNS = TRUE, child_namespace
      )

      update_numeric_input_list(session,
        num_names_currency, json_salary_list_main,
        useChildNS = TRUE,
        child_namespace
      )

      update_text_input_list(
        session, char_names_not_currency, json_salary_list_main,
        useChildNS = TRUE, child_namespace
      )

      update_numeric_input_list(session,
        num_names_not_currency, json_salary_list_main,
        useChildNS = TRUE,
        child_namespace
      )

      update_checkbox_list(
        session, logic_names, json_salary_list_main,
        useChildNS = TRUE,
        child_namespace
      )

      salary_list_period <- salary_list$period
      char_period <- names(which(sapply(salary_list_period, function(x) is.character(x))))
      num_period <- names(which(sapply(salary_list_period, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(salary_list_period, function(x) is.logical(x))))
      child_namespace <- "period"

      update_numeric_input_list(session,
        num_period, salary_list_period,
        useChildNS = TRUE,
        child_namespace
      )

      update_text_input_list(
        session, char_period, salary_list_period,
        useChildNS = TRUE, child_namespace
      )

      update_checkbox_list(
        session, logic_period, salary_list_period,
        useChildNS = TRUE,
        child_namespace
      )

      salary_list_non_working_days <- salary_list$non_working_days
      num_non_working_days <- names(which(sapply(salary_list_non_working_days, function(x) is.numeric(x))))
      logic_non_working_days <- names(which(sapply(salary_list_non_working_days, function(x) is.logical(x))))
      child_namespace <- "non_working_days"

      update_numeric_input_list(session,
        num_non_working_days, salary_list_non_working_days,
        useChildNS = TRUE,
        child_namespace
      )

      update_checkbox_list(
        session, logic_non_working_days, salary_list_non_working_days,
        useChildNS = TRUE,
        child_namespace
      )

      salary_list_modified_days <- salary_list$modified_days
      char_modified_days <- names(which(sapply(salary_list_modified_days, function(x) is.character(x))))
      num_modified_days <- names(which(sapply(salary_list_modified_days, function(x) is.numeric(x))))
      logic_modified_days <- names(which(sapply(salary_list_modified_days, function(x) is.logical(x))))
      child_namespace <- "modified_days"

      update_numeric_input_list(session,
        num_modified_days, salary_list_modified_days,
        useChildNS = TRUE,
        child_namespace
      )

      update_text_input_list(
        session, char_modified_days, salary_list_modified_days,
        useChildNS = TRUE, child_namespace
      )

      update_checkbox_list(
        session, logic_modified_days, salary_list_modified_days,
        useChildNS = TRUE,
        child_namespace
      )
    })

    outputOptions(output, "non_working_days_box", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_dates_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_single_panel", suspendWhenHidden = FALSE)

    outputOptions(output, "salary_period_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_box", suspendWhenHidden = FALSE)
    outputOptions(output, "modified_days_box", suspendWhenHidden = FALSE)
    outputOptions(output, "save_salary_box", suspendWhenHidden = FALSE)

    return(reactive(input$`main-currency`))
  })
}
