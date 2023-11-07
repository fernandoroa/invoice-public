box::use(
  shiny[...],
  lubridate[...],
)

box::use(
  .. / utils / constants[...],
  .. / logic / json_save[...]
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      3,
      uiOutput(ns("salary_dates_panel")),
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
      )
    )
  )
}

server <- function(id, rv_jsons, sublist, file_reac, exchange_rate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$non_working_days_box <- renderUI({
      num_nwd <- names(which(sapply(rv_jsons[[sublist]]$non_working_days, function(x) is.numeric(x))))
      logic_nwd <- names(which(sapply(rv_jsons[[sublist]]$non_working_days, function(x) is.logical(x))))
      tagList(
        wellPanel(
          h4(strong("non-working Days")),
          splitLayout(
            cellWidths = c("50%", "10%", "30%"),
            lapply(num_nwd, function(x) {
              numericInput(
                ns(paste0("non_working_days", x)),
                "",
                rv_jsons[[sublist]]$non_working_days[[x]]
              )
            }),
            div(),
            lapply(logic_nwd, function(x) {
              checkboxInput(
                ns(paste0("non_working_days", x)),
                gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                rv_jsons[[sublist]]$non_working_days[[x]]
              )
            })
          )
        )
      )
    })

    output$modified_days_box <- renderUI({
      char_modified <- names(which(sapply(rv_jsons[[sublist]]$modified_days, function(x) is.character(x))))
      num_modified <- names(which(sapply(rv_jsons[[sublist]]$modified_days, function(x) is.numeric(x))))
      logic_modified <- names(which(sapply(rv_jsons[[sublist]]$modified_days, function(x) is.logical(x))))

      tagList(
        wellPanel(
          h4(strong("Modified Pay Days")),
          div(
            class = "three_column_grid_left_big",
            lapply(char_modified, function(x) {
              textInput(
                ns(paste0("modified_days", x)),
                gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
                rv_jsons[[sublist]]$modified_days[[x]]
              )
            }),
            lapply(num_modified, function(x) {
              numericInput(
                ns(paste0("modified_days", x)),
                gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
                rv_jsons[[sublist]]$modified_days[[x]]
              )
            }),
            lapply(logic_modified, function(x) {
              checkboxInput(
                ns(paste0("modified_days", x)),
                gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                rv_jsons[[sublist]]$modified_days[[x]]
              )
            })
          )
        )
      )
    })

    output$salary_box <- renderUI({
      char_names <- names(which(sapply(rv_jsons[[sublist]]$main, function(x) is.character(x))))
      num_names <- names(which(sapply(rv_jsons[[sublist]]$main, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(rv_jsons[[sublist]]$main, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names, value = TRUE)
      num_names_currency <- grep("currency", num_names, value = TRUE)

      char_names_not_currency <- grep("currency", char_names, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names, value = TRUE, invert = TRUE)


      wellPanel(
        h4(strong("Salary Details")),
        {
          char_names_currency_list <- lapply(char_names_currency, function(x) {
            textInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_jsons[[sublist]]$main[[x]]
            )
          })
          num_names_currency_list <- lapply(num_names_currency, function(x) {
            numericInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_jsons[[sublist]]$main[[x]]
            )
          })
          char_list <- lapply(char_names_not_currency, function(x) {
            textInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_jsons[[sublist]]$main[[x]]
            )
          })
          num_list <- lapply(num_names_not_currency, function(x) {
            div(
              class = "go-bottom",
              numericInput(
                ns(paste0("main", x)),
                div(
                  class = "wrap",
                  gsub("_", " ", x, perl = TRUE)
                ),
                rv_jsons[[sublist]]$main[[x]]
              )
            )
          })
          logic_list <- lapply(logic_names, function(x) {
            checkboxInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                gsub("_", " ", gsub(pattern_a, pattern_b, x))
              ),
              rv_jsons[[sublist]]$main[[x]]
            )
          })
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
              char_list
            )
          )
        },
        div(
          class = "two_column_grid",
          logic_list,
          div(
            helpText("Go to Main tab to save all"),
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
      char_period <- names(which(sapply(rv_jsons[[sublist]]$period, function(x) is.character(x))))
      num_period <- names(which(sapply(rv_jsons[[sublist]]$period, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(rv_jsons[[sublist]]$period, function(x) is.logical(x))))
      wellPanel(
        h4(strong("Salary Period(s)")),
        splitLayout(
          cellWidths = c("30%", "30%", "10%", "20%"),
          lapply(num_period, function(x) {
            numericInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_jsons[[sublist]]$period[[x]]
            )
          }),
          lapply(char_period, function(x) {
            textInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_jsons[[sublist]]$period[[x]]
            )
          }),
          div(),
          lapply(logic_period, function(x) {
            checkboxInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_jsons[[sublist]]$period[[x]]
            )
          })
        )
      )
    })

    output$salary_dates_panel <- renderUI({
      wellPanel(
        h4(strong("Salary Dates")),
        splitLayout(
          div(
            style = "display: flex;
               flex-direction: column;
               justify-content: space-between;
               max-width:150px;
               align-items:center;",
            br(),
            checkboxInput(ns(paste0("dates", "use")), "Show Dates", rv_jsons[[sublist]]$dates$use),
            actionButton(ns("increaseDate"), ""),
            span("1 Month"),
            br(),
            actionButton(ns("decreaseDate"), "")
          ),
          tagList(
            dateInput(ns(paste0("dates", "start")), "Start Date: ", value = as.Date(rv_jsons[[sublist]]$dates$start)),
            textInput(ns(paste0("dates", "date_connector")), "date connector", rv_jsons[[sublist]]$dates$date_connector),
            dateInput(ns(paste0("dates", "end")), "End Date: ", value = as.Date(rv_jsons[[sublist]]$dates$end))
          )
        ),
        div(
          class = "two_column_grid",
          div(
            class = "go-center",
            span(strong("Deliver title"))
          ),
          div(
            class = "go-center",
            textInput(ns(paste0("dates", "delivery_month_text")), "", rv_jsons[[sublist]]$dates$delivery_month_text)
          )
        )
      )
    })

    observeEvent(input$increaseDate, {
      sdate <- input$datesstart
      edate <- input$datesend
      smon <- month(sdate)
      emon <- month(edate)
      updateDateInput(session, "datesstart", value = sdate + mon_span[smon + 1])
      updateDateInput(session, "datesend", value = edate + mon_span[emon + 2])
    })

    observeEvent(input$decreaseDate, {
      sdate <- input$datesstart
      edate <- input$datesend
      smon <- month(sdate)
      emon <- month(edate)
      updateDateInput(session, "datesstart", value = sdate - mon_span[smon])
      updateDateInput(session, "datesend", value = edate - mon_span[emon + 1])
    })

    output$save_download_salary <- downloadHandler(
      filename = function() {
        "salary.json"
      },
      content = function(file) {
        file_name <- "salary.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_json_save(
          input,
          nested_list = rv_jsons[[sublist]],
          prefix = "",
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), {
      updateCheckboxInput(
        session,
        paste0("dates", "use"),
        value = rv_jsons[[sublist]]$dates$use
      )

      updateDateInput(session,
        paste0("dates", "start"),
        value = as.Date(rv_jsons[[sublist]]$dates$start)
      )

      updateTextInput(
        session, paste0("dates", "date_connector"),
        value = rv_jsons[[sublist]]$dates$date_connector
      )

      updateDateInput(session, paste0("dates", "end"),
        value = as.Date(rv_jsons[[sublist]]$dates$end)
      )

      updateTextInput(
        session, paste0("dates", "delivery_month_text"),
        value = rv_jsons[[sublist]]$dates$delivery_month_text
      )
      json_salary_list_main <- rv_jsons[[sublist]]$main
      char_names <- names(which(sapply(json_salary_list_main, function(x) is.character(x))))
      num_names <- names(which(sapply(json_salary_list_main, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(json_salary_list_main, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names, value = TRUE)
      num_names_currency <- grep("currency", num_names, value = TRUE)

      char_names_not_currency <- grep("currency", char_names, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names, value = TRUE, invert = TRUE)

      lapply(char_names_currency, function(x) {
        updateTextInput(
          session,
          paste0("main", x),
          value = json_salary_list_main[[x]]
        )
      })
      lapply(num_names_currency, function(x) {
        updateNumericInput(
          session,
          paste0("main", x),
          value = json_salary_list_main[[x]]
        )
      })
      lapply(char_names_not_currency, function(x) {
        updateTextInput(
          session,
          paste0("main", x),
          value = json_salary_list_main[[x]]
        )
      })
      lapply(num_names_not_currency, function(x) {
        updateNumericInput(
          session,
          paste0("main", x),
          value = json_salary_list_main[[x]]
        )
      })
      lapply(logic_names, function(x) {
        updateCheckboxInput(
          session,
          paste0("main", x),
          value = json_salary_list_main[[x]]
        )
      })
      salary_list_period <- rv_jsons[[sublist]]$period
      char_period <- names(which(sapply(salary_list_period, function(x) is.character(x))))
      num_period <- names(which(sapply(salary_list_period, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(salary_list_period, function(x) is.logical(x))))

      lapply(num_period, function(x) {
        updateNumericInput(
          session,
          paste0("period", x),
          value = salary_list_period[[x]]
        )
      })

      lapply(char_period, function(x) {
        updateTextInput(
          session,
          paste0("period", x),
          value = salary_list_period[[x]]
        )
      })

      lapply(logic_period, function(x) {
        updateCheckboxInput(
          session,
          paste0("period", x),
          value = salary_list_period[[x]]
        )
      })
    })

    observeEvent(exchange_rate(), ignoreInit = TRUE, {
      if(is.numeric(exchange_rate())) {
        updateNumericInput(session, paste0("main", "currency_exchange_to_Final_Currency"), value = exchange_rate())
      }
    })

    outputOptions(output, "non_working_days_box", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_dates_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_period_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_box", suspendWhenHidden = FALSE)
    outputOptions(output, "modified_days_box", suspendWhenHidden = FALSE)

    return(reactive(input$maincurrency))
  })
}
