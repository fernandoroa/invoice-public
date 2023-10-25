box::use(
  shiny[...],
  dplyr[...],
  rjson[...],
  jsonlite[...],
  lubridate[...],
  tinytex[...]
)

box::use(
  logic / exchange[...],
)

# ubuntu
# sudo apt install texlive-latex-base

# Windows
# https://github.com/rstudio/tinytex-releases # download manually

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    selected = "Invoice, businesses and date",
    "Invoice Generator",
    tabPanel(
      "Save and Generate",
      fluidPage(
        fluidRow(
          column(
            3,
            wellPanel(
              div(
                class = "two_column",
                actionButton(ns("reload"), "Reload Json files"),
                radioButtons(ns("lang"), "Language", c("english" = 1, "other" = 2))
              ),
              div(
                id = "generate_buttons",
                actionButton(
                  ns("modify_all"),
                  strong(
                    "Save All",
                    code("*.json"),
                    "required!"
                  )
                ),
                helpText("saving .json changes is mandatory"),
                br(),
                downloadButton(ns("report"), "Generate invoice in .pdf")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Invoice, businesses and date",
      fluidPage(
        fluidRow(
          column(
            3,
            wellPanel(
              textInput(ns("invoiceNumber"), "Invoice Number", paste(format(Sys.time(), "%Y-")))
            ),
            uiOutput(ns("first_well_panel")),
          ),
          column(
            3,
            div(
              style = "max-width:600px",
              uiOutput(ns("consultant_business_box"))
            )
          ),
          column(
            3,
            div(
              style = "max-width:600px",
              uiOutput(ns("business_to_bill_box")),
            )
          ),
          column(
            3,
            img(src = "static/invoice_header.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "Salary dates and days",
      fluidPage(
        fluidRow(
          column(
            3,
            uiOutput(ns("salary_dates_panel")),
            uiOutput(ns("salary_period_panel"))
          ),
          column(
            3,
            uiOutput(ns("salary_box"))
          ),
          column(
            3,
            uiOutput(ns("sick_box"))
          ),
          column(
            3,
            img(src = "static/invoice_salary.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "Oneliners & Grouped costs",
      fluidPage(
        fluidRow(
          column(
            3,
            uiOutput(ns("oneliners_box"))
          ),
          column(
            3,
            img(src = "static/invoice_oneliners.svg", style = "max-width:25vw")
          ),
          column(
            3,
            uiOutput(ns("grouped_box"))
          ),
          column(
            3,
            img(src = "static/invoice_grouped.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "Bank Account information",
      fluidPage(
        fluidRow(
          column(
            3,
            uiOutput(ns("consultant_account_box"))
          ),
          column(6),
          column(
            3,
            img(src = "static/invoice_bank.svg", style = "max-width:25vw")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    rv_json_lists <- reactiveValues(
      json_main_list = rjson::fromJSON(file = "app/json/final_currency_inv_date.json"),
      json_dates_list = rjson::fromJSON(file = "app/json/salary_dates.json"),
      json_salary_list = rjson::fromJSON(file = "app/json/salary_main.json"),
      json_period_list = rjson::fromJSON(file = "app/json/salary_period.json"),
      json_oneliners_list = rjson::fromJSON(file = "app/json/oneline_costs.json"),
      json_grouped_list = rjson::fromJSON(file = "app/json/grouped_costs.json"),
      json_sick_list = rjson::fromJSON(file = "app/json/salary_sick_days.json"),
      json_nwd_list = rjson::fromJSON(file = "app/json/salary_nwdays.json"),
      json_business_to_bill_list = rjson::fromJSON(file = "app/json/business_to_bill.json"),
      json_consultant_account_list = rjson::fromJSON(file = "app/json/consultant_account.json"),
      json_consultant_business_list = rjson::fromJSON(file = "app/json/consultant_contact.json")
    )

    observeEvent(input$reload,
      {
        #
        # read .json
        #
        rv_json_lists$json_main_list <- rjson::fromJSON(file = "app/json/final_currency_inv_date.json")
        rv_json_lists$json_business_to_bill_list <- rjson::fromJSON(file = "app/json/business_to_bill.json")
        rv_json_lists$json_consultant_account_list <- rjson::fromJSON(file = "app/json/consultant_account.json")
        rv_json_lists$json_consultant_business_list <- rjson::fromJSON(file = "app/json/consultant_contact.json")
        rv_json_lists$json_salary_list <- rjson::fromJSON(file = "app/json/salary_main.json")
        rv_json_lists$json_dates_list <- rjson::fromJSON(file = "app/json/salary_dates.json")
        rv_json_lists$json_period_list <- rjson::fromJSON(file = "app/json/salary_period.json")
        rv_json_lists$json_oneliners_list <- rjson::fromJSON(file = "app/json/oneline_costs.json")
        rv_json_lists$json_grouped_list <- rjson::fromJSON(file = "app/json/grouped_costs.json")
        rv_json_lists$json_sick_list <- rjson::fromJSON(file = "app/json/salary_sick_days.json")
        rv_json_lists$json_nwd_list <- rjson::fromJSON(file = "app/json/salary_nwdays.json")
      },
      ignoreInit = TRUE
    )


    pattern_a <- "([[:lower:]]+)([[:upper:]])([[:alpha:]]+)([[:digit:]]?)"
    pattern_b <- "\\1 \\2\\3 \\4"

    mon_span <- c(31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31)

    observeEvent(input$get_exchanges, {
      if (input$final_currency != input$salary_maincurrency) {
        date <- as.character(input$exchangeDate)
        while (TRUE) {
          exchange_df <- try(get_exchange_rates(input$final_currency, input$salary_maincurrency, date), silent = TRUE)
          date <- as.character(as.Date(date) - 1)
          if (!inherits(exchange_df, "try-error")) break
        }
        exchange_salary <- signif(exchange_df$Adjusted_Sy, 5)
        updateNumericInput(session, paste0("salary_main", "currency_exchange_to_Final_Currency"), value = exchange_salary)
      }
      inputs <- reactiveValuesToList(input)
      oneliners_currency_name_strings <- grep("onelinerscurrency", names(inputs), value = TRUE)
      grouped_currency_name_strings <- grep("groupedcurrency", names(inputs), value = TRUE)

      oneline_currencies_inputs <- inputs[which(names(inputs) %in% oneliners_currency_name_strings)]
      grouped_currency_inputs <- inputs[which(names(inputs) %in% grouped_currency_name_strings)]

      oneliners_currencies_list <- oneline_currencies_inputs[sapply(oneline_currencies_inputs, is.character)]
      grouped_currencies_list <- grouped_currency_inputs[sapply(grouped_currency_inputs, is.character)]

      oneliners_currency_exchange_value_list <- oneline_currencies_inputs[sapply(oneline_currencies_inputs, is.numeric)]
      grouped_currency_exchange_value_list <- grouped_currency_inputs[sapply(grouped_currency_inputs, is.numeric)]

      oneliners_currencies_list_names <- names(oneliners_currency_exchange_value_list)
      grouped_currencies_list_names <- names(grouped_currency_exchange_value_list)

      for (currency_idx in seq_along(oneliners_currencies_list)) {
        currency <- oneliners_currencies_list[currency_idx]
        if (input$final_currency != currency) {
          date <- as.character(input$exchangeDate)
          while (TRUE) {
            exchange_df <- try(get_exchange_rates(input$final_currency, currency, date), silent = TRUE)
            date <- as.character(as.Date(date) - 1)
            if (!inherits(exchange_df, "try-error")) break
          }
          exchange_oneliners <- signif(exchange_df$Adjusted_Sy, 5)
          updateNumericInput(session, oneliners_currencies_list_names[currency_idx], value = exchange_oneliners)
        }
      }
      for (currency_idx in seq_along(grouped_currencies_list)) {
        currency <- grouped_currencies_list[currency_idx]
        if (input$final_currency != currency) {
          date <- as.character(input$exchangeDate)
          while (TRUE) {
            exchange_df <- try(get_exchange_rates(input$final_currency, currency, date), silent = TRUE)
            date <- as.character(as.Date(date) - 1)
            if (!inherits(exchange_df, "try-error")) break
          }
          exchange_grouped <- signif(exchange_df$Adjusted_Sy, 5)
          updateNumericInput(session, grouped_currencies_list_names[currency_idx], value = exchange_grouped)
        }
      }
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

    observeEvent(input$increaseDate_Final, {
      cdate <- input$invoiceDate
      edate <- input$exchangeDate
      cmon <- month(cdate)
      emon <- month(edate)
      updateDateInput(session, "invoiceDate", value = cdate + mon_span[cmon + 2])
      updateDateInput(session, "exchangeDate", value = edate + mon_span[emon + 2])
    })

    observeEvent(input$decreaseDate_Final, {
      cdate <- input$invoiceDate
      edate <- input$exchangeDate
      cmon <- month(cdate)
      emon <- month(edate)
      updateDateInput(session, "invoiceDate", value = cdate - mon_span[cmon + 1])
      updateDateInput(session, "exchangeDate", value = cdate - mon_span[emon + 1])
    })

    observeEvent(c(input$modify_salary, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_salary_list), function(x) {
          input[[paste0("salary_main", x)]]
        })

        names(list) <- names(rv_json_lists$json_salary_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/salary_main.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_grouped, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_grouped_list), function(x) {
          input[[paste0("grouped",x)]]
        })

        names(list) <- names(rv_json_lists$json_grouped_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/grouped_costs.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_oneliners, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_oneliners_list), function(x) {
          input[[paste0("oneliners",x)]]
        })

        names(list) <- names(rv_json_lists$json_oneliners_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/oneline_costs.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_sick, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_sick_list), function(x) {
          input[[paste0("sick", x)]]
        })

        names(list) <- names(rv_json_lists$json_sick_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/salary_sick_days.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_nwd, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_nwd_list), function(x) {
          input[[paste0("nwd", x)]]
        })

        names(list) <- names(rv_json_lists$json_nwd_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/salary_nwdays.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_main, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_main_list), function(x) {
          input[[x]]
        })

        names(list) <- names(rv_json_lists$json_main_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/final_currency_inv_date.json")
      },
      ignoreInit = TRUE
    )


    observeEvent(c(input$modify_salary_dates, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_dates_list), function(x) {
          input[[paste0("dates", x)]]
        })

        names(list) <- names(rv_json_lists$json_dates_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/salary_dates.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_period, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_period_list), function(x) {
          input[[paste0("period", x)]]
        })

        names(list) <- names(rv_json_lists$json_period_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/salary_period.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_billto, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_business_to_bill_list), function(x) {
          input[[x]]
        })

        names(list) <- names(rv_json_lists$json_business_to_bill_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/business_to_bill.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_account, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_consultant_account_list), function(x) {
          input[[x]]
        })

        names(list) <- names(rv_json_lists$json_consultant_account_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/consultant_account.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_consultant, input$modify_all),
      {
        list <- list()

        list <- lapply(names(rv_json_lists$json_consultant_business_list), function(x) {
          input[[x]]
        })

        names(list) <- names(rv_json_lists$json_consultant_business_list)
        json_data <- jsonlite::toJSON(x = list, pretty = TRUE)
        write(json_data, "app/json/consultant_contact.json")
      },
      ignoreInit = TRUE
    )

    output$first_well_panel <- renderUI({
      wellPanel(
        h4(strong("Currency and Invoice Date")),
        div(
          class = "two_column_right_big",
          textInput(
            ns("final_currency"),
            div(
              class = "wrap",
              HTML("<i>Final</i> Currency")
            ),
            rv_json_lists$json_final_currency_list$final_currency
          ),
          div(
            id = "exchange_container", style = "display:inline-block", title = "Updates exchange values in other tabs",
            actionButton(
              ns("get_exchanges"),
              "Get exchange values"
            )
          )
        ),
        splitLayout(
          div(
            style = "display: flex;
               flex-direction: column;
               justify-content: space-between;
               max-width:150px;
               align-items:center;",
            actionButton(ns("decreaseDate_Final"), "decrease"),
            br(),
            span("1 Month"),
            br(),
            actionButton(ns("increaseDate_Final"), "increase")
          ),
          tagList(
            dateInput(ns("invoiceDate"), "Invoice Date: ", value = as.Date(rv_json_lists$json_final_currency_list$invoiceDate)),
            dateInput(ns("exchangeDate"),
              div(
                class = "wrap",
                "Currency Exchange Date: "
              ),
              value = as.Date(rv_json_lists$json_final_currency_list$exchangeDate)
            )
          )
        ),
        actionButton(ns("modify_main"),
          strong(
            "Save", code("final_currency_inv_date.json")
          ),
          style = "white-space: normal;
                   word-wrap: break-word;"
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
            br(), br(), br(),
            actionButton(ns("decreaseDate"), "decrease"),
            br(),
            span("1 Month"),
            br(),
            actionButton(ns("increaseDate"), "increase")
          ),
          tagList(
            checkboxInput(ns(paste0("dates", "use")), "Show Dates", rv_json_lists$json_dates_list$use),
            dateInput(ns(paste0("dates", "start")), "Start Date: ", value = as.Date(rv_json_lists$json_dates_list$start)),
            textInput(ns(paste0("dates", "date_connector")), "date connector", rv_json_lists$json_dates_list$date_connector),
            dateInput(ns(paste0("dates", "end")), "End Date: ", value = as.Date(rv_json_lists$json_dates_list$end))
          )
        ),
        textInput(ns(paste0("dates", "delivery_month_text")), "Deliver title", rv_json_lists$json_dates_list$delivery_month_text),
        br(),
        actionButton(ns("modify_salary_dates"),
          strong(
            "Save", code("salary_dates.json"),
            "required! after changes"
          ),
          style = "white-space: normal;
                   word-wrap: break-word;"
        )
      )
    })

    output$salary_box <- renderUI({
      char_names <- names(which(sapply(rv_json_lists$json_salary_list, function(x) is.character(x))))
      num_names <- names(which(sapply(rv_json_lists$json_salary_list, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(rv_json_lists$json_salary_list, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names, value = TRUE)
      num_names_currency <- grep("currency", num_names, value = TRUE)

      char_names_not_currency <- grep("currency", char_names, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names, value = TRUE, invert = TRUE)


      wellPanel(
        h4(strong("Salary Details")),
        {
          char_names_currency_list <- lapply(char_names_currency, function(x) {
            textInput(
              ns(paste0("salary_main", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_json_lists$json_salary_list[[x]]
            )
          })
          num_names_currency_list <- lapply(num_names_currency, function(x) {
            numericInput(
              ns(paste0("salary_main", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_json_lists$json_salary_list[[x]]
            )
          })
          div(
            class = "two_column_grid",
            div(
              char_names_currency_list
            ),
            div(
              num_names_currency_list
            )
          )
        },
        {
          char_list <- lapply(char_names_not_currency, function(x) {
            textInput(
              ns(paste0("salary_main", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_json_lists$json_salary_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_list)) {
                char_list[(middle_idx + 1):length(char_list)]
              }
            )
          )
        },
        {
          num_list <- lapply(num_names_not_currency, function(x) {
            numericInput(
              ns(paste0("salary_main", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_json_lists$json_salary_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_list)) {
                num_list[(middle_idx + 1):length(num_list)]
              }
            )
          )
        },
        {
          logic_list <- lapply(logic_names, function(x) {
            checkboxInput(
              ns(paste0("salary_main", x)),
              div(
                class = "wrap",
                gsub("_", " ", gsub(pattern_a, pattern_b, x))
              ),
              rv_json_lists$json_salary_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_list)) {
                logic_list[(middle_idx + 1):length(logic_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_salary"),
          strong(
            "Save", code("salary_main.json"),
            "required! after changes"
          ),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$oneliners_box <- renderUI({
      char_names_oneliners <- names(which(sapply(rv_json_lists$json_oneliners_list, function(x) is.character(x))))
      num_names_oneliners <- names(which(sapply(rv_json_lists$json_oneliners_list, function(x) is.numeric(x))))
      logic_names_oneliners <- names(which(sapply(rv_json_lists$json_oneliners_list, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names_oneliners, value = TRUE)
      num_names_currency <- grep("currency", num_names_oneliners, value = TRUE)

      char_names_not_currency <- grep("currency", char_names_oneliners, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names_oneliners, value = TRUE, invert = TRUE)

      wellPanel(
        h4(strong("One-Line Costs")),
        {
          char_names_currency_list <- lapply(char_names_currency, function(x) {
            textInput(
              ns(paste0("oneliners", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_json_lists$json_oneliners_list[[x]]
            )
          })
          num_names_currency_list <- lapply(num_names_currency, function(x) {
            numericInput(
              ns(paste0("oneliners", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_json_lists$json_oneliners_list[[x]]
            )
          })
          div(
            class = "two_column_grid",
            div(
              char_names_currency_list
            ),
            div(
              num_names_currency_list
            )
          )
        },
        {
          char_names_oneliners_not_currency_list <- lapply(char_names_not_currency, function(x) {
            textInput(
              ns(paste0("oneliners",x)),
              gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
              rv_json_lists$json_oneliners_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_names_oneliners_not_currency_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_names_oneliners_not_currency_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_names_oneliners_not_currency_list)) {
                char_names_oneliners_not_currency_list[(middle_idx + 1):length(char_names_oneliners_not_currency_list)]
              }
            )
          )
        },
        {
          num_names_oneliners_not_currency_list <- lapply(num_names_not_currency, function(x) {
            numericInput(
              ns(paste0("oneliners",x)),
              gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE)),
              rv_json_lists$json_oneliners_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_names_oneliners_not_currency_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_names_oneliners_not_currency_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_names_oneliners_not_currency_list)) {
                num_names_oneliners_not_currency_list[(middle_idx + 1):length(num_names_oneliners_not_currency_list)]
              }
            )
          )
        },
        {
          logic_names_oneliners_list <- lapply(logic_names_oneliners, function(x) {
            checkboxInput(
              ns(paste0("oneliners",x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_oneliners_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_names_oneliners_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_names_oneliners_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_names_oneliners_list)) {
                logic_names_oneliners_list[(middle_idx + 1):length(logic_names_oneliners_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_oneliners"),
          strong("Save", code("oneline_costs.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$grouped_box <- renderUI({
      char_names_grouped <- names(which(sapply(rv_json_lists$json_grouped_list, function(x) is.character(x))))
      num_names_grouped <- names(which(sapply(rv_json_lists$json_grouped_list, function(x) is.numeric(x))))
      logic_names_grouped <- names(which(sapply(rv_json_lists$json_grouped_list, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names_grouped, value = TRUE)
      num_names_currency <- grep("currency", num_names_grouped, value = TRUE)

      char_names_not_currency <- grep("currency", char_names_grouped, value = TRUE, invert = TRUE)
      num_names_not_currency <- grep("currency", num_names_grouped, value = TRUE, invert = TRUE)

      wellPanel(
        h4(strong("Grouped Costs")),
         {
          char_names_currency_list <- lapply(char_names_currency, function(x) {
            textInput(
              ns(paste0("grouped", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_json_lists$json_grouped_list[[x]]
            )
          })
          num_names_currency_list <- lapply(num_names_currency, function(x) {
            numericInput(
              ns(paste0("grouped", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_json_lists$json_grouped_list[[x]]
            )
          })
          div(
            class = "two_column_grid",
            div(
              char_names_currency_list
            ),
            div(
              num_names_currency_list
            )
          )
        },
        {
          char_names_grouped_list <- lapply(char_names_not_currency, function(x) {
            textInput(
              ns(paste0("grouped", x)),
              gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
              rv_json_lists$json_grouped_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_names_grouped_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_names_grouped_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_names_grouped_list)) {
                char_names_grouped_list[(middle_idx + 1):length(char_names_grouped_list)]
              }
            )
          )
        },
        {
          num_names_grouped_list <- lapply(num_names_not_currency, function(x) {
            numericInput(
              ns(paste0("grouped", x)),
              gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE)),
              rv_json_lists$json_grouped_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_names_grouped_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_names_grouped_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_names_grouped_list)) {
                num_names_grouped_list[(middle_idx + 1):length(num_names_grouped_list)]
              }
            )
          )
        },
        {
          logic_names_grouped_list <- lapply(logic_names_grouped, function(x) {
            checkboxInput(
              ns(paste0("grouped", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_grouped_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_names_grouped_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_names_grouped_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_names_grouped_list)) {
                logic_names_grouped_list[(middle_idx + 1):length(logic_names_grouped_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_grouped"),
          strong("Save", code("grouped_costs.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$salary_period_panel <- renderUI({
      char_period <- names(which(sapply(rv_json_lists$json_period_list, function(x) is.character(x))))      
      num_period <- names(which(sapply(rv_json_lists$json_period_list, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(rv_json_lists$json_period_list, function(x) is.logical(x))))
        wellPanel(
          h4(strong("Salary Period")),
          splitLayout(
          lapply(num_period, function(x) {
            numericInput(
              ns(paste0("period", x)), 
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_period_list[[x]]
            )
          }),
          lapply(char_period, function(x) {
            textInput(
              ns(paste0("period", x)), 
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_period_list[[x]]
            )
          })
          ),
          lapply(logic_period, function(x) {
            checkboxInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_period_list[[x]]
            )
          }),
          helpText("this box content must be saved before generating .pdf"),
          actionButton(ns("modify_period"),
            strong(
              "Save", code("salary_period.json"),
              "required! after changes"
            ),
            style = "white-space: normal;
              word-wrap: break-word;"
          )
        
        )
    })

    output$sick_box <- renderUI({
      char_sick <- names(which(sapply(rv_json_lists$json_sick_list, function(x) is.character(x))))
      num_sick <- names(which(sapply(rv_json_lists$json_sick_list, function(x) is.numeric(x))))
      logic_sick <- names(which(sapply(rv_json_lists$json_sick_list, function(x) is.logical(x))))

      num_nwd <- names(which(sapply(rv_json_lists$json_nwd_list, function(x) is.numeric(x))))
      logic_nwd <- names(which(sapply(rv_json_lists$json_nwd_list, function(x) is.logical(x))))

      tagList(
        wellPanel(
          h4(strong("Sick Days Pay")),
          lapply(char_sick, function(x) {
            textInput(
              ns(paste0("sick", x)), 
              gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
              rv_json_lists$json_sick_list[[x]]
            )
          }),
          {
          num_list <- lapply(num_sick, function(x) {
              numericInput(
              ns(paste0("sick", x)), 
              gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
              rv_json_lists$json_sick_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_list)) {
                num_list[(middle_idx + 1):length(num_list)]
              }
            )
          )
        },
          lapply(logic_sick, function(x) {
            checkboxInput(
              ns(paste0("sick", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_sick_list[[x]]
            )
          }),
          helpText("this box content must be saved before generating .pdf"),
          actionButton(ns("modify_sick"),
            strong(
              "Save", code("salary_sick_days.json"),
              "required! after changes"
            ),
            style = "white-space: normal;
                           word-wrap: break-word;"
          )
        ),
        wellPanel(
          h4(strong("non-working Days")),
          lapply(num_nwd, function(x) {
            numericInput(
              ns(paste0("nwd", x)), 
              "",
              rv_json_lists$json_nwd_list[[x]]
            )
          }),
          lapply(logic_nwd, function(x) {
            checkboxInput(
              ns(paste0("nwd", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_nwd_list[[x]]
            )
          }),
          helpText("this box content must be saved before generating .pdf"),
          actionButton(ns("modify_nwd"),
            strong(
              "Save", code("salary_nwdays.json"),
              "required! after changes"
            ),
            style = "white-space: normal;
              word-wrap: break-word;"
          )
        )
      )
    })

    output$business_to_bill_box <- renderUI({
      wellPanel(
        h4(strong("Bill To:")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_billto"),
          strong("Save", code("business_to_bill.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        ),
        br(),
        br(),
        lapply(seq_along(rv_json_lists$json_business_to_bill_list), function(x) {
          textInput(ns(names(rv_json_lists$json_business_to_bill_list[x])),
            gsub("_", " ", gsub(pattern_a, pattern_b, names(rv_json_lists$json_business_to_bill_list[x]))),
            value = rv_json_lists$json_business_to_bill_list[[x]]
          )
        }),
      )
    })

    output$consultant_account_box <- renderUI({
      char_consultant_account <- names(which(sapply(rv_json_lists$json_consultant_account_list, function(x) is.character(x))))
      logic_char_consultant_account <- names(which(sapply(rv_json_lists$json_consultant_account_list, function(x) is.logical(x))))

      wellPanel(
        h4(strong("Consultant Account")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_account"),
          strong("Save", code("consultant_account.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        ),
        br(),
        br(),
        lapply(char_consultant_account, function(x) {
          textInput(ns(x),
            gsub("_", " ", gsub(pattern_a, pattern_b, x)),
            value = rv_json_lists$json_consultant_account_list[[x]]
          )
        }),
        lapply(logic_char_consultant_account, function(x) {
          checkboxInput(
            ns(x),
            gsub("_", " ", gsub(pattern_a, pattern_b, x)),
            rv_json_lists$json_consultant_account_list[[x]]
          )
        }),
      )
    })

    output$consultant_business_box <- renderUI({
      wellPanel(
        h4(strong("Consultant details")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_consultant"),
          strong("Save", code("consultant_contact.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        ),
        br(),
        br(),
        lapply(seq_along(rv_json_lists$json_consultant_business_list), function(x) {
          textInput(ns(names(rv_json_lists$json_consultant_business_list[x])),
            gsub("_", " ", gsub(pattern_a, pattern_b, names(rv_json_lists$json_consultant_business_list[x]))),
            value = rv_json_lists$json_consultant_business_list[[x]]
          )
        }),
      )
    })
    output$report <- downloadHandler(
      filename = "invoice.pdf",
      content = function(file) {
        temp_report <- file.path(getwd(), "app/inv_md_dont_modify.Rmd")
        file.copy("app/invoice.Rmd", temp_report, overwrite = TRUE)

        all_params <- reactiveValuesToList(input)
        params <- list(invoiceNumber = all_params$invoiceNumber, lang = all_params$lang)

        rmarkdown::render(temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    outputOptions(output, "first_well_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "consultant_business_box", suspendWhenHidden = FALSE)
    outputOptions(output, "business_to_bill_box", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_dates_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_period_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_box", suspendWhenHidden = FALSE)
    outputOptions(output, "sick_box", suspendWhenHidden = FALSE)
    outputOptions(output, "oneliners_box", suspendWhenHidden = FALSE)
    outputOptions(output, "grouped_box", suspendWhenHidden = FALSE)
    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)

  })
}
