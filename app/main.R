box::use(
  shiny[...],
  dplyr[...],
  rjson[...],
  jsonlite[...],
  lubridate[...],
  purrr[discard, keep],
  utils[zip, unzip],
)

box::use(
  logic / exchange[...],
  logic / json_save[...],
  utils / constants[...],
  modules / upload,
  modules / currency_date,
  modules / bill_to,
  modules / consultant_business,
  modules / download_zip
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    selected = "Invoice, businesses and date",
    "Invoice Generator",
    tabPanel(
      "Main",
      fluidPage(
        fluidRow(
          column(
            3,
            wellPanel(
              h4(strong("Invoice rendering")),
              div(
                class = "two_column",
                actionButton(ns("reload"), "Discard unsaved changes"),
                radioButtons(ns("lang"), "Language", c("english" = 1, "other" = 2))
              ),
              div(
                class = "generate_buttons",
                br(),
                helpText("Save changes and generates .pdf"),
                div(
                  downloadButton(ns("report"), "Render Document")
                )
              )
            )
          ),
          column(
            3,
            download_zip$ui(ns("download_zip_ns"))
          ),
          column(
            3,
            upload$ui(ns("zip_upload_ns")),
            upload$ui(ns("json_upload_ns"))
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
              div(
                class = "two_column_grid",
                div(
                  class = "go-center",
                  span(strong("Invoice Number"))
                ),
                div(
                  class = "go-center",
                  textInput(ns("invoiceNumber"), "", paste(format(Sys.time(), "%Y-")))
                )
              )
            ),
            div(
              style = "max-width:600px",
              bill_to$ui(ns("bill_to_ns"))
            )
          ),
          column(
            3,
            div(
              style = "max-width:600px",
              consultant_business$ui(ns("consultant_business_ns"))
            )
          ),
          column(
            3,
            currency_date$ui(ns("currency_date_ns"))
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
            6,
            div(
              class = "salary_and_days_container",
              div(
                class = "salary",
                uiOutput(ns("salary_box"))
              ),
              div(
                class = "modified",
                uiOutput(ns("modified_box"))
              ),
              div(
                class = "non_working_days",
                uiOutput(ns("non_working_days_box"))
              )
            )
          ),
          column(
            3,
            img(src = "static/invoice_salary.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "One-liners costs",
      fluidPage(
        fluidRow(
          column(
            8,
            uiOutput(ns("oneliners_box"))
          ),
          column(1),
          column(
            3,
            img(src = "static/invoice_oneliners.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "Grouped costs",
      fluidPage(
        fluidRow(
          column(1),
          column(
            7,
            uiOutput(ns("grouped_box"))
          ),
          column(1),
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
            7,
            uiOutput(ns("consultant_account_box"))
          ),
          column(2),
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
server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_json_lists <- reactiveValues(
      json_final_currency_list = rjson::fromJSON(file = "app/json/final_currency_inv_date.json"),
      json_salary_list = rjson::fromJSON(file = "app/json/salary.json"),
      json_oneliners_list = rjson::fromJSON(file = "app/json/oneliner_costs.json"),
      json_grouped_list = rjson::fromJSON(file = "app/json/grouped_costs.json"),
      json_business_to_bill_list = rjson::fromJSON(file = "app/json/business_to_bill.json"),
      json_consultant_account_list = rjson::fromJSON(file = "app/json/consultant_account.json"),
      json_consultant_business_list = rjson::fromJSON(file = "app/json/consultant_contact.json")
    )

    observeEvent(input$reload,
      {
        rv_json_lists$json_final_currency_list <- rjson::fromJSON(file = "app/json/final_currency_inv_date.json")
        rv_json_lists$json_business_to_bill_list <- rjson::fromJSON(file = "app/json/business_to_bill.json")
        rv_json_lists$json_consultant_account_list <- rjson::fromJSON(file = "app/json/consultant_account.json")
        rv_json_lists$json_consultant_business_list <- rjson::fromJSON(file = "app/json/consultant_contact.json")
        rv_json_lists$json_salary_list <- rjson::fromJSON(file = "app/json/salary.json")
        rv_json_lists$json_oneliners_list <- rjson::fromJSON(file = "app/json/oneliner_costs.json")
        rv_json_lists$json_grouped_list <- rjson::fromJSON(file = "app/json/grouped_costs.json")
      },
      ignoreInit = TRUE
    )


    json_upload_var <- upload$server("json_upload_ns", zip_upload_var, ".json")
    zip_upload_var <- upload$server("zip_upload_ns", json_upload_var, ".zip")

    input_maincurrency <- reactive({
      input$maincurrency
    })

    currency_date_vars <- currency_date$server("currency_date_ns", rv_json_lists, input_maincurrency)

    bill_to$server("bill_to_ns", rv_json_lists$json_business_to_bill_list, file_reac)

    consultant_business$server("consultant_business_ns", rv_json_lists$json_consultant_business_list, file_reac)

    download_zip$server("download_zip_ns", rv_json_lists, input)

    observeEvent(currency_date_vars$exchange_salary(), {
      updateNumericInput(session, paste0("main", "currency_exchange_to_Final_Currency"), value = currency_date_vars$exchange_salary())
    })

    observeEvent(file_reac(),
      {
        input_file <- file_reac()

        if (isTruthy(grepl("zip$", input_file$datapath))) {
          unzip(input_file$datapath, exdir = "app/json", junkpaths = TRUE)
        }
        if (isTruthy(grepl("json$", input_file$datapath))) {
          lapply(seq_along(input_file$name), function(x) {
            file.copy(input_file$datapath[x], file.path(getwd(), "app", "json", input_file$name[x]), overwrite = TRUE)
          })
        }

        rv_json_lists$json_final_currency_list <- rjson::fromJSON(file = "app/json/final_currency_inv_date.json")
        rv_json_lists$json_business_to_bill_list <- rjson::fromJSON(file = "app/json/business_to_bill.json")
        rv_json_lists$json_consultant_account_list <- rjson::fromJSON(file = "app/json/consultant_account.json")
        rv_json_lists$json_consultant_business_list <- rjson::fromJSON(file = "app/json/consultant_contact.json")
        rv_json_lists$json_salary_list <- rjson::fromJSON(file = "app/json/salary.json")
        rv_json_lists$json_oneliners_list <- rjson::fromJSON(file = "app/json/oneliner_costs.json")
        rv_json_lists$json_grouped_list <- rjson::fromJSON(file = "app/json/grouped_costs.json")

        updateTextInput(
          session,
          "final_currency",
          value = rv_json_lists$json_final_currency_list$final_currency
        )
        updateDateInput(
          session,
          "exchangeDate",
          value = as.Date(rv_json_lists$json_final_currency_list$exchangeDate)
        )
        updateDateInput(
          session,
          "invoiceDate",
          value = as.Date(rv_json_lists$json_final_currency_list$invoiceDate)
        )

        consultant_account_list <- rv_json_lists$json_consultant_account_list %>% discard(names(.) %in% "file_identifier")
        char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.character(x))))
        logic_char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.logical(x))))

        lapply(logic_char_consultant_account, function(x) {
          updateCheckboxInput(
            session,
            x,
            value = consultant_account_list[[x]]
          )
        })

        lapply(char_consultant_account, function(x) {
          updateTextInput(
            session,
            x,
            value = consultant_account_list[[x]]
          )
        })

        updateCheckboxInput(
          session,
          paste0("dates", "use"),
          value = rv_json_lists$json_salary_list$dates$use
        )

        updateDateInput(session,
          paste0("dates", "start"),
          value = as.Date(rv_json_lists$json_salary_list$dates$start)
        )

        updateTextInput(
          session, paste0("dates", "date_connector"),
          value = rv_json_lists$json_salary_list$dates$date_connector
        )

        updateDateInput(session, paste0("dates", "end"),
          value = as.Date(rv_json_lists$json_salary_list$dates$end)
        )

        updateTextInput(
          session, paste0("dates", "delivery_month_text"),
          value = rv_json_lists$json_salary_list$dates$delivery_month_text
        )
        json_salary_list_main <- rv_json_lists$json_salary_list$main
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
        salary_list_period <- rv_json_lists$json_salary_list$period
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

        oneliners_list <- rv_json_lists$json_oneliners_list %>% discard(names(.) %in% "file_identifier")
        oneliners_list_names <- names(oneliners_list)
        lapply(oneliners_list_names, function(name) {
          num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_oneliners <- list()
          num_names_oneliners <- char_names_oneliners <- list()

          char_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.character(x))))
          num_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.numeric(x))))
          logic_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.logical(x))))

          char_names_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE)
          num_names_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE)

          char_names_not_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE, invert = TRUE)
          num_names_not_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE, invert = TRUE)

          lapply(char_names_currency[[name]], function(x) {
            updateTextInput(session,
              paste0("oneliners", name, x),
              value = oneliners_list[[name]][[x]]
            )
          })
          lapply(num_names_currency[[name]], function(x) {
            updateNumericInput(session,
              paste0("oneliners", name, x),
              value = oneliners_list[[name]][[x]]
            )
          })
          lapply(char_names_not_currency[[name]], function(x) {
            updateTextInput(session,
              paste0("oneliners", name, x),
              value = oneliners_list[[name]][[x]]
            )
          })
          lapply(num_names_not_currency[[name]], function(x) {
            updateNumericInput(session,
              paste0("oneliners", name, x),
              value = oneliners_list[[name]][[x]]
            )
          })
          lapply(logic_names_oneliners[[name]], function(x) {
            updateCheckboxInput(
              session,
              paste0("oneliners", name, x),
              value = oneliners_list[[name]][[x]]
            )
          })
        })
        grouped_list <- rv_json_lists$json_grouped_list %>% discard(names(.) %in% "file_identifier")

        root_names <- c(
          "currency_exchange_to_Final_Currency", "use",
          "GeneralName", "currency"
        )

        grouped_list_root <- grouped_list %>% keep(names(.) %in% root_names)

        char_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.character(x))))
        num_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.numeric(x))))
        logic_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.logical(x))))

        char_names_currency <- grep("currency", char_names_grouped, value = TRUE)
        num_names_currency <- grep("currency", num_names_grouped, value = TRUE)

        char_names_not_currency <- grep("currency", char_names_grouped, value = TRUE, invert = TRUE)

        grouped_sublists <- grouped_list %>% discard(names(.) %in% root_names)

        grouped_list_names <- names(grouped_sublists)

        lapply(char_names_currency, function(x) {
          updateTextInput(
            session,
            paste0("grouped", x),
            value = grouped_list[[x]]
          )
        })
        lapply(num_names_currency, function(x) {
          updateNumericInput(
            session,
            paste0("grouped", x),
            value = grouped_list[[x]]
          )
        })
        lapply(char_names_not_currency, function(x) {
          updateTextInput(
            session,
            paste0("grouped", x),
            value = grouped_list[[x]]
          )
        })
        lapply(logic_names_grouped, function(x) {
          updateCheckboxInput(
            session,
            paste0("grouped", x),
            value = grouped_list[[x]]
          )
        })

        lapply(grouped_list_names, function(name) {
          num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_grouped <- list()
          num_names_grouped <- char_names_grouped <- list()

          char_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.character(x))))
          num_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.numeric(x))))
          logic_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.logical(x))))

          lapply(char_names_grouped[[name]], function(x) {
            updateTextInput(
              session,
              paste0("grouped", name, x),
              value = grouped_list[[name]][[x]]
            )
          })
          lapply(num_names_grouped[[name]], function(x) {
            updateNumericInput(
              session,
              paste0("grouped", name, x),
              value = grouped_list[[name]][[x]]
            )
          })
        })
      },
      ignoreInit = TRUE
    )

    file_reac <- reactiveVal()

    observeEvent(zip_upload_var(),
      {
        req(zip_upload_var())
        file_reac(zip_upload_var())
      },
      ignoreInit = TRUE
    )

    observeEvent(json_upload_var(),
      {
        req(json_upload_var())
        file_reac(json_upload_var())
      },
      ignoreInit = TRUE
    )

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

    output$modify_salary <- downloadHandler(
      filename = function() {
        "salary.json"
      },
      content = function(file) {
        file_name <- "salary.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_json_save(
          input,
          nested_list = rv_json_lists$json_salary_list,
          prefix = "",
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    output$modify_grouped <- downloadHandler(
      filename = function() {
        "grouped_costs.json"
      },
      content = function(file) {
        file_name <- "grouped_costs.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_and_root_save(input,
          nested_list = rv_json_lists$json_grouped_list,
          prefix = "grouped",
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    output$modify_oneliners <- downloadHandler(
      filename = function() {
        "oneliner_costs.json"
      },
      content = function(file) {
        file_name <- "oneliner_costs.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_json_save(input,
          nested_list = rv_json_lists$json_oneliners_list,
          prefix = "oneliners",
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    output$modify_account <- downloadHandler(
      filename = function() {
        "consultant_account.json"
      },
      content = function(file) {
        file_name <- "consultant_account.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_json_lists$json_consultant_account_list,
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )


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
            checkboxInput(ns(paste0("dates", "use")), "Show Dates", rv_json_lists$json_salary_list$dates$use),
            actionButton(ns("increaseDate"), ""),
            span("1 Month"),
            br(),
            actionButton(ns("decreaseDate"), "")
          ),
          tagList(
            dateInput(ns(paste0("dates", "start")), "Start Date: ", value = as.Date(rv_json_lists$json_salary_list$dates$start)),
            textInput(ns(paste0("dates", "date_connector")), "date connector", rv_json_lists$json_salary_list$dates$date_connector),
            dateInput(ns(paste0("dates", "end")), "End Date: ", value = as.Date(rv_json_lists$json_salary_list$dates$end))
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
            textInput(ns(paste0("dates", "delivery_month_text")), "", rv_json_lists$json_salary_list$dates$delivery_month_text)
          )
        )
      )
    })

    output$salary_box <- renderUI({
      char_names <- names(which(sapply(rv_json_lists$json_salary_list$main, function(x) is.character(x))))
      num_names <- names(which(sapply(rv_json_lists$json_salary_list$main, function(x) is.numeric(x))))
      logic_names <- names(which(sapply(rv_json_lists$json_salary_list$main, function(x) is.logical(x))))

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
              rv_json_lists$json_salary_list$main[[x]]
            )
          })
          num_names_currency_list <- lapply(num_names_currency, function(x) {
            numericInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                gsub("_", " ", x, perl = TRUE)
              ),
              rv_json_lists$json_salary_list$main[[x]]
            )
          })
          char_list <- lapply(char_names_not_currency, function(x) {
            textInput(
              ns(paste0("main", x)),
              div(
                class = "wrap",
                sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
              ),
              rv_json_lists$json_salary_list$main[[x]]
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
                rv_json_lists$json_salary_list$main[[x]]
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
              rv_json_lists$json_salary_list$main[[x]]
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
            downloadButton(ns("modify_salary"),
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

    output$oneliners_box <- renderUI({
      oneliners_list <- rv_json_lists$json_oneliners_list %>% discard(names(.) %in% "file_identifier")
      oneliners_list_names <- names(oneliners_list)

      wellPanel(
        tagList(
          lapply(seq_along(oneliners_list_names), function(idx) {
            name <- oneliners_list_names[[idx]]
            num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- list()
            logic_names_oneliners <- num_names_oneliners <- char_names_oneliners <- list()

            char_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.character(x))))
            num_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.numeric(x))))
            logic_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.logical(x))))

            char_names_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE)
            num_names_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE)

            char_names_not_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE, invert = TRUE)
            num_names_not_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE, invert = TRUE)

            char_names_currency_list <- lapply(char_names_currency[[name]], function(x) {
              textInput(
                ns(paste0("oneliners", name, x)),
                if (idx == 1) {
                  div(
                    class = "wrap",
                    sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
                  )
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            num_names_currency_list <- lapply(num_names_currency[[name]], function(x) {
              numericInput(
                ns(paste0("oneliners", name, x)),
                if (idx == 1) {
                  div(
                    class = "wrap",
                    gsub("_", " ", x, perl = TRUE)
                  )
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            char_names_oneliners_not_currency_list <- lapply(char_names_not_currency[[name]], function(x) {
              textInput(
                ns(paste0("oneliners", name, x)),
                if (idx == 1) {
                  gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x))
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            num_names_oneliners_not_currency_list <- lapply(num_names_not_currency[[name]], function(x) {
              numericInput(
                ns(paste0("oneliners", name, x)),
                if (idx == 1) {
                  gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE))
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            div(
              class = "six_column_grid",
              div(
                class = "go-bottom",
                h4(strong(name))
              ),
              div(
                class = "go-bottom",
                char_names_currency_list
              ),
              div(
                class = "go-bottom",
                num_names_currency_list
              ),
              div(
                class = "go-bottom",
                char_names_oneliners_not_currency_list
              ),
              div(class = "go-bottom", num_names_oneliners_not_currency_list),
              div(
                class = "go-bottom",
                lapply(logic_names_oneliners[[name]], function(x) {
                  checkboxInput(
                    ns(paste0("oneliners", name, x)),
                    gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                    oneliners_list[[name]][[x]]
                  )
                })
              )
            )
          })
        ),
        helpText("Go to Main tab to save all"),
        downloadButton(ns("modify_oneliners"),
          strong("Save and Download", code("oneliner_costs.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$grouped_box <- renderUI({
      grouped_list <- rv_json_lists$json_grouped_list %>% discard(names(.) %in% "file_identifier")

      root_names <- c(
        "currency_exchange_to_Final_Currency", "use",
        "GeneralName", "currency"
      )

      grouped_list_root <- grouped_list %>% keep(names(.) %in% root_names)

      char_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.character(x))))
      num_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.numeric(x))))
      logic_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names_grouped, value = TRUE)
      num_names_currency <- grep("currency", num_names_grouped, value = TRUE)

      char_names_not_currency <- grep("currency", char_names_grouped, value = TRUE, invert = TRUE)

      grouped_sublists <- grouped_list %>% discard(names(.) %in% root_names)
      grouped_list_names <- names(grouped_sublists)

      tagList(
        wellPanel(
          {
            tagList({
              char_names_currency_list <- lapply(char_names_currency, function(x) {
                textInput(
                  ns(paste0("grouped", x)),
                  div(
                    class = "wrap",
                    sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
                  ),
                  grouped_list[[x]]
                )
              })
              num_names_currency_list <- lapply(num_names_currency, function(x) {
                numericInput(
                  ns(paste0("grouped", x)),
                  div(
                    class = "wrap",
                    gsub("_", " ", x, perl = TRUE)
                  ),
                  grouped_list[[x]]
                )
              })
              char_names_grouped_list <- lapply(char_names_not_currency, function(x) {
                textInput(
                  ns(paste0("grouped", x)),
                  gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
                  grouped_list[[x]]
                )
              })
              logic_names_grouped_list <- lapply(logic_names_grouped, function(x) {
                checkboxInput(
                  ns(paste0("grouped", x)),
                  gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                  grouped_list[[x]]
                )
              })
              div(
                class = "five_column_grid",
                h4(strong("Grouped Costs")),
                div(
                  class = "go-bottom",
                  char_names_grouped_list
                ),
                div(
                  class = "go-bottom",
                  char_names_currency_list
                ),
                div(
                  class = "go-bottom",
                  num_names_currency_list
                ),
                div(
                  class = "go-bottom",
                  logic_names_grouped_list
                )
              )
            })
          },
          div(
            class = "five_column_grid",
            div(),
            div(
              helpText("Go to Main tab to save all"),
              downloadButton(ns("modify_grouped"),
                strong("Save and Download", code("grouped_costs.json")),
                style = "white-space: normal;
                           word-wrap: break-word;"
              )
            )
          )
        ),
        wellPanel(
          tagList(
            lapply(seq_along(grouped_list_names), function(idx) {
              name <- grouped_list_names[idx]
              num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_grouped <- list()
              num_names_grouped <- char_names_grouped <- list()

              char_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.character(x))))
              num_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.numeric(x))))
              logic_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.logical(x))))

              char_names_list <- lapply(char_names_grouped[[name]], function(x) {
                textInput(
                  ns(paste0("grouped", name, x)),
                  if (idx == 1) {
                    div(
                      class = "wrap",
                      sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
                    )
                  } else {
                    ""
                  },
                  grouped_list[[name]][[x]]
                )
              })
              num_names_list <- lapply(num_names_grouped[[name]], function(x) {
                numericInput(
                  ns(paste0("grouped", name, x)),
                  if (idx == 1) {
                    div(
                      class = "wrap",
                      gsub("_", " ", x, perl = TRUE)
                    )
                  } else {
                    ""
                  },
                  grouped_list[[name]][[x]]
                )
              })
              div(
                class = "three_column_grid_center",
                div(
                  class = "go-bottom",
                  h4(strong(name))
                ),
                div(
                  class = "go-bottom",
                  char_names_list
                ),
                div(
                  class = "go-bottom",
                  num_names_list
                )
              )
            })
          )
        )
      )
    })

    output$salary_period_panel <- renderUI({
      char_period <- names(which(sapply(rv_json_lists$json_salary_list$period, function(x) is.character(x))))
      num_period <- names(which(sapply(rv_json_lists$json_salary_list$period, function(x) is.numeric(x))))
      logic_period <- names(which(sapply(rv_json_lists$json_salary_list$period, function(x) is.logical(x))))
      wellPanel(
        h4(strong("Salary Period(s)")),
        splitLayout(
          cellWidths = c("30%", "30%", "10%", "20%"),
          lapply(num_period, function(x) {
            numericInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_salary_list$period[[x]]
            )
          }),
          lapply(char_period, function(x) {
            textInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_salary_list$period[[x]]
            )
          }),
          div(),
          lapply(logic_period, function(x) {
            checkboxInput(
              ns(paste0("period", x)),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_salary_list$period[[x]]
            )
          })
        )
      )
    })

    output$modified_box <- renderUI({
      char_modified <- names(which(sapply(rv_json_lists$json_salary_list$modified_days, function(x) is.character(x))))
      num_modified <- names(which(sapply(rv_json_lists$json_salary_list$modified_days, function(x) is.numeric(x))))
      logic_modified <- names(which(sapply(rv_json_lists$json_salary_list$modified_days, function(x) is.logical(x))))

      tagList(
        wellPanel(
          h4(strong("Modified Pay Days")),
          div(
            class = "three_column_grid_left_big",
            lapply(char_modified, function(x) {
              textInput(
                ns(paste0("modified_days", x)),
                gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
                rv_json_lists$json_salary_list$modified_days[[x]]
              )
            }),
            lapply(num_modified, function(x) {
              numericInput(
                ns(paste0("modified_days", x)),
                gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
                rv_json_lists$json_salary_list$modified_days[[x]]
              )
            }),
            lapply(logic_modified, function(x) {
              checkboxInput(
                ns(paste0("modified_days", x)),
                gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                rv_json_lists$json_salary_list$modified_days[[x]]
              )
            })
          )
        )
      )
    })

    output$non_working_days_box <- renderUI({
      num_nwd <- names(which(sapply(rv_json_lists$json_salary_list$non_working_days, function(x) is.numeric(x))))
      logic_nwd <- names(which(sapply(rv_json_lists$json_salary_list$non_working_days, function(x) is.logical(x))))
      tagList(
        wellPanel(
          h4(strong("non-working Days")),
          splitLayout(
            cellWidths = c("50%", "10%", "30%"),
            lapply(num_nwd, function(x) {
              numericInput(
                ns(paste0("non_working_days", x)),
                "",
                rv_json_lists$json_salary_list$non_working_days[[x]]
              )
            }),
            div(),
            lapply(logic_nwd, function(x) {
              checkboxInput(
                ns(paste0("non_working_days", x)),
                gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                rv_json_lists$json_salary_list$non_working_days[[x]]
              )
            })
          )
        )
      )
    })

    output$consultant_account_box <- renderUI({
      consultant_account_list <- rv_json_lists$json_consultant_account_list %>% discard(names(.) %in% "file_identifier")
      char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.character(x))))
      logic_char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.logical(x))))

      wellPanel(
        h4(strong("Consultant Account")),
        lapply(logic_char_consultant_account, function(x) {
          checkboxInput(
            ns(x),
            gsub("_", " ", gsub(pattern_a, pattern_b, x)),
            consultant_account_list[[x]]
          )
        }),
        {
          char_inputs <- lapply(char_consultant_account, function(x) {
            textInput(ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              value = consultant_account_list[[x]]
            )
          })
          char_inputs_len <- length(char_inputs)
          half <- ceiling(char_inputs_len / 2)
          div(
            class = "two_column_grid_gap",
            div(char_inputs[1:half]),
            div(char_inputs[(half + 1):char_inputs_len]),
          )
        },
        helpText("Go to Main tab to save all"),
        downloadButton(ns("modify_account"),
          strong("Save and Download", code("consultant_account.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$report <- downloadHandler(
      filename = "invoice.pdf",
      content = function(file) {
        plain_json_save(
          input,
          plain_list = rv_json_lists$json_consultant_business_list,
          folders = "app/json", file_name = "consultant_contact.json"
        )
        plain_json_save(
          input,
          plain_list = rv_json_lists$json_consultant_account_list,
          folders = "app/json", file_name = "consultant_account.json"
        )
        plain_json_save(
          input,
          plain_list = rv_json_lists$json_business_to_bill_list,
          folders = "app/json", file_name = "business_to_bill.json"
        )
        nested_json_save(
          input,
          nested_list = rv_json_lists$json_salary_list,
          prefix = "",
          folders = "app/json",
          file_name = "salary.json"
        )
        nested_and_root_save(input,
          nested_list = rv_json_lists$json_grouped_list,
          prefix = "grouped",
          folders = "app/json",
          file_name = "grouped_costs.json"
        )
        nested_json_save(input,
          nested_list = rv_json_lists$json_oneliners_list,
          prefix = "oneliners",
          folders = "app/json",
          file_name = "oneliner_costs.json"
        )
        plain_json_save(
          input,
          plain_list = rv_json_lists$json_final_currency_list,
          folders = "app/json",
          file_name = "final_currency_inv_date.json"
        )

        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)
        temp_report <- file.path(folder, "inv_md_dont_modify.Rmd")
        file.copy("app/invoice.Rmd", temp_report, overwrite = TRUE)
        app_path <- file.path(getwd(), "app")
        all_params <- reactiveValuesToList(input)
        params <- list(invoiceNumber = all_params$invoiceNumber, lang = all_params$lang, app_path = app_path)

        rmarkdown::render(temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )

    outputOptions(output, "salary_dates_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_period_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "salary_box", suspendWhenHidden = FALSE)
    outputOptions(output, "modified_box", suspendWhenHidden = FALSE)
    outputOptions(output, "oneliners_box", suspendWhenHidden = FALSE)
    outputOptions(output, "grouped_box", suspendWhenHidden = FALSE)
    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)
    outputOptions(output, "non_working_days_box", suspendWhenHidden = FALSE)
  })
}
