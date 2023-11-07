box::use(
  shiny[...],
  dplyr[...],
  rjson[...],
  jsonlite[...],
  lubridate[...],
  purrr[discard, keep],
  utils[zip, unzip],
  stats[runif]
)

box::use(
  logic / exchange[...],
  logic / json_save[...],
  utils / constants[...],
  modules / upload,
  modules / currency_date,
  modules / business,
  modules / download_zip,
  modules / generate_pdf,
  modules / salary,
  modules / oneliner
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
              generate_pdf$ui(ns("report_ns"))
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
              business$ui(ns("bill_to_ns"), "business_to_bill_box")
            )
          ),
          column(
            3,
            div(
              style = "max-width:600px",
              business$ui(ns("consultant_business_ns"), "consultant_business_box")
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
          salary$ui(ns("salary_ns")),
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
            oneliner$ui(ns("oneliner_ns"))
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
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    json_upload_var <- upload$server("json_upload_ns", zip_upload_var, ".json")
    zip_upload_var <- upload$server("zip_upload_ns", json_upload_var, ".zip")

    currency_date_vars <- currency_date$server("currency_date_ns", rv_json_lists$json_final_currency_list, salary_currency, input, file_reac)

    business$server("bill_to_ns", rv_json_lists$json_business_to_bill_list, file_reac,
      useLabel = FALSE,
      basename = "business_to_bill.json", output_id = "business_to_bill_box",
      box_title = "Business to bill"
    )

    business$server("consultant_business_ns", rv_json_lists$json_consultant_business_list,
      file_reac,
      basename = "consultant_contact.json", output_id = "consultant_business_box",
      box_title = "Consultant details"
    )

    download_zip$server("download_zip_ns", rv_json_lists, input)
    generate_pdf$server("report_ns", rv_json_lists, input)

    salary_currency <- salary$server("salary_ns", rv_json_lists$json_salary_list, file_reac, currency_date_vars$exchange_salary)

    oneliner$server("oneliner_ns", rv_json_lists$json_oneliners_list, file_reac, currency_date_vars$exchange_oneliners)

    observeEvent(file_reac(),
      {
        rv_json_lists$json_final_currency_list <- rjson::fromJSON(file = "app/json/final_currency_inv_date.json")
        rv_json_lists$json_business_to_bill_list <- rjson::fromJSON(file = "app/json/business_to_bill.json")
        rv_json_lists$json_consultant_account_list <- rjson::fromJSON(file = "app/json/consultant_account.json")
        rv_json_lists$json_consultant_business_list <- rjson::fromJSON(file = "app/json/consultant_contact.json")
        rv_json_lists$json_salary_list <- rjson::fromJSON(file = "app/json/salary.json")
        rv_json_lists$json_oneliners_list <- rjson::fromJSON(file = "app/json/oneliner_costs.json")
        rv_json_lists$json_grouped_list <- rjson::fromJSON(file = "app/json/grouped_costs.json")

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
        input_file <- zip_upload_var()
        unzip(input_file$datapath, exdir = "app/json", junkpaths = TRUE)
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    observeEvent(json_upload_var(),
      {
        req(json_upload_var())
        input_file <- json_upload_var()
        lapply(seq_along(input_file$name), function(x) {
          file.copy(input_file$datapath[x], file.path(getwd(), "app", "json", input_file$name[x]), overwrite = TRUE)
        })
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    output$save_download_grouped <- downloadHandler(
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

    output$save_download_account <- downloadHandler(
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
              downloadButton(ns("save_download_grouped"),
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
        downloadButton(ns("save_download_account"),
          strong("Save and Download", code("consultant_account.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })


    outputOptions(output, "grouped_box", suspendWhenHidden = FALSE)
    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)
  })
}
