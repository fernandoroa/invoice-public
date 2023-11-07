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
  modules / oneliner,
  modules / grouped_costs
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
            grouped_costs$ui(ns("grouped_ns"))
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
    grouped_costs$server("grouped_ns", rv_json_lists$json_grouped_list, file_reac, currency_date_vars$exchange_grouped)

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

    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)
  })
}
