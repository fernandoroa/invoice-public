box::use(
  shiny[...],
  dplyr[...],
  rjson[rjson_fromJSON = fromJSON],
  jsonlite[...],
  lubridate[...],
  purrr[discard, keep],
  utils[zip, unzip],
  stats[runif]
)

box::use(
  logic / exchange[...],
  logic / save_files[...],
  utils / constants[...],
  modules / upload,
  modules / currency_date,
  modules / business,
  modules / download_zip,
  modules / generate_pdf,
  modules / salary,
  modules / oneliner,
  modules / grouped_costs,
  modules / account,
  modules / json_ace,
  modules / rmd_ace,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = div(
      a(
        href = "https://www.github.com/fernandoroa/invoice-public",
        img(
          src = "static/github.svg",
          style = "margin-top: -5px;
                               padding-right:10px;
                               padding-bottom:0;",
          height = 25
        )
      ), "Invoice Generator"
    ),
    windowTitle = "Invoice",
    selected = "Invoice, businesses and date",
    tabPanel(
      "Main",
      fluidPage(
        fluidRow(
          column(
            3,
            wellPanel(
              div(
                class = "two_column",
                actionButton(ns("reload"), "Discard unsaved changes"),
                radioButtons(ns("lang"), "Language", c("english" = 1, "other" = 2))
              )
            ),
            wellPanel(
              div(
                class = "generate_buttons",
                h4(strong("Invoice rendering")),
                generate_pdf$ui(ns("report_ns"))
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
      "Salary and related",
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
      "One-liner costs",
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
      "Bank Account",
      fluidPage(
        fluidRow(
          column(
            7,
            account$ui(ns("account_ns"))
          ),
          column(2),
          column(
            3,
            img(src = "static/invoice_bank.svg", style = "max-width:25vw")
          )
        )
      )
    ),
    tabPanel(
      "Titles",
      fluidPage(
        fluidRow(
          json_ace$ui(ns("json_ace_ns"))
        )
      )
    ),
    tabPanel(
      "Invoice.Rmd",
      fluidPage(
        fluidRow(
          rmd_ace$ui(ns("rmd_ace_ns"))
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
      final_currency_list = rjson_fromJSON(file = "app/json/final_currency_inv_date.json"),
      salary_list = rjson_fromJSON(file = "app/json/salary.json"),
      oneliners_list = rjson_fromJSON(file = "app/json/oneliner_costs.json"),
      grouped_list = rjson_fromJSON(file = "app/json/grouped_costs.json"),
      business_to_bill_list = rjson_fromJSON(file = "app/json/business_to_bill.json"),
      consultant_account_list = rjson_fromJSON(file = "app/json/consultant_account.json"),
      consultant_business_list = rjson_fromJSON(file = "app/json/consultant_business.json")
    )

    file_reac <- reactiveVal()
    files_ready_reac <- reactiveVal(TRUE)

    observeEvent(input$reload,
      {
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    json_upload_var <- upload$server("json_upload_ns", zip_upload_var, ".json")
    zip_upload_var <- upload$server("zip_upload_ns", json_upload_var, ".zip")

    currency_date_vars <- currency_date$server(
      "currency_date_ns", rv_json_lists, "final_currency_list", salary_currency,
      input, files_ready_reac
    )

    business$server("bill_to_ns", rv_json_lists,
      files_ready_reac,
      useLabel = FALSE,
      basename = "business_to_bill",
      box_title = "Business to bill"
    )

    business$server("consultant_business_ns", rv_json_lists,
      files_ready_reac,
      basename = "consultant_business",
      box_title = "Consultant details"
    )

    download_zip$server("download_zip_ns", rv_json_lists, input)
    generate_pdf$server("report_ns", rv_json_lists, input)

    salary_currency <- salary$server(
      "salary_ns", rv_json_lists, "salary_list",
      files_ready_reac, currency_date_vars$exchange_salary
    )

    oneliner$server(
      "oneliner_ns", rv_json_lists, "oneliners_list",
      files_ready_reac, currency_date_vars$exchange_oneliners
    )

    grouped_costs$server(
      "grouped_ns", rv_json_lists, "grouped_list",
      files_ready_reac, currency_date_vars$exchange_grouped
    )

    account$server("account_ns", rv_json_lists, files_ready_reac)

    json_ace$server("json_ace_ns", files_ready_reac)

    rmd_ace$server("rmd_ace_ns", files_ready_reac)

    observeEvent(file_reac(),
      {
        rv_json_lists$final_currency_list <- rjson_fromJSON(file = "app/json/final_currency_inv_date.json")
        rv_json_lists$business_to_bill_list <- rjson_fromJSON(file = "app/json/business_to_bill.json")
        rv_json_lists$consultant_account_list <- rjson_fromJSON(file = "app/json/consultant_account.json")
        rv_json_lists$consultant_business_list <- rjson_fromJSON(file = "app/json/consultant_business.json")
        rv_json_lists$salary_list <- rjson_fromJSON(file = "app/json/salary.json")
        rv_json_lists$oneliners_list <- rjson_fromJSON(file = "app/json/oneliner_costs.json")
        rv_json_lists$grouped_list <- rjson_fromJSON(file = "app/json/grouped_costs.json")

        files_ready_reac(!files_ready_reac())
      },
      ignoreInit = TRUE
    )

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
  })
}
