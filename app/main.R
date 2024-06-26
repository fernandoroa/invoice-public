box::use(
  shiny[...],
  dplyr[...],
  rjson[rjson_fromJSON = fromJSON],
  jsonlite[...],
  lubridate[...],
  purrr[discard, keep],
  utils[zip, unzip],
  stats[runif],
  fs[path_dir]
)

box::use(
  logic / exchange[...],
  logic / save_files[...],
  utils / constants[...],
  utils / create_files[...],
  utils / validate[...],
  utils / continue_sequence[...],
  utils / json_to_reactive_values[...],
  modules / upload,
  modules / currency_date,
  modules / bump_month,
  modules / business,
  modules / download_zip,
  modules / generate_pdf,
  modules / salary,
  modules / oneliner,
  modules / grouped_costs,
  modules / account,
  modules / json_ace,
  modules / rmd_ace,
  modules / spreadsheet
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
                div(
                  actionButton(ns("reload"), "Discard unsaved changes"),
                  div(
                    class = "wrap",
                    actionButton(ns("reset_files"), "Reset all")
                  ),
                  helpText("Discards saved and unsaved changes by"),
                  helpText(strong("deleting the temp folder with your data"))
                ),
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
            currency_date$ui(ns("currency_date_ns")),
            bump_month$ui(ns("bump_month_ns"))
          ),
          column(
            3,
            img(src = "static/invoice_header.svg")
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
            img(src = "static/invoice_salary.svg")
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
            img(src = "static/invoice_oneliners.svg")
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
            img(src = "static/invoice_grouped.svg")
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
            img(src = "static/invoice_bank.svg")
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
    ),
    tabPanel(
      "spreadsheet",
      fluidPage(
        fluidRow(
          spreadsheet$ui(ns("spreadsheet_ns"))
        )
      )
    )
  )
}

#' @export
server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # use TRUE only IN YOUR PERSONAL COMPUTER, defaults to FALSE
    # when TRUE, the Reset button will NOT delete the "working" temp folder (i.e. will not erase your saved data) !!!
    # when TRUE the Reset button will keep saved changes, and a pop-up will be raised
    # when TRUE the "working" folder is not temporary, but the `app` folder of this app
    local_safe_computer_mode <- FALSE

    if (local_safe_computer_mode) {
      temp_folder_session <- file.path(getwd(), "app") # local, not a temp folder
    } else {
      temp_folder_session <- create_files()
    }
    rv_temp_folder_session <- reactiveVal(temp_folder_session)

    rv_json_lists <- reactiveValues(
      final_currency_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "invoice_and_final_currency.json")),
      salary_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "salary.json")),
      oneliners_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "oneliner_costs.json")),
      grouped_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "grouped_costs.json")),
      business_to_bill_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "business_to_bill.json")),
      consultant_account_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "consultant_account.json")),
      consultant_business_list = rjson_fromJSON(file = file.path(temp_folder_session, "json", "consultant_business.json"))
    )

    file_reac <- reactiveVal()
    files_ready_reac <- reactiveVal(TRUE)

    observeEvent(input$reload,
      {
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$reset_files,
      {
        if (!local_safe_computer_mode) {
          unlink(rv_temp_folder_session(), recursive = TRUE, force = TRUE)
          temp_folder_session <- create_files()
          rv_temp_folder_session(temp_folder_session)
        } else {
          showNotification("You are in local mode, keeping saved changes, discarding unsaved changes")
        }
        file_reac(runif(1))
      },
      ignoreInit = TRUE
    )

    json_upload_var <- upload$server("json_upload_ns", zip_upload_var, ".json")
    zip_upload_var <- upload$server("zip_upload_ns", json_upload_var, ".zip")

    bump_month_vars <- bump_month$server("bump_month_ns")

    currency_date_vars <- currency_date$server(
      "currency_date_ns", rv_json_lists, "final_currency_list", salary_currency,
      input, files_ready_reac,
      rv_temp_folder_session,
      bump_month_vars
    )

    business$server("bill_to_ns", rv_json_lists,
      files_ready_reac,
      useLabel = FALSE,
      basename = "business_to_bill",
      box_title = "Business to bill",
      rv_temp_folder_session
    )

    business$server("consultant_business_ns", rv_json_lists,
      files_ready_reac,
      useLabel = TRUE,
      basename = "consultant_business",
      box_title = "Consultant details",
      rv_temp_folder_session
    )

    download_zip$server("download_zip_ns", rv_json_lists, input, oneliner_vars, grouped_vars, rv_temp_folder_session)
    generate_pdf$server("report_ns", rv_json_lists, input, oneliner_vars, grouped_vars, rv_temp_folder_session)

    salary_currency <- salary$server(
      "salary_ns", rv_json_lists, "salary_list",
      files_ready_reac, currency_date_vars,
      rv_temp_folder_session,
      bump_month_vars
    )

    oneliner_vars <- oneliner$server(
      "oneliner_ns", rv_json_lists, "oneliners_list",
      files_ready_reac, currency_date_vars$exchange_oneliners,
      rv_temp_folder_session, input, grouped_vars
    )

    grouped_vars <- grouped_costs$server(
      "grouped_ns", rv_json_lists, "grouped_list",
      files_ready_reac, currency_date_vars$exchange_grouped,
      rv_temp_folder_session, input, oneliner_vars
    )

    observeEvent(oneliner_vars$add_oneliner(), {
      rv_json_lists <- json_to_reactive_values(rv_temp_folder_session, rv_json_lists)

      last_element <- duplicate_last_list_element(rv_json_lists$oneliners_list)
      rv_json_lists$oneliners_list <- c(rv_json_lists$oneliners_list, last_element)
      files_ready_reac(!files_ready_reac())
    })

    observeEvent(grouped_vars$add_grouped_element(), {
      rv_json_lists <- json_to_reactive_values(rv_temp_folder_session, rv_json_lists)

      last_element <- duplicate_last_list_element(rv_json_lists$grouped_list)
      rv_json_lists$grouped_list <- c(rv_json_lists$grouped_list, last_element)
      files_ready_reac(!files_ready_reac())
    })

    account$server("account_ns", rv_json_lists, files_ready_reac, rv_temp_folder_session)

    json_ace$server("json_ace_ns", files_ready_reac, rv_temp_folder_session)

    rmd_ace$server("rmd_ace_ns", files_ready_reac, rv_temp_folder_session)

    spreadsheet$server("spreadsheet_ns", rv_json_lists, oneliner_vars, grouped_vars, rv_temp_folder_session, input)

    observeEvent(file_reac(),
      {
        rv_json_lists <- json_to_reactive_values(rv_temp_folder_session, rv_json_lists)

        files_ready_reac(!files_ready_reac())
      },
      ignoreInit = TRUE
    )

    observeEvent(zip_upload_var(),
      {
        req(zip_upload_var())
        input_file <- zip_upload_var()

        json_folder_path <- file.path(rv_temp_folder_session(), "json")
        json_folder_pre_path <- file.path(rv_temp_folder_session(), "pre_json")
        dir.create(json_folder_pre_path, recursive = TRUE)

        unzip(input_file$datapath,
          exdir = json_folder_pre_path,
          junkpaths = TRUE
        )

        all_valid <- validate_json_files(json_folder_pre_path)
        if (all_valid) {
          json_files_path <- list.files(json_folder_pre_path, full.names = TRUE)
          json_files_path_base <- list.files(json_folder_pre_path)
          lapply(seq_along(json_files_path), function(x) {
            file.copy(json_files_path[x],
              file.path(rv_temp_folder_session(), "json", json_files_path_base[x]),
              overwrite = TRUE
            )
          })
          file_reac(runif(1))
        } else {
          showNotification("One or more invalid .json files, nothing done", type = "error", duration = 15)
        }
        unlink(json_folder_pre_path, recursive = TRUE, force = TRUE)
      },
      ignoreInit = TRUE
    )

    observeEvent(json_upload_var(),
      {
        req(json_upload_var())
        input_file <- json_upload_var()

        json_folder_pre_path <- path_dir(input_file$datapath) |> unique()

        all_valid <- validate_json_files(json_folder_pre_path)
        if (all_valid) {
          lapply(seq_along(input_file$name), function(x) {
            file.copy(input_file$datapath[x],
              file.path(rv_temp_folder_session(), "json", input_file$name[x]),
              overwrite = TRUE
            )
          })
          file_reac(runif(1))
        } else {
          showNotification("One or more invalid .json files, nothing done", type = "error", duration = 15)
        }

        unlink(json_folder_pre_path, recursive = TRUE, force = TRUE)
      },
      ignoreInit = TRUE
    )
  })
}
