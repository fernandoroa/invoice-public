box::use(
  shiny[...],
  dplyr[...],
  rjson[...],
  jsonlite[...],
  lubridate[...],
  tinytex[...],
)

# ubuntu
# sudo apt install texlive-latex-base

# Windows
# https://github.com/rstudio/tinytex-releases # download manually

#' @export
ui <- function(id) {
  ns <- NS(id)

  navbarPage(
    "Invoice Generator",
    tabPanel(
      "Main Fields",
      fluidPage(
        fluidRow(
          column(
            3
            #
            #    manual inputs, see params in .Rmd
            #
            , wellPanel(
              div(
                class = "two_column",
                actionButton(ns("reload"), "Reload Json files"),
                div(
                  id = "invoice-form-container",
                  textInput(ns("invoiceNumber"), "Invoice Number", paste(format(Sys.time(), "%Y-")))
                ),
                radioButtons(ns("lang"), "Language", c("english" = 1, "other" = 2)),
              )
            ),
            uiOutput(ns("first_well_panel")),
            uiOutput(ns("period_well_panel"))
          ),
          column(
            3,
            uiOutput(ns("salary_box")),
            wellPanel(
              h2("Generate invoice!"),
              actionButton(
                ns("modify_all"),
                strong(
                  "Save All",
                  code("*.json"),
                  "required!"
                )
              ),
              downloadButton(ns("report"), "Generate invoice in .pdf"),
              helpText("saving .json changes is mandatory"),
            )
          ),
          column(
            3,
            uiOutput(ns("CCE_box")),
            uiOutput(ns("benefits_box")),
            uiOutput(ns("local_box")),
            uiOutput(ns("sick_box"))
          ),
          column(
            3,
            #
            #   3rd box
            #
            div(
              style = "max-width:600px",
              uiOutput(ns("bank_box")),
              uiOutput(ns("consultant_account_box")),
              uiOutput(ns("consultant_business_box"))
            )
          )
        )
      )
    ),
    tabPanel("Component 2"),
    tabPanel("Component 3")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_json_lists <- reactiveValues(
      json_main_list = rjson::fromJSON(file = "app/json/main.json"),
      json_period_list = rjson::fromJSON(file = "app/json/input_period.json"),
      json_salary_list = rjson::fromJSON(file = "app/json/input_salary.json"),
      json_benefits_list = rjson::fromJSON(file = "app/json/input_benefits.json"),
      json_local_list = rjson::fromJSON(file = "app/json/input_local.json"),
      json_sick_list = rjson::fromJSON(file = "app/json/input_sick.json"),
      json_nwd_list = rjson::fromJSON(file = "app/json/input_nwd.json"),
      json_CCE_list = rjson::fromJSON(file = "app/json/input_CCE.json"),
      json_business_to_bill_list = rjson::fromJSON(file = "app/json/input_business_to_bill.json"),
      json_consultant_account_list = rjson::fromJSON(file = "app/json/input_consultant_account.json"),
      json_consultant_business_list = rjson::fromJSON(file = "app/json/input_consultant_business.json")
    )

    observeEvent(input$reload,
      {
        #
        # read .json
        #
        rv_json_lists$json_main_list <- rjson::fromJSON(file = "app/json/main.json")
        rv_json_lists$json_business_to_bill_list <- rjson::fromJSON(file = "app/json/input_business_to_bill.json")
        rv_json_lists$json_consultant_account_list <- rjson::fromJSON(file = "app/json/input_consultant_account.json")
        rv_json_lists$json_consultant_business_list <- rjson::fromJSON(file = "app/json/input_consultant_business.json")
        rv_json_lists$json_salary_list <- rjson::fromJSON(file = "app/json/input_salary.json")
        rv_json_lists$json_period_list <- rjson::fromJSON(file = "app/json/input_period.json")
        rv_json_lists$json_benefits_list <- rjson::fromJSON(file = "app/json/input_benefits.json")
        rv_json_lists$json_local_list <- rjson::fromJSON(file = "app/json/input_local.json")
        rv_json_lists$json_sick_list <- rjson::fromJSON(file = "app/json/input_sick.json")
        rv_json_lists$json_nwd_list <- rjson::fromJSON(file = "app/json/input_nwd.json")
        rv_json_lists$json_CCE_list <- rjson::fromJSON(file = "app/json/input_CCE.json")
      },
      ignoreInit = TRUE
    )


    pattern_a <- "([[:lower:]]+)([[:upper:]])([[:alpha:]]+)([[:digit:]]?)"
    pattern_b <- "\\1 \\2\\3 \\4"

    mon_span <- c(31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31)

    observeEvent(input$increaseDate, {
      cdate <- input$invoiceDate
      sdate <- input$start
      edate <- input$end
      cmon <- month(cdate)
      smon <- month(sdate)
      emon <- month(edate)
      updateDateInput(session, "invoiceDate", value = cdate + mon_span[cmon + 2])
      updateDateInput(session, "start", value = sdate + mon_span[smon + 1])
      updateDateInput(session, "end", value = edate + mon_span[emon + 2])
    })

    observeEvent(input$decreaseDate, {
      cdate <- input$invoiceDate
      sdate <- input$start
      edate <- input$end
      cmon <- month(cdate)
      smon <- month(sdate)
      emon <- month(edate)
      updateDateInput(session, "invoiceDate", value = cdate - mon_span[cmon + 1])
      updateDateInput(session, "start", value = sdate - mon_span[cmon])
      updateDateInput(session, "end", value = edate - mon_span[cmon + 1])
    })

    observeEvent(c(input$modify_salary, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_salary_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_salary_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_salary.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_local, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_local_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_local_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_local.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_CCE, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_CCE_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_CCE_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_CCE.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_benefits, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_benefits_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_benefits_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_benefits.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_sick, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_sick_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_sick_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_sick.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_nwd, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_nwd_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_nwd_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_nwd.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_main, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_main_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_main_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/main.json")
      },
      ignoreInit = TRUE
    )


    observeEvent(c(input$modify_period, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_period_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_period_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_period.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_billto, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_business_to_bill_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_business_to_bill_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_business_to_bill.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_account, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_consultant_account_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_consultant_account_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_consultant_account.json")
      },
      ignoreInit = TRUE
    )

    observeEvent(c(input$modify_consultant, input$modify_all),
      {
        input_list <- list()

        input_list <- lapply(names(rv_json_lists$json_consultant_business_list), function(x) {
          input[[x]]
        })

        names(input_list) <- names(rv_json_lists$json_consultant_business_list)
        json_data <- jsonlite::toJSON(x = input_list, pretty = TRUE)
        write(json_data, "app/json/input_consultant_business.json")
      },
      ignoreInit = TRUE
    )

    output$first_well_panel <- renderUI({
      wellPanel(
        h4(code("main.json"), strong("content")),
        splitLayout(
          textInput(
            ns("final_currency"),
            div(
              class = "wrap",
              HTML("<i>Final</i> Currency")
            ),
            rv_json_lists$json_main_list$final_currency
          ),
          dateInput(ns("invoiceDate"), "Invoice Date: ", value = as.Date(rv_json_lists$json_main_list$invoiceDate)),
        ),
        actionButton(ns("modify_main"),
          strong(
            "Save", code("main.json"),
            "required! after changes"
          ),
          style = "white-space: normal;
                   word-wrap: break-word;"
        )
      )
    })

    output$period_well_panel <- renderUI({
      wellPanel(
        h4(code("input_period.json"), strong("content")),
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
            checkboxInput(ns("use_period"), "Show Period", rv_json_lists$json_period_list$use_period),
            dateInput(ns("start"), "Start Date: ", value = as.Date(rv_json_lists$json_period_list$start)),
            dateInput(ns("end"), "End Date: ", value = as.Date(rv_json_lists$json_period_list$end))
          )
        ),
        br(),
        actionButton(ns("modify_period"),
          strong(
            "Save", code("input_period.json"),
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

      wellPanel(
        h4(code("input_salary.json"), strong("content")),
        {
          char_list <- lapply(char_names, function(x) {
            textInput(
              ns(x),
              div(
                class = "wrap",
                sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE)
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
          num_list <- lapply(num_names, function(x) {
            numericInput(
              ns(x),
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
              ns(x),
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
            "Save", code("input_salary.json"),
            "required! after changes"
          ),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })
    output$CCE_box <- renderUI({
      char_names_CCE <- names(which(sapply(rv_json_lists$json_CCE_list, function(x) is.character(x))))
      num_names_CCE <- names(which(sapply(rv_json_lists$json_CCE_list, function(x) is.numeric(x))))
      logic_names_CCE <- names(which(sapply(rv_json_lists$json_CCE_list, function(x) is.logical(x))))

      wellPanel(
        h4(code("input_CCE.json"), strong("content")),
        {
          char_names_CCE_list <- lapply(char_names_CCE, function(x) {
            textInput(
              ns(x),
              gsub("_", "", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
              rv_json_lists$json_CCE_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_names_CCE_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_names_CCE_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_names_CCE_list)) {
                char_names_CCE_list[(middle_idx + 1):length(char_names_CCE_list)]
              }
            )
          )
        },
        {
          num_names_CCE_list <- lapply(num_names_CCE, function(x) {
            numericInput(
              ns(x),
              gsub("_", "", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE)),
              rv_json_lists$json_CCE_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_names_CCE_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_names_CCE_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_names_CCE_list)) {
                num_names_CCE_list[(middle_idx + 1):length(num_names_CCE_list)]
              }
            )
          )
        },
        {
          logic_names_CCE_list <- lapply(logic_names_CCE, function(x) {
            checkboxInput(
              ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_CCE_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_names_CCE_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_names_CCE_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_names_CCE_list)) {
                logic_names_CCE_list[(middle_idx + 1):length(logic_names_CCE_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_CCE"),
          strong("Save", code("input_CCE.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$benefits_box <- renderUI({
      char_names_benefits <- names(which(sapply(rv_json_lists$json_benefits_list, function(x) is.character(x))))
      num_names_benefits <- names(which(sapply(rv_json_lists$json_benefits_list, function(x) is.numeric(x))))
      logic_names_benefits <- names(which(sapply(rv_json_lists$json_benefits_list, function(x) is.logical(x))))

      wellPanel(
        h4(code("input_benefits.json"), strong("content")),
        {
          char_names_benefits_list <- lapply(char_names_benefits, function(x) {
            textInput(
              ns(x),
              gsub("_", "", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
              rv_json_lists$json_benefits_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_names_benefits_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_names_benefits_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_names_benefits_list)) {
                char_names_benefits_list[(middle_idx + 1):length(char_names_benefits_list)]
              }
            )
          )
        },
        {
          num_names_benefits_list <- lapply(num_names_benefits, function(x) {
            numericInput(
              ns(x),
              gsub("_", "", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE)),
              rv_json_lists$json_benefits_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_names_benefits_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_names_benefits_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_names_benefits_list)) {
                num_names_benefits_list[(middle_idx + 1):length(num_names_benefits_list)]
              }
            )
          )
        },
        {
          logic_names_benefits_list <- lapply(logic_names_benefits, function(x) {
            checkboxInput(
              ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_benefits_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_names_benefits_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_names_benefits_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_names_benefits_list)) {
                logic_names_benefits_list[(middle_idx + 1):length(logic_names_benefits_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_benefits"),
          strong("Save", code("input_benefits.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$local_box <- renderUI({
      char_names_local <- names(which(sapply(rv_json_lists$json_local_list, function(x) is.character(x))))
      num_names_local <- names(which(sapply(rv_json_lists$json_local_list, function(x) is.numeric(x))))
      logic_names_local <- names(which(sapply(rv_json_lists$json_local_list, function(x) is.logical(x))))

      wellPanel(
        h4(code("input_local.json"), strong("content")),
        {
          char_names_local_list <- lapply(char_names_local, function(x) {
            textInput(
              ns(x),
              gsub("_", "", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
              rv_json_lists$json_local_list[[x]]
            )
          })
          middle_idx <- ceiling(length(char_names_local_list) / 2)
          div(
            class = "two_column_grid",
            div(
              char_names_local_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(char_names_local_list)) {
                char_names_local_list[(middle_idx + 1):length(char_names_local_list)]
              }
            )
          )
        },
        {
          num_names_local_list <- lapply(num_names_local, function(x) {
            numericInput(
              ns(x),
              gsub("_", "", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE)),
              rv_json_lists$json_local_list[[x]]
            )
          })
          middle_idx <- ceiling(length(num_names_local_list) / 2)
          div(
            class = "two_column_grid",
            div(
              num_names_local_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(num_names_local_list)) {
                num_names_local_list[(middle_idx + 1):length(num_names_local_list)]
              }
            )
          )
        },
        {
          logic_names_local_list <- lapply(logic_names_local, function(x) {
            checkboxInput(
              ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_local_list[[x]]
            )
          })
          middle_idx <- ceiling(length(logic_names_local_list) / 2)
          div(
            class = "two_column_grid",
            div(
              logic_names_local_list[1:middle_idx]
            ),
            div(
              if ((middle_idx + 1) <= length(logic_names_local_list)) {
                logic_names_local_list[(middle_idx + 1):length(logic_names_local_list)]
              }
            )
          )
        },
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_local"),
          strong("Save", code("input_local.json"), "required! after changes"),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$sick_box <- renderUI({
      num_sick <- names(which(sapply(rv_json_lists$json_sick_list, function(x) is.numeric(x))))
      logic_sick <- names(which(sapply(rv_json_lists$json_sick_list, function(x) is.logical(x))))

      num_nwd <- names(which(sapply(rv_json_lists$json_nwd_list, function(x) is.numeric(x))))
      logic_nwd <- names(which(sapply(rv_json_lists$json_nwd_list, function(x) is.logical(x))))

      tagList(
        wellPanel(
          h4(code("input_sick.json"), strong("content")),
          lapply(num_sick, function(x) {
            numericInput(
              ns(x), gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
              rv_json_lists$json_sick_list[[x]]
            )
          }),
          lapply(logic_sick, function(x) {
            checkboxInput(
              ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_sick_list[[x]]
            )
          }),
          helpText("this box content must be saved before generating .pdf"),
          actionButton(ns("modify_sick"),
            strong(
              "Save", code("input_sick.json"),
              "required! after changes"
            ),
            style = "white-space: normal;
                           word-wrap: break-word;"
          )
        ),
        wellPanel(
          h4(code("input_nwd.json"), strong("content")),
          lapply(num_nwd, function(x) {
            numericInput(
              ns(x), gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE),
              rv_json_lists$json_nwd_list[[x]]
            )
          }),
          lapply(logic_nwd, function(x) {
            checkboxInput(
              ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              rv_json_lists$json_nwd_list[[x]]
            )
          }),
          helpText("this box content must be saved before generating .pdf"),
          actionButton(ns("modify_nwd"),
            strong(
              "Save", code("input_nwd.json"),
              "required! after changes"
            ),
            style = "white-space: normal;
                           word-wrap: break-word;"
          )
        )
      )
    })

    output$bank_box <- renderUI({
      wellPanel(
        h4(code("input_business_to_bill.json"), strong("content")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_billto"),
          strong("Save", code("input_business_to_bill.json"), "required! after changes"),
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
        h4(code("input_consultant_account.json"), strong("content")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_account"),
          strong("Save", code("input_consultant_account.json"), "required! after changes"),
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
        h4(code("input_consultant_business.json"), strong("content")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton(ns("modify_consultant"),
          strong("Save", code("input_consultant_business.json"), "required! after changes"),
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

        params <- reactiveValuesToList(input)

        pattern <- paste0(names(rv_json_lists$json_business_to_bill_list), collapse = "|")
        pattern <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern)
        pattern2 <- "modify_period|modify_nwd|modify_consultant|modify_account|modify_benefits|modify_CCE|modify_local|modify_salary|modify_billto|increaseDate|decreaseDate|modify_sick|modify_all|modify_main|reload"
        pattern3 <- paste0(names(rv_json_lists$json_salary_list), collapse = "|")
        pattern3 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern3)
        pattern4 <- paste0(names(rv_json_lists$json_local_list), collapse = "|")
        pattern4 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern4)
        pattern5 <- paste0(names(rv_json_lists$json_sick_list), collapse = "|")
        pattern5 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern5)
        pattern6 <- paste0(names(rv_json_lists$json_main_list), collapse = "|")
        pattern6 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern6)
        pattern7 <- paste0(names(rv_json_lists$json_nwd_list), collapse = "|")
        pattern7 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern7)
        pattern8 <- paste0(names(rv_json_lists$json_CCE_list), collapse = "|")
        pattern8 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern8)
        pattern9 <- paste0(names(rv_json_lists$json_benefits_list), collapse = "|")
        pattern9 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern9)
        pattern10 <- paste0(names(rv_json_lists$json_consultant_account_list), collapse = "|")
        pattern10 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern10)
        pattern11 <- paste0(names(rv_json_lists$json_consultant_business_list), collapse = "|")
        pattern11 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern11)
        pattern12 <- paste0(names(rv_json_lists$json_period_list), collapse = "|")
        pattern12 <- gsub("\\((.*?)\\)", "\\\\(\\1\\\\)", pattern12)

        params <- params[-grep(pattern, names(params))]
        params <- params[-grep(pattern2, names(params))]
        params <- params[-grep(pattern3, names(params))]
        params <- params[-grep(pattern4, names(params))]
        params <- params[-grep(pattern5, names(params))]
        params <- params[-grep(pattern6, names(params))]
        params <- params[-grep(pattern7, names(params))]
        params <- params[-grep(pattern8, names(params))]
        params <- params[-grep(pattern9, names(params))]
        params <- params[-grep(pattern10, names(params))]
        params <- params[-grep(pattern11, names(params))]
        params <- params[-grep(pattern12, names(params))]

        rmarkdown::render(temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
