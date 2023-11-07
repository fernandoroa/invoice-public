box::use(
  shiny[...],
  lubridate[...],
)

box::use(
  .. / logic / exchange[...],
  .. / logic / json_save[...],
  .. / utils / constants[...],
)


ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("currency_date"))
}

server <- function(id, rv_sublist, salary_currency, inputs, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$currency_date <- renderUI({
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
            rv_sublist$final_currency
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
            br(),
            actionButton(ns("increaseDate_Final"), ""),
            span("1 Month"),
            br(),
            actionButton(ns("decreaseDate_Final"), "")
          ),
          tagList(
            dateInput(ns("exchangeDate"),
              div(
                class = "wrap",
                "Currency Exchange Date: "
              ),
              value = as.Date(rv_sublist$exchangeDate)
            ),
            dateInput(ns("invoiceDate"), "Invoice Date: ", value = as.Date(rv_sublist$invoiceDate))
          )
        ),
        downloadButton(ns("button_id"),
          class = "button",
          strong(
            "Save and Download", code("final_currency_inv_date.json")
          ),
          style = "white-space: normal;
                   word-wrap: break-word;"
        )
      )
    })

    output$button_id <- downloadHandler(
      filename = function() {
        "final_currency_inv_date.json"
      },
      content = function(file) {
        file_name <- "final_currency_inv_date.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_sublist,
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )
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

    observeEvent(file_reac(), {
      updateTextInput(
        session,
        "final_currency",
        value = rv_sublist$final_currency
      )
      updateDateInput(
        session,
        "exchangeDate",
        value = as.Date(rv_sublist$exchangeDate)
      )
      updateDateInput(
        session,
        "invoiceDate",
        value = as.Date(rv_sublist$invoiceDate)
      )
    })

    currency_date_rv <- reactiveValues()

    observeEvent(input$get_exchanges, {
      showModal(modalDialog(
        title = "Getting all exchange rates",
        "Shows alert if currency is not found. Please check!"
      ))
      if (toupper(input$final_currency) != toupper(salary_currency())) {
        exchange_df <- try_exchange_rates(input$exchangeDate, input$final_currency, salary_currency())
        if (inherits(exchange_df, "data.frame")) {
          exchange_salary <- signif(exchange_df$Adjusted_Sy, 5)
          currency_date_rv$exchange_salary <- exchange_salary
        } else {
          currency_date_rv$exchange_salary <- NA
          showNotification(paste0("the exchange for ", toupper(salary_currency()), " was not found"))
        }
      }

      inputs_list <- reactiveValuesToList(inputs)
      oneliner_ns <- "oneliner_ns"
      oneliners_currency_name_strings <- grep(paste0(oneliner_ns, ".*currency"), names(inputs_list), value = TRUE)
      grouped_currency_name_strings <- grep("grouped.*currency", names(inputs_list), value = TRUE)

      oneline_currencies_inputs <- inputs_list[which(names(inputs_list) %in% oneliners_currency_name_strings)]
      grouped_currency_inputs <- inputs_list[which(names(inputs_list) %in% grouped_currency_name_strings)]

      oneliners_currencies_list <- oneline_currencies_inputs[sapply(oneline_currencies_inputs, is.character)]
      grouped_currencies_list <- grouped_currency_inputs[sapply(grouped_currency_inputs, is.character)]

      oneliners_currency_exchange_value_list <- oneline_currencies_inputs[sapply(oneline_currencies_inputs, is.numeric)]
      grouped_currency_exchange_value_list <- grouped_currency_inputs[sapply(grouped_currency_inputs, is.numeric)]

      oneliners_currencies_list_names <- names(oneliners_currency_exchange_value_list)
      oneliners_currencies_list_names_no_ns <- sub(paste0("^", oneliner_ns, "-"), "", oneliners_currencies_list_names)
      grouped_currencies_list_names <- names(grouped_currency_exchange_value_list)

      currency_date_rv$exchange_oneliners <- list()
      for (currency_idx in seq_along(oneliners_currencies_list)) {
        currency <- oneliners_currencies_list[currency_idx]
        if (toupper(input$final_currency) != toupper(currency)) {
          exchange_df <- try_exchange_rates(input$exchangeDate, input$final_currency, currency)

          if (inherits(exchange_df, "data.frame")) {
            exchange_oneliner <- signif(exchange_df$Adjusted_Sy, 5)
            currency_date_rv$exchange_oneliners[oneliners_currencies_list_names_no_ns[currency_idx]] <- exchange_oneliner
          } else {
            currency_date_rv$exchange_oneliners <- NA
            showNotification(paste0("the exchange for ", toupper(currency), " was not found"))
          }
        }
      }

      for (currency_idx in seq_along(grouped_currencies_list)) {
        currency <- grouped_currencies_list[currency_idx]
        if (toupper(input$final_currency) != toupper(currency)) {
          exchange_df <- try_exchange_rates(input$exchangeDate, input$final_currency, currency)
          if (inherits(exchange_df, "data.frame")) {
            exchange_grouped <- signif(exchange_df$Adjusted_Sy, 5)
            currency_date_rv$exchange_grouped <- exchange_grouped
          } else {
            currency_date_rv$exchange_grouped <- NA
            showNotification(paste0("the exchange for ", toupper(currency), " was not found"))
          }
        }
      }

      removeModal()
    })

    outputOptions(output, "currency_date", suspendWhenHidden = FALSE)

    return(list(
      exchange_salary = reactive({
        currency_date_rv$exchange_salary
      }),
      exchange_grouped = reactive({
        currency_date_rv$exchange_grouped
      }),
      exchange_oneliners = reactive({
        currency_date_rv$exchange_oneliners
      })
    ))
  })
}
