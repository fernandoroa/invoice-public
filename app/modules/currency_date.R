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

server <- function(id, rv_json_lists, input_maincurrency) {
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
              value = as.Date(rv_json_lists$json_final_currency_list$exchangeDate)
            ),
            dateInput(ns("invoiceDate"), "Invoice Date: ", value = as.Date(rv_json_lists$json_final_currency_list$invoiceDate))
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
          plain_list = rv_json_lists$json_final_currency_list,
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

    currency_date_rv <- reactiveValues()

    observeEvent(input$get_exchanges, {
      if (input$final_currency != input_maincurrency()) {
        showModal(modalDialog(
          title = "Getting exchange rates",
          "Please wait!"
        ))
        date <- as.character(input$exchangeDate)
        while (TRUE) {
          exchange_df <- try(get_exchange_rates(input$final_currency, input_maincurrency(), date), silent = TRUE)
          date <- as.character(as.Date(date) - 1)
          if (!inherits(exchange_df, "try-error")) break
        }
        exchange_salary <- signif(exchange_df$Adjusted_Sy, 5)
        currency_date_rv$exchange_salary <- exchange_salary
        removeModal()
      }

      if (FALSE) {
        inputs <- reactiveValuesToList(input)

        oneliners_currency_name_strings <- grep("oneliners.*currency", names(inputs), value = TRUE)
        grouped_currency_name_strings <- grep("grouped.*currency", names(inputs), value = TRUE)

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
      }
    })

    outputOptions(output, "currency_date", suspendWhenHidden = FALSE)

    return(list(
      exchange_salary = reactive({
        currency_date_rv$exchange_salary
      })
    ))
  })
}
