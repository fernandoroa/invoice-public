box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / logic / json_save[...],
)


ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("business_to_bill_box"))
}

server <- function(id, rv_json_lists__json_business_to_bill_list, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$business_to_bill_box <- renderUI({
      wellPanel(
        h4(strong("Bill To:")),
        br(),
        {
          bill_to_fields <- rv_json_lists__json_business_to_bill_list %>%
            discard(names(.) %in% "file_identifier")
          lapply(seq_along(bill_to_fields), function(x) {
            div(
              class = "form-group-container",
              textInput(ns(names(bill_to_fields[x])),
                "",
                value = bill_to_fields[[x]]
              )
            )
          })
        },
        br(),
        helpText("Go to Main tab to save all"),
        downloadButton(ns("modify_billto"),
          strong("Save and Download", code("business_to_bill.json")),
          style = "white-space: normal;
          word-wrap: break-word;"
        )
      )
    })

    output$modify_billto <- downloadHandler(
      filename = function() {
        "business_to_bill.json"
      },
      content = function(file) {
        file_name <- "business_to_bill.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_json_lists__json_business_to_bill_list,
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), {
      bill_to_fields <- rv_json_lists__json_business_to_bill_list %>% discard(names(.) %in% "file_identifier")
      lapply(seq_along(bill_to_fields), function(x) {
        updateTextInput(session,
          names(bill_to_fields[x]),
          value = bill_to_fields[[x]]
        )
      })
    })

    outputOptions(output, "business_to_bill_box", suspendWhenHidden = FALSE)
  })
}
