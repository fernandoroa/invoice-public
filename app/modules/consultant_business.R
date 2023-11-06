box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / logic / json_save[...],
  .. / utils / constants[...],
)


ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("consultant_business_box"))
}

server <- function(id, rv_json_lists__json_consultant_business_list, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$consultant_business_box <- renderUI({
      wellPanel(
        h4(strong("Consultant details")),
        br(),
        {
          consultant_business_list <- rv_json_lists__json_consultant_business_list %>%
            discard(names(.) %in% "file_identifier")
          lapply(seq_along(consultant_business_list), function(x) {
            textInput(ns(names(consultant_business_list[x])),
              gsub("_", " ", gsub(pattern_a, pattern_b, names(consultant_business_list[x]))),
              value = consultant_business_list[[x]]
            )
          })
        },
        br(),
        helpText("Go to Main tab to save all"),
        downloadButton(ns("modify_consultant"),
          strong("Save and Download", code("consultant_contact.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$modify_consultant <- downloadHandler(
      filename = function() {
        "consultant_contact.json"
      },
      content = function(file) {
        file_name <- "consultant_contact.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_json_lists__json_consultant_business_list,
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), {
      consultant_business_list <- rv_json_lists__json_consultant_business_list %>%
        discard(names(.) %in% "file_identifier")
      lapply(seq_along(consultant_business_list), function(x) {
        updateTextInput(
          session,
          names(consultant_business_list[x]),
          value = consultant_business_list[[x]]
        )
      })
    })
    outputOptions(output, "consultant_business_box", suspendWhenHidden = FALSE)
  })
}
