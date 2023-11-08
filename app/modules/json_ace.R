box::use(
  shiny[...],
  shinyAce[aceEditor, updateAceEditor]
)

box::use(
  .. / logic / save_files[...]
)
ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      5,
      aceEditor(
        outputId = ns("ace"),
        selectionId = "selection",
        mode = "json",
        placeholder = ".json not loaded",
        value = paste0(readLines("app/json/field_names.json"))
      )
    ),
    column(
      3,
      tagList(
        p("These labels are used in the invoice", code(".pdf"), "file"),
        p("corresponding to two languages, that you can"),
        p("select in the", em("Main"), "tab"),
        br(),
        helpText("To reset changes, go to", em("Main"), "tab"),
        br(),
        helpText("To upload a file, go to", em("Main"), "tab"),
        br(),
        wellPanel(
          helpText("Go to Main tab to save all .json files"),
          downloadButton(
            ns("save"),
            strong(
              "Save and Download", code("field_names.json")
            ),
          )
        )
      )
    )
  )
}

server <- function(id, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(file_reac(), {
      updateAceEditor(session, "ace", value = paste0(readLines("app/json/field_names.json"), collapse = "\n"))
    })

    output$save <- downloadHandler(
      filename = function() {
        "field_names.json"
      },
      content = function(file) {
        file_name <- "field_names.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        folders <- c(folder, "app/json")

        ace_save(input, "ace", folders, file_name, useNS = FALSE)

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )
  })
}
