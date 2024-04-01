box::use(
  shiny[...],
  shinyAce[aceEditor, updateAceEditor]
)

box::use(
  .. / logic / save_files[...]
)
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ace_ui"))
}

server <- function(id, file_reac, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ace_ui <- renderUI({
      tagList(
        column(
          5,
          aceEditor(
            outputId = ns("ace"),
            selectionId = "selection",
            mode = "json",
            placeholder = ".json not loaded",
            value = paste0(readLines(file.path(temp_folder_session(), "json/field_names.json"), warn = FALSE)),
            autoScrollEditorIntoView = TRUE,
            minLines = 20,
            maxLines = 60
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
    })

    observeEvent(file_reac(), {
      updateAceEditor(session, "ace",
        value = paste0(readLines(file.path(temp_folder_session(), "json/field_names.json"), warn = FALSE),
          collapse = "\n"
        )
      )
    })

    output$save <- downloadHandler(
      filename = function() {
        "field_names.json"
      },
      content = function(file) {
        file_name <- "field_names.json"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        folders <- c(folder, file.path(temp_folder_session(), "json"))

        is_valid <- ace_save(input, "ace", folders, file_name, useNS = FALSE)
        if (!is_valid) {
          folder <- file.path(temp_folder_session(), "json")
        }

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )
    outputOptions(output, "ace_ui", suspendWhenHidden = FALSE)
  })
}
