box::use(
  shiny[...],
  shinyAce[aceEditor, updateAceEditor]
)

box::use(
  .. / logic / save_files[...],
  .. / modules / upload_rmd
)
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rmd_ui"))
}

server <- function(id, file_reac, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rmd_upload_var <- upload_rmd$server("rmd_upload_ns")
    rmd_ready_reac <- reactiveVal(TRUE)

    output$rmd_ui <- renderUI({
      tagList(
        column(
          7,
          aceEditor(
            outputId = ns("ace"),
            selectionId = "selection",
            mode = "rmd",
            placeholder = ".Rmd not loaded",
            value = paste0(readLines(file.path(temp_folder_session(), "invoice.Rmd"))),
            autoScrollEditorIntoView = TRUE,
            minLines = 20,
            maxLines = 60,
          )
        ),
        column(
          3,
          tagList(
            p("This is the R-markdown file", code(".Rmd")),
            p("that reads the", code(".json"), "files to render the Invoice"),
            br(),
            helpText("To reset changes, go to", em("Main"), "tab"),
            br(),
            wellPanel(
              helpText("This file cannot be downloaded in the", em("Main"), "tab"),
              downloadButton(
                ns("save"),
                strong(
                  "Save and Download", code("invoice.Rmd")
                )
              )
            ),
            upload_rmd$ui(ns("rmd_upload_ns"))
          )
        )
      )
    })

    observeEvent(c(rmd_ready_reac(), file_reac()), ignoreInit = TRUE, {
      updateAceEditor(session, "ace",
        value = paste0(
          readLines(file.path(temp_folder_session(), "invoice.Rmd")),
          collapse = "\n"
        )
      )
    })

    output$save <- downloadHandler(
      filename = function() {
        "invoice.Rmd"
      },
      content = function(file) {
        file_name <- "invoice.Rmd"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        folders <- c(folder, temp_folder_session())

        ace_save(input, "ace", folders, file_name, useNS = FALSE)

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "Rmd"
    )
    observeEvent(rmd_upload_var(),
      {
        req(rmd_upload_var())
        input_file <- rmd_upload_var()
        lapply(seq_along(input_file$name), function(x) {
          file.copy(input_file$datapath[x], file.path(temp_folder_session(), input_file$name[x]), overwrite = TRUE)
        })
        rmd_ready_reac(!rmd_ready_reac())
      },
      ignoreInit = TRUE
    )
    outputOptions(output, "rmd_ui", suspendWhenHidden = FALSE)
  })
}
