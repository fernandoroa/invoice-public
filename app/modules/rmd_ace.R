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
  tagList(
    column(
      7,
      aceEditor(
        outputId = ns("ace"),
        selectionId = "selection",
        mode = "rmd",
        placeholder = ".Rmd not loaded",
        value = paste0(readLines("app/invoice.Rmd")),
        autoScrollEditorIntoView = TRUE,
        minLines = 20,
        maxLines = 60,
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
}

server <- function(id, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rmd_upload_var <- upload_rmd$server("rmd_upload_ns")
    rmd_ready_reac <- reactiveVal(TRUE)

    observeEvent(c(rmd_ready_reac(), file_reac()), ignoreInit = TRUE, {
      updateAceEditor(session, "ace", value = paste0(readLines("app/invoice.Rmd"), collapse = "\n"))
    })

    output$save <- downloadHandler(
      filename = function() {
        "invoice.Rmd"
      },
      content = function(file) {
        file_name <- "invoice.Rmd"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        folders <- c(folder, "app")

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
          file.copy(input_file$datapath[x], file.path(getwd(), "app", input_file$name[x]), overwrite = TRUE)
        })
        rmd_ready_reac(!rmd_ready_reac())
      },
      ignoreInit = TRUE
    )
  })
}
