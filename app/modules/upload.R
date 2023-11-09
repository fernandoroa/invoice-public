box::use(
  shiny[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(
    ns("upload_ui")
  )
}

server <- function(id, other_input_upload, file_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    file_reac <- reactiveVal()

    output$upload_ui <- renderUI({
      other_input_upload()

      if (file_type == ".json") {
        title <- h4(
          paste("Upload", file_type, "file(s)")
        )
        multiple <- TRUE
      } else {
        title <- tagList(
          h4(paste("Upload", file_type)),
          span("with", code(".json"), "files")
        )
        multiple <- FALSE
      }

      wellPanel(
        div(
          class = "generate_buttons",
          title,
          fileInput(ns("file_input_id"),
            "",
            multiple = multiple,
            accept = c(
              file_type
            )
          )
        )
      )
    })

    observeEvent(input$file_input_id,
      {
        req(input$file_input_id)
        file_reac(input$file_input_id)
      },
      ignoreInit = TRUE
    )
    outputOptions(output, "upload_ui", suspendWhenHidden = FALSE)
    return(reactive(file_reac()))
  })
}
