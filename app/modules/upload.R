box::use(
  shiny[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(
    ns("upload_ui")
  )
}

server <- function(id, input_zip_upload, file_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    file_reac <- reactiveVal()

    output$upload_ui <- renderUI({
      input_zip_upload()

      wellPanel(
        div(
          class = "generate_buttons",
          h4(
            paste("Upload", file_type, "file(s)")
          ),
          fileInput(ns("file_input_id"),
            "",
            multiple = TRUE,
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
    return(reactive(file_reac()))
  })
}
