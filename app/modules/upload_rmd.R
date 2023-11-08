box::use(
  shiny[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(
    ns("upload_ui")
  )
}

server <- function(id, file_type = ".Rmd") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    file_reac <- reactiveVal()

    output$upload_ui <- renderUI({
      title <- tagList(
        h4(paste("Upload", file_type))
      )
      multiple <- FALSE

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
    return(reactive(file_reac()))
  })
}
