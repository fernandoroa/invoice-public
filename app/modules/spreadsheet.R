box::use(
  shiny[...],
  lubridate[...],
)

box::use(
  .. / utils / constants[...],
  .. / logic / list_to_df[...],
  .. / logic / input_fun[...],
  .. / logic / update_fun[...]
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      3,
      uiOutput(ns("save_data.frame_box"))
    )
  )
}


server <- function(id, rv_jsons, oneliner_vars, grouped_vars, temp_folder_session, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$save_data.frame_box <- renderUI({
      div(
        class = "fit-content",
        wellPanel(
          div(
            helpText("Download raw costs and main data in a .csv file"),
            helpText("for checking purposes"),
            downloadButton(ns("save_spreadsheet"),
              strong(
                "Download", code(".csv")
              ),
              style = "white-space: normal;
                           word-wrap: break-word;"
            )
          )
        )
      )
    })

    output$save_spreadsheet <- downloadHandler(
      filename = function() {
        "costs.csv"
      },
      content = function(file) {
        file_name <- "costs.csv"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        save_data.frame(
          inputs, rv_jsons,
          oneliner_vars$to_remove(),
          grouped_vars$to_remove(), folder
        )

        csv_path <- file.path(folder, file_name)
        file.copy(csv_path, file)
      },
      contentType = "csv"
    )
  })
}
