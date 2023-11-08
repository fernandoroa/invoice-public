box::use(
  shiny[...],
  utils[zip],
)

box::use(
  .. / logic / json_save[...],
)

ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    div(
      class = "generate_buttons",
      h4("Download Source files as .zip"),
      helpText("with current changes"),
      span(code(".zip"), "contains", code(".json"), "files"),
      downloadButton(ns("downloadPresets"), "Download .zip", class = "btn-success")
    )
  )
}

server <- function(id, rv_json_lists, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$downloadPresets <- downloadHandler(
      filename = function() {
        "json.zip"
      },
      content = function(file) {
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        save_all(inputs, c(folder, "app/json"), rv_json_lists)

        file.copy("app/json/field_names.json", folder)

        zip_path <- file.path(folder, "json.zip")
        files_to_zip <- dir(folder, full.names = TRUE)
        zip(zipfile = zip_path, files = files_to_zip, flags = "-0jrm")
        file.copy(zip_path, file)
      },
      contentType = "zip"
    )
  })
}
