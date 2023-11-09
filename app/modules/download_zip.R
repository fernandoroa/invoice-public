box::use(
  shiny[...],
  utils[zip],
)

box::use(
  .. / logic / save_files[...],
)

ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    div(
      class = "generate_buttons",
      h4("Download as .zip"),
      helpText("saves current changes"),
      helpText("go to .Rmd tab to save that file"),
      br(),
      div(
        class = "bottom-margin-10",
        span(code(".zip"), "contains", code(".json"), "files")
      ),
      downloadButton(ns("downloadPresets"), "Download .zip", class = "btn-success")
    )
  )
}

server <- function(id, rv_json_lists, inputs, oneliner_to_remove, grouped_to_remove, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$downloadPresets <- downloadHandler(
      filename = function() {
        "json.zip"
      },
      content = function(file) {
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        save_all(
          inputs,
          c(folder, file.path(temp_folder_session(), "json")),
          rv_json_lists, oneliner_to_remove(), grouped_to_remove()
        )

        file.copy(file.path(temp_folder_session(), "json/field_names.json"), folder)

        zip_path <- file.path(folder, "json.zip")
        files_to_zip <- dir(folder, full.names = TRUE)
        zip(zipfile = zip_path, files = files_to_zip, flags = "-0jrm")
        file.copy(zip_path, file)
      },
      contentType = "zip"
    )
  })
}
