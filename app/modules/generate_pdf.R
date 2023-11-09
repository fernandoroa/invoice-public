box::use(
  shiny[...],
  rmarkdown[render],
)

box::use(
  .. / logic / save_files[...],
)

ui <- function(id) {
  ns <- NS(id)
  div(
    class = "generate_buttons",
    br(),
    helpText("Save changes and generate .pdf"),
    div(
      downloadButton(ns("report"), "Render Document")
    )
  )
}

server <- function(id, rv_json_lists, inputs, oneliner_to_remove, grouped_to_remove, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$report <- downloadHandler(
      filename = function() {
        "invoice.pdf"
      },
      content = function(file) {
        save_all(
          inputs,
          file.path(temp_folder_session(), "json"),
          rv_json_lists, oneliner_to_remove(), grouped_to_remove()
        )
        ace_save(inputs, "ace", folders = temp_folder_session(), file_name = "invoice.Rmd", useNS = TRUE, namespace = "rmd_ace_ns")

        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)
        temp_report <- file.path(folder, "inv_md_dont_modify.Rmd")
        file.copy(file.path(temp_folder_session(), "invoice.Rmd"), temp_report, overwrite = TRUE)
        all_params <- reactiveValuesToList(inputs)
        params <- list(invoiceNumber = all_params$invoiceNumber, lang = all_params$lang, app_path = temp_folder_session())

        render(temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      },
      contentType = "pdf"
    )
  })
}
