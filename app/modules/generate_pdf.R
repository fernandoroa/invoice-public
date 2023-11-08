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

server <- function(id, rv_json_lists, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$report <- downloadHandler(
      filename = function() {
        "invoice.pdf"
      },
      content = function(file) {
        save_all(inputs, "app/json", rv_json_lists)
        ace_save(inputs, "ace", folders = "app", file_name = "invoice.Rmd", useNS = TRUE, namespace = "rmd_ace_ns")

        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)
        temp_report <- file.path(folder, "inv_md_dont_modify.Rmd")
        file.copy("app/invoice.Rmd", temp_report, overwrite = TRUE)
        app_path <- file.path(getwd(), "app")
        all_params <- reactiveValuesToList(inputs)
        params <- list(invoiceNumber = all_params$invoiceNumber, lang = all_params$lang, app_path = app_path)

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
