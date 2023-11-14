box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / logic / save_files[...],
  .. / utils / constants[...],
  .. / logic / input_fun[...],
  .. / logic / update_fun[...]
)


ui <- function(id, output_id) {
  ns <- NS(id)
  uiOutput(ns(output_id))
}

server <- function(id, rv_jsons, file_reac, useLabel = TRUE, basename, box_title, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output_id <- paste0(basename, "_box")
    sublist <- paste0(basename, "_list")
    filename <- paste0(basename, ".json")
    output[[output_id]] <- renderUI({
      wellPanel(
        h4(strong(box_title)),
        br(),
        {
          filtered_sublist <- rv_jsons[[sublist]] %>%
            discard(names(.) %in% "file_identifier")

          create_text_input_with_patterns_container(filtered_sublist, useLabel, ns)
        },
        br(),
        helpText("Go to Main tab to save all .json files"),
        downloadButton(ns("save_download"),
          strong("Save and Download", code(filename)),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$save_download <- downloadHandler(
      filename = function() {
        filename
      },
      content = function(file) {
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        plain_json_save(
          input,
          plain_list = rv_jsons[[sublist]],
          folders = c(folder, file.path(temp_folder_session(), "json")),
          filename
        )

        json_path <- file.path(folder, filename)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), {
      filtered_sublist <- rv_jsons[[sublist]] %>%
        discard(names(.) %in% "file_identifier")

      update_text_input_list_by_idx(session, filtered_sublist)
    })
    outputOptions(output, output_id, suspendWhenHidden = FALSE)
  })
}
