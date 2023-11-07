box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / logic / json_save[...],
  .. / utils / constants[...],
)


ui <- function(id, output_id) {
  ns <- NS(id)
  uiOutput(ns(output_id))
}

server <- function(id, rv_sublist, file_reac, useLabel = TRUE, basename, output_id, box_title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output[[output_id]] <- renderUI({
      wellPanel(
        h4(strong(box_title)),
        br(),
        {
          filtered_sublist <- rv_sublist %>%
            discard(names(.) %in% "file_identifier")
          className <- ifelse(useLabel, "", "form-group-container")
          lapply(seq_along(filtered_sublist), function(x) {
            div(
              class = className,
              textInput(ns(names(filtered_sublist[x])),
                {
                  if (useLabel) {
                    gsub("_", " ", gsub(pattern_a, pattern_b, names(filtered_sublist[x])))
                  } else {
                    ""
                  }
                },
                value = filtered_sublist[[x]]
              )
            )
          })
        },
        br(),
        helpText("Go to Main tab to save all"),
        downloadButton(ns("save_download"),
          strong("Save and Download", code(basename)),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    output$save_download <- downloadHandler(
      filename = function() {
        basename
      },
      content = function(file) {
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_sublist,
          folders = c(folder, "app/json"),
          basename
        )

        json_path <- file.path(folder, basename)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), {
      filtered_sublist <- rv_sublist %>%
        discard(names(.) %in% "file_identifier")

      lapply(seq_along(filtered_sublist), function(x) {
        updateTextInput(
          session,
          names(filtered_sublist[x]),
          value = filtered_sublist[[x]]
        )
      })
    })
    outputOptions(output, output_id, suspendWhenHidden = FALSE)
  })
}
