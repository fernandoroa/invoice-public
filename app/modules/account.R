box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / utils / constants[...],
  .. / logic / save_files[...]
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("consultant_account_box"))
}

server <- function(id, rv_sublist, file_reac) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$save_download_account <- downloadHandler(
      filename = function() {
        "consultant_account.json"
      },
      content = function(file) {
        file_name <- "consultant_account.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        plain_json_save(
          input,
          plain_list = rv_sublist$consultant_account_list,
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    output$consultant_account_box <- renderUI({
      consultant_account_list <- rv_sublist$consultant_account_list %>% discard(names(.) %in% "file_identifier")
      char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.character(x))))
      logic_char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.logical(x))))

      wellPanel(
        h4(strong("Consultant Account")),
        lapply(logic_char_consultant_account, function(x) {
          checkboxInput(
            ns(x),
            gsub("_", " ", gsub(pattern_a, pattern_b, x)),
            consultant_account_list[[x]]
          )
        }),
        {
          char_inputs <- lapply(char_consultant_account, function(x) {
            textInput(ns(x),
              gsub("_", " ", gsub(pattern_a, pattern_b, x)),
              value = consultant_account_list[[x]]
            )
          })
          char_inputs_len <- length(char_inputs)
          half <- ceiling(char_inputs_len / 2)
          div(
            class = "two_column_grid_gap",
            div(char_inputs[1:half]),
            div(char_inputs[(half + 1):char_inputs_len]),
          )
        },
        helpText("Go to Main tab to save all .json files"),
        downloadButton(ns("save_download_account"),
          strong("Save and Download", code("consultant_account.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })


    observeEvent(file_reac(), {
      consultant_account_list <- rv_sublist$consultant_account_list %>% discard(names(.) %in% "file_identifier")
      char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.character(x))))
      logic_char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.logical(x))))

      lapply(logic_char_consultant_account, function(x) {
        updateCheckboxInput(
          session,
          x,
          value = consultant_account_list[[x]]
        )
      })

      lapply(char_consultant_account, function(x) {
        updateTextInput(
          session,
          x,
          value = consultant_account_list[[x]]
        )
      })
    })

    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)
  })
}
