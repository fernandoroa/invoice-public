box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / utils / constants[...],
  .. / logic / save_files[...],
  .. / logic / input_fun[...],
  .. / logic / update_fun[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("consultant_account_box"))
}

server <- function(id, rv_sublist, file_reac, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$save_download_account <- downloadHandler(
      filename = function() {
        "consultant_account.json"
      },
      content = function(file) {
        file_name <- "consultant_account.json"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)
        if (is.null(rv_sublist$consultant_account_list$show)) {
          rv_sublist$consultant_account_list$show <- TRUE
        }
        plain_json_save(
          input,
          plain_list = rv_sublist$consultant_account_list,
          folders = c(folder, file.path(temp_folder_session(), "json")),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    output$consultant_account_box <- renderUI({
      consultant_account_list <- rv_sublist$consultant_account_list %>% discard(names(.) %in% "file_identifier")
      if (is.null(consultant_account_list$show)) {
        consultant_account_list$show <- TRUE
      }
      char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.character(x))))
      logic_char_consultant_account <- names(which(sapply(consultant_account_list, function(x) is.logical(x))))

      wellPanel(
        h4(strong("Consultant Account")),
        {
          logic_inputs <- create_check_box_input(logic_char_consultant_account, consultant_account_list, ns)

          logic_inputs_len <- length(logic_inputs)
          half <- ceiling(logic_inputs_len / 2)
          div(
            class = "two_column_grid_gap",
            div(logic_inputs[1:half]),
            div(logic_inputs[(half + 1):logic_inputs_len]),
          )
        },
        {
          char_inputs <- create_text_input_with_patterns(char_consultant_account, consultant_account_list, ns)

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

      update_checkbox_list(session, logic_char_consultant_account, consultant_account_list)

      update_text_input_list(session, char_consultant_account, consultant_account_list)
    })

    outputOptions(output, "consultant_account_box", suspendWhenHidden = FALSE)
  })
}
