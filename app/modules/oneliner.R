box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard]
)

box::use(
  .. / utils / constants[...],
  .. / logic / save_files[...],
  .. / modules / single_oneliner
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("oneliners_box"))
}

server <- function(id, rv_jsons, sublist, file_reac, exchange_rates, temp_folder_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_input_to_remove <- reactiveVal()

    output$oneliners_box <- renderUI({
      file_reac()
      oneliners_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")
      oneliners_list_names <- names(oneliners_list)

      wellPanel(
        tagList(
          lapply(seq_along(oneliners_list_names), function(idx) {
            name <- oneliners_list_names[[idx]]
            single_oneliner$ui(ns(name))
          })
        ),
        br(),
        helpText("Go to Main tab to save all .json files"),
        downloadButton(ns("save_download_oneliners"),
          strong("Save and Download", code("oneliner_costs.json")),
          style = "white-space: normal;
                           word-wrap: break-word;"
        )
      )
    })

    observeEvent(file_reac(), {
      file_reac()
      oneliners_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")
      oneliners_list_names <- names(oneliners_list)

      to_remove <- lapply(seq_along(oneliners_list_names), function(idx) {
        name <- oneliners_list_names[[idx]]
        oneliners_list_active <- oneliners_list[[name]]

        char_names_oneliners_this_name <- names(which(sapply(oneliners_list_active, function(x) is.character(x))))
        num_names_oneliners_this_name <- names(which(sapply(oneliners_list_active, function(x) is.numeric(x))))
        logic_names_oneliners_this_name <- names(which(sapply(oneliners_list_active, function(x) is.logical(x))))

        char_names_currency_this_name <- grep("currency", char_names_oneliners_this_name, value = TRUE)
        num_names_currency_this_name <- grep("currency", num_names_oneliners_this_name, value = TRUE)

        char_names_not_currency_this_name <- grep("currency", char_names_oneliners_this_name, value = TRUE, invert = TRUE)
        num_names_not_currency_this_name <- grep("currency", num_names_oneliners_this_name, value = TRUE, invert = TRUE)

        input_to_remove <- single_oneliner$server(
          name, char_names_currency_this_name, num_names_currency_this_name,
          char_names_not_currency_this_name, num_names_not_currency_this_name,
          logic_names_oneliners_this_name, oneliners_list_active, idx
        )
      })
      rv_input_to_remove(to_remove)
    })

    output$save_download_oneliners <- downloadHandler(
      filename = function() {
        "oneliner_costs.json"
      },
      content = function(file) {
        file_name <- "oneliner_costs.json"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        to_remove <- c()
        for (e in rv_input_to_remove()) {
          to_remove <- c(to_remove, e())
        }
        nested_json_save(input,
          nested_list = rv_jsons[[sublist]],
          prefix = "",
          folders = c(folder, file.path(temp_folder_session(), "json")),
          file_name,
          to_remove = to_remove
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(exchange_rates(), ignoreInit = TRUE, {
      lapply(seq_along(exchange_rates()), function(idx) {
        updateNumericInput(
          session,
          names(exchange_rates()[idx]),
          value = exchange_rates()[[idx]]
        )
      })
    })

    outputOptions(output, "oneliners_box", suspendWhenHidden = FALSE)
    return(reactive(rv_input_to_remove()))
  })
}
