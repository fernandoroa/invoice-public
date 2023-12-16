box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard, keep]
)

box::use(
  .. / .. / utils / constants[...],
  .. / .. / logic / save_files[...],
  .. / .. / logic / input_fun[...],
  . / grouped_element
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("grouped_box"))
}

server <- function(id, rv_jsons, sublist, file_reac, exchange_rate, temp_folder_session, inputs, oneliner_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_input_to_remove <- reactiveVal()
    rv_add_signal <- reactiveVal(TRUE)

    output$grouped_box <- renderUI({
      file_reac()
      grouped_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")

      grouped_list_root <- grouped_list %>% keep(names(.) %in% root_names)

      char_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.character(x))))
      num_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.numeric(x))))
      logic_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names_grouped, value = TRUE)
      num_names_currency <- grep("currency", num_names_grouped, value = TRUE)

      char_names_not_currency <- grep("currency", char_names_grouped, value = TRUE, invert = TRUE)

      grouped_sublists <- grouped_list %>% discard(names(.) %in% root_names)
      grouped_list_names <- names(grouped_sublists)

      tagList(
        wellPanel({
          tagList({
            char_names_currency_list <- create_text_input_wrap(char_names_currency, grouped_list, ns)

            num_names_currency_list <- create_numeric_input_wrap(num_names_currency, grouped_list, ns)

            char_names_grouped_list <- create_text_input_nc_simple(char_names_not_currency, grouped_list, ns)

            logic_names_grouped_list <- create_check_box_input(logic_names_grouped, grouped_list, ns)

            div(
              class = "five_column_grid",
              h4(strong("Grouped Costs")),
              div(
                class = "go-bottom",
                char_names_grouped_list
              ),
              div(
                class = "go-bottom",
                char_names_currency_list
              ),
              div(
                class = "go-bottom",
                num_names_currency_list
              ),
              div(
                class = "go-bottom",
                logic_names_grouped_list
              )
            )
          })
        }),
        wellPanel(
          tagList(
            lapply(seq_along(grouped_list_names), function(idx) {
              name <- grouped_list_names[[idx]]
              grouped_element$ui(ns(name))
            })
          )
        ),
        div(
          class = "fit-content",
          wellPanel(
            div(
              div(
                class = "add-button-container",
                br(),
                actionButton(ns("save_and_add_element"), "Save All Changes, then add row")
              ),
              br(),
              helpText("Go to Main tab to save all .json files"),
              downloadButton(ns("save_download_grouped"),
                strong("Save and Download", code("grouped_costs.json")),
                style = "white-space: normal;
                           word-wrap: break-word;"
              )
            )
          )
        )
      )
    })

    observeEvent(file_reac(), {
      grouped_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")

      grouped_sublists <- grouped_list %>% discard(names(.) %in% root_names)

      grouped_list_names <- names(grouped_sublists)

      to_remove <- lapply(
        seq_along(grouped_list_names), function(idx) {
          name <- grouped_list_names[idx]
          grouped_sublists_this_name <- grouped_sublists[[name]]

          char_names_grouped_this_name <- names(which(sapply(grouped_sublists_this_name, function(x) is.character(x))))
          num_names_grouped_this_name <- names(which(sapply(grouped_sublists_this_name, function(x) is.numeric(x))))

          input_to_remove <- grouped_element$server(
            name, char_names_grouped_this_name, num_names_grouped_this_name,
            grouped_sublists_this_name, idx
          )
        }
      )
      rv_input_to_remove(to_remove)
    })

    observeEvent(input$save_and_add_element, ignoreInit = TRUE, {
      to_remove <- c()
      for (e in rv_input_to_remove()) {
        to_remove <- c(to_remove, e())
      }
      file_name <- "grouped_costs.json"

      save_all(
        inputs,
        file.path(temp_folder_session(), "json"),
        rv_jsons, oneliner_vars$to_remove(), rv_input_to_remove()
      )

      rv_add_signal(!rv_add_signal())
    })

    output$save_download_grouped <- downloadHandler(
      filename = function() {
        "grouped_costs.json"
      },
      content = function(file) {
        file_name <- "grouped_costs.json"
        folder <- gsub("file", "folder_", tempfile(tmpdir = file.path(temp_folder_session(), "tmp_dir")))
        dir.create(folder, recursive = TRUE)

        to_remove <- c()
        for (e in rv_input_to_remove()) {
          to_remove <- c(to_remove, e())
        }

        nested_and_root_save(
          input,
          nested_list = rv_jsons[[sublist]],
          folders = c(folder, file.path(temp_folder_session(), "json")),
          file_name,
          to_remove = to_remove
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(exchange_rate(), ignoreInit = TRUE, {
      updateNumericInput(
        session,
        "currency_exchange_to_Final_Currency",
        value = exchange_rate() |> as.numeric()
      )
    })

    outputOptions(output, "grouped_box", suspendWhenHidden = FALSE)

    return(
      list(
        to_remove = reactive(rv_input_to_remove()),
        add_grouped_element = reactive({
          req(input$save_and_add_element)
          rv_add_signal()
        })
      )
    )
  })
}
