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
  uiOutput(ns("oneliners_box"))
}

server <- function(id, rv_jsons, sublist, file_reac, exchange_rates) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv_oneliners_list_names_remove <- reactiveVal()
    output$oneliners_box <- renderUI({
      oneliners_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")
      oneliners_list_names <- names(oneliners_list)
      rv_oneliners_list_names_remove(paste0(oneliners_list_names, "remove_oneliner"))

      wellPanel(
        tagList(
          lapply(seq_along(oneliners_list_names), function(idx) {
            name <- oneliners_list_names[[idx]]
            num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- list()
            logic_names_oneliners <- num_names_oneliners <- char_names_oneliners <- list()

            char_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.character(x))))
            num_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.numeric(x))))
            logic_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.logical(x))))

            char_names_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE)
            num_names_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE)

            char_names_not_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE, invert = TRUE)
            num_names_not_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE, invert = TRUE)

            char_names_currency_list <- lapply(char_names_currency[[name]], function(x) {
              textInput(
                ns(paste0(name, x)),
                if (idx == 1) {
                  div(
                    class = "wrap",
                    sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
                  )
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            num_names_currency_list <- lapply(num_names_currency[[name]], function(x) {
              numericInput(
                ns(paste0(name, x)),
                if (idx == 1) {
                  div(
                    class = "wrap",
                    gsub("_", " ", x, perl = TRUE)
                  )
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            char_names_oneliners_not_currency_list <- lapply(char_names_not_currency[[name]], function(x) {
              textInput(
                ns(paste0(name, x)),
                if (idx == 1) {
                  gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x))
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            num_names_oneliners_not_currency_list <- lapply(num_names_not_currency[[name]], function(x) {
              numericInput(
                ns(paste0(name, x)),
                if (idx == 1) {
                  gsub("_", " ", gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = TRUE))
                } else {
                  ""
                },
                oneliners_list[[name]][[x]]
              )
            })
            if (idx > 1) {
              drop_button <- div(
                class = "go-bottom",
                actionButton(ns(paste0(name, "remove_oneliner")), "Drop")
              )
            } else {
              drop_button <- div()
            }
            div(
              id = ns(name),
              class = "seven_column_grid",
              div(
                class = "go-bottom",
                h4(strong(name))
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
                char_names_oneliners_not_currency_list
              ),
              div(class = "go-bottom", num_names_oneliners_not_currency_list),
              div(
                class = "go-bottom",
                lapply(logic_names_oneliners[[name]], function(x) {
                  checkboxInput(
                    ns(paste0(name, x)),
                    gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                    oneliners_list[[name]][[x]]
                  )
                })
              ),
              drop_button
            )
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
    rv_input_to_remove <- reactiveVal()
    observeEvent(sapply(rv_oneliners_list_names_remove(), function(x) input[[x]], simplify = FALSE),
      {
        name_input_button_list <- sapply(rv_oneliners_list_names_remove(), function(x) input[[x]], simplify = FALSE)
        for (input_name in names(name_input_button_list)) {
          if (isTruthy(input_name)) {
            if (isTruthy(input[[input_name]])) {
              oneliner_to_remove <- sub("remove_oneliner$", "", input_name)
              rv_input_to_remove(c(rv_input_to_remove(), oneliner_to_remove))
              removeUI(
                selector = paste0("#", ns(oneliner_to_remove))
              )
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    output$save_download_oneliners <- downloadHandler(
      filename = function() {
        "oneliner_costs.json"
      },
      content = function(file) {
        file_name <- "oneliner_costs.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_json_save(input,
          nested_list = rv_jsons[[sublist]],
          prefix = "",
          folders = c(folder, "app/json"),
          file_name,
          to_remove = rv_input_to_remove()
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(file_reac(), ignoreInit = TRUE, {
      oneliners_list <- rv_jsons[[sublist]] %>% discard(names(.) %in% "file_identifier")
      oneliners_list_names <- names(oneliners_list)

      lapply(oneliners_list_names, function(name) {
        num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_oneliners <- list()
        num_names_oneliners <- char_names_oneliners <- list()

        char_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.character(x))))
        num_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.numeric(x))))
        logic_names_oneliners[[name]] <- names(which(sapply(oneliners_list[[name]], function(x) is.logical(x))))

        char_names_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE)
        num_names_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE)

        char_names_not_currency[[name]] <- grep("currency", char_names_oneliners[[name]], value = TRUE, invert = TRUE)
        num_names_not_currency[[name]] <- grep("currency", num_names_oneliners[[name]], value = TRUE, invert = TRUE)

        lapply(char_names_currency[[name]], function(x) {
          updateTextInput(session,
            paste0(name, x),
            value = oneliners_list[[name]][[x]]
          )
        })
        lapply(num_names_currency[[name]], function(x) {
          updateNumericInput(session,
            paste0(name, x),
            value = oneliners_list[[name]][[x]]
          )
        })
        lapply(char_names_not_currency[[name]], function(x) {
          updateTextInput(session,
            paste0(name, x),
            value = oneliners_list[[name]][[x]]
          )
        })
        lapply(num_names_not_currency[[name]], function(x) {
          updateNumericInput(session,
            paste0(name, x),
            value = oneliners_list[[name]][[x]]
          )
        })
        lapply(logic_names_oneliners[[name]], function(x) {
          updateCheckboxInput(
            session,
            paste0(name, x),
            value = oneliners_list[[name]][[x]]
          )
        })
      })
    })

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
