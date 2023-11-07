box::use(
  shiny[...],
  magrittr[`%>%`],
  purrr[discard, keep]
)

box::use(
  .. / utils / constants[...],
  .. / logic / json_save[...]
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("grouped_box"))
}

server <- function(id, rv_sublist, file_reac, exchange_rate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$grouped_box <- renderUI({
      grouped_list <- rv_sublist %>% discard(names(.) %in% "file_identifier")

      root_names <- c(
        "currency_exchange_to_Final_Currency", "use",
        "GeneralName", "currency"
      )

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
        wellPanel(
          {
            tagList({
              char_names_currency_list <- lapply(char_names_currency, function(x) {
                textInput(
                  ns(x),
                  div(
                    class = "wrap",
                    sub("_", " ", sub("(.*)_([[:alpha:]])(.*)", "\\1 \\U\\2\\L\\3", x, perl = TRUE))
                  ),
                  grouped_list[[x]]
                )
              })
              num_names_currency_list <- lapply(num_names_currency, function(x) {
                numericInput(
                  ns(x),
                  div(
                    class = "wrap",
                    gsub("_", " ", x, perl = TRUE)
                  ),
                  grouped_list[[x]]
                )
              })
              char_names_grouped_list <- lapply(char_names_not_currency, function(x) {
                textInput(
                  ns(x),
                  gsub("_", " ", gsub("(.*)([[:upper:]])", "\\1 \\2", x)),
                  grouped_list[[x]]
                )
              })
              logic_names_grouped_list <- lapply(logic_names_grouped, function(x) {
                checkboxInput(
                  ns(x),
                  gsub("_", " ", gsub(pattern_a, pattern_b, x)),
                  grouped_list[[x]]
                )
              })
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
          },
          div(
            class = "five_column_grid",
            div(),
            div(
              helpText("Go to Main tab to save all"),
              downloadButton(ns("save_download_grouped"),
                strong("Save and Download", code("grouped_costs.json")),
                style = "white-space: normal;
                           word-wrap: break-word;"
              )
            )
          )
        ),
        wellPanel(
          tagList(
            lapply(seq_along(grouped_list_names), function(idx) {
              name <- grouped_list_names[idx]
              num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_grouped <- list()
              num_names_grouped <- char_names_grouped <- list()

              char_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.character(x))))
              num_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.numeric(x))))
              logic_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.logical(x))))

              char_names_list <- lapply(char_names_grouped[[name]], function(x) {
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
                  grouped_list[[name]][[x]]
                )
              })
              num_names_list <- lapply(num_names_grouped[[name]], function(x) {
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
                  grouped_list[[name]][[x]]
                )
              })
              div(
                class = "three_column_grid_center",
                div(
                  class = "go-bottom",
                  h4(strong(name))
                ),
                div(
                  class = "go-bottom",
                  char_names_list
                ),
                div(
                  class = "go-bottom",
                  num_names_list
                )
              )
            })
          )
        )
      )
    })

    output$save_download_grouped <- downloadHandler(
      filename = function() {
        "grouped_costs.json"
      },
      content = function(file) {
        file_name <- "grouped_costs.json"
        folder <- paste0(gsub("file", "folder_", tempfile()))
        dir.create(folder)

        nested_and_root_save(input,
          nested_list = rv_sublist,
          prefix = "",
          folders = c(folder, "app/json"),
          file_name
        )

        json_path <- file.path(folder, file_name)
        file.copy(json_path, file)
      },
      contentType = "json"
    )

    observeEvent(exchange_rate(), ignoreInit = TRUE, {
      if (is.numeric(exchange_rate())) {
        updateNumericInput(session, paste0("currency_exchange_to_Final_Currency"), value = exchange_rate())
      }
    })

    observeEvent(file_reac(), {
      grouped_list <- rv_sublist %>% discard(names(.) %in% "file_identifier")

      root_names <- c(
        "currency_exchange_to_Final_Currency", "use",
        "GeneralName", "currency"
      )

      grouped_list_root <- grouped_list %>% keep(names(.) %in% root_names)

      char_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.character(x))))
      num_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.numeric(x))))
      logic_names_grouped <- names(which(sapply(grouped_list_root, function(x) is.logical(x))))

      char_names_currency <- grep("currency", char_names_grouped, value = TRUE)
      num_names_currency <- grep("currency", num_names_grouped, value = TRUE)

      char_names_not_currency <- grep("currency", char_names_grouped, value = TRUE, invert = TRUE)

      grouped_sublists <- grouped_list %>% discard(names(.) %in% root_names)

      grouped_list_names <- names(grouped_sublists)

      lapply(char_names_currency, function(x) {
        updateTextInput(
          session,
          x,
          value = grouped_list[[x]]
        )
      })
      lapply(num_names_currency, function(x) {
        updateNumericInput(
          session,
          x,
          value = grouped_list[[x]]
        )
      })
      lapply(char_names_not_currency, function(x) {
        updateTextInput(
          session,
          x,
          value = grouped_list[[x]]
        )
      })
      lapply(logic_names_grouped, function(x) {
        updateCheckboxInput(
          session,
          x,
          value = grouped_list[[x]]
        )
      })

      lapply(grouped_list_names, function(name) {
        num_names_not_currency <- char_names_not_currency <- num_names_currency <- char_names_currency <- logic_names_grouped <- list()
        num_names_grouped <- char_names_grouped <- list()

        char_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.character(x))))
        num_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.numeric(x))))
        logic_names_grouped[[name]] <- names(which(sapply(grouped_sublists[[name]], function(x) is.logical(x))))

        lapply(char_names_grouped[[name]], function(x) {
          updateTextInput(
            session,
            paste0(name, x),
            value = grouped_list[[name]][[x]]
          )
        })
        lapply(num_names_grouped[[name]], function(x) {
          updateNumericInput(
            session,
            paste0(name, x),
            value = grouped_list[[name]][[x]]
          )
        })
      })
    })
    outputOptions(output, "grouped_box", suspendWhenHidden = FALSE)
  })
}
