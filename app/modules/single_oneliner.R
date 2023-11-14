box::use(
  shiny[...]
)

box::use(
  .. / logic / input_fun[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("item_id"))
}

server <- function(id, char_names_currency_this_name, num_names_currency_this_name,
                   char_names_not_currency_this_name, num_names_not_currency_this_name,
                   logic_names_oneliners_this_name, oneliners_list_active, idx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_input_to_remove <- reactiveVal("")
    char_names_currency_list <- create_text_input(char_names_currency_this_name, oneliners_list_active, ns, idx)

    num_names_currency_list <- create_numeric_input(num_names_currency_this_name, oneliners_list_active, ns, idx)

    char_names_oneliners_not_currency_list <- create_text_input_nc(
      char_names_not_currency_this_name,
      oneliners_list_active, ns, idx
    )

    num_names_oneliners_not_currency_list <- create_numeric_input_nc(
      num_names_not_currency_this_name,
      oneliners_list_active, ns, idx
    )

    logic_names_oneliners_list <- create_check_box_input(logic_names_oneliners_this_name, oneliners_list_active, ns)

    if (idx > 1) {
      drop_button <- create_drop_button(ns)
    } else {
      drop_button <- div()
    }

    output$item_id <- renderUI({
      div(
        id = ns(id),
        class = "seven_column_grid",
        div(
          class = "go-bottom",
          h4(strong(id))
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
        div(
          class = "go-bottom",
          num_names_oneliners_not_currency_list
        ),
        div(
          class = "go-bottom",
          logic_names_oneliners_list
        ),
        drop_button
      )
    })

    observeEvent(input$remove_row, ignoreInit = TRUE, {
      rv_input_to_remove(id)
      removeUI(
        selector = paste0("#", ns(id))
      )
    })

    outputOptions(output, "item_id", suspendWhenHidden = FALSE)

    return(reactive(rv_input_to_remove()))
  })
}
