box::use(
  shiny[...]
)

box::use(
  .. / .. / logic / input_fun[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("item_id"))
}

server <- function(id, char_names_grouped_this_name, num_names_grouped_this_name,
                   grouped_sublists_this_name, idx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_input_to_remove <- reactiveVal("")

    char_names_list <- create_text_input(char_names_grouped_this_name, grouped_sublists_this_name, ns, idx)

    num_names_list <- create_numeric_input(num_names_grouped_this_name, grouped_sublists_this_name, ns, idx)

    if (idx > 1) {
      drop_button <- create_drop_button(ns)
    } else {
      drop_button <- div()
    }

    output$item_id <- renderUI({
      div(
        id = ns(id),
        class = "four_column_grid_center",
        div(
          class = "go-bottom",
          h4(strong(id))
        ),
        div(
          class = "go-bottom",
          char_names_list
        ),
        div(
          class = "go-bottom",
          num_names_list
        ),
        div(
          class = "go-center-vertical",
          drop_button
        )
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
