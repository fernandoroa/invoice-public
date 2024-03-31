box::use(
  shiny[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bump_month"))
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    bump_month_rv <- reactiveValues(increaseEverything = TRUE, decreaseEverything = TRUE, update_dates = TRUE)

    output$bump_month <- renderUI({
      wellPanel(
        h4(strong("Bump Month Everywhere")),
        splitLayout(
          div(
            class = "flex-dates",
            br(),
            actionButton(ns("increaseMonth"), ""),
            span("1 Month"),
            br(),
            actionButton(ns("decreaseMonth"), "")
          ),
          div(
            class = "go-bottom",
            div(
              actionButton(
                ns("update_dates"),
                "Update dates to current month"
              )
            )
          )
        )
      )
    })

    observeEvent(input$increaseMonth, ignoreInit = TRUE, {
      bump_month_rv$increaseEverything <- !bump_month_rv$increaseEverything
    })

    observeEvent(input$decreaseMonth, ignoreInit = TRUE, {
      bump_month_rv$decreaseEverything <- !bump_month_rv$decreaseEverything
    })

    observeEvent(input$update_dates, ignoreInit = TRUE, {
      bump_month_rv$update_dates <- !bump_month_rv$update_dates
    })

    outputOptions(output, "bump_month", suspendWhenHidden = FALSE)

    return(list(
      increaseEverything = reactive({
        bump_month_rv$increaseEverything
      }),
      decreaseEverything = reactive({
        bump_month_rv$decreaseEverything
      }),
      update_dates = reactive({
        bump_month_rv$update_dates
      })
    ))
  })
}
