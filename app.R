library(shiny)
library(dplyr)
library(jsonlite)
library(rjson)
jsonL <- rjson::fromJSON(file = "input.json")

shinyApp(
  ui = fluidPage(
    titlePanel('Invoice Generator'),
    sidebarLayout(
      sidebarPanel(
        checkboxInput("inter","Include Intermediary bank",FALSE),
        numericInput("salary","Salary",value = 2000),
        textInput("invoiceNumber","Invoice Number","2022-"),
    dateInput(inputId = 'start_date','Start Date: ', value = "2022-02-01"),
    dateInput('end_date','End Date: ', value = "2022-02-28"),
    dateInput('inv_date','Invoice Date: ',value = "2022-02-28"),
   "Number of days counted: ",
    downloadButton("report", "Generate invoice")
  ),
  mainPanel(
    div(style="max-width:600px",
    uiOutput("jsonfields")
  )
  ))),
  server = function(input, output) {

    observeEvent(input$modify, {
      inputList<-list()

      inputList <- lapply(names(jsonL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(jsonL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input.json")
    })

    output$jsonfields<- renderUI({
      wellPanel(
        h3("This is the content of input.json"),
        actionButton("modify", "Update .json"),
        lapply(seq_along(jsonL), function(x) {
        textInput(names(jsonL[x]),names(jsonL[x]), value = jsonL[[x]])
      }),
      )
    })

    output$report <- downloadHandler(
      filename = "invoice.pdf",
      content = function(file) {

        tempReport <- file.path(getwd(), "/inv_md.Rmd")
        file.copy("invoice.Rmd", tempReport, overwrite = TRUE)

        params <- list(name = input$name,
                       start_date = input$start_date,
                       end_date   = input$end_date,
                       date       = input$inv_date,
                       inter      = input$inter,
                       salary     = input$salary,
                       invoiceNumber = input$invoiceNumber

                       )

        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
