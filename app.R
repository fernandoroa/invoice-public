library(shiny)
library(dplyr)
library(rjson)
library(jsonlite)
jsonL <- fromJSON(file = "input.json")
jsonSalaryL <- fromJSON(file = "input_salary.json")

originalSalary<-8000

shinyApp(
  ui = fluidPage(
    titlePanel('Invoice Generator'),
    fluidPage(
      fluidRow(
        column(3
        , wellPanel(
        h4("other info"),
        textInput("invoiceNumber","Invoice Number","2022-"),
        numericInput("exchangeRate","exchange Rate",4.1683),
        dateInput(inputId = 'start_date','Start Date: ', value = "2022-02-01"),
        dateInput('end_date','End Date: ', value = "2022-02-28"),
        dateInput('inv_date','Invoice Date: ',value = "2022-02-28")
        )
        , wellPanel(
        downloadButton("report", "Generate invoice in .pdf")
        )
        )
        ,column(3
        , wellPanel(
          h4("from input_salary.json"),
          textInput("originalCurrency","original Currency"
                    ,jsonSalaryL$originalCurrency),
          numericInput("originalSalary","original Salary"
                       ,jsonSalaryL$originalSalary),
          textInput("finalCurrency","final Currency",
                    jsonSalaryL$finalCurrency),
          checkboxInput("inter",HTML("<strong>Include Intermediary bank</strong>")
                        ,jsonSalaryL$intermediaryBank),
          actionButton("modify_salary"
                       , HTML("<strong>Update input_salary.json after changes!</strong>")
                       )

        )
  ),
  column(3,
    div(style="max-width:600px",
    uiOutput("jsonfields")
  )
  ))
  )
  )
  , server = function(input, output) {

    observeEvent(input$modify_salary, {
      inputList<-list()

      inputList <- lapply(names(jsonSalaryL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(jsonSalaryL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input_salary.json")
    })

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
        helpText("this box content must be saved before generating .pdf"),
        actionButton("modify"
                     , HTML("<strong>Save (input.json) required! after changes</strong>") ),
        br(),
        br(),
        lapply(seq_along(jsonL), function(x) {
        textInput(names(jsonL[x]),names(jsonL[x]), value = jsonL[[x]])
      }),
      )
    })

    output$report <- downloadHandler(
      filename = "invoice.pdf",
      content = function(file) {

        tempReport <- file.path(getwd(), "/inv_md_dont_modify.Rmd")
        file.copy("invoice.Rmd", tempReport, overwrite = TRUE)

        params <- list(name = input$name,
                       start_date = input$start_date,
                       end_date   = input$end_date,
                       date       = input$inv_date,
                       inter      = input$inter,
                       invoiceNumber = input$invoiceNumber,
                       originalCurrency = input$originalCurrency,
                       originalSalary = input$originalSalary,
                       finalCurrency = input$finalCurrency,
                       exchangeRate = input$exchangeRate
                       )

        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
