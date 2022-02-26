library(shiny)
library(dplyr)
library(rjson)
library(jsonlite)

#
# read .json
#
jsonL <- rjson::fromJSON(file = "input.json")
jsonSalaryL <- rjson::fromJSON(file = "input_salary.json")

#
# subset of vars for one of the .json
#

char_names<-names(which(sapply(jsonSalaryL, function(x) is.character(x)) ))
num_names <-names(which(sapply(jsonSalaryL, function(x) is.numeric(x)) ))
logic_names <-names(which(sapply(jsonSalaryL, function(x) is.logical(x)) ))

shinyApp(
  ui = fluidPage(
    titlePanel('Invoice Generator'),
    fluidPage(
      fluidRow(
        column(3
               #
               #    manual inputs, see params in .Rmd
               #
        , wellPanel(
        h4("other info"),
        textInput("invoiceNumber","Invoice Number","2022-"),
        numericInput("exchangeRate","exchange Rate",4.1683),
        dateInput(inputId = 'start_date','Start Date: ', value = "2022-02-01"),
        dateInput('end_date','End Date: ', value = "2022-02-28"),
        dateInput('inv_date','Invoice Date: ',value = "2022-02-28")
        )
        , wellPanel(
          helpText("clicking the other box buttons after changes is mandatory"),
        downloadButton("report", "Generate invoice in .pdf")
        )
        )
        ,column(3
                #
                #   2nd box
                #
        , uiOutput("second_box")
        ),
        column(3,
          div(style="max-width:600px",
          uiOutput("third_box")
        )
        )
        )
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

    output$second_box<- renderUI({
     wellPanel(
      h4("from input_salary.json"),
      lapply(char_names, function(x){
        textInput(x,x
                  ,jsonSalaryL[[x]])
      }),
      lapply(num_names, function(x){
        numericInput(x,x
                     ,jsonSalaryL[[x]])
      }),
      lapply(logic_names, function(x){
        checkboxInput(x,x
                      ,jsonSalaryL[[x]])
      }),
      helpText("this box content must be saved before generating .pdf"),

      actionButton("modify_salary"
                   , HTML("<strong>Update input_salary.json after changes!</strong>")
      )
    )
    })

    output$third_box<- renderUI({
      #
      #   3rd box
      #
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

        params <<- reactiveValuesToList(input)

        pattern<-paste0(names(jsonL), collapse="|")
        pattern<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern)
        pattern2<-"modify_salary|modify"
        pattern3<-paste0(names(jsonSalaryL), collapse="|")
        pattern3<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern3)

        params <-params[-grep(pattern, names(params))]
        params <-params[-grep(pattern2, names(params))]
        params <-params[-grep(pattern3, names(params))]
        # names(jsonL)

        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
