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
        numericInput("exchangeRate","exchange Rate",4.1683)
        ,splitLayout(
          div(style="display: flex;
               flex-direction: column;
               justify-content: space-between;
               max-width:150px;
               align-items:center;"
              ,br(),br(),br()
            , actionButton("decreaseDate", "decrease")
            ,br()
            , span("1 Month")
            ,br()
            , actionButton("increaseDate","increase")
          )
          ,
          tagList(
            dateInput(inputId = 'start_date','Start Date: ', value = "2022-02-01"),
            dateInput('end_date','End Date: ', value = "2022-02-28"),
            dateInput('inv_date','Invoice Date: ',value = "2022-02-28")

          )
        )
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
  , server = function(session,input, output) {

    patternA <- "([[:lower:]]+)([[:upper:]])([[:alpha:]]+)([[:digit:]]?)"
    patternB<-"\\1 \\2\\3 \\4"

    mon_span <- c(31,31,28,31,30,31,30,31,31,30,31,30,31,31)

    observeEvent(input$increaseDate, {
      cdate<-input$inv_date
      sdate<-input$start_date
      edate<-input$end_date
      cmon<-month(cdate)
      smon<-month(sdate)
      emon<-month(edate)
      updateDateInput(session, "inv_date", value=cdate+mon_span[cmon+2])
      updateDateInput(session, "start_date", value=sdate+mon_span[smon+1])
      updateDateInput(session, "end_date", value=edate+mon_span[emon+2])
    })
    observeEvent(input$decreaseDate, {
      cdate<-input$inv_date
      sdate<-input$start_date
      edate<-input$end_date
      cmon <-month(cdate)
      smon<-month(sdate)
      emon<-month(edate)
      updateDateInput(session, "inv_date", value=cdate-mon_span[cmon+1])
      updateDateInput(session, "start_date", value=sdate-mon_span[cmon])
      updateDateInput(session, "end_date", value=edate-mon_span[cmon+1])
    })

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
      h4(code("input_salary.json"), "content"),
      lapply(char_names, function(x){
        textInput(x,gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                  ,jsonSalaryL[[x]])
      }),
      lapply(num_names, function(x){
        numericInput(x,gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                     ,jsonSalaryL[[x]])
      }),
      lapply(logic_names, function(x){
        checkboxInput(x,gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                      ,jsonSalaryL[[x]])
      }),
      helpText("this box content must be saved before generating .pdf"),

      actionButton("modify_salary"
                   , strong("Save",code("input_salary.json"), "required! after changes")
                   )

    )
    })

    output$third_box<- renderUI({

      #
      #   3rd box
      #

      wellPanel(
        h4(code("input.json"), "content"),
        helpText("this box content must be saved before generating .pdf"),
        actionButton("modify"
                     , strong("Save",code("input.json"), "required! after changes") ),
        br(),
        br(),
        lapply(seq_along(jsonL), function(x) {
        textInput(names(jsonL[x])
                  , gsub("_"," ",gsub(patternA, patternB, names(jsonL[x])) )
                  , value = jsonL[[x]])
      }),
      )
    })

    output$report <- downloadHandler(
      filename = "invoice.pdf",
      content = function(file) {

        tempReport <- file.path(getwd(), "/inv_md_dont_modify.Rmd")
        file.copy("invoice.Rmd", tempReport, overwrite = TRUE)

        params <- reactiveValuesToList(input)

        pattern<-paste0(names(jsonL), collapse="|")
        pattern<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern)
        pattern2<-"modify_salary|modify|increaseDate|decreaseDate"
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
