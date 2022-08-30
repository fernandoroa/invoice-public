library(shiny)
library(dplyr)
library(rjson)
library(jsonlite)
library(lubridate)
# sudo apt install texlive-latex-base
# tinytex::install_tinytex()
library(tinytex)
#
# read .json
#

mainL <- rjson::fromJSON(file = "main.json")
jsonL <- rjson::fromJSON(file = "input_address_bank.json")
jsonSalaryL <- rjson::fromJSON(file = "input_salary.json")
json3rdCurrencyL <- rjson::fromJSON(file = "input_local.json")
jsonSickL        <- rjson::fromJSON(file = "input_sick.json")

#
# subset of vars for one of the .json
#

char_names  <- names(which(sapply(jsonSalaryL, function(x) is.character(x)) ))
num_names   <- names(which(sapply(jsonSalaryL, function(x) is.numeric(x)) ))
logic_names <- names(which(sapply(jsonSalaryL, function(x) is.logical(x)) ))

char_names3  <- names(which(sapply(json3rdCurrencyL, function(x) is.character(x)) ))
num_names3   <- names(which(sapply(json3rdCurrencyL, function(x) is.numeric(x)) ))
logic_names3 <- names(which(sapply(json3rdCurrencyL, function(x) is.logical(x)) ))

num_sick     <- names(which(sapply(jsonSickL, function(x) is.numeric(x)) ))
logic_sick   <- names(which(sapply(jsonSickL, function(x) is.logical(x)) ))

shinyApp(
  ui = fluidPage(
    tags$head(tags$style(
      HTML("
           .well {
           background-color: #A0A0A0;
           }")
    )),
    titlePanel('Invoice Generator'),
    fluidPage(
      fluidRow(
        column(3
               #
               #    manual inputs, see params in .Rmd
               #
        , wellPanel(
        h4("other options"),
        textInput("invoiceNumber","Invoice Number","2022-"),
        radioButtons("lang","Language",c("english"=1,"other"=2) ),
        ),
        wellPanel(
        h4(code("main.json"), strong("content")),
        numericInput("exchangeUSDPLN","exchange Rate USD (final) to PLN (original)",mainL$exchangeUSDPLN),
        numericInput("exchangeUSDBRL","exchange Rate USD (final) to BRL (local)",mainL$exchangeUSDBRL)
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
            dateInput(inputId = 'startDate','Start Date: ', value = as.Date(mainL$startDate)),
            dateInput('endDate','End Date: ', value = as.Date(mainL$endDate) ),
            dateInput('invoiceDate','Invoice Date: ',value = as.Date(mainL$invoiceDate) ),
          )
        ),
        actionButton("modify_main"
                     , strong("Save",code("main.json"),
                              "required! after changes")
                     , style=  "white-space: normal;
                           word-wrap: break-word;"
                    )
        )
        )
        ,column(3
                #
                #   2nd box
                #
        , uiOutput("second_box")
        , wellPanel(
          h2("Generate invoice!"),
          actionButton("modify_all"
                       , strong("Save All",
                                code("*.json"),
                                "required!"
                       )
          ),
          downloadButton("report", "Generate invoice in .pdf"),
          helpText("saving .json changes is mandatory"),
        )
        ),
        column(3,
               uiOutput("second_box_b"),
               uiOutput("second_box_c")

        ),
        column(3,
               #
               #   3rd box
               #
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
      cdate<-input$invoiceDate
      sdate<-input$startDate
      edate<-input$endDate
      cmon<-month(cdate)
      smon<-month(sdate)
      emon<-month(edate)
      updateDateInput(session, "invoiceDate", value=cdate+mon_span[cmon+2])
      updateDateInput(session, "startDate", value=sdate+mon_span[smon+1])
      updateDateInput(session, "endDate", value=edate+mon_span[emon+2])
    })

    observeEvent(input$decreaseDate, {
      cdate<-input$invoiceDate
      sdate<-input$startDate
      edate<-input$endDate
      cmon <-month(cdate)
      smon<-month(sdate)
      emon<-month(edate)
      updateDateInput(session, "invoiceDate", value=cdate-mon_span[cmon+1])
      updateDateInput(session, "startDate", value=sdate-mon_span[cmon])
      updateDateInput(session, "endDate", value=edate-mon_span[cmon+1])
    })

    observeEvent(c(input$modify_salary, input$modify_all), {
      inputList <- list()

      inputList <- lapply(names(jsonSalaryL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(jsonSalaryL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input_salary.json")
    })

    observeEvent( c(input$modify_3rd, input$modify_all), {
      inputList<-list()

      inputList <- lapply(names(json3rdCurrencyL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(json3rdCurrencyL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input_local.json")
    })

    observeEvent( c(input$modify_sick, input$modify_all), {
      inputList<-list()

      inputList <- lapply(names(jsonSickL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(jsonSickL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input_sick.json")
    })

    observeEvent( c(input$modify_main, input$modify_all), {
      inputList<-list()

      inputList <- lapply(names(mainL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(mainL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "main.json")
    })

    observeEvent(c(input$modify, input$modify_all), {
      inputList<-list()

      inputList <- lapply(names(jsonL), function(x) {
        input[[x]]
      })

      names(inputList) <- names(jsonL)
      jsonData <- jsonlite::toJSON(x=inputList, pretty=TRUE)
      write(jsonData, "input_address_bank.json")
    })

    output$second_box<- renderUI({

      #
      #   2nd box
      #

     wellPanel(
      h4(code("input_salary.json"), strong("content")),
      lapply(char_names, function(x){
        textInput(x,gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                  ,jsonSalaryL[[x]])
      }),
      lapply(num_names, function(x){
        numericInput(x,gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = T)
                     ,jsonSalaryL[[x]])
      }),
      lapply(logic_names, function(x){
        checkboxInput(x,
                      gsub("_"," ",gsub(patternA, patternB, x ) )
                      , jsonSalaryL[[x]])
      }),
      helpText("this box content must be saved before generating .pdf"),

      actionButton("modify_salary"
                   , strong("Save",code("input_salary.json"),
                            "required! after changes")
                   , style=  "white-space: normal;
                           word-wrap: break-word;"
                     )
    )
    })

    output$second_box_b<- renderUI({

      #
      #   2nd box b
      #

      wellPanel(
        h4(code("input_local.json"), strong("content")),
        lapply(char_names3, function(x){
          textInput(x,gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                    ,json3rdCurrencyL[[x]])
        }),
        lapply(num_names3, function(x){
          numericInput(x,gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = T)
                       ,json3rdCurrencyL[[x]])
        }),
        lapply(logic_names3, function(x){
          checkboxInput(x,
                        # gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                        gsub("_"," ",gsub(patternA, patternB, x ) )
                        ,json3rdCurrencyL[[x]])
        }),
        helpText("this box content must be saved before generating .pdf"),

        actionButton("modify_3rd"
                     , strong("Save",code("input_local.json"), "required! after changes"
                              )
                     , style=  "white-space: normal;
                           word-wrap: break-word;"
        )
        )
    })

    output$second_box_c<- renderUI({

      #
      #   2nd box c
      #

      wellPanel(
        h4(code("input_sick.json"), strong("content")),
        lapply(num_sick, function(x){
          numericInput(x,gsub("(.*?)([[:upper:]])", "\\1 \\2", x, perl = T)
                       ,jsonSickL[[x]])
        }),
        lapply(logic_sick, function(x){
          checkboxInput(x,
                        # gsub("(.*)([[:upper:]])", "\\1 \\2", x)
                        gsub("_"," ",gsub(patternA, patternB, x ) )
                        ,jsonSickL[[x]])
        }),
        helpText("this box content must be saved before generating .pdf"),

        actionButton("modify_sick"
                     , strong("Save",code("input_sick.json"),
                              "required! after changes")
                     , style=  "white-space: normal;
                           word-wrap: break-word;"
                )
        )
    })

    output$third_box<- renderUI({

      #
      #   3rd box
      #

      wellPanel(
        h4(code("input_address_bank.json"), strong("content")),
        helpText("this box content must be saved before generating .pdf"),
        actionButton("modify"
                     , strong("Save",code("input_address_bank.json"), "required! after changes")
                     , style=  "white-space: normal;
                           word-wrap: break-word;"
                             ),

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
        pattern2<-"modify_3rd|modify_salary|modify|increaseDate|decreaseDate|modify_sick|modify_all|modify_main"
        pattern3<-paste0(names(jsonSalaryL), collapse="|")
        pattern3<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern3)
        pattern4<-paste0(names(json3rdCurrencyL), collapse="|")
        pattern4<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern4)
        pattern5<-paste0(names(jsonSickL), collapse="|")
        pattern5<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern5)
        pattern6<-paste0(names(mainL), collapse="|")
        pattern6<-gsub("\\((.*?)\\)","\\\\(\\1\\\\)",pattern6)

        params <-params[-grep(pattern, names(params))]
        params <-params[-grep(pattern2, names(params))]
        params <-params[-grep(pattern3, names(params))]
        params <-params[-grep(pattern4, names(params))]
        params <-params[-grep(pattern5, names(params))]
        params <-params[-grep(pattern6, names(params))]

        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
