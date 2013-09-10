library(licor)

gc()

fo = "licor2matrix.xlsx"
fn = file.path(tempdir(),fo)

is.glimmer <-function(){
  str_detect(.Platform$pkgType,"source")
}

if(is.glimmer()){
  
  shinyServer(function(input, output) {
    
    datasetInput <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      out = read.licor(filename = inFile$name, datapath=inFile$datapath)
      licor2matrix(out)
    })
    
    datasetOutput <- reactive({
      write.licor(datasetInput(), fn)
      loadWorkbook(fn)
    })
    
    reportOutput <-reactive({
      inFile <- input$file1
      print(inFile)
      if (is.null(inFile)){
        return("Please select a file.")
      } else {
        #HTML("Processing ...")
#         data = datasetInput()
#         if(!is.null(data)){
#           knit2html("../reports/report.Rmd")
#           includeHTML("report.html")
#         } 
      }
      
    })
    
    output$hint <- renderText({
      reportOutput()
    })
    
    
    output$downloadData <- downloadHandler(
      filename = "licor2matrix.xlsx",
      content = function(file) {
        saveWorkbook(datasetOutput(), file)
      },
      contentType = "application/xlsx"
    )
    
  })
  
} else {
  
  shinyServer(function(input, output) {
    
    datasetInput <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      out = read.licor(filename = inFile$name, datapath=inFile$datapath)
      licor2matrix(out)
    })
    
    datasetOutput <- reactive({
      #Don't do this directly in the downloadHandler: causes problems!
      #print(fn)
      write.licor(datasetInput(), fn, input$hasSummary,input$hasJoined, input$useColor)
      loadWorkbook(fn)
    })
    
    reportOutput <-reactive({
      inFile <- input$file1
      #print(inFile)
      if (is.null(inFile)){
        return("Please select a file.")
      } else {
        data = datasetInput()
        if(!is.null(data)){
          knit2html("../reports/report.Rmd")
          includeHTML("report.html")
        } 
      }
      
    })
    
    output$hint <- renderText({
      reportOutput()
    })
    
    
    
    output$downloadData <- downloadHandler(
      filename = "licor2matrix.xlsx",
      content = function(file) {
        saveWorkbook(datasetOutput(), file)
      },
      contentType = "application/xlsx"
    )
    
  })
  
}


