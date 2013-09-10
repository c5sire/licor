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
    
    output$hint <- renderText({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      
      data = datasetInput()
      if(!is.null(data)){
        knit2html("../reports/report.Rmd")
        includeHTML("report.html")
      } 
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
    
    output$hint <- renderText({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      if(input$useColor) "<br>Colorizing the Excel file may take some time!"
      data = datasetInput()
      if(!is.null(data)){
        knit2html("../reports/report.Rmd")
        includeHTML("report.html")
      } 
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


