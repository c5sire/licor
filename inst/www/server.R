library(licor)
gc()

fo = "licor2matrix.xlsx"
fn = file.path(getwd(),fo)

shinyServer(function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    out = read.licor(filename = inFile$name, datapath=inFile$datapath)
    res = licor2matrix(out)
    res
  })
  
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    out = datasetInput()
    summary.licor(out)
   },digits = 0)
  
  datasetOutput <- reactive({
    #Don't do this directly in the downloadHandler: causes problems!
    write.licor(datasetInput(),fn,input$hasSummary,input$hasJoined, input$useColor)
    loadWorkbook(fn)
  })
  
  output$hint <- renderText({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    if(input$useColor) "<br>Colorizing the Excel file may take some time!"
  })
  
 
 output$downloadData <- downloadHandler(
    filename = "licor2matrix.xlsx",
    content = function(file) {
      saveWorkbook(datasetOutput(), file)
    }
  )
  
})

