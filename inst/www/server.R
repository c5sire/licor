library(licor)
library(stringr)

shinyServer(function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    out = read.licor(filename = inFile$name, datapath=inFile$datapath)
    
    licor2matrix(out)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    out = datasetInput()
    summary.licor(out)
    
   },digits = 0)
  

getExtension <-reactive({
  inFile <- input$file1$name
  if(str_detect(inFile,".xlsx")) return(".xlsx")
  return(".csv")
})  
  
 
 output$downloadData <- downloadHandler(
    filename = function() { paste("licor2matrix",getExtension(),sep=".") },
    content = function(file) {
      write.licor(datasetInput(), file)
    }

)
  
})

