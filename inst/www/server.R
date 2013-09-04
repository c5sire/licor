library(licor)

shinyServer(function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    #data = read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    data = licor2matrix(inFile$datapath,write=F)
    data$data
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    datasetInput()
    
   },digits = 0)
  
  
  
  output$downloadData <- downloadHandler(
    
    #filename = function() { gsub(".csv","_out.csv",basename(input$file1$datapath)) },
    filename = function() { "licor2matrix.csv" },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  
})