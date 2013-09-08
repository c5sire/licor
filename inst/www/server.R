library(licor)
library(stringr)

res = gc()

shinyServer(function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    out = read.licor(filename = inFile$name, datapath=inFile$datapath)
    
    licor2matrix(out)
  })
  
  doSummary <- reactive({input$summary})
  
  doJoin <- reactive({input$join})
  
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
    filename = function() { "licor2matrix.xlsx" },
    content = function(file) {
      #write.licor(datasetInput(), file, input$summary, input$join)
      #print(input$file1)
#       if(!str_detect(file,".xlsx")){
#         to = paste(file,"xlsx", sep=".")
#         file.rename(file,to)
#         file = to
#       }
      write.licor(datasetInput(), file, TRUE,TRUE)
    }
  )
  
})

