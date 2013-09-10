is.glimmer <-function(){
  str_detect(.Platform$pkgType,"source")
}

if(is.glimmer()){
  shinyUI(pageWithSidebar(
    headerPanel("Licor to matrix converter (version 0.0.4)"),
    sidebarPanel(
      fileInput('file1', 'Choose tab delimited or Excel File',
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/xlsx'))
      ,
      tags$hr(),
      downloadButton('downloadData', 'Download converted data!')
      
      
    ),
    mainPanel(
      tableOutput('contents'),
      htmlOutput('hint')
    )
  ))  
} else {
  


shinyUI(pageWithSidebar(
  headerPanel("Licor to matrix converter (version 0.0.4)"),
  sidebarPanel(
    fileInput('file1', 'Choose tab delimited or Excel File',
              accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/xlsx'))
    ,
    tags$hr(),
    checkboxInput("hasSummary","Save summary",TRUE),
    checkboxInput("hasJoined","Save joined marker data",TRUE),
    checkboxInput("useColor","Colorize marker data",FALSE),
    tags$hr(),
    downloadButton('downloadData', 'Download converted data!')
    
    
  ),
  mainPanel(
    tableOutput('contents'),
    htmlOutput('hint')
  )
))

}

