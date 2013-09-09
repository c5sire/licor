shinyUI(pageWithSidebar(
  headerPanel("Licor to matrix converter (version 0.0.4)"),
  sidebarPanel(
    fileInput('file1', 'Choose tab delimited or Excel File',
              accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/xlsx')),
    
    tags$hr(),
    downloadButton('downloadData', 'Download converted data!')
    
    
  ),
  mainPanel(
    tableOutput('contents')
  )
))