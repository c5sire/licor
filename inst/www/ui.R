shinyUI(pageWithSidebar(
  headerPanel("Licor to matrix converter (version 0.0.3)"),
  sidebarPanel(
    fileInput('file1', 'Choose tab delimited or Excel File',
              #accept=c('application/xls','application/vnd.ms-excel', 'text/csv','text/comma-separated-values,text/plain')),
              accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/xlsx')),
    tags$hr(),
    
    downloadButton('downloadData', 'Download converted data!')
  ),
  mainPanel(
    tableOutput('contents')
  )
))