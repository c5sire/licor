shinyUI(pageWithSidebar(
  headerPanel("Licor to matrix converter (version 0.0.4)"),
  sidebarPanel(
    fileInput('file1', 'Choose tab delimited or Excel File',
              #accept=c('application/xls','application/vnd.ms-excel', 'text/csv','text/comma-separated-values,text/plain')),
              accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/xlsx')),
    tags$hr(),
    #Somehow the following doesn't work
    
    #checkboxInput('summary', 'Save summary in Excel', TRUE),
    #checkboxInput('join', 'Save joined table in Excel', TRUE),
    
    tags$hr(),
    downloadButton('downloadData', 'Download converted data!')
    
    
  ),
  mainPanel(
    tableOutput('contents')
  )
))