# Data Entry -------------------------------------------------------------------
tabPanel("Data Entry",
         tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
         headerPanel(title="Data Entry"),
         helpText("In this page you can choose a dataset from the literature or upload your own data (please use the provided template)."),
         
         sidebarPanel(
           selectInput("dataset", "Choose from a published dataset", 
                       choices=c("none", "Ruggles RSC", "Hoskin ALEN", "Belmonte Egypt")),
           
           hr(),
           fileInput('file1', 'or Upload your own Data',
                     accept=c('text/xlsx', '.xlsx')),
           em("Note"), ": Make sure the datafile follows the template which",
           downloadLink('downloadTemplate', "can be downloaded here"),
           
           # conditionalPanel("output.datatest == 1",
                            hr(),
                            downloadButton('downloadData', "Download Data")
           # )
           
           , width=3),
         mainPanel(
           verbatimTextOutput("error"),
           tags$head(tags$style("#error{font-size: 9pt}")),
           tableOutput("table")
         )
)