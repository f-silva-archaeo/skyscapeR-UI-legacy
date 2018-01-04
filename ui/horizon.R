# Horizon -------------------------------------------------------------------
tabPanel("Horizon",
         tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
         headerPanel(title="Horizon Visualization"),
         helpText("In this page you can visualize and handle horizon data."),
         
         sidebarPanel(
           uiOutput("go_imp"),

           hr(),
           
           bsCollapse(id = "colHor", multiple = TRUE, open = "Horizon Info",
                      bsCollapsePanel("Horizon Info",
                                      verbatimTextOutput("hor"),
                                      downloadButton("download.hor", "Download Horizon"),
                                      style = "primary"),
                      
                      bsCollapsePanel("Celestial Objects", 
                                      fluidRow( column(12, h5("Time Period:")) ),
                                      fluidRow(
                                        column(6,
                                               numericInput(paste("cel_hor","From",sep='_'), label = "from", value=2000, width=100),
                                               bsTooltip(paste("cel_hor","From",sep='_'), "Type in year of interest. Use negative for BCE.", "right", options = list(container = "body"))
                                        ),
                                        column(6,
                                               numericInput(paste("cel_hor","To",sep='_'), label = "to", value=2000, width=100),
                                               bsTooltip(paste("cel_hor","To",sep='_'), "If not interested in a time range type in the same value as in the From field. Use negative for BCE.", "right", options = list(container = "body"))
                                        )
                                      ),
                                      fluidRow(
                                        column(12, checkboxGroupInput(paste("cel_hor","celestial",sep='_'), 'Sun and Moon:', c("Solstices" = "sol", "Equinoxes" = "equ", "minor Lunar Extremes" = "minlex", "major Lunar Extremes" = "majlex", "Spring Full Moon" = "sfm", "Autumn Full Moon" = "afm"), inline=F)),
                                        bsTooltip(paste("cel_hor","celestial",sep='_'), "Select solar and lunar events to display", "right", options = list(container = "body"))
                                        
                                      ),
                                      fluidRow(
                                        column(12, selectInput(paste("cel_hor","Stars",sep='_'), label = "Stars:", Stars$NAME, multiple=T)),
                                        bsTooltip(paste("cel_hor","Stars",sep='_'), "Type name of star(s) to display, or choose from list", "right", options = list(container = "body"))
                                      ),
                                      fluidRow(
                                        column(12, textInput(paste("cel_hor","Custom",sep='_'), value = "", label = "Custom Dec:")),
                                        bsTooltip(paste("cel_hor","Custom",sep='_'), "Type in any declination you would like to see plotted. Separate multiple values by a semicolon ;", "right", options = list(container = "body"))
                                      ),
                                      
                                      style = "default"),
                      
                      bsCollapsePanel("Display Measurements",
                                      uiOutput("mes_sel"),
                                      style = "default")
           )
                            
         ),
         
         mainPanel(
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner",
                        width = 70
               ),
               plotOutput("horizon",  click = clickOpts("hor_click", clip=F), height="600px")
           ),
           textOutput("hor.info"),
           
           hr(),
           
          
           sliderInput("hor.slider", label="Rotate Horizon", min=0, max=360, value=180, step=1, width = "100%")
         )
         
)
