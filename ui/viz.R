# Visualization -------------------------------------------------------------------
tabPanel("Visualization",
         tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
         headerPanel(title="Data Visualization"),
         helpText("In this page you can visualize the data selected in the 'Data Entry' page."),
         
         sidebarPanel(
           bsCollapse(id = "colViz", multiple = TRUE, open = "Plot Style",
                      bsCollapsePanel("Plot Style", 
                                      # Points with error bars
                                      fluidRow(
                                        column(6, checkboxInput("chk_dots", label = "Data Points", value = TRUE)),
                                        column(6, conditionalPanel("input.chk_dots == 1",
                                                                   numericInput("vis_dot_unc", "Uncertainty", value=3, width=100, min=0),
                                                                   bsTooltip("vis_dot_unc", "This will be used for measurements without uncertainty, or if larger than Dec.unc", "right", options = list(container = "body"))
                                        )
                                        )
                                      ),
                                      
                                      # Histogram
                                      checkboxInput("chk_hist", label = "Histogram", value = FALSE),
                                      conditionalPanel("input.chk_hist == 1",
                                                       fluidRow(
                                                         column(6, numericInput("vis.hist_size", "Bin Size", value=5, width=100, min=0) ),
                                                         column(6, numericInput("vis.hist_start", "Bin Start", value=0, width=100) )
                                                       )
                                      ),
                                      
                                      # SPD / Curvigram
                                      fluidRow(
                                        column(6, checkboxInput("chk_spd", label = "Curvigram", value = FALSE)),
                                        column(6, conditionalPanel("input.chk_spd == 1",
                                                                   numericInput("vis_spd_unc", "Uncertainty", value=3, width=100, min=0),
                                                                   bsTooltip("vis_spd_unc", "This will be used for measurements without uncertainty, or if larger than Dec.unc", "right", options = list(container = "body"))
                                        )
                                        )
                                      ),
                                      style = "primary"),
                      
                      bsCollapsePanel("Celestial Objects",
                                      fluidRow( column(12, h5("Time Period:")) ),
                                      fluidRow(
                                        column(6,
                                               numericInput(paste("cel_vis","From",sep='_'), label = "from", value=2000, width=100),
                                               bsTooltip(paste("cel_vis","From",sep='_'), "Type in year of interest. Use negative for BCE.", "right", options = list(container = "body"))
                                        ),
                                        column(6,
                                               numericInput(paste("cel_vis","To",sep='_'), label = "to", value=2000, width=100),
                                               bsTooltip(paste("cel_vis","To",sep='_'), "If not interested in a time range type in the same value as in the From field. Use negative for BCE.", "right", options = list(container = "body"))
                                        )
                                      ),
                                      fluidRow(
                                        column(12, checkboxGroupInput(paste("cel_vis","celestial",sep='_'), 'Sun and Moon:', c("Solstices" = "sol", "Equinoxes" = "equ", "minor Lunar Extremes" = "minlex", "major Lunar Extremes" = "majlex", "Spring Full Moon" = "sfm", "Autumn Full Moon" = "afm"), inline=F)),
                                        bsTooltip(paste("cel_vis","celestial",sep='_'), "Select solar and lunar events to display", "right", options = list(container = "body"))
                                        
                                      ),
                                      fluidRow(
                                        column(12, selectInput(paste("cel_vis","Stars",sep='_'), label = "Stars:", Stars$NAME, multiple=T)),
                                        bsTooltip(paste("cel_vis","Stars",sep='_'), "Type name of star(s) to display, or choose from list", "right", options = list(container = "body"))
                                      ),
                                      fluidRow(
                                        column(12, textInput(paste("cel_vis","Custom",sep='_'), value = "", label = "Custom Dec:")),
                                        bsTooltip(paste("cel_vis","Custom",sep='_'), "Type in any declination you would like to see plotted. Separate multiple values by a semicolon ;", "right", options = list(container = "body"))
                                      ),
                                      style = "default")
           )
         ),
         
         mainPanel(
           tags$head(tags$style(HTML(mycss))),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner",
                        width = 70
               ),
               plotOutput("plot", click = clickOpts("plot_click"), height="600px")
           ),
           textOutput("plot.info"),
           hr()
           
         )
)
