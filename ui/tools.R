# Tools -------------------------------------------------------------------
navbarMenu("Tools",
           # These should be hands-on, data input output kind of thing with a copy to clipboard button
           # and links to websites (like the magnetic declination bit)
           tabPanel("Unit Conversion",
                    fluidPage(
                      
                      tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
                      headerPanel(title="Tools :: Unit Conversion"),
                      helpText("This tool converts degrees between the deg-min-sec form and the decimal-point (ie xx.xxxº) form."),
                      hr(),
                      
                      fluidRow(
                        column(2, numericInput("conv.deg", "Degree (º)", 0, min = 0, max = 360, step = 1, width = 150) ),
                        column(2, numericInput("conv.min", "Arcminute (')", 0, min = 0, max = 60, step = 1, width = 150) ),
                        column(2, numericInput("conv.sec", "Arcsecond ('')", 0, min = 0, max = 60, step = NA, width = 150) )
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(2),
                        column(1, actionButton("deg_to_dec", "V", width=50),
                               bsTooltip("deg_to_dec", "Convert to decimal-point format",
                                         "left", options = list(container = "body"))),
                        
                        column(1, actionButton("dec_to_deg", HTML("&Lambda;"), width=50),
                               bsTooltip("dec_to_deg", "Convert to deg-min-sec format",
                                         "right", options = list(container = "body")))
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(2),
                        column(2, numericInput("conv.dec", "Degree with decimal point(º)", 0, min = 0, max = 360, step = NA, width = 150) )
                      )
                      
                    )
           ),
           
           tabPanel("Coordinate Conversion",
                    fluidPage(
                      
                      tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
                      headerPanel(title="Tools :: Coordinate Conversion"),
                      helpText("This tool converts between horizontal (azimuth, altitude) and equatorial (declination, right ascension) coordinates."),
                      hr(),
                      
                      fluidRow(
                        column(3, checkboxGroupInput("coor_chk", "Parameters", c("Nutation correction" = "nut", "Refraction correction" = "ref", "Aberration correction" = "abe"), selected = c("nut", "ref", "abe")),
                               bsTooltip("coor_chk", "Please choose the corrections you want to apply.",
                                         "bottom", options = list(container = "body"))
                        ),
                        column(2, numericInput("coor_lat", "Latitude (º)", 0, min = -90, max = 90, step = 1, width = 150),
                               bsTooltip("coor_lat", "Please enter the latitude in decimal point form. Northern latitudes as positive values, and southern as negative.",
                                         "bottom", options = list(container = "body"))
                        ),
                        column(2, numericInput("coor_lon", "Longitude (º)", 0, min = -180, max = 360, step = 1, width = 150),
                               bsTooltip("coor_lon", "Please enter the longitude in decimal point form. Eastern longitudes as positive values, and western as negative.",
                                         "bottom", options = list(container = "body"))
                        ),
                        column(2, numericInput("coor_elev", "Elevation (m)", 0, step = 1, width = 150),
                               bsTooltip("coor_elev", "This is only required if refraction correction is switched on.",
                                         "bottom", options = list(container = "body"))
                        )
                        
                      ),
                      
                      hr(),
                      
                      fluidRow(
                        column(3),
                        column(2, numericInput("coor_az", "Azimuth (º)", 0, min = 0, max = 360, step = 1, width = 150) ),
                        column(2, numericInput("coor_alt", "Altitude (º)", 0, min = -90, max = 90, step = 1, width = 150) )
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(4),
                        column(1, actionButton("coor_eq", "V", width=50),
                               bsTooltip("coor_eq", "Convert to Equatorial Coordinates",
                                         "left", options = list(container = "body"))
                        ),
                        column(1, actionButton("coor_hor", HTML("&Lambda;"), width=50),
                               bsTooltip("coor_hor", "Convert to Horizontal Coordinates",
                                         "right", options = list(container = "body"))
                        )
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(3),
                        column(2, numericInput("coor_dec", "Declination (º)", 0, min = -90, max = 90, step = 1, width = 150) ),
                        column(2, numericInput("coor_ra", "Right Ascension (º)", 0, min = 0, max = 24, step = 1, width = 150) )
                      )
                      
                    )
           ),
           
           tabPanel("Horizon Altitude",
                    fluidPage(
                      
                      tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
                      headerPanel(title="Tools :: Horizon Altitude"),
                      helpText("This tool calculates horizon altitudes from two elevation measurements (site and horizon point)."),
                      hr(),
                      
                      fluidRow(
                        column(2, numericInput("hor.site", "Site Elevation (m)", 0, min = NA, max = NA, step = 1, width = 150) ),
                        
                        column(2, numericInput("hor.ref", "Horizon Elevation (m)", 0, min = NA, max = NA, step = 1, width = 150) ),
                        
                        column(2, numericInput("hor.dist", "Horizon Distance (m)", 0, min = NA, max = NA, step = 1, width = 150) )
                        
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(4, ""),
                        column(2, actionButton("hor.calc", "Calculate", width=150)),
                        column(2, actionButton("help_altitude", "?")),
                        bsModal("man_altitude", "Instructions :: Horizon Altitude Tool", "help_altitude", size="large",
                                includeMarkdown("man/man_altitude.md")
                        )
                      ),
                      
                      hr(),
                      
                      fluidRow(
                        column(4, ""),
                        column(2, numericInput("hor.alt", "Horizon Altitude (º)", NA, width = 150))
                      )
                      
                    )
           ),
           
           tabPanel("Magnetic Declination",
                    fluidPage(
                      
                      tags$head(tags$style(HTML("h1 { font-size: 30px}"))),
                      headerPanel(title="Tools :: Magnetic Declination"),
                      helpText("This tool helps you estimate the magnetic declination (difference between magnetic and true north) using a variety of methods."),
                      
                      fluidRow(
                        column(2, numericInput("mdec_lat_site", "Latitude", 0, min = -90, max = 90, width = 150),
                               bsTooltip("mdec_lat_site", "Please enter the latitude in decimal point form. Northern latitudes as positive values, and southern as negative.",
                                         "bottom", options = list(container = "body"))
                        ),
                        column(2, numericInput("mdec_lon_site", "Longitude", 0, min = -180, max = 180, width = 150),
                               bsTooltip("mdec_lon_site", "Please enter the longitude in decimal point form. Eastern longitudes as positive values, and western as negative.",
                                         "bottom", options = list(container = "body"))
                        ),
                        column(2, dateInput("mdec_date", "Date", value = NULL, min = "1590-01-01", max = "2019-12-31", width = 150, format="dd/mm/yyyy"),
                               bsTooltip("mdec_date", "Please enter the date when the measurements were taken.",
                                         "bottom", options = list(container = "body")))
                      ),
                      
                      bsCollapse(id = "magdec", multiple = TRUE, open = "IGRF Model",
                                 bsCollapsePanel("IGRF Model", 
                                                 textOutput('mdec_igrf'),
                                                 a('For more information click here', href= 'https://www.ngdc.noaa.gov/geomag-web/'),
                                                 style = "primary"),
                                 
                                 bsCollapsePanel("Radial Technique", 
                                                 fluidRow(
                                                   column(2, actionButton("help_mdec", "Read Me First!")),
                                                   bsModal("man_magdec", "Instructions :: Radial Technique", "help_mdec", size="large",
                                                           includeMarkdown("man/man_magdec.md")
                                                   )
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(12, helpText("Insert the geo-coordinates of your reference points, as well as their azimuth (as measured from the site)")) ),
                                                 fluidRow(
                                                   column(2, numericInput("mdec_lat_ref1", "Latitude", 0, min = -90, max = 90, width = 150),
                                                          bsTooltip("mdec_lat_ref1", "Please enter the latitude in decimal point form. Northern latitudes as positive values, and southern as negative.",
                                                                    "bottom", options = list(container = "body"))
                                                   ),
                                                   column(2, numericInput("mdec_lon_ref1", "Longitude", 0, min = -180, max = 180, width = 150),
                                                          bsTooltip("mdec_lon_ref1", "Please enter the longitude in decimal point form. Eastern longitudes as positive values, and western as negative.",
                                                                    "bottom", options = list(container = "body"))
                                                   ),
                                                   column(2, numericInput("mdec_az_ref1", "Azimuth", 0, min = 0, max = 360, width = 150),
                                                          bsTooltip("mdec_az_ref1", "Please enter the azimuth in decimal point form.",
                                                                    "bottom", options = list(container = "body"))
                                                   )
                                                 ),
                                                 tags$div(id = 'placeholder'),
                                                 
                                                 fluidRow(
                                                   column(2, actionButton('mdec_plus',"+", width=50),
                                                          bsTooltip("mdec_plus", "Add another reference site.",
                                                                    "right", options = list(container = "body"))
                                                   ),
                                                   column(2,""),
                                                   column(2, actionButton('mdec_calc',"Calculate", width=150))
                                                 ),
                                                 br(),
                                                 verbatimTextOutput('mdec_out'),
                                                 style = "default")
                                 
                                 
                      )
                      
                    
                    )
           ),
           
           tabPanel("Solar Declination Table",
                    mainPanel(
                      fluidRow(
                        column(1, "Epoch:"),
                        column(3, numericInput("sundec.epoch", label = NULL, value=2000, width=100, step=100)),
                        column(3, downloadButton("sundec.down", "Download"))
                      ),
                      fluidRow(
                        helpText("Declination values calculated for noon on given day. Dates given in the proleptic Gregorian calendar.")
                      ),
                      
                      hr(),
                      
                      tableOutput("sundec")
                    )
                    
                    
                    # sidebarPanel(
                    #   numericInput("sundec.epoch", label = "Epoch", value=2000, width=100, step=100),
                    #   downloadButton("sundec.down", "Download"),
                    #   helpText("Declination values calculated for noon on given day. Dates given in the proleptic Gregorian calendar.")
                    # ),
                    # mainPanel(tableOutput("sundec"))
           )
           
)

