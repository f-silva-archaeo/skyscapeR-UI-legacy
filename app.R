###########################################
## skyscapeR app
##
## (C) Fabio Silva 2016
###########################################

global <- function() { 
  source(file.path("global.R"), local=TRUE)$value
}

ui <- navbarPage("skyscapeR v0.3b",
                 theme = shinythemes::shinytheme("darkly"),
                 
                 source(file.path("ui","about.R"), local = TRUE)$value,
                 source(file.path("ui","entry.R"), local = TRUE)$value,
                 source(file.path("ui","viz.R"), local = TRUE)$value,
                 source(file.path("ui","horizon.R"), local = TRUE)$value,
                 source(file.path("ui","tools.R"), local = TRUE)$value,
                 source(file.path("ui","help.R"), local = TRUE)$value
                 
)

server <- function(input, output, session) {
  source(file.path("server","entry.R"), local = TRUE)$value
  source(file.path("server","viz.R"), local = TRUE)$value
  source(file.path("server","horizon.R"), local = TRUE)$value
  source(file.path("server","tools_unit.R"), local = TRUE)$value
  source(file.path("server","tools_coord.R"), local = TRUE)$value
  source(file.path("server","tools_alt.R"), local = TRUE)$value
  source(file.path("server","tools_mag.R"), local = TRUE)$value
  source(file.path("server","tools_solar.R"), local = TRUE)$value
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server, onStart = global)