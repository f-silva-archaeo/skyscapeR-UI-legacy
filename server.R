###########################################
## skyscapeR SERVER
##
## (C) Fabio Silva 2016
###########################################

function(input, output, session) {
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