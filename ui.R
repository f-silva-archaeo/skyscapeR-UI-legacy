###########################################
## skyscapeR UI
##
## (C) Fabio Silva 2016
###########################################

navbarPage("skyscapeR v0.4b",
           theme = shinythemes::shinytheme("darkly"),
           
           source(file.path("ui","about.R"), local = TRUE)$value,
           source(file.path("ui","entry.R"), local = TRUE)$value,
           source(file.path("ui","viz.R"), local = TRUE)$value,
           source(file.path("ui","horizon.R"), local = TRUE)$value,
           source(file.path("ui","tools.R"), local = TRUE)$value,
           source(file.path("ui","help.R"), local = TRUE)$value
           
)