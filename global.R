###########################################
## skyscapeR GLOBAL
##
## (C) Fabio Silva 2017
###########################################

## Libraries
library(shiny)
library(shinyBS)
library(astrolibR)
library(palinsol)
library(gdata)
library(data.table)
library(markdown)
library(png)
library(oce)
library(RColorBrewer)
library(date)
library(readxl)
library(WriteXLS)

## Variables
version <- "v0.4"
year <- as.numeric(substr(Sys.time(),1,4))
cpal <- brewer.pal(9,'Purples')[2:9]; cpal <- rev(cpal); cpal <- rep(cpal,3) # Colour palette for custom decs
spal <- brewer.pal(9,'Reds')[2:9]; spal <- rev(spal); spal <- rep(spal,3) # Colour palette for custom decs

## Param
options(warn=-1)


## Data
load(file.path("data","decs.RData"))
load(file.path("data","Stars.RData"))
dataTemplate <- data.frame(ID="001", 
                           Name="Example 1", 
                           Lat=35.45, Lon=-8.67, 
                           HWT.ID= "K5JP31AA",                      # HeyWhatsThat ID
                           Mag.Az= 110, Mag.Az.unc= 10,             # Magnetic Azimuth and uncertainty
                           Mag.Dec= NA, Survey.date= "01/06/2017",  # Magnetic Declination and date of fieldwork
                           Az=NA, Az.unc=NA,                        # True Azimuth and uncertainty
                           Alt=3, Alt.unc=0.5,                      # Horizon Altitude and uncertainty
                           Dec= NA, Dec.unc= NA,                    # Declination and uncertainty
                           Notes="This is an example of the data you can include.")

## Global functions
source('skyscapeR.R')

mycss <- "
  #plot-container {
    position: relative;
  }
  
  #loading-spinner {
    position: absolute;
    left: 50%;
    top: 50%;
    z-index: -1;
    margin-top: -33px;  /* half of the spinner's height */
    margin-left: -33px; /* half of the spinner's width */
  }

  #plot.recalculating {
    z-index: -2;
  }
"
