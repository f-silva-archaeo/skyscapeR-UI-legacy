# Tools -------------------------------------------------------------
# Tools :: Magnetic Declination -------------------------------------------------------------
n.refs <- 1

observeEvent(input$mdec_plus, {
  n.refs <<- n.refs + 1
  btn <- input$mdec_plus
  id <- paste0('txt', btn)
  insertUI(
    selector = '#placeholder',
    # TODO bsTooltip not working
    ui = tags$div(
      fluidRow(
        column(2, numericInput(paste0("mdec_lat_ref",n.refs), "Latitude", 0, min = -90, max = 90, width = 150),
               bsTooltip(paste0("mdec_lat_ref",n.refs), "Please enter the latitude in decimal point form. Northern latitudes as positive values, and southern as negative.",
                         "bottom", options = list(container = "body"))
        ),
        column(2, numericInput(paste0("mdec_lon_ref",n.refs), "Longitude", 0, min = -180, max = 180, width = 150),
               bsTooltip(paste0("mdec_lon_ref",n.refs), "Please enter the longitude in decimal point form. Eastern longitudes as positive values, and western as negative.",
                         "bottom", options = list(container = "body"))
        ),
        column(2, numericInput(paste0("mdec_az_ref",n.refs), "Azimuth", 0, min = 0, max = 360, width = 150),
               bsTooltip(paste0("mdec_az_ref",n.refs), "Please enter the azimuth in decimal point form.",
                         "bottom", options = list(container = "body"))
        )
      ),
      id = id
    )
  )
})

observeEvent(input$mdec_calc, {
  magDec <- c()
  for (i in 1:n.refs) {
    lat.ref <- eval(parse(text=paste0("input$mdec_lat_ref",i)))
    lon.ref <- eval(parse(text=paste0("input$mdec_lon_ref",i)))
    az.ref <- eval(parse(text=paste0("input$mdec_az_ref",i)))
    bearingGPS <- atan2(sinpi((lon.ref-input$mdec_lon_site)/180)*cospi(lat.ref/180), cospi(input$mdec_lat_site/180)*sinpi(lat.ref/180)-sinpi(input$mdec_lat_site/180)*cospi(lat.ref/180)*cospi((lon.ref-input$mdec_lon_site)/180))
    magDec[i] <- (bearingGPS*180/pi + 360) %% 360 - az.ref
  }
  
  output$mdec_out <- renderText({
    if (n.refs > 1) {
      paste0("Mag Dec: ", round(mean(magDec),2), "º \nStd Error: ", round(sd(magDec)/sqrt(n.refs),2), "º")
    } else { paste0("Mag Dec: ", round(mean(magDec),2),"º") }
  })
  
})

output$mdec_igrf <- renderText({
  paste0("Mag Dec: ", round(magneticField(input$mdec_lon_site,input$mdec_lat_site,as.POSIXlt(as.date(as.character(input$mdec_date), order="ymd")))$dec,2),"º")
})