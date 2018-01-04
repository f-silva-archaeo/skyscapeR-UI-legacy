# Horizon -------------------------------------------------------------

hor <- reactive ({
  validate(
    need(nchar(input$HWTID)==8,"Please enter a valid HWT ID")
  )
  HWTID <- input$HWTID
  if (nchar(HWTID) > 0) {
    hor <- HWT.download(HWTID)
    return(hor)
  } else { return(NULL) }
})

output$hor <- renderText({
  req(input$HWTID)
  paste0("Site: ",hor()$Name, "\nLatitude: ",abs(hor()$Lat), if(hor()$Lat>0) {" N"} else {" S"}, "\nLongitude: ",abs(hor()$Lon), if(hor()$Lon>0) {" E"} else {" W"}, "\nElevation: ",hor()$Elev*0.3, " m", "\nID: ", hor()$ID)
})

output$hor.info <- renderText({
  req(input$hor_click)
  az <- round(as.numeric(input$hor_click$x),2) %% 360
  alt.f <- splinefun(hor()$az,hor()$alt)
  alt <- round(alt.f(az),2)
  paste0("[ Az = ", az, "º, Hor Alt = ", alt, "º ]")
})

output$go_imp <- renderUI({
  ## List available options (only available HWT.IDs)
  choice <- c()
  if (is.data.frame(data())) {
    ind <- which(!is.na(data()$HWT.ID))
    if (NROW(ind)>0) {
      choice <- paste(data()$HWT.ID[ind])
      names(choice) <- paste(data()$Name[ind])
    }
  }
  
  tagList(
    selectizeInput('HWTID', 'Select or type in a HeyWhatsThat ID', choices= choice, options = list(create = TRUE, onInitialize = I('function() { this.setValue(" "); }')))
  )
  
})


# Display Measurements ----------------------------------------------------
output$mes_sel <- renderUI({
  choices <- "No Az-Alt data available. Please upload or select a different dataset"
  sel <- FALSE
  if (is.data.frame(data())) {
    ind <- which(!is.na(data()$Az))
    if (NROW(ind)>0) {
      choices <- c("Select All", paste(data()$Name[ind]))
      # names(choice) <- paste(data()$Name[ind])
    }
    
    ## Auto-select value with same HWT ID, if available
    ind <- which(data()$HWT.ID[ind] == hor()$ID)
    if (NROW(ind)>0) { sel <- choices[ind+1] } else { sel <- FALSE }
  }
  
  tagList(
    selectInput("mes_choice_dat", label = "Choose Measurements:", choices, multiple=T, selected=sel),
    
    fixedRow(
      column(6, numericInput("mes_choice_dat_az", "Uncertainty in Azimuth:",3, min=0, width = 100) ),
      bsTooltip("mes_choice_dat_az", "This will be used for measurements without uncertainty, or if larger than Az.unc", "right", options = list(container = "body")),
      column(6, numericInput("mes_choice_dat_alt", "Uncertainty in Altitude:",3, min=0, width = 100) ),
      bsTooltip("mes_choice_dat_alt", "This will be used for measurements without uncertainty, or if larger than Alt.unc", "right", options = list(container = "body"))
    ),
    fixedRow( column(8, radioButtons("mes_choice_dat_viz", "Show uncertainty as:", choices=c("error-bar", "window"), selected = "error-bar"), offset=2) )
  )
})


# Display Measurements :: select all --------------------------------------
observe({
  if ("Select All" %in% input$mes_choice_dat) {
    ind <- which(!is.na(data()$Az))
    dat <- data()[ind,]
    choices <- c("Select All", paste(dat$Name))
    selected_choices <- setdiff(choices, "Select All")
    updateSelectInput(session, "mes_choice_dat", selected = selected_choices)
  }
})


# Orbits :: calculate -----------------------------------------------------  
orbits <- reactive({
  orbs <- list(); orbs2 <- list()
  n <- 1; m <- 1
  
  validate(
    need(is.numeric(input$cel_hor_From), "The field 'Epoch From' in the 'Display Celestial Features' box cannot be empty"),
    need(is.numeric(input$cel_hor_To), "The field 'Epoch To' in the 'Display Celestial Features' box cannot be empty")
  )
  
  if (NROW(input$cel_hor_celestial) > 0) {
    if (sum(input$cel_hor_celestial == "sol")) {
      orb <- orbit(WS(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "darkblue"; orb$name <- "sol"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(SS(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "darkblue"; orb$name <- "sol"; orbs[[n]] <- orb; n <- n+1
      # if (input$cel_hor_To != input$cel_hor_From) { 
        orb <- orbit(WS(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "darkblue"; orb$name <- "sol"; orbs2[[m]] <- orb; m <- m+1 
        orb <- orbit(SS(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "darkblue"; orb$name <- "sol"; orbs2[[m]] <- orb; m <- m+1 
      # }
    }
    
    if (sum(input$cel_hor_celestial == "equ")) {
      orb <- orbit(0, hor()$Lat, hor()$Lon); orb$col <- "blue"; orb$name <- "equ"; orbs[[n]] <- orb; n <- n+1
      # if (input$cel_hor_To != input$cel_hor_From) { 
        orb <- orbit(0, hor()$Lat, hor()$Lon); orb$col <- "blue"; orb$name <- "equ"; orbs2[[m]] <- orb; m <- m+1
      # }
    }
    
    if (sum(input$cel_hor_celestial == "minlex")) {
      orb <- orbit(nmLX(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "green"; orb$name <- "minlex"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(smLX(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "green"; orb$name <- "minlex"; orbs[[n]] <- orb; n <- n+1

      # if (input$cel_hor_To != input$cel_hor_From) { 
        orb <- orbit(nmLX(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "green"; orb$name <- "minlex"; orbs2[[m]] <- orb; m <- m+1
        orb <- orbit(smLX(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "green"; orb$name <- "minlex"; orbs2[[m]] <- orb; m <- m+1
      # }
    }
    
    if (sum(input$cel_hor_celestial == "majlex")) {
      orb <- orbit(nMLX(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "darkgreen"; orb$name <- "majlex"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(sMLX(input$cel_hor_From), hor()$Lat, hor()$Lon); orb$col <- "darkgreen"; orb$name <- "majlex"; orbs[[n]] <- orb; n <- n+1
      
      # if (input$cel_hor_To != input$cel_hor_From) { 
        orb <- orbit(nMLX(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "darkgreen"; orb$name <- "majlex"; orbs2[[m]] <- orb; m <- m+1
        orb <- orbit(sMLX(input$cel_hor_To), hor()$Lat, hor()$Lon); orb$col <- "darkgreen"; orb$name <- "majlex"; orbs2[[m]] <- orb; m <- m+1
      # }
    }
    
    if (sum(input$cel_hor_celestial == "sfm")) {
      aux <- c(SFM(input$cel_hor_From), SFM(input$cel_hor_To))
      min.sfm <- min(aux); max.sfm <- max(aux)
      
      orb <- orbit(min.sfm, hor()$Lat, hor()$Lon); orb$col <- "yellow"; orb$name <- "sfm"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(max.sfm, hor()$Lat, hor()$Lon); orb$col <- "yellow"; orb$name <- "sfm"; orbs2[[m]] <- orb; m <- m+1
      
      orb <- orbit(aux[2], hor()$Lat, hor()$Lon); orb$col <- "yellow"; orb$name <- "sfm"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(aux[5], hor()$Lat, hor()$Lon); orb$col <- "yellow"; orb$name <- "sfm"; orbs2[[m]] <- orb; m <- m+1
    }
    
    if (sum(input$cel_hor_celestial == "afm")) {
      aux <- c(AFM(input$cel_hor_From), AFM(input$cel_hor_To))
      min.afm <- min(aux); max.afm <- max(aux)
      
      orb <- orbit(min.afm, hor()$Lat, hor()$Lon); orb$col <- "magenta"; orb$name <- "afm"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(max.afm, hor()$Lat, hor()$Lon); orb$col <- "magenta"; orb$name <- "afm"; orbs2[[m]] <- orb; m <- m+1
      
      orb <- orbit(aux[2], hor()$Lat, hor()$Lon); orb$col <- "magenta"; orb$name <- "afm"; orbs[[n]] <- orb; n <- n+1
      orb <- orbit(aux[5], hor()$Lat, hor()$Lon); orb$col <- "magenta"; orb$name <- "afm"; orbs2[[m]] <- orb; m <- m+1
    }
  }
  
  if (NROW(input$cel_hor_Stars) > 0) {
    for (i in 1:NROW(input$cel_hor_Stars)) {
      orb <- orbit(palaeosky(star(input$cel_hor_Stars[i]), input$cel_hor_From)$dec,  hor()$Lat, hor()$Lon); orb$col <- spal[i]; orb$name <- input$cel_hor_Stars[i]; orbs[[n]] <- orb; n <- n+1
      # if (input$cel_hor_To != input$cel_hor_From) {
        dd <- stardec.min.max(star(input$cel_hor_Stars[i]), input$cel_hor_From, input$cel_hor_To)
        orb <- orbit(dd[1],  hor()$Lat, hor()$Lon); orb$col <- spal[i]; orb$name <- input$cel_hor_Stars[i]; orbs[[n-1]] <- orb # replace orb
        orb <- orbit(dd[2],  hor()$Lat, hor()$Lon); orb$col <- spal[i]; orb$name <- input$cel_hor_Stars[i]; orbs2[[m]] <- orb; m <- m+1
      # }
    }
  }
  
  if (nchar(input$cel_hor_Custom) > 0) {
    custom.decs <- as.numeric(unlist(strsplit(input$cel_hor_Custom, ";")))
    custom.decs <- custom.decs[!is.na(custom.decs)]
    for (i in 1:NROW(custom.decs)) {
      orb <- orbit(custom.decs[i], hor()$Lat, hor()$Lon); orb$col <- cpal[i]; orb$name <- paste0("Custom: ", custom.decs[i]); orbs[[n]] <- orb; n <- n+1
      # if (input$cel_hor_To != input$cel_hor_From) { 
        orbs2[[m]] <- orb; m <- m+1 
      # }
    }
  }
  
  return(list(From = orbs, To = orbs2))
})


# Output :: horizon plot --------------------------------------------------
output$horizon <- renderPlot({
  if (!is.null(hor())) {
    title <- paste0("Site: ",hor()$Name)
    
    layout(c(1,2),widths=c(1,1),heights=c(4,1))
    par(mar=c(5,2,2,1))
    plot.horizon(hor(), start=180-input$hor.slider, main=title)
    mtext(paste0('skyscapeR ',version,' Fabio Silva (2017)'),3, adj=0, cex=0.5)
    
    ## Display Celestial Features
    if (NROW(orbits()$From) > 0) {
      ind <- sort(unlist(lapply(orbits()$From, '[[', 3)), index.return=T)$ix
      for (i in ind) {
        if (NROW(orbits()$To) > 0) { 
          plot.orbit.range(orbits()$From[[i]], orbits()$To[[i]], col=orbits()$From[[i]]$col)
        } else {
          plot.orbit(orbits()$From[[i]], col=orbits()$From[[i]]$col) 
        }
      }
    }
    par(new=T)
    plot.horizon(hor(), start=180-input$hor.slider, main=title)
    
    
    ## Display Measurements
    if (NROW(input$mes_choice_dat)>0) {
      ind <- which(data()$Name == input$mes_choice_dat)
      unc.az <- pmax(data()$Az.unc[ind], rep(input$mes_choice_dat_az,NROW(data())), na.rm=T)
      unc.alt <- pmax(data()$Alt.unc[ind], rep(input$mes_choice_dat_alt,NROW(data())), na.rm=T)
      if (input$mes_choice_dat_viz == "error-bar") {
        points(data()$Az[ind], data()$Alt[ind], pch=18)
        arrows(data()$Az[ind]-unc.az, data()$Alt[ind], data()$Az[ind]+unc.az, data()$Alt[ind], length=0.05, angle=90, code=3)
        arrows(data()$Az[ind], data()$Alt[ind]-unc.alt, data()$Az[ind], data()$Alt[ind]+unc.alt, length=0.05, angle=90, code=3)
      
        points(data()$Az[ind]-360, data()$Alt[ind], pch=18)
        arrows(data()$Az[ind]-unc.az-360, data()$Alt[ind], data()$Az[ind]+unc.az-360, data()$Alt[ind], length=0.05, angle=90, code=3)
        arrows(data()$Az[ind]-360, data()$Alt[ind]-unc.alt, data()$Az[ind]-360, data()$Alt[ind]+unc.alt, length=0.05, angle=90, code=3)
      
        points(data()$Az[ind]+360, data()$Alt[ind], pch=18)
        arrows(data()$Az[ind]-unc.az+360, data()$Alt[ind], data()$Az[ind]+unc.az+360, data()$Alt[ind], length=0.05, angle=90, code=3)
        arrows(data()$Az[ind]+360, data()$Alt[ind]-unc.alt, data()$Az[ind]+360, data()$Alt[ind]+unc.alt, length=0.05, angle=90, code=3)
        
        } else {
          rect(data()$Az[ind]-unc.az-360, data()$Alt[ind]-unc.alt, data()$Az[ind]+unc.az-360, data()$Alt[ind]+unc.alt)
          rect(data()$Az[ind]-unc.az, data()$Alt[ind]-unc.alt, data()$Az[ind]+unc.az, data()$Alt[ind]+unc.alt)
          rect(data()$Az[ind]-unc.az+360, data()$Alt[ind]-unc.alt, data()$Az[ind]+unc.az+360, data()$Alt[ind]+unc.alt)
      }
    }
    
    # Colour Legend -------------------------------------------------------------
    par(mar=c(1,2,1,1))
    plot(-100,-100,xlim=c(180-input$hor.slider,180-input$hor.slider+360), yaxs='i', xaxs='i', ylim=c(0,10), axes=F)
    col <- c()
    label <- c()
    if (sum(input$cel_hor_celestial == "sol")) {
      label <- c(label, "Solstices")
      col <- c(col, "darkblue")
    }
    
    if (sum(input$cel_hor_celestial == "equ")) {
      label <- c(label, "Equinoxes")
      col <- c(col, "blue")
    }
    
    if (sum(input$cel_hor_celestial == "minlex")) {
      label <- c(label, "minor Lunar Extremes")
      col <- c(col, "green")
    }
    
    if (sum(input$cel_hor_celestial == "majlex")) {
      label <- c(label, "major Lunar Extremes")
      col <- c(col, "darkgreen")
    }
    
    if (sum(input$cel_hor_celestial == "sfm")) {
      label <- c(label, "Spring Full Moon")
      col <- c(col, "yellow")
    }
    
    if (sum(input$cel_hor_celestial == "afm")) {
      label <- c(label, "Autumn Full Moon")
      col <- c(col, "magenta")
    }
    
    if (NROW(input$cel_hor_Stars) > 0) {
      for (i in 1:NROW(input$cel_hor_Stars)) {
        label <- c(label, input$cel_hor_Stars[i])
        col <- c(col, spal[i])
      }
    }
    
    if (nchar(input$cel_hor_Custom) > 0) {
      custom.decs <- as.numeric(unlist(strsplit(input$cel_hor_Custom, ";")))
      custom.decs <- custom.decs[!is.na(custom.decs)]
      for (i in 1:NROW(custom.decs)) {
        label <- c(label, paste0("Custom: ",custom.decs[i]))
        col <- c(col, cpal[i])
      }
    }
    
    if (!is.null(label)) { legend("top", legend=label, lty=1, lwd=3, col=col, bty="o", ncol=5, cex=0.8) }
    
  }
})


# Output :: download horizon ----------------------------------------------
output$download.hor <- downloadHandler(
  filename = function() {
    paste(hor()$Name, "zip", sep=".")
  },
  
  content = function(file) {
    curdir <- getwd()
    setwd(tempdir())
    
    horExport(hor(), name=hor()$Name, author="skyscapeR (c) Fabio Silva 2016")
    zip(zipfile=file, files=c("landscape.ini", "horizon.txt"))
    setwd(curdir)
  },
  contentType = "application/zip"
)