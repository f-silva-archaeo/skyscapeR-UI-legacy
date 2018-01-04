# Data Visualization -------------------------------------------------------------
output$plot.info <- renderText({
  req(input$plot_click)
  paste0("[ Dec = ", round(as.numeric(input$plot_click$x) ,2), "º ]") #, Height = ", round(as.numeric(input$plot_click$y),2), ")")
})

output$plot <- renderPlot({
  if (!is.data.frame(data())) {
    "Please upload or select a dataset in the Data Entry tab"
  } else {
    
    x.min <- round(min(data()[['Dec']])/5,0)*5 - 10
    x.max <- round(max(data()[['Dec']])/5,0)*5 + 10
    if (input$chk_hist == T) {
      bb <- seq(x.min+input$vis.hist_start, x.max, input$vis.hist_size)
      h <- hist(data()[['Dec']], freq=T, breaks=bb, plot=F)$counts
      y.max <- max(h)
    } else { y.max <- 1}
    
    layout(c(1,2),widths=c(1,1),heights=c(4,1))
    par(mar=c(5,1,2,1))
    plot(-999,-999, xlim=c(x.min,x.max), ylim=c(0,y.max), xlab="Declination (º)", ylab="", axes=F)
    mtext(paste0('skyscapeR ',version,' Fabio Silva (2017)'), 3, adj=0, cex=0.5)
    axis(1, at=seq(x.min,x.max, 5), labels = T)
    
    # Histogram -------------------------------------------------------------
    if (input$chk_hist == T) {
      n <- NROW(data())
      bb <- seq(x.min+input$vis.hist_start, x.max, input$vis.hist_size)
      hist(data()[['Dec']], 
           main=paste0('N=',n,' datapoints'), col="grey", add=T, freq=T, breaks=bb)
    }
    
    # Data Points -------------------------------------------------------------
    if (input$chk_dots == T) {
      xx <- (1:NROW(data()[['Dec']]))/NROW(data()[['Dec']])*y.max
      unc <- pmax(data()[['Dec.unc']], rep(input$vis_dot_unc,NROW(data())), na.rm=T)
      points(data()[['Dec']], xx, pch=19)
      arrows(data()[['Dec']]-unc, xx, data()[['Dec']]+unc, xx, length=0.05, angle=90, code=3)
    }
    
    # SPD -------------------------------------------------------------
    if (input$chk_spd == T) {
      unc <- pmax(data()[['Dec.unc']], rep(input$vis_spd_unc,NROW(data())), na.rm=T)
      spd <- SPD(as.matrix(data()[['Dec']]),unc)
      lines(spd$dec, spd$density*y.max, col='black', lwd=2)
    }
    
    # Celestial Features -------------------------------------------------------------
    validate(
      need(is.numeric(input$cel_vis_From), "The field 'Epoch From' in the 'Display Celestial Features' box cannot be empty"),
      need(is.numeric(input$cel_vis_To), "The field 'Epoch To' in the 'Display Celestial Features' box cannot be empty")
    )
    
    if (NROW(input$cel_vis_celestial) > 0) {
      
      if (sum(input$cel_vis_celestial == "sol")) {
        xx <- seq(WS(input$cel_vis_From),WS(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('darkblue',alpha.f=0.5), border='darkblue')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='darkblue') }
        
        xx <- seq(SS(input$cel_vis_From),SS(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('darkblue',alpha.f=0.5), border='darkblue')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='darkblue') }
      }
      
      if (sum(input$cel_vis_celestial == "equ")) { 
        lines(rep(0,2),c(-10,y.max), lty=3, col='blue') 
      }
      
      if (sum(input$cel_vis_celestial == "minlex")) {
        xx <- seq(nmLX(input$cel_vis_From),nmLX(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('green',alpha.f=0.5), border='green')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='green') }
        
        xx <- seq(smLX(input$cel_vis_From),smLX(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('green',alpha.f=0.5), border='green')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='green') }
      }
      
      if (sum(input$cel_vis_celestial == "majlex")) {
        xx <- seq(nMLX(input$cel_vis_From),nMLX(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('darkgreen',alpha.f=0.5), border='darkgreen')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='darkgreen') }
        
        xx <- seq(sMLX(input$cel_vis_From),sMLX(input$cel_vis_To), length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('darkgreen',alpha.f=0.5), border='darkgreen')
        } else { lines(rep(xx[1],2),c(-10,y.max), col='darkgreen') }
      }
      
      if (sum(input$cel_vis_celestial == "sfm")) {
        aux <- c(SFM(input$cel_vis_From), SFM(input$cel_vis_To))
        min.sfm <- min(aux); max.sfm <- max(aux)
        xx <- seq(min.sfm, max.sfm, length.out=1000)
      
        y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
        x.polygon <- c(xx, rev(xx))
        polygon(x.polygon, y.polygon, col=adjustcolor('yellow',alpha.f=0.2), border='yellow')
        
        if (abs(diff(c(aux[2],aux[5])))>0) {
          xx <- seq(aux[2], aux[5], length.out=1000)
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('yellow',alpha.f=0.5), border='yellow')
        } else { lines(rep(aux[2],2),c(-10,y.max), col='yellow') }
      }
      
      if (sum(input$cel_vis_celestial == "afm")) {
        aux <- c(AFM(input$cel_vis_From), AFM(input$cel_vis_To))
        min.afm <- min(aux); max.afm <- max(aux)
        xx <- seq(min.afm, max.afm, length.out=1000)
        
        y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
        x.polygon <- c(xx, rev(xx))
        polygon(x.polygon, y.polygon, col=adjustcolor('magenta',alpha.f=0.2), border='magenta')
        
        if (abs(diff(c(aux[2],aux[5])))>0) {
          xx <- seq(aux[2], aux[5], length.out=1000)
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor('magenta',alpha.f=0.5), border='magenta')
        } else { lines(rep(aux[2],2),c(-10,y.max), col='magenta') }
        
      }
    }
    
    
    if (NROW(input$cel_vis_Stars) > 0) {
      for (i in 1:NROW(input$cel_vis_Stars)) {
        xx <- seq(palaeosky(star(input$cel_vis_Stars[i]), input$cel_vis_From)$dec, palaeosky(star(input$cel_vis_Stars[i]), input$cel_vis_To)$dec, length.out=1000)
        if (abs(diff(xx[1:2]))>0) {
          y.polygon <- c(rep(-10,NROW(xx)), rep(y.max, NROW(xx)))
          x.polygon <- c(xx, rev(xx))
          polygon(x.polygon, y.polygon, col=adjustcolor(spal[i],alpha.f=0.5), border=spal[i])
        } else { lines(rep(xx[1],2),c(-10,y.max), col=spal[i]) }
      }
    }
    
    if (nchar(input$cel_vis_Custom) > 0) {
      custom.decs <- as.numeric(unlist(strsplit(input$cel_vis_Custom, ";")))
      custom.decs <- custom.decs[!is.na(custom.decs)]
      for (i in 1:NROW(custom.decs)) {
        lines(rep(custom.decs[i],2),c(-10,y.max), col=cpal[i])
      }
    }
    
    # Colour Legend -------------------------------------------------------------
    par(mar=c(1,1,1,1))
    plot(-100,-100,xlim=c(x.min,x.max), ylim=c(0,10), axes=F)
    col <- c()
    label <- c()
    type <- c()
    if (sum(input$cel_vis_celestial == "sol")) {
      label <- c(label, "Solstices")
      col <- c(col, "darkblue")
      type <- c(type,1)
    }
    
    if (sum(input$cel_vis_celestial == "equ")) {
      label <- c(label, "Equinoxes")
      col <- c(col, "blue")
      type <- c(type,3)
    }
    
    if (sum(input$cel_vis_celestial == "minlex")) {
      label <- c(label, "minor Lunar Extremes")
      col <- c(col, "green")
      type <- c(type,1)
    }
    
    if (sum(input$cel_vis_celestial == "majlex")) {
      label <- c(label, "major Lunar Extremes")
      col <- c(col, "darkgreen")
      type <- c(type,1)
    }
    
    if (sum(input$cel_vis_celestial == "sfm")) {
      label <- c(label, "Spring Full Moon")
      col <- c(col, "yellow")
      type <- c(type,1)
    }
    
    if (sum(input$cel_vis_celestial == "afm")) {
      label <- c(label, "Autumn Full Moon")
      col <- c(col, "magenta")
      type <- c(type,1)
    }
    
    if (NROW(input$cel_vis_Stars) > 0) {
      for (i in 1:NROW(input$cel_vis_Stars)) {
        label <- c(label, input$cel_vis_Stars[i])
        col <- c(col, spal[i])
        type <- c(type,1)
      }
    }
    
    if (nchar(input$cel_vis_Custom) > 0) {
      for (i in 1:NROW(custom.decs)) {
        label <- c(label, paste0("Custom: ",custom.decs[i]))
        col <- c(col, cpal[i])
        type <- c(type,1)
      }
    }
    
    if (!is.null(label)) { legend("top", legend=label, lty=type, lwd=3, col=col, bty="o", ncol=5, cex=0.8) }
    
  }
  
})