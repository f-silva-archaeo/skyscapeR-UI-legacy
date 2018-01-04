# Tools -------------------------------------------------------------
# Tools :: Solar Declination Table -------------------------------------------------------------
sundec <- reactive({
  year <- input$sundec.epoch
  jd0 <- jdcnv(year,1,1,12)
  yy <- seq(jd0,length.out = 365)
  ss <- sunpos(yy)$dec
  
  months <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  kk <- 1
  cc <- matrix("",31,12)
  for (mm in 1:NROW(months)) {
    for (dd in 1:months[mm]) {
      cc[dd,mm] <- round(ss[kk],2)
      kk <- kk + 1
    }
  }
  colnames(cc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  rownames(cc) <- 1:31
  
  return(cc)
})

output$sundec <- renderTable({
  sundec()
}, rownames=T)

output$sundec.down <- downloadHandler(
  filename = function() { paste0('sundec_', input$sundec.epoch, '.csv') },
  content = function(file) {
    write.csv(sundec(), file, row.names=T)
  }
)