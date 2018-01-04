# Import Data -------------------------------------------------------------
data <- reactive({
  inFile <- input$file1
  inDataset <- input$dataset
  
  if (is.null(inFile) & (is.null(inDataset) | inDataset == "none")) {
    return("Please upload or select a dataset")
    
  } else if (!is.null(inFile)) {
    # Import file
    file.rename(from=inFile$datapath, to=paste0(inFile$datapath,".xlsx"))
    inFile$datapath <- paste0(inFile$datapath,".xlsx")
    aux <- read_excel(inFile$datapath)
    
    # Data Checking
    error <- dCheck(aux)
    
    if (error == " ") {
      # Data Calculations
      aux <- dCalc(aux)
      
      return(aux)
    } else { return(error) }
    
    
  } else if (inDataset != "none") {
    aux <- switch(inDataset,
                  "Hoskin ALEN" = 1,
                  "Belmonte Egypt" = 2,
                  "Ruggles RSC" = 3)
    aux2 <- dataTemplate[-1,]
    aux2[1:NROW(decs[[aux]]),] <- matrix(NA,NROW(decs[[aux]]),16)
    aux2$Dec <- decs[[aux]]$Dec
    if (!is.null(decs[[aux]]$ID)) { aux2$ID <- decs[[aux]]$ID }
    return(aux2)
  }
})


data.trim <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  } else {
    xcols <- c(1,2,3,4,5,10,11,12,13,14,15,16)
    ind <- c()
    for (i in 1:NROW(xcols)) {
      if (sum(is.na(data[xcols[i]]))<NROW(data)) { ind <- c(ind,i)}
    }
    cols <- xcols[ind]
    
    aux <- data.frame(data[,cols])
    colnames(aux) <- colnames(data)[cols]
    return(aux)
  }
}

output$error <- renderText({
  if (typeof(data()) == "character") {
    data()
  }
})



# Download Template -------------------------------------------------------------
output$downloadTemplate <- downloadHandler(
  filename = function() { 'template.xlsx' },
  content = function(file) {
    WriteXLS('dataTemplate', file)
  }
)


# Download Data -------------------------------------------------------------
output$downloadData <- downloadHandler(
  filename = function() { 'skyscapeR_data.xlsx' },
  content = function(file) {
    WriteXLS(data(), file)
  }
)


# Data Entry -------------------------------------------------------------
output$table <- renderTable({
  if (typeof(data()) != "character") {
    data.trim(data())
  }
}, bordered = F, hover = T, spacing = 'xs', digits = 2, na = 'missing')


# Data Checks -------------------------------------------------------------
dCheck <- function(data) {
  log <- c()
  
  if (typeof(data$Lat) != "double") { 
    ind <- which(is.na(as.numeric(data$Lat)))
    log <- c(log, paste0("Non-numeric latitudes found in row(s) ", paste(ind, collapse=", "), ". Please ensure there are no N or S symbols or other characters")) 
  }
  
  if (typeof(data$Lon) != "double") { 
    ind <- which(is.na(as.numeric(data$Lon)))
    log <- c(log, paste0("Non-numeric longitudes found in row(s) ", paste(ind, collapse=", "), ". Please ensure there are no W or E symbols or other characters")) 
  }
  
  if (typeof(data$Az) == "character") { 
    ind <- which(is.na(as.numeric(data$Az)))
    log <- c(log, paste0("Non-numeric azimuths found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Az field contains only numbers"))
  }
  
  if (typeof(data$Mag.Az) == "character") { 
    ind <- which(is.na(as.numeric(data$Mag.Az)))
    log <- c(log, paste0("Non-numeric azimuths found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Mag.Az field contains only numbers"))
  }
  
  if (typeof(data$Az.unc) == "character") { 
    ind <- which(is.na(as.numeric(data$Az.unc)))
    log <- c(log, paste0("Non-numeric uncertainties found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Az.unc field contains only numbers or is empty"))
  }
  
  if (typeof(data$Mag.Az.unc) == "character") { 
    ind <- which(is.na(as.numeric(data$Mag.Az.unc)))
    log <- c(log, paste0("Non-numeric uncertainties found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Mag.Az.unc field contains only numbers or is empty"))
  }
  
  if (typeof(data$Alt) == "character") { 
    ind <- which(is.na(as.numeric(data$Alt)))
    log <- c(log, "Non-numeric altitudes found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Alt field contains only numbers") 
  }
  
  if (typeof(data$Alt.unc) == "character") { 
    ind <- which(is.na(as.numeric(data$Alt.unc)))
    log <- c(log, paste0("Non-numeric uncertainties found in row(s) ", paste(ind, collapse=", "), ". Please ensure the Alt.unc field contains only numbers or is empty"))
  }
  
  ind <- which(nchar(data$HWT.ID) > 8)
  if (NROW(ind) > 0) {
    data$HWT.ID[ind] <- gsub(" ", "", data$HWT.ID[ind], fixed = TRUE)
  }
  ind <- which(nchar(data$HWT.ID) > 8)
  if (NROW(ind) > 0) {
    log <- c(log, paste0("Incorrect HWT.ID data found in row(s) ", paste(ind, collapse=", "), ". Please ensure that it is in correct format and no extra characters appear")) 
  }
  
  if (sum(is.na(as.numeric(data$Mag.Az)) & is.na(as.numeric(data$Az))) > 0) { 
    ind <- which(is.na(as.numeric(data$Az)) & is.na(as.numeric(data$Mag.Az)))
    log <- c(log, paste0("No azimuth data found in row(s) ", paste(ind, collapse=", "), ". Please ensure that at least one of the Az fields are populated")) 
  }
  
  if (sum(is.na(data$Alt)) > 0) { 
    ind <- which(is.na(as.numeric(data$Alt)))
    log <- c(log, paste0("No altitude data found in row(s) ", paste(ind, collapse=", "), ". Please ensure that the Alt field is populated")) 
  }
  
  if (sum(!is.na(data$Mag.Az) & is.na(data$Mag.Dec) & is.na(data$Az) & is.na(data$Survey.date)) > 0) {
    ind <- which(is.na(data$Mag.Dec) & is.na(data$Az) & is.na(data$Survey.date))
    log <- c(log, paste0("No Mag.Dec, Survey.date or Az data found in row(s) ", paste(ind, collapse=", "), ". Please ensure that at least one of these fields is populated")) 
  }
  
  
  # if (sum( is.na(data$Az) & !is.na(data$Mag.Az) & is.na(data$Mag.Dec) & is.na(as.date(data$Survey.date, order='dmy'))) > 0) {
  #   ind <- which(is.na(as.date(data$Survey.date, order='dmy')))
  #   log <- c(log, paste0("No Survey.date or incorrect format in row(s) ", paste(ind, collapse=", "), ". Please ensure that at least one of the Survey.date, Mag.Dec or Az fields is populated"))
  # }
  
  log <- c(log, " ")
  return(paste(log,collapse=".\n"))
}

# Data Calculations -------------------------------------------------------------
dCalc <- function(data) {
  jd <- jdcnv(2000, 1, 1, 0.)
  
  # data <- dataTemplate
  for (i in 1:NROW(data)) {
    test <- data[i,]
    
    ### Mag to True
    if (is.na(test$Az) & !is.na(test$Mag.Az)) {
      if (is.na(test$Mag.Dec)) {
        if (is.na(test$Survey.date)) { 
          test$Survey.date <- as.date(substr(Sys.time(),1,10), order="ymd") 
        } else {
          test$Survey.date <- as.date(test$Survey.date, order="dmy") 
        }
        test$Mag.Dec <- magneticField(test$Lon, test$Lat, as.POSIXlt(test$Survey.date))$dec
        test$Survey.date <- as.character(test$Survey.date)
      }
      test$Az <- round(test$Mag.Az + test$Mag.Dec,2)
      test$Az.unc <- test$Mag.Az.unc
    }
    
    ### Declination
    if (is.na(test$Dec)) {
      test$Dec <- round(hor2eq(test$Alt, test$Az, jd, test$Lat, test$Lon)$dec,2)
      
      if (!is.na(test$Alt.unc) | !is.na(test$Az.unc)) {
        dec.tt <- round(hor2eq(test$Alt+max(test$Alt.unc,0, na.rm=T), test$Az+max(test$Az.unc,0, na.rm=T), jd, test$Lat, test$Lon)$dec,2)
        dec.bb <- round(hor2eq(test$Alt-max(test$Alt.unc,0, na.rm=T), test$Az-max(test$Az.unc,0, na.rm=T), jd, test$Lat, test$Lon)$dec,2)
        dec.tb <- round(hor2eq(test$Alt+max(test$Alt.unc,0, na.rm=T), test$Az-max(test$Az.unc,0, na.rm=T), jd, test$Lat, test$Lon)$dec,2)
        dec.bt <- round(hor2eq(test$Alt-max(test$Alt.unc,0, na.rm=T), test$Az+max(test$Az.unc,0, na.rm=T), jd, test$Lat, test$Lon)$dec,2)
        dec.max <- max(c(dec.tt, dec.bb, dec.tb, dec.bt), na.rm=T)
        dec.min <- min(c(dec.tt, dec.bb, dec.tb, dec.bt), na.rm=T)
        test$Dec.unc <- mean(diff(c(dec.min,test$Dec,dec.max)))
      }
    }
    
    data[i,] <- test
  }
  
  return(data)
}
