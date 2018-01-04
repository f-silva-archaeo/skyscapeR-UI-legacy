###########################################
## skyscapeR FUNCTIONS
##
## (C) Fabio Silva 2016
###########################################

jd <- jdcnv(2000, 1, 1, 0.)  # J2000.0

###############################################################
SPD <- function(dec, sd) {
  if (NROW(sd)==1) { sd <- rep(sd, NROW(dec)) }
  
  xx <- seq(-90,90,by=0.01)
  spd <- array(0,NROW(xx))
  for (i in 1:NROW(dec)) {
    spd <- spd + dnorm(xx, dec[i], sd[i])
  }
  
  result <- c()
  result$dec <- xx
  result$density <- spd/max(spd)
  class(result) <- "SPD"
  return(result)
}

###############################################################
obliquity = function(t) {
  aux <- ber90(t-1950, degree=T)
  names(aux)<-NULL
  return(aux[1])
}

###############################################################
SFM = function(t) {
  sfm <- c(-15,-4,5)
  dob <- obliquity(2000)-obliquity(t)
  return(c(sfm[sfm<0]-dob,sfm[sfm>0]+dob))
}

###############################################################
AFM = function(t) {
  afm <- c(-5,4,15)
  dob <- obliquity(2000)-obliquity(t)
  return(c(afm[afm<0]-dob,afm[afm>0]+dob))
}

###############################################################
WS = function(t) {
  aux <- obliquity(t)
  return(-aux)
}

###############################################################
SS = function(t) {
  aux <- obliquity(t)
  return(aux[1])
}

###############################################################
nmLX = function(t) {
  aux <- obliquity(t)
  return(aux[1]-5.145)
}

###############################################################
smLX = function(t) {
  aux <- obliquity(t)
  return(-aux[1]+5.145)
}

###############################################################
nMLX = function(t) {
  aux <- obliquity(t)
  return(aux[1]+5.145)
}

###############################################################
sMLX = function(t) {
  aux <- obliquity(t)
  return(-aux[1]-5.145)
}

###############################################################
palaeosky = function(star, year) {
  if (class(star)=="star") {
    # precession
    trash <- capture.output(coor <- precess(star$ra,star$dec, 2000, year), file=NULL)
    
    # proper motion
    adj.pm <- star$proper.motion*(year-2000)/1000
    coor$ra <- coor$ra + ten(0,0,adj.pm[1])*sign(adj.pm[1])
    coor$dec <- coor$dec + ten(0,0,adj.pm[2])*sign(adj.pm[2])
    
    # nutation
    adj.nu <- co_nutate(jdcnv(year,1,1,0), star$ra, star$dec)
    coor$ra <- coor$ra + ten(0,0,adj.nu$d_ra)*sign(adj.nu$d_ra)
    coor$dec <- coor$dec + ten(0,0,adj.nu$d_dec)*sign(adj.nu$d_dec)
    
    # aberration
    adj.ab <- co_aberration(jdcnv(year,1,1,0), star$ra, star$dec, obliquity(year)/180*pi)
    coor$ra <- coor$ra + ten(0,0,adj.ab$d_ra)*sign(adj.ab$d_ra)
    coor$dec <- coor$dec + ten(0,0,adj.ab$d_dec)*sign(adj.ab$d_dec)
    
    # output
    star$ra <- coor$ra
    star$dec <- coor$dec
    star$epoch <- as.character(year)
    
    return(star) 
  } else { stop('No object of class Star detected. Please check man pages: ?palaeosky')}
  
}

#############################################
star <- function(string) {
  ind <- which(Stars$NAME == string)
  if (substr(string,1,3)=="HIP") { ind <- which(Stars$HIP.ID == string)}
  if (substr(string,1,2)=="HR") { ind <- which(Stars$HR.ID == string)}
  if (substr(string,1,3)=="M" & nchar(string)==3) { ind <- which(Stars$MESSIER.ID == string)}
  if (nchar(string)==5 & substr(string,2,2)==" ") { ind <- which(Stars$BAYER.DESIGNATION == string)}
  
  star <- c()
  star$name <- as.character(Stars$NAME[ind])
  star$constellation <- as.character(Stars$CONSTELLATION[ind])
  star$colour <- as.character(Stars$COLOUR[ind])
  star$app.mag <- Stars$VMAG[ind]
  star$ra <- Stars$RA[ind]
  star$dec <- Stars$DEC[ind]
  star$proper.motion <- c(Stars$PM_RA[ind],Stars$PM_DEC[ind])
  star$epoch <- "J2000.0"
  class(star) <- "star"
  return(star)
}


###############################################################
plot.horizon <- function(hor, plot.az=F, start=0, yl, ...) {
  # plot.new()
  
  # rejiggle so plot starts at given start point
  if (start < 0) {start <- start + 360}
  ind <- which(hor$az < start)
  if (NROW(ind)>0) {
    hor$az <- c(hor$az,hor$az[ind]+360); hor$alt <- c(hor$alt,hor$alt[ind])
  }
  
  if (missing(yl)) { yl <- c(floor(min(hor$alt, na.rm=T))-5,45) }
  
  if (plot.az) { 
    xx <- "azimuth" 
    ll <- seq(-360,720,by=90)
  } else { 
    xx <- ""
    ll <- c("N","E","S","W","N","E","S","W","N","E","S","W","N")
  }  
  plot.default(hor$az, hor$alt, type='l', xlab = xx, ylab = "Altitude (º)", yaxs='i', xaxs='i', ylim = yl, xlim = c(start,start+360), axes=F, ...)
  x <- c(hor$az,rev(hor$az))
  y <- c(hor$alt,rep(-20,NROW(hor$az)))
  polygon(x,y, col=rgb(217/255,95/255,14/255,1))
  
  axis(1, at = seq(-360,720,by=90), labels = ll)
  axis(1, at = seq(-315,720,by=90), labels = F, tcl=-0.25)
  axis(2, at = seq(0,yl[2],by=10))
  box()
}


###############################################################
plot.orbit <- function(orb, col, ...) {
  if (missing(col)) { col <- "red" }
  
    lines(orb$az, orb$alt, col=col)
    lines(orb$az-360, orb$alt, col=col)
    lines(orb$az+360, orb$alt, col=col)
    lines(orb$az+360*2, orb$alt, col=col)
    lines(orb$az-360*2, orb$alt, col=col)
}

###############################################################
test.open <- function(az,alt) {
  if (abs(az[1] - az[NROW(az)]) < 1) {
    return(FALSE)
  } else { return(TRUE)}
}

###############################################################
plot.orbit.range <- function(orbit1,orbit2, col, ...) {

  t <- c(NA,NA)
  t[1] <- test.open(orbit1$az,orbit1$alt)
  t[2] <- test.open(orbit2$az,orbit2$alt)
  
  if (sum(t == c(T,F)) ==2 | sum(t == c(F,T)) == 2) {
    aux <- c(orbit1$az+360*2,orbit1$az+360,orbit1$az,orbit1$az-360,orbit1$az-360*2)
    orbit1$az <- aux
    orbit1$alt <- rep(orbit1$alt,5)
    y.polygon <- c(orbit1$alt, rep(90,NROW(orbit1$alt)))
    x.polygon <- c(orbit1$az, rev(orbit1$az))
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    

    ind <- which.min(orbit2$az)
    tt2.az <- shift(c(orbit2$az,orbit2$az), ind, type='lead'); tt2.az <- tt2.az[-(NROW(orbit2$az):NROW(tt2.az))]
    tt2.alt <- shift(c(orbit2$alt,orbit2$alt), ind, type='lead'); tt2.alt <- tt2.alt[-(NROW(orbit2$alt):NROW(tt2.alt))]
    ind1 <- 1:which.max(tt2.az); ind2 <- (which.max(tt2.az)+1):NROW(tt2.az)
    y.polygon <- c(tt2.alt[ind1], rev(tt2.alt[ind2]))
    x.polygon <- c(tt2.az[ind1], tt2.az[ind2])
    polygon(x.polygon, y.polygon, col='white', border=col)
    
    x.polygon <- c(tt2.az[ind1]-360, tt2.az[ind2]-360)
    polygon(x.polygon, y.polygon, col='white', border=col)
    x.polygon <- c(tt2.az[ind1]-360*2, tt2.az[ind2]-360*2)
    polygon(x.polygon, y.polygon, col='white', border=col)
    
    x.polygon <- c(tt2.az[ind1]+360, tt2.az[ind2]+360)
    polygon(x.polygon, y.polygon, col='white', border=col)
    x.polygon <- c(tt2.az[ind1]+360*2, tt2.az[ind2]+360*2)
    polygon(x.polygon, y.polygon, col='white', border=col)
    
    
  } else if (sum(t == c(F,F)) == 2) {
    if (which.min(orbit2$az) < which.max(orbit2$az)) {
      aux <- orbit1
      orbit1 <- orbit2
      orbit2 <- aux
    }
    if ((orbit2$az[1] - tail(orbit2$az,1))<0.00001) { 
      orbit2$az <- orbit2$az[-1]
      orbit2$alt <- orbit2$alt[-1]
    }
    
    if ((orbit1$az[1] - tail(orbit1$az,1))<0.00001) { 
      orbit1$az <- orbit1$az[-1]
      orbit1$alt <- orbit1$alt[-1]
    }
    
    ind <- which.min(orbit1$az)
    tt1.az <- shift(c(orbit1$az,orbit1$az), ind, type='lead'); tt1.az <- tt1.az[-(NROW(orbit1$az):NROW(tt1.az))]
    tt1.alt <- shift(c(orbit1$alt,orbit1$alt), ind, type='lead'); tt1.alt <- tt1.alt[-(NROW(orbit1$alt):NROW(tt1.alt))]
    ind1 <- 1:which.max(tt1.az); ind2 <- (which.max(tt1.az)+1):NROW(tt1.az)
    y.polygon <- c(tt1.alt[ind1], rev(tt1.alt[ind2]))
    x.polygon <- c(tt1.az[ind1], tt1.az[ind2])
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    
    x.polygon <- c(tt1.az[ind1]-360, tt1.az[ind2]-360)
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    x.polygon <- c(tt1.az[ind1]-360*2, tt1.az[ind2]-360*2)
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    x.polygon <- c(tt1.az[ind1]+360, tt1.az[ind2]+360)
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    x.polygon <- c(tt1.az[ind1]+360*2, tt1.az[ind2]+360*2)
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    
    ind <- which.min(orbit2$az)
    tt2.az <- shift(c(orbit2$az,orbit2$az), ind, type='lead'); tt2.az <- tt2.az[-(NROW(orbit2$az):NROW(tt2.az))]
    tt2.alt <- shift(c(orbit2$alt,orbit2$alt), ind, type='lead'); tt2.alt <- tt2.alt[-(NROW(orbit2$alt):NROW(tt2.alt))]
    ind1 <- which(tt2.alt < orbit2$alt[ind]); ind2 <- which(tt2.alt >= orbit2$alt[ind])
    ind1 <- 1:which.max(tt2.az); ind2 <- (which.max(tt2.az)+1):NROW(tt2.az)
    y.polygon <- c(tt2.alt[ind1], rev(tt2.alt[ind2]))
    x.polygon <- c(tt2.az[ind1], tt2.az[ind2])
    polygon(x.polygon, y.polygon, col='white', border=col)
    
    x.polygon <- c(tt2.az[ind1]-360, tt2.az[ind2]-360)
    polygon(x.polygon, y.polygon, col='white', border=col)
    x.polygon <- c(tt2.az[ind1]-360*2, tt2.az[ind2]-360*2)
    polygon(x.polygon, y.polygon, col='white', border=col)
    x.polygon <- c(tt2.az[ind1]+360, tt2.az[ind2]+360)
    polygon(x.polygon, y.polygon, col='white', border=col)
    x.polygon <- c(tt2.az[ind1]+360*2, tt2.az[ind2]+360*2)
    polygon(x.polygon, y.polygon, col='white', border=col)
    
    
  } else {
    aux <- c(orbit1$az+360,orbit1$az,orbit1$az-360)
    orbit1$az <- aux
    orbit1$alt <- rep(orbit1$alt,3)
    
    aux <- c(orbit2$az+360,orbit2$az,orbit2$az-360)
    orbit2$az <- aux
    orbit2$alt <- rep(orbit2$alt,3)
    lines(orbit2$az, orbit2$alt, type='l')
    
    y.polygon <- c(orbit1$alt, rev(orbit2$alt))
    x.polygon <- c(orbit1$az, rev(orbit2$az))
    polygon(x.polygon, y.polygon, col=adjustcolor(col,alpha.f=0.5), border=col)
    
  }  
  
  
}

###############################################################
HWT.download = function(HWTID) {
  ## Horizon metadata
  test <- readLines(paste0("http://www.heywhatsthat.com/iphone/pan.cgi?id=",HWTID))
  hor <- c()
  
  # Lat/Lon/Elev
  mypattern = '<div class=\"details_data\">([^<]*)</div>'
  datalines = grep(mypattern,test,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1',matches)
  names(result) = NULL
  result[c(1,2,4)]
  
  hor$Lat <- as.numeric(strtrim(result[1],regexpr("&deg", result[1])[1]-1))
  if (substr(result[1],nchar(result[1]),nchar(result[2])) == "S") {hor$Lat <- -hor$Lat}
  hor$Lon <- as.numeric(strtrim(result[2],regexpr("&deg", result[2])[1]-1))
  if (substr(result[2],nchar(result[2]),nchar(result[2])) == "W") {hor$Lon <- -hor$Lon}
  aux <- strtrim(result[4],regexpr("&nbsp;", result[4])[1]-1)
  hor$Elev <- as.numeric(substr(aux,2,nchar(aux)))
  
  # Site Name
  grep("pan_top_title", test)
  mypattern = '<div id=\"pan_top_title\" class=\"ellipsis\" style=\"position: absolute; top: 46px; width: 296px\">([^<]*)</div>'
  datalines = grep(mypattern,test,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1',matches)
  names(result) = NULL
  hor$Name <- result
  
  ## Horizon data
  curdir <- getwd()
  setwd(tempdir())
  download.file(paste0('http://www.heywhatsthat.com/results/',HWTID,'/image_north.png'), mode ='wb', destfile='aux.png', quiet = T)
  horizon <- readPNG(paste0(getwd(),'/aux.png'))
  download.file(paste0('http://www.heywhatsthat.com/bin/image-north-with-alts.png?alts=0%201&id=',HWTID), mode ='wb', destfile='aux.png', quiet = T)
  horizon.alt <- readPNG(paste0(getwd(),'/aux.png'))
  setwd(curdir)
  
  # clean-up : removal of triangles and cardinals
  ind <- which(round(horizon[,,1]*256) == 199, arr.ind = T)
  if (NROW(ind)>0) {
    for (i in 1:NROW(ind)) {
      horizon[ind[i,1],ind[i,2],1:3] <- c(251,251,251)/256
    }
  }
  ind <- which(round(horizon[,,1]*256) == 0 & round(horizon[,,2]*256) == 0 & round(horizon[,,3]*256) == 0, arr.ind = T)
  if (NROW(ind)>0) {
    for (i in 1:NROW(ind)) {
      horizon[ind[i,1],ind[i,2],1:3] <- c(251,251,251)/256
    }
  }
  
  # get altitude values
  ind <- which(round(horizon.alt[,,1]*256)==140, arr.ind=T);
  lines <- ind[1:2,1]
  altitude <- array(NA, dim=dim(horizon)[1:2]);
  if (diff(lines)==0) { lines[2] <- dim(altitude)[1] }
  altitude[lines[2],] = 0; altitude[lines[1],] = 1;
  
  # linear extrapolation of altitude
  m <- 1./(lines[1]-lines[2]);
  b <- 1 - m*lines[1];
  for (k in 1:dim(altitude)[1]) {
    altitude[k,] <- m*k + b;
  }
  
  # identification of altitude
  alt <- array(NA,c(1,dim(altitude)[2]))
  for (l in 1:dim(altitude)[2]) {
    for (k in 1:dim(altitude)[1]) {
      if (round(horizon[k,l,1]*256) != 251) {  
        alt[l] <- altitude[k,l]
        break
      }
    }
  }
  
  # return result
  hor$alt <- as.vector(alt)
  hor$az <- seq(0,359,length.out = NCOL(horizon))-180
  ind <- which(hor$az<0)
  hor$az <- c(hor$az,hor$az[ind]+360); hor$az <- hor$az[-ind]
  hor$alt <- c(hor$alt,hor$alt[ind]); hor$alt <- hor$alt[-ind]
  hor$georef <- c(hor$Lat,hor$Lon)
  hor$ID <- HWTID
  class(hor) <- "horizon"
  return(hor)
}


###############################################################
horExport = function(hor, name="Default", author="Default", description, ground_col, hor_col) {
  
  if (missing(name)) {name = hor$name}
  if (missing(description)) {description = ""}
  if (substr(description,nchar(description),nchar(description)) != ".") {description <- paste0(description,".")}
  description <- paste(description,"Horizon created using skyscapeR.", sep=" ")
  if (missing(ground_col)) {ground_col = ".15,.45,.15"}
  if (missing(hor_col)) {hor_col = ".75,.45,.45"}
  
  # Horizon data
  data <- data.frame(x = hor$az, y = hor$alt)
  write.table(data, file="horizon.txt", sep = " ", row.names=F, col.names=F)
  
  # Landscape.ini file
  fileConn<-file("landscape.ini")
  string.text <- c("[landscape]", paste0("name = ",name), "type = polygonal", paste0("author = ",author),
                   paste0("description = ", description), "polygonal_horizon_list = horizon.txt", 
                   "polygonal_angle_rotatez = 0", paste0("ground_color = ",ground_col), 
                   paste0("horizon_line_color =  ",hor_col), "",
                   "[location]", "planet = Earth", paste0("latitude = ",hor$Lat,"d"),
                   paste0("longitude = ",hor$Lon,"d"), paste0("altitude = ",round(hor$Elev*0.3,0)))
  writeLines(string.text, fileConn)
  close(fileConn)
}

###############################################################
orbit = function(dec, lat, lon) {
  ra <- seq(0,360, by=0.5)
  tmp <- eq2hor(ra,dec,jd,lat,lon)
  
  # test for break
  ind <- which(abs(diff(tmp$az))>180)
  if (NROW(ind) == 1) {
    tmp$az[seq(ind+1,NROW(tmp$az))] <- tmp$az[seq(ind+1,NROW(tmp$az))] - 360
  } else if (NROW(ind) == 2){ 
    tmp$az[seq(1,ind[1])] <- tmp$az[seq(1,ind[1])] - 360
    tmp$az[seq(ind[2]+1,NROW(tmp$az))] <- tmp$az[seq(ind[2]+1,NROW(tmp$az))] - 360
  }
  # if (max(diff(tmp$az))>180) { tmp$az[tmp$az > 180] <- tmp$az[tmp$az > 180]-360 }
  
  # ind <- sort(tmp$az, index.return=T)$ix
  ind <- seq(1,NROW(tmp$az))
  
  # return result
  orbit <- c()
  orbit$az <- tmp$az[ind]
  orbit$alt <- tmp$alt[ind]
  orbit$dec <- dec
  orbit$georef <- c(lat,lon)
  class(orbit) <- "orbit"
  return(orbit)
}


###############################################################
stardec.min.max = function(star, from, to) {
  xx <- seq(from, to, 100)
  dd <- c()
  for (i in 1: NROW(xx)) {
    dd[i] <- palaeosky(star, xx[i])$dec
  }
  ff <- splinefun(xx,dd)
  
  xx <- seq(from,to,0.01)
  dd <- ff(xx)
  
  return(c(min(dd),max(dd)))
}
