# Tools -------------------------------------------------------------
# Tools :: Coordinate Conversion -------------------------------------------------------------
observeEvent(input$coor_hor, {
  jd <- jdcnv(2000, 1, 1, 0.)
  chk <- c(T,F,F,F)
  if (sum(input$coor_chk == "nut")) { chk[2] <- T }
  if (sum(input$coor_chk == "ref")) { chk[3] <- T }
  if (sum(input$coor_chk == "abe")) { chk[4] <- T }
  HOR <- eq2hor(input$coor_ra, input$coor_dec, jd, input$coor_lat, input$coor_lon, precess_ = chk[1], nutate_ = chk[2], refract_ = chk[3], aberration_ = chk[4], altitude = input$coor_elev)
  updateNumericInput(session, "coor_az", value = HOR$az)
  updateNumericInput(session, "coor_alt", value = HOR$alt)
})

observeEvent(input$coor_eq, {
  jd <- jdcnv(2000, 1, 1, 0.)
  chk <- c(T,F,F,F)
  if (sum(input$coor.chk == "nut")) { chk[2] <- T }
  if (sum(input$coor.chk == "ref")) { chk[3] <- T }
  if (sum(input$coor.chk == "abe")) { chk[4] <- T }
  EQ <- hor2eq(input$coor_alt, input$coor_az, jd, input$coor_lat, input$coor_lon, precess_ = chk[1], nutate_ = chk[2], refract_ = chk[3], aberration_ = chk[4], altitude = input$coor_elev)
  updateNumericInput(session, "coor_dec", value = EQ$dec)
  updateNumericInput(session, "coor_ra", value = EQ$ra)
})
