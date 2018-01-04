# Tools -------------------------------------------------------------
# Tools :: Horizon Altitude -------------------------------------------------------------
observeEvent(input$hor.calc, {
  aux <- atan( (input$hor.ref - input$hor.site)/input$hor.dist ) * 180 / pi
  # output$hor.alt <- str(aux)
  updateNumericInput(session, "hor.alt", value = aux)
})