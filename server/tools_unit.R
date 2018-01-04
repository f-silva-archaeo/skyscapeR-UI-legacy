# Tools -------------------------------------------------------------
# Tools :: Unit Conversion -------------------------------------------------------------
observeEvent(input$dec_to_deg, {
  x <- input$conv.dec
  updateNumericInput(session, "conv.deg", value = trunc(x))
  updateNumericInput(session, "conv.min", value = trunc( (x-trunc(x))*60 ))
  updateNumericInput(session, "conv.sec", value = ( (x-trunc(x))*60 - trunc( (x-trunc(x))*60 ))*60 )
})

observeEvent(input$deg_to_dec, {
  updateNumericInput(session, "conv.dec", value = input$conv.deg + input$conv.min/60 + input$conv.sec/3600)
  updateNumericInput(session, "conv.dec", value = input$conv.deg + input$conv.min/60 + input$conv.sec/3600)
  updateNumericInput(session, "conv.dec", value = input$conv.deg + input$conv.min/60 + input$conv.sec/3600)
})
