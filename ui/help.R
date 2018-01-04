# Help -------------------------------------------------------------------
tabPanel("Help",
         # fluidRow( column(12, includeMarkdown("howto.md") ) )
         tags$iframe(style="height:600px; width:100%", src="UserGuide_v0.4b.pdf")
)