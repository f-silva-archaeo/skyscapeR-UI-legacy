# About -------------------------------------------------------------------
tabPanel("About",
         fluidRow(
           column(6,
                  includeMarkdown("about.md"),
                  img(src="Logos_TSD_Sophia.jpg", width = 500)
           ),
           column(4,
                  img(src="circumpolar.jpg", width = 600)
           )
         )
         
)