################################################################################
# Online Fallback Page
#
# Author: Lathan Liou
# Created: Wed Dec 16 11:58:28 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
         
         box(
           title = strong("Online Fallback"),
           status = "primary",
           solidHeader = TRUE,
           tags$style(HTML("
.box.box-solid.box-primary>.box-header {
  color:#ffffff;
  background:#266EAB
                    }

.box.box-solid.box-primary{
border-bottom-color#ffffff;
border-left-color:#F7F7F7;
border-right-color:#F7F7F7;
border-top-color:#ffffff;
}
                                    ")),
           width = 12,
           online_fallback_UI("inputonline_fallback")
         )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputonline_fallback"),
                                  summaryUI("online_fallback_count")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputonline_fallback"),
                                  plotUI("online_fallback_plot")),
                  shiny::tabPanel("Compare",
                                  compareUI("online_fallback_comp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/online_fallback_code.Rmd", quiet = T))))
                ) ## close tabset panel
  ) ## close column
  
) ##close fluid row