################################################################################
# Alpha Spending Page
#
# Author: Lathan Liou
# Created: Wed Dec 16 11:58:28 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
         
         box(
           title = strong("Alpha Spending"),
           status = "primary",
           solidHeader = TRUE,
           tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
           width = 12,
           Alpha_spending_UI("inputAlpha_spending")
         )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputAlpha_spending"),
                                  summaryUI("Alpha_spending_count")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputAlpha_spending"),
                                  plotUI("Alpha_spending_plot")),
                  shiny::tabPanel("Compare",
                                  compareUI("Alpha_spending_comp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/Alpha_spending_code.Rmd", quiet = T))))
                ) ## close tabset panel
  ) ## close column
  
) ##close fluid row