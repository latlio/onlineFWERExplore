################################################################################
# ADDIS Spending Page
#
# Author: Lathan Liou
# Created: Wed Dec 16 11:58:28 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
         
         box(
           title = strong("ADDIS Spending"),
           status = "primary",
           solidHeader = TRUE,
           tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
           width = 12,
           ADDIS_spending_UI("inputADDIS_spending")
         )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputADDIS_spending"),
                                  summaryUI("ADDIS_spending_count")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputADDIS_spending"),
                                  plotUI("ADDIS_spending_plot")),
                  shiny::tabPanel("Compare",
                                  compareUI("ADDIS_spending_comp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/ADDIS_spending_code.Rmd", quiet = T))))
                ) ## close tabset panel
  ) ## close column
  
) ##close fluid row