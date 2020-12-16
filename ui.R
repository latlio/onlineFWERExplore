################################################################################
# UI of the Shiny app
#
# Author: Lathan Liou
# Created: Wed Dec 16 12:01:08 2020 ------------------------------
################################################################################
source("src/ui-mods.R")
# source("src/router.R")

# 1. Shiny ----
library(shiny)
library(shinyWidgets) #custom widgets, allows for shinydashboard elements
library(shinycssloaders) #custom loading icons
library(shinyjs) #improved user exp
library(shinyBS) #custom widgets
library(bsplus)
# library(shinyalert) 
library(shinyFeedback) #for user feedback messages
# library(tippy) #for hovers
# library(highcharter) #for animated plots
library(plotly)
library(waiter) #for loading screen
library(sever) #for waiting screen
library(knitr)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny.router) #for links
# library(shinyanimate)

# 2. Data Manipulation
library(tidyverse)
library(dplyr)
library(lubridate)
# library(reactable)

#make sure github dev version is installed
# devtools::install_github("https://github.com/dsrobertson/onlineFDR")
library(StanHeaders)
library(onlineFDR)

ui <- shiny::fluidPage(
    tagList(
      includeCSS("src/styles.css"),
      shinyjs::useShinyjs(),
      shinyWidgets::useShinydashboard(),
      shinyWidgets::useShinydashboardPlus(),
      shinyFeedback::useShinyFeedback(),
      waiter::use_waiter(),
      sever::use_sever(),
      waiter::waiter_show_on_load(html = tagList(waiter::spin_fading_circles(),
                                                 "Initializing onlineFWERExplore")),
      tags$head(
        tags$script(src = "src/JSModule.js"),
        tags$style(HTML("
                    @import url('//fonts.googleapis.com/css2?family=Poppins:wght@300');"),
                   HTML("
                    @import url('//fonts.googleapis.com/css2?family=Lato:wght@400');"),
                   ".bttn { vertical-align: middle; height: 30px; width: 100%; font-size: 12px; font-family: Poppins, sans-serif;}",
                   ".panel-group {font-family: Lato, sans-serif; font-size: 14px;} ",
                   ".h1 {font-family: Lato;}",
                   ".p {font-family: Lato;}")
      ),
      ####make the navbar pages####
      shiny::navbarPage(HTML(paste0("onlineFWER", tags$sub("explore"))),
                        windowTitle = "onlineFWERExplore",
                        shiny::tabPanel("Get Started",
                                        source("src/file_upload.R")$value),
                        shiny::navbarMenu("Algorithms",
                                          shiny::tabPanel("ADDIS Spending",
                                                          source("src/ADDIS_spending_page.R")$value),
                                          shiny::tabPanel("Alpha Spending",
                                                          source("src/Alpha_spending_page.R")$value), #close tabPanel
                                          shiny::tabPanel("Online Fallback",
                                                          source("src/online_fallback_page.R")$value),
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }")
                        ),# close navbarMenu
                        shiny::tabPanel("About",
                                        source("src/about_page.R")$value)
      ) ##close navbarpage
    ) ## close taglist
  ) ## close fluid page