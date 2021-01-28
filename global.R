################################################################################
# A Shiny App using the onlineFDR package
#
# Author: Lathan Liou
# Created: Wed Jan 27 10:31:27 2021 ------------------------------

################################################################################

#set memory limit
options(shiny.maxRequestSize = 500*1024^2)

#clear before deployment
rm(list = ls())

# 1. Shiny ----
library(shiny)
library(shinyWidgets) #custom widgets, allows for shinydashboard elements
library(shinycssloaders) #custom loading icons
library(shinyjs) #improved user exp
library(shinyBS) #custom widgets
library(bsplus)
library(shinyFeedback) #for user feedback messages
# library(tippy) #for hovers
# library(highcharter) #for animated plots
library(DT)
library(textillate)
library(plotly)
library(waiter) #for loading screen
library(sever) #for waiting screen
library(shinybusy) #for progress bar
library(knitr)
library(rmarkdown)
library(markdown)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(cicerone) #interactive walkthrough
# library(shinyanimate)

# 2. Data Manipulation
library(tidyverse)
library(lubridate)

#make sure github dev version is installed
# devtools::install_github("https://github.com/dsrobertson/onlineFDR")
library(onlineFDR)

source("ui.R")
source("server.R")