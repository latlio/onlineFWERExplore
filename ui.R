################################################################################
# UI of the Shiny app
#
# Author: Lathan Liou
# Created: Wed Dec 16 12:01:08 2020 ------------------------------
################################################################################
source("src/ui-mods.R")

ui <- shiny::fluidPage(
    tagList(
      includeCSS("www/css/styles.css"),
      shinyjs::useShinyjs(),
      shinyWidgets::useShinydashboard(),
      shinyWidgets::useShinydashboardPlus(),
      shinyFeedback::useShinyFeedback(),
      waiter::use_waiter(),
      sever::use_sever(),
      waiter::waiter_show_on_load(html = tagList(waiter::spin_fading_circles(),
                                                 "Initializing onlineFWERexplore")),
      cicerone::use_cicerone(),
      tags$head(
        tags$script(src = "src/JSModule.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "css/lato.css"),
        tags$style(
          HTML(
            ".bttn { vertical-align: middle; height: 30px; width: 100%; font-size: 12px; font-family: Lato, sans-serif;}",
            ".panel-group {font-family: Lato, sans-serif; font-size: 14px;} ",
            ".h1 {font-family: Lato;}",
            ".p {font-family: Lato;}")
        )),
      ####make the navbar pages####
      shiny::navbarPage(HTML(paste0("onlineFWER", tags$sub("explore"))),
                        windowTitle = "onlineFWERExplore",
                        id = "navmaster",
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