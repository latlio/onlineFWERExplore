################################################################################
# About Page
#
# Author: Lathan Liou
# Created: Wed Jan 27 10:28:00 2021 ------------------------------
################################################################################
fluidPage(
  fluidRow(
    column(12,
           widgetUserBox(
             title = "Lathan Liou",
             subtitle = "Research Assistant @ MRC BSU",
             type = NULL,
             width = 12,
             src = "https://i.ibb.co/ZV9JFKw/1q-cropped.jpg",
             background = TRUE,
             backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
             closable = FALSE,
             "Designed the onlineFDR app",
             footer = p("Feel free to email any questions or comments to", a("Lathan", href= "mailto:lathanliu21@gmail.com/"))
           )
    )# close column
  ), #close fluidrow
  
  fluidRow(
    column(12,
           widgetUserBox(
             title = "David Robertson",
             subtitle = "Senior Research Associate @ MRC BSU",
             type = NULL,
             width = 12,
             src = "https://www.mrc-bsu.cam.ac.uk/wp-content/uploads/2014/03/RobertsonDavid.jpg",
             background = TRUE,
             backgroundUrl = "https://c4.wallpaperflare.com/wallpaper/929/1017/70/city-lights-bokeh-lights-blurred-blurry-wallpaper-preview.jpg",
             closable = FALSE,
             "Developed the onlineFDR package",
             footer = p("Feel free to email any questions or comments to", a("David", href= "mailto:david.robertson@mrc-bsu.cam.ac.uk/"))
           )
    ) #close column
  ), #close fluidrow
  fluidRow(
    bsplus::bs_accordion(id = "about") %>%
      bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
      bs_append(title = "Other contributors", p(
        "Aaditya Ramdas",
        br(),
        "Adel Javanmard",
        br(),
        "Andrea Montanari",
        br(),
        "Jinjin Tian",
        br(),
        "Tijana Zrnic",
        br(),
        "Natasha A. Karp"
      )) %>%
      bs_append(title = "Package dependencies & credits", p(
        "shinyWidgets",
        br(),
        "shinycssloaders",
        br(),
        "shinyjs",
        br(),
        "shinyBS",
        br(),
        "bsplus",
        br(),
        "shinyFeedback",
        br(),
        "shinydashboard",
        br(),
        "shinydashboardPlus",
        br(),
        "cicerone",
        br(),
        "DT",
        br(),
        "textillate",
        br(),
        "waiter",
        br(),
        "sever",
        br(),
        "shinybusy",
        br(),
        br(),
        "tidyverse",
        br(),
        "plotly",
        br(),
        "lubridate",
        br(),
        "knitr",
        style = "font-family: 'Consolas'"))
  ) #close fluidrow
) # close fluidpage

