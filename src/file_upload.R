################################################################################
# File upload page
#
# Author: Lathan Liou
# Created: Mon Sep 21 15:02:57 2020 ------------------------------
################################################################################
fluidPage(
  fluidRow(
    h1("User Guide"),
    bsplus::bs_accordion(id = "guide") %>%
      bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
      bs_append(title = "Introduction", content = HTML("This application is designed to allow users to interactively run procedures that control the False Discovery Rate (FDR) for online hypothesis testing. Source code and additional information for this application are available via <a href=\"https://github.com/dsrobertson/onlineFDR\">GitHub</a>.")) %>%
      bs_append(title = "Application usage", content = p(
        img(src = "user-diagram.png"),
        br(),
        "For more information, check out the", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html", "Get Started"), "page in our vignette."))  %>%
      bs_append(title = "Help & feedback", content = HTML("For additional help or to submit feedback or bug reports,
       please contact: <br>
       David Robertson <br>
       MRC Biostatistics Unit <br>
       <a href=\"mailto:david.robertson@mrc-bsu.cam.ac.uk@gmail.com\">Email</a>"))
    ), #close fluidrow
  br(),
  fluidRow(
    h1("Upload your dataset"),
    p("Ensure that your CSV file contains at the minimum, a column of p-values with the name 'pval'. If you're including dates, ensure that they are in the format YYYY-MM-DD. "),
    fileInput("file", NULL,
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain',
                         '.csv'))
  )
) #close fluidpage