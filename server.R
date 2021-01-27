################################################################################
# Server logic of the Shiny app
#
# Author: Lathan Liou
# Created: Wed Dec 16 12:03:20 2020 ------------------------------
################################################################################
source("src/server-mods.R")
source("src/cicerone_guide.R")

#for hover functionality
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

`%!in%` = Negate(`%in%`)

server <- function(input, output, session) {
  sever()
  Sys.sleep(0.5)
  waiter_hide()
  
  #initialize walkthrough
  Sys.sleep(1.5)
  guide$init()$start()
  
  #Load in data
  in_data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    shiny::validate(need(ext %in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv'), 
      "Please upload a csv file!"))

    data <- read_csv(input$file$datapath) %>%
      dplyr::mutate(across(any_of("date"), ~as.Date(.x, format = "%m/%d/%y")))
  })
  
  #warning if wrong file type
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    if (ext %!in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv')) {
      shiny::showNotification("Your file format is not supported. Please upload a CSV file!", type = "err", 
                              duration = NULL)
    }
  })
  
  #Dynamically display alg jump button ONLY when file is uploaded
  output$showjump <- renderUI({
    if(is.null(in_data())) return()
    shiny::selectInput("tabjump", "Take me to the following algorithm page", c("Select", "ADDIS Spending", "Alpha Spending", "Online Fallback"))
  })
  
  #jump to user-clicked algorithm
  observeEvent(input$tabjump, {
    if(input$tabjump == "ADDIS Spending"){
      updateTabsetPanel(session, "navmaster", selected = "ADDIS Spending")
    } else if (input$tabjump == "Alpha Spending") {
      updateTabsetPanel(session, "navmaster", selected = "Alpha Spending")
    } else if (input$tabjump == "Online Fallback") {
      updateTabsetPanel(session, "navmaster", selected = "Online Fallback")
    }
  })

  #### ADDIS Spending ####
  
  ADDIS_spending_result <- callModule(ADDIS_spending_Server, id = "inputADDIS_spending", data = in_data)
  callModule(ADDIS_spending_countServer, "ADDIS_spending_count", ADDIS_spending_result)
  callModule(ADDIS_spending_plotServer, "ADDIS_spending_plot", ADDIS_spending_result)
  callModule(ADDIS_spending_compServer, "ADDIS_spending_comp", ADDIS_spending_result, data = in_data)

  #### Alpha Spending ####
  
  Alpha_spending_result <- callModule(Alpha_spending_Server, id = "inputAlpha_spending", data = in_data)
  callModule(Alpha_spending_countServer, "Alpha_spending_count", Alpha_spending_result)
  callModule(Alpha_spending_plotServer, "Alpha_spending_plot", Alpha_spending_result)
  callModule(Alpha_spending_compServer, "Alpha_spending_comp", Alpha_spending_result, data = in_data)
  
  #### online fallback ####
  
  online_fallback_result <- callModule(online_fallback_Server, id = "inputonline_fallback", data = in_data)
  callModule(online_fallback_countServer, "online_fallback_count", online_fallback_result)
  callModule(online_fallback_plotServer, "online_fallback_plot", online_fallback_result)
  callModule(online_fallback_compServer, "online_fallback_comp", online_fallback_result, data = in_data)
}
