################################################################################
# Server Modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 11:50:56 2020 ------------------------------
################################################################################

# globaldata <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     stash <- reactiveValues()
#     stash$in_data <- reactive({
#       req(input$file)
#       
#       ext <- tools::file_ext(input$file$name)
#       validate(need(ext == "csv", "Please upload a csv file!"))
#       
#       data <- read_csv(input$file$datapath)
#     })
#     return(list(getdata = reactive(stash$in_data)))
#   })
# }

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

LONDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run LOND algorithm
  LONDres <- eventReactive(input$go, {
    # validate(need(is.null(data()), "Please upload a dataset"))
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    dep = ifelse(input$dep == "True", T, F)
    random = ifelse(input$random == "True", T, F)
    original = ifelse(input$original == "True", T, F)
    seed = as.numeric(input$seed)
    
    set.seed(seed)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    
    out <- LOND(d = data(),
                alpha = alpha,
                random = random,
                original = original)
    shiny::removeModal()
    
    return(out)
  }) #close eventReactive
  
  #toggle advanced options
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })

  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })

  #download handler
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs(paste0("myModuleJS('", ns(""), "');"))
  #     global$response <- FALSE
  #   }
  # })
  
  #download params
  output$download2 <- downloadHandler(
      filename = function() {
        paste("LONDparams-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write_csv(user_params(), file)
      }
    )
  
  return(list(LONDres = LONDres))
}

LONDtableServer <- function(input, output, session, LONDresult) {
  output$table <- renderReactable({
    data <- LONDresult$LONDres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                                }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LOND significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                         var total = 0
                         colInfo.data.forEach(function(row) {
                          total += row[colInfo.column.id]
                          })
                          return total
                                              }")
                )
              )
    ) #close reactable
  }) #close render reactable
}

LONDcountServer <- function(input, output, session, LONDresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
    # shinyanimate::startAnim(session, "downloadbutton", "fadeInDown")
  })
  
  output$count <- renderUI({  
    
    data <- LONDresult$LONDres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LOND-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LONDresult$LONDres(), file)
    }
  )
}

LONDplotServer <- function(input, output, session, LONDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDresult$LONDres() %>%
      mutate(index = row_number(),
             LOND = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LOND, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    # highchart() %>%
    #   hc_xAxis(title = list(text = "Index")) %>%
    #   hc_yAxis(title = list(text = "Log adjusted test level ")) %>%
    #   hc_add_series(new_data,
    #                 "line",
    #                 hcaes(x = index,
    #                       y = alpha,
    #                       group = adjustment)) %>%
    #   hc_colors(c("#4682B4", "#FF6347", "#2E8B57"))
    
    # ggplot(new_data, aes(x = index, y = alpha, col = adjustment)) +
    #   geom_line() + 
    #   theme_bw() + 
    #   labs(x = "Index",
    #        y = "Log adjusted test level",
    #        col = "Adjustment")
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LONDcompServer <- function(input, output, session, LONDresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }

  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LONDresult$LONDres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx()
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(index = row_number(),
             LOND = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LOND, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })

  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- LONDresult$LONDres()
      
      div(
        p(
          paste0("LOND rejected ", sum(current_alg_data$R), " null hypotheses.")
        ),
        p(
          paste0(input$alg, " rejected ", sum(select_alg_data$R), " null hypotheses.")
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      ) #close div
    }
    })
}

LORDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$version, {
    if(input$version == "++"){
      shinyjs::disable(id = "w0")
      shinyjs::disable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "3" || input$version == 3) {
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "discard"){
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "tau.discard")
      shinyjs::disable(id = "b0")
    }
    else if(input$version == "dep"){
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else{
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::enable(id = "tau.discard")
    }
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # Run LORD algorithm
  LORDres <- eventReactive(input$go, {
    
    # if(is.null(data())){
    #   shiny::showNotification("Please upload a dataset", type = "err")
    # }
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    b0 = as.numeric(input$b0)
    req(input$b0)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    random = ifelse(input$random == "True", T, F)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$b0, {
      req(input$b0)
      if(as.numeric(input$b0) <= 0 | as.numeric(input$b0) > 
         as.numeric(input$alpha) - as.numeric(input$w0) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "b0",
          text = "Value must be positive and 
          the sum of w0 and b0 must not be greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("b0")
      }
    }
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- LORD(d = data(),
                   alpha = alpha,
                   version = version,
                   w0 = w0,
                   b0 = b0,
                   tau.discard = tau.discard,
                   random = random)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
 
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LONDparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LORDres = LORDres))
}

LORDtableServer <- function(input, output, session, LORDresult) {
  output$table <- renderReactable({
    data <- LORDresult$LORDres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORD significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LORDcountServer <- function(input, output, session, LORDresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- LORDresult$LORDres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDresult$LORDres(), file)
    }
  )
}

LORDplotServer <- function(input, output, session, LORDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDresult$LORDres() %>%
      mutate(index = row_number(),
             LORD = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORD, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  
  })
}

LORDcompServer <- function(input, output, session, LORDresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LORDresult$LORDres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LORD = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORD, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- LORDresult$LORDres()
      
      div(
        p(
          paste0("LORD rejected ", sum(current_alg_data$R), " null hypotheses.")
        ),
        p(
          paste0(input$alg, " rejected ", sum(select_alg_data$R), " null hypotheses.")
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}

SAFFRONServer <- function(input, output, session, data) {
  ns <- session$ns
  
  SAFFRONres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    random = ifelse(input$random == "True", T, F)
    discard = ifelse(input$discard == "True", T, F)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 | 
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- SAFFRON(d = data(),
                      alpha = alpha,
                      w0 = w0,
                      lambda = lambda,
                      random = random,
                      discard = discard,
                      tau.discard = tau.discard)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  output$download <- downloadHandler(
    filename = function() {
      paste("SAFFRON-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(SAFFRONres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("SAFFRONparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(SAFFRONres = SAFFRONres))
}

SAFFRONtableServer <- function(input, output, session, SAFFRONresult) {
  output$table <- renderReactable({
    data <- SAFFRONresult$SAFFRONres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRON significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONcountServer <- function(input, output, session, SAFFRONresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- SAFFRONresult$SAFFRONres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("SAFFRON-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(SAFFRONresult$SAFFRONres(), file)
    }
  )
}

SAFFRONplotServer <- function(input, output, session, SAFFRONresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- SAFFRONresult$SAFFRONres() %>%
      mutate(index = row_number(),
             SAFFRON = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRON, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

SAFFRONcompServer <- function(input, output, session, SAFFRONresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- SAFFRONresult$SAFFRONres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             SAFFRON = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRON, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- SAFFRONresult$SAFFRONres()
      
      div(
        p(
          paste0("SAFFRON rejected ", sum(current_alg_data$R), " null hypotheses.")
        ),
        p(
          paste0(input$alg, " rejected ", sum(select_alg_data$R), " null hypotheses.")
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}

ADDISServer <- function(input, output, session, data) {
  ns <- session$ns
  
  ADDISres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    tau = as.numeric(input$tau)
    req(input$tau)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau, {
      req(input$tau)
      if(as.numeric(input$tau) > 1 | as.numeric(input$tau) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = FALSE)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })

  output$download <- downloadHandler(
    filename = function() {
      paste("ADDIS-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(ADDISres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("ADDISparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(ADDISres = ADDISres))
}

ADDIStableServer <- function(input, output, session, ADDISresult) {
  output$table <- renderReactable({
    data <- ADDISresult$ADDISres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "ADDIS significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

ADDIScountServer <- function(input, output, session, ADDISresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- ADDISresult$ADDISres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("ADDIS-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(ADDISresult$ADDISres(), file)
    }
  )
}

ADDISplotServer <- function(input, output, session, ADDISresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- ADDISresult$ADDISres() %>%
      mutate(index = row_number(),
             ADDIS = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

ADDIScompServer <- function(input, output, session, ADDISresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- ADDISresult$ADDISres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             ADDIS = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- ADDISresult$ADDISres()
      
      div(
        p(
          paste0("ADDIS rejected ", sum(current_alg_data$R), " null hypotheses.")
        ),
        p(
          paste0(input$alg, " rejected ", sum(select_alg_data$R), " null hypotheses.")
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}

ADDISaServer <- function(input, output, session, data) {
  ns <- session$ns
  
  ADDISres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    tau = as.numeric(input$tau)
    req(input$tau)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau, {
      req(input$tau)
      if(as.numeric(input$tau) > 1 | as.numeric(input$tau) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = TRUE)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("ADDISasync-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(ADDISares(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("ADDISasyncparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(ADDISres = ADDISres))
}

alphainvestingServer <- function(input, output, session, data) {
  ns <- session$ns
  
  alphainvestingres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    random = ifelse(input$random == "True", T, F)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- Alpha_investing(d = data(),
                              alpha = alpha,
                              w0 = w0,
                              random = random)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("alphainvesting-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(alphainvestingres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("alphainvestingparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(alphainvestingres = alphainvestingres))
}

alphainvestingtableServer <- function(input, output, session, alphainvestingresult) {
  output$table <- renderReactable({
    data <- alphainvestingresult$alphainvestingres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "Alpha_investing significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

alphainvestingcountServer <- function(input, output, session, alphainvestingresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- alphainvestingresult$alphainvestingres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("alphainvesting-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(alphainvestingresult$alphainvestingres(), file)
    }
  )
}

alphainvestingplotServer <- function(input, output, session, alphainvestingresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- alphainvestingresult$alphainvestingres() %>%
      mutate(index = row_number(),
             Alpha_investing = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Alpha_investing, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

alphainvestingcompServer <- function(input, output, session, alphainvestingresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- alphainvestingresult$alphainvestingres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             AlphaInvesting = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(AlphaInvesting, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- alphainvestingresult$alphainvestingres()
      
      div(
        p(
          paste0("Alpha Investing rejected ", sum(current_alg_data$R), " null hypotheses.")
        ),
        p(
          paste0(input$alg, " rejected ", sum(select_alg_data$R), " null hypotheses.")
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}

LONDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  LONDSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- myLONDstar(d = data(),
                         alpha = alpha,
                         version = version)
    shiny::removeModal()
    
    output
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LONDSTAR-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LONDSTARres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LONDSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LONDSTARres = LONDSTARres))
}

LONDSTARtableServer <- function(input, output, session, LONDSTARresult) {
  output$table <- renderReactable({
    data <- LONDSTARresult$LONDSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LONDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LONDSTARcountServer <- function(input, output, session, LONDSTARresult) {
  output$count <- renderUI({
    data <- LONDSTARresult$LONDSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LONDSTARplotServer <- function(input, output, session, LONDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDSTARresult$LONDSTARres() %>%
      mutate(index = row_number(),
             LONDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LONDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LONDSTARcompServer <- function(input, output, session, LONDSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LONDSTARresult$LONDSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LONDSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LONDSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}

LORDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  LORDSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- myLORDstar(d = data(),
                         alpha = alpha,
                         version = version,
                         w0 = w0)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORDSTAR-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDSTARres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LORDSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LORDSTARres = LORDSTARres))
}

LORDSTARtableServer <- function(input, output, session, LORDSTARresult) {
  output$table <- renderReactable({
    data <- LORDSTARresult$LORDSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LORDSTARcountServer <- function(input, output, session, LORDSTARresult) {
  output$count <- renderUI({
    data <- LORDSTARresult$LORDSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LORDSTARplotServer <- function(input, output, session, LORDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDSTARresult$LORDSTARres() %>%
      mutate(index = row_number(),
             LORDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LORDSTARcompServer <- function(input, output, session, LORDSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LORDSTARresult$LORDSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LORDSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORDSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}

SAFFRONSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  SAFFRONSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    discard = ifelse(input$discard == "True", T, F)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    
    output <- mySAFFRONstar(pval = data(),
                          alpha = alpha,
                          version = version,
                          w0 = w0,
                          lambda = lambda,
                          discard = discard,
                          tau.discard = tau.discard)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download_bttn",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.data.frame(data)) {
      shiny::showNotification("Please upload a dataset first!", type = "err")
    }
  })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("SAFFRONSTAR-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(SAFFRONSTARres(), file)
    }
  )
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("SAFFRONSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(SAFFRONSTARres = SAFFRONSTARres))
}

SAFFRONSTARtableServer <- function(input, output, session, SAFFRONSTARresult) {
  output$table <- renderReactable({
    data <- SAFFRONSTARresult$SAFFRONSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRONstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONSTARcountServer <- function(input, output, session, SAFFRONSTARresult) {
  output$count <- renderUI({
    data <- SAFFRONSTARresult$SAFFRONSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

SAFFRONSTARplotServer <- function(input, output, session, SAFFRONSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- SAFFRONSTARresult$SAFFRONSTARres() %>%
      mutate(index = row_number(),
             SAFFRONstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRONstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

SAFFRONSTARcompServer <- function(input, output, session, SAFFRONSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD2 = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- SAFFRONSTARresult$SAFFRONSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             SAFFRONSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRONSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}