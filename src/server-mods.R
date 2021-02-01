################################################################################
# Server Modules
#
# Author: Lathan Liou
# Created: Wed Dec 16 12:03:08 2020 ------------------------------
################################################################################

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

#### ALG SERVERS ####
ADDIS_spending_Server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run ADDIS spending algorithm
  ADDIS_spending_res <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    lambda = as.numeric(input$lambda)
    tau = as.numeric(input$tau)
    dep = ifelse(input$dep == "True", T, F)
    
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
         str_detect(input$tau, "[a-zA-Z\\,\\-]+")) {
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
    
    observeEvent(input$boundnum, {
      if(str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(input$boundnum == 0) {
      out <- ADDIS_spending(d = data(),
                            alpha = alpha,
                            lambda = lambda,
                            tau = tau,
                            dep = dep)
    } else {
      boundnum = as.numeric(input$boundnum)
      gammai <- setBound("ADDIS_spending", N = boundnum)
      out <- ADDIS_spending(d = data(),
                            alpha = alpha,
                            gammai = gammai,
                            lambda = lambda,
                            tau = tau,
                            dep = dep)
    }
    
    
    shiny::removeModal()
    
    out
  }) %>% #close eventReactive
    bindCache(input$alpha,
              input$lambda,
              input$tau,
              input$dep) %>%
    bindEvent(input$go)
  
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateTextInput(session, "lambda", value = 0.25)
    updateTextInput(session, "tau", value = 0.5)
    updateSwitchInput(session, "dep", value = FALSE)
    updateSwitchInput(session, "bound", value = FALSE)
    updateTextInput(session, "boundnum", value = 0)
  })
  
  #toggle advanced options
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$bound)
  })
  
  #record user params
  ADDIS_spending_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # Output error messages
  observeEvent(input$go, {
    
    if(!is.null(data())){
      tryCatch({
        ADDIS_spending_res()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  list(ADDIS_spending_res = ADDIS_spending_res,
       ADDIS_spending_params = ADDIS_spending_params)
}
Alpha_spending_Server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run Alpha spending algorithm
  Alpha_spending_res <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    random = ifelse(input$random == "True", T, F)
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
    
    observeEvent(input$boundnum, {
      if(str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(input$boundnum == 0) {
      out <- Alpha_spending(d = data(),
                            alpha = alpha,
                            random = random)
    } else {
      boundnum = as.numeric(input$boundnum)
      gammai <- setBound("Alpha_spending", N = boundnum)
      out <- Alpha_spending(d = data(),
                            alpha = alpha,
                            gammai = gammai,
                            random = random)
    }
    
    shiny::removeModal()
    
    out
  }) %>% #close eventReactive
    bindCache(input$alpha,
              input$random,
              input$seed) %>%
    bindEvent(input$go)
  
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "bound", value = FALSE)
    updateTextInput(session, "boundnum", value = 0)
  })
  
  #toggle advanced options
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$bound)
  })
  
  #record user params
  Alpha_spending_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # Output error messages
  observeEvent(input$go, {
    
    if(!is.null(data())){
      tryCatch({
        Alpha_spending_res()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  list(Alpha_spending_res = Alpha_spending_res,
       Alpha_spending_params = Alpha_spending_params)
}
online_fallback_Server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run online fallback algorithm
  online_fallback_res <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    random = ifelse(input$random == "True", T, F)
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
    
    observeEvent(input$boundnum, {
      if(str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(input$boundnum == 0) {
      out <- online_fallback(d = data(),
                             alpha = alpha,
                             random = random)
    } else {
      boundnum = as.numeric(input$boundnum)
      gammai <- setBound("online_fallback", N = boundnum)
      out <- online_fallback(d = data(),
                             alpha = alpha,
                             gammai = gammai,
                             random = random)
    }
    
    shiny::removeModal()
    
    out
  }) %>% #close eventReactive
    bindCache(input$alpha,
              input$random,
              input$seed) %>%
    bindEvent(input$go)
  
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "bound", value = FALSE)
    updateTextInput(session, "boundnum", value = 0)
  })
  
  #toggle advanced options
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$bound)
  })
  
  #record user params
  online_fallback_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # Output error messages
  observeEvent(input$go, {
    
    if(!is.null(data())){
      tryCatch({
        online_fallback_res()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err")
      })
    }
  })
  
  list(online_fallback_res = online_fallback_res,
       online_fallback_params = online_fallback_params)
}

#### COUNT SERVERS ####
ADDIS_spending_countServer <- function(input, output, session, ADDIS_spending_result) {
  ns <- session$ns
  
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- ADDIS_spending_result$ADDIS_spending_res()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Lato, sans-serif;
        font-size: 18px;
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("ADDIS_spending-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("ADDIS_spending-", Sys.Date(), ".csv", sep = "")
      write_csv(ADDIS_spending_result$ADDIS_spending_res(), filename)
      filename2 <- paste("ADDIS_spending-", Sys.Date(), ".csv", sep = "")
      write_csv(ADDIS_spending_result$ADDIS_spending_params(), filename2)
      R_session <- paste("ADDIS_spending-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}
Alpha_spending_countServer <- function(input, output, session, Alpha_spending_result) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- Alpha_spending_result$Alpha_spending_res()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Lato, sans-serif;
        font-size: 18px;
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("Alpha_spending-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("Alpha_spending-", Sys.Date(), ".csv", sep = "")
      write_csv(Alpha_spending_result$Alpha_spending_res(), filename)
      filename2 <- paste("Alpha_spending-", Sys.Date(), ".csv", sep = "")
      write_csv(Alpha_spending_result$Alpha_spending_params(), filename2)
      R_session <- paste("Alpha_spending-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}
online_fallback_countServer <- function(input, output, session, online_fallback_result) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- online_fallback_result$online_fallback_res()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Lato, sans-serif;
        font-size: 18px;
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("online_fallback-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("online_fallback-", Sys.Date(), ".csv", sep = "")
      write_csv(online_fallback_result$online_fallback_res(), filename)
      filename2 <- paste("online_fallback-", Sys.Date(), ".csv", sep = "")
      write_csv(online_fallback_result$online_fallback_params(), filename2)
      R_session <- paste("online_fallback-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}

#### PLOT SERVERS ####
ADDIS_spending_plotServer <- function(input, output, session, ADDIS_spending_result) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- ADDIS_spending_result$ADDIS_spending_res() %>%
      mutate(index = row_number(),
             ADDIS_spending = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS_spending, Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(ADDIS_spending_result$ADDIS_spending_res() %>% slice_tail())
  
  output$num <- renderUI({
    current_alg_data <- ADDIS_spending_result$ADDIS_spending_res()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("ADDIS Spending rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}
Alpha_spending_plotServer <- function(input, output, session, Alpha_spending_result) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- Alpha_spending_result$Alpha_spending_res() %>%
      mutate(index = row_number(),
             Alpha_spending = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Alpha_spending, Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(Alpha_spending_result$Alpha_spending_res() %>% slice_tail())
  
  output$num <- renderUI({
    current_alg_data <- Alpha_spending_result$Alpha_spending_res()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("Alpha Spending rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}
online_fallback_plotServer <- function(input, output, session, online_fallback_result) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- online_fallback_result$online_fallback_res() %>%
      mutate(index = row_number(),
             online_fallback = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(online_fallback, Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(online_fallback_result$online_fallback_res() %>% slice_tail())
  
  output$num <- renderUI({
    current_alg_data <- online_fallback_result$online_fallback_res()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("Online Fallback rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}

#### COMP SERVERS ####
ADDIS_spending_compServer <- function(input, output, session, ADDIS_spending_result, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           ADDIS_spending = ADDIS_spending(data),
           Alpha_spending = Alpha_spending(data),
           Online_fallback = online_fallback(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- ADDIS_spending_result$ADDIS_spending_res()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx()
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(index = row_number(),
             ADDIS_spending = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS_spending, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(data_to_plot() %>% slice(5))
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- ADDIS_spending_result$ADDIS_spending_res()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("ADDIS Spending rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
Alpha_spending_compServer <- function(input, output, session, Alpha_spending_result, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           ADDIS_spending = ADDIS_spending(data),
           Alpha_spending = Alpha_spending(data),
           Online_fallback = online_fallback(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- Alpha_spending_result$Alpha_spending_res()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx()
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(index = row_number(),
             Alpha_spending = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Alpha_spending, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(data_to_plot() %>% slice(5))
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- Alpha_spending_result$Alpha_spending_res()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("Alpha Spending rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
online_fallback_compServer <- function(input, output, session, online_fallback_result, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           ADDIS_spending = ADDIS_spending(data),
           Alpha_spending = Alpha_spending(data),
           Online_fallback = online_fallback(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- online_fallback_result$online_fallback_res()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx()
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(index = row_number(),
             Online_fallback = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Online_fallback, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  }) %>%
    bindCache(data_to_plot() %>% slice(5))
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- online_fallback_result$online_fallback_res()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("Online Fallback rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}