################################################################################
# UI modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 09:52:20 2020 ------------------------------
################################################################################

#### BUILDING BLOCKS -- DOESNT WORK CURRENTLY ####
alphaUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  uiOutput(ns("alpha"))
}

depUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_dep"),
                    "Dependent:"),
        shinyWidgets::switchInput(ns("dep"), 
                                  NULL,
                                  value = FALSE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px"),
        shinyBS::bsTooltip(ns("label_dep"),
                           "Your p-values are dependent.",
                           placement = "right",
                           trigger = "hover"))
  )
}

randomUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px"),
        shinyBS::bsTooltip(ns("label_random"),
                           "The order of p-values in each batch of experiments (those that
                            have the same date) is randomized.",
                           placement = "right",
                           trigger = "hover"))
  )
}

originalUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Original:"),
        shinyWidgets::switchInput(ns("original"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px"))
  )
}

calculateUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 100px;",
        shiny::actionButton(ns("go"),
                            "Calculate"))
  )
}

downloadUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(
    div(style="display: inline-block;vertical-align:top; width: 100px;",
        shiny::actionButton(ns("init"), "Download", icon = icon("download")),
        shiny::downloadButton(ns("download"), "Download", style = "visibility: hidden;")
    )
  )
}

#### ALGORITHM INPUT UI ####
LONDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"),
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_dep"),
                    "Dependent:"),
        shinyWidgets::switchInput(ns("dep"), 
                                  NULL,
                                  value = FALSE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px")
        ),
    shinyBS::bsTooltip(ns("label_dep"),
                       "Your p-values are dependent.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Original:"),
        shinyWidgets::switchInput(ns("original"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px")),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
  # tagList(
  #   alphaUI(ns("alpha")),
  #   depUI(ns("dep")),
  #   randomUI(ns("random")),
  #   originalUI(ns("original")),
  #   br(),
  #   br(),
  #   div(style="display: inline-block;vertical-align:top; width: 100px;",
  #       shiny::actionButton(ns("go"),
  #                           "Calculate")),
  #   downloadUI("actualdownload")
  # )
}

LORDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"),
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Version:"),
        shiny::selectInput(ns("version"), NULL, c("++", "3", "discard", "dep"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("b0"), "Payout:", width = 80, value = 0.045, placeholder = ".045"),
    shinyBS::bsTooltip(ns("b0"),
                       "Payout for rejecting a hypothesis",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("tau.discard"), "Threshold:", width = 80, 
                     value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("tau.discard"),
                       "Optional threshold for hypotheses to be selected for testing",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

SAFFRONUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("lambda"), "Lambda", width = 80, value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("lambda"),
                       "Optional threshold for a candidate hypothesis",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_discard"),
                    "Discard"),
        shinyWidgets::switchInput(ns("discard"),
                                  NULL,
                                  value = FALSE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_discard"),
                       "If TRUE then runs the ADDIS algorithm with 
                            adaptive discarding of conservative nulls",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("tau.discard"), "Threshold:", width = 80, 
                     value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("tau.discard"),
                       "Optional threshold for hypotheses to be selected for testing",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

ADDISUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.00625, 
                     placeholder = ".00625"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("lambda"), "Lambda", width = 80, value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("lambda"),
                       "Optional threshold for a candidate hypothesis",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("tau"), "Threshold:", width = 80, 
                     value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("tau"),
                       "Optional threshold for hypotheses to be selected for testing",
                       placement = "right",
                       trigger = "hover"),
    # div(style="display: inline-block;vertical-align:top; width: 200px;",
    #     tags$strong(id = "label_asyncADDIS",
    #                 "Asynchronous:"),
    #     shinyWidgets::switchInput("asyncADDIS",
    #                               NULL,
    #                               value = FALSE,
    #                               onLabel = "True",
    #                               offLabel = "False",
    #                               width = "80px")),
    # shinyBS::bsTooltip("label_asyncADDIS",
    #                    "Runs the version for an asynchronous testing process",
    #                    placement = "right",
    #                    trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

alphainvestingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.025, 
                     placeholder = "0.025"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

LONDSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Version:"),
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

LORDSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Version:"),
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

SAFFRONSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Version:"),
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
    shinyBS::bsTooltip(ns("w0"),
                       "Initial wealth of the procedure",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("lambda"), "Lambda", width = 80, value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("lambda"),
                       "Optional threshold for a candidate hypothesis",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_discard"),
                    "Discard"),
        shinyWidgets::switchInput(ns("discard"),
                                  NULL,
                                  value = FALSE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_discard"),
                       "If TRUE then runs the ADDIS algorithm with 
                            adaptive discarding of conservative nulls",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("tau.discard"), "Threshold:", width = 80, 
                     value = 0.5, placeholder = ".5"),
    shinyBS::bsTooltip(ns("tau.discard"),
                       "Optional threshold for hypotheses to be selected for testing",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download"),
      label = "Download results",
      style = "fill",
      color = "primary",
      size = "sm"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

#### OTHER UI ####
tableUI <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("table")) %>%
    shinycssloaders::withSpinner(type = 6,
                                 color = "#0066CC")
}

summaryUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("count"))
}

compareUI <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    p("Make sure you already clicked Calculate. This feature compares the results of two algorithms. Parameters for the selected algorithm are assumed to be same as those for the current algorithm, otherwise they are set to the default values (see Code)."),
    column(width = 12,
           align = "center",
           div(style = "display: inline-block;vertical-align:top;text-align:center",
               strong("Pick an algorithm for comparison"),
               shiny::selectInput(ns("alg"), NULL, c("LOND", "LORD2", "LORD3", "LORDdiscard", "LORDdep", "SAFFRON", "ADDIS")))),
    shinyWidgets::actionBttn(
      inputId = ns("compare"),
      label = "Compare",
      style = "fill",
      color = "primary"
    ),
    br(),
    plotlyOutput(ns("comp")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC")
  ) #close taglist
}

plotUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("plot")) %>%
    shinycssloaders::withSpinner(type = 6,
                                 color = "#0066CC")
}

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

placeholderUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px")
  )
}

placeholder2UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder2"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px")
  )
}