
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)

# source("helpers.R")

dashboardPage(
  dashboardHeader(title="Model Performance Visualization"),
  dashboardSidebar(
    sidebarMenu(
      # fileInput("modelFiles", "Choose Model file(s)",
      #           multiple = TRUE,
      #           accept = c(
      #             ".Rdata",
      #             ".rda")
      # ),
      menuItem("Parameter Estimation", tabName = "parameterEstimation", icon = icon("bar-chart")),
      menuItem("Model Comparison", tabName = "modelComparison", icon = icon("bar-chart")),
      menuItem("Model Performance", tabName = "modelPerformance", icon = icon("bar-chart")),
      menuItem("Predict New Data", tabName = "prediction", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "parameterEstimation",
        fluidRow(uiOutput("varImpBox"),
                 uiOutput("modelSelect")),
        fluidRow(
          box(title="Parameter Estimation", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("parameterPlot"),
              bsButton("parameterPlotPopover", label = "", icon = icon("question"),
                       style = "info", size = "extra-small"),
              bsPopover(id = "parameterPlotPopover", title = "Parameter Performance Profile",
                        content = getContent("parameterPlotPopover"),
                        placement = "right",
                        trigger = "hover",
                        options = list(container = "body"))),
          uiOutput("featureSelectionBox")
        ),
        h2("Resampling Results"),
        fluidRow(uiOutput("resampTable"))
      ),
      tabItem(tabName = "modelComparison",
              fluidRow(uiOutput("modelPerfComparisonBox"),
                       uiOutput("multiModelSelect")),
              fluidRow(uiOutput("runtimeBox"))
      ),
      tabItem(tabName = "modelPerformance",
              fluidRow(uiOutput("modelPredSelect")),
              fluidRow(uiOutput("PCABox")),
              fluidRow(uiOutput("confMatrixBox")),
              fluidRow(uiOutput("predPerfBox"))
      ),
      tabItem(tabName = "prediction",
              fluidRow(uiOutput("modelPredNewSelect")),
              fluidRow(uiOutput("predNewBox"))
      )
    )
  )
)
  
