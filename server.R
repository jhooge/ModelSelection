library(shiny)
library(DT)
library(shinyBS)
library(markdown)
library(dplyr)

# source("global.R")
# source("helpers.R")
source("visualizations.R")

shinyServer(function(input, output) {
  
  ## File Uploads
  output$testDataUpload <- renderUI({
    uiOutput <- fileInput('inputData', 'Upload Test Data',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.tsv'))
    return(uiOutput)
  })
  
  output$newDataUpload <- renderUI({
    uiOutput <- fileInput('newData', 'Upload New Data for Prediction',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.tsv'))
    return(uiOutput)
  })
  
  
  ## Reactive Variables
  testDataMatrix <- reactive({
    inFile <- input$inputData
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE, 
             sep = ",", 
             stringsAsFactors = T)
  })
  
  newData <- reactive({
    inFile <- input$newData   
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE, 
             sep = ",", 
             stringsAsFactors = T)
  })
  
  selectedModel <- reactive({
    model <- getModel(input$selectModel, models)
    return(model)
  })
  
  selectedPredModel <- reactive({
    model <- getModel(input$selectPredModel, models)
    return(model)
  })
  
  selectedPredNewModel <- reactive({
    model <- getModel(input$selectPredNewModel, models)
    return(model)
  })
  
  trainPredProb <- reactive({
    model <- selectedPredModel()
    
    predProb <- probabilities(model)
    predProb <- subset(predProb, subset=(dataType=="Training"))
  })
  
  testPredProb <- reactive({
    model <- selectedPredModel()
    data  <- testDataMatrix()
    
    testX <- data[, 2:ncol(data)]
    testY <- as.factor(data[, 1])
    
    predProb <- probabilities(model, testX, testY) 
    predProb <- subset(predProb, subset=(dataType=="Test"))
  })
  
  
  ## Selectors
  output$modelSelect <- renderUI({
    labels <- lapply(models, getLabel)
    selected <- labels[[sample(1:length(models), 1)]] ## choose a random model
    
    uiSelect <- selectInput("selectModel", "Select Model", 
                            choices = labels,
                            selected = selected)
    uiMetricSelect <- uiOutput("multiMetricSelect")
    uiDescr <- wellPanel(htmlOutput("modelDescr"))
    uiOutput <- box(title="Model Selection", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    uiSelect, uiMetricSelect, uiDescr)
    return(uiOutput)
  })
  
  output$modelPredSelect <- renderUI({
    labels <- lapply(models, getLabel)
    selected <- labels[[sample(1:length(models), 1)]] ## choose a random model
    
    uiSelect <- selectInput("selectPredModel", "Select Model", 
                            choices = labels,
                            selected = selected) ## choose a random model
    uiInput  <- uiOutput("testDataUpload")
    uiOutput <- box(title="Model Selection", width=8,
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    fluidRow(uiSelect),
                    uiInput)
    return(uiOutput)
  })
  
  output$modelPredNewSelect <- renderUI({
    labels <- lapply(models, getLabel)
    
    uiSelect <- selectInput("selectPredNewModel", "Select Model for Prediction", 
                            choices = labels,
                            selected = labels[[sample(1:length(models), 1)]]) ## choose a random model
    uiInput  <- uiOutput("newDataUpload")
    uiOutput <- box(title="Model Selection", width=8,
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    fluidRow(uiSelect),
                    uiInput)
    return(uiOutput)
  })
  
  output$multiModelSelect <- renderUI({
    labels <- lapply(models, getLabel)
    
    uiSelect <- selectizeInput(
      'multiModels', 'Select Models for Comparison',
      choices = labels,
      selected = colnames(data)[1:2], ## Fix the NULL issue
      multiple = TRUE)
    
    uiOutput <- box(title="Model Comparison", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    uiSelect)
    return(uiOutput)
  })
  
  output$multiMetricSelect <- renderUI({
    model <- selectedModel()
    perfNames <- model$perfNames
    
    uiOutput <- selectizeInput(
      'metrics', 'Select Performance Metrics',
      choices = perfNames,
      selected = perfNames[1:5], ## Fix the NULL issue
      multiple = TRUE)
    return(uiOutput)
  })
  

  ## Plot functions (Performance)
  output$parameterPlot <- renderPlot({
    label <- input$selectModel
    model <- getModel(label, models) 
    
    fig <- plotParamPerformance(model, label)
    return(fig)
  })
  
  output$varImpPlot <- renderPlot({
    label <- input$selectModel
    model <- getModel(label, models) 
    
    fig <- plotVarImp(model)
    return(fig)
  })
  
  output$featurePlot <- renderPlot({
    label <- input$selectModel
    model <- getModel(label, models) 
    
    fig <- plotFeaturePerformance(model, label, type="none")
    return(fig)
  })
  
  output$xyPlot <- renderPlot({
    label <- input$selectModel
    model <- getModel(label, models) 
    
    fig <- plotFeaturePerformance(model, label, type="xy")
    return(fig)
  })
  
  output$densityPlot <- renderPlot({
    label <- input$selectModel
    model <- getModel(label, models) 
    
    fig <- plotFeaturePerformance(model, label, type="density")
    return(fig)
  })
  
  output$trainPCAPlot <- renderPlotly({
    model <- selectedPredModel()
    data <- NULL
    
    ## for some reason ifelse returns a list and not a data.frame!
    if (class(model) == "train") {data <- model$trainingData}
    if (class(model) == "rfe") {data <- model$fit$trainingData}
    colnames(data)[colnames(data) == ".outcome"] <- "Class"
    
    fig <- plot3dPCA(model, data)
    return(print(fig))
  })
  
  output$testPCAPlot <- renderPlotly({
    model <- selectedPredModel()
    data <-  testDataMatrix()
    
    fig <- plot3dPCA(model, data)
    return(print(fig))
  })
  
  output$trainPerfPlot <- renderPlot({
    model <- selectedPredModel()
    predProb <- trainPredProb()
    title <- sprintf("Class Probability (%s)\n%s", 
                     "Training", getLabel(model))
    
    fig <- plotClassProbs(predProb, main=title)
    return(fig)
  })
  
  output$testPerfPlot <- renderPlot({
    model <- selectedPredModel()
    predProb <- testPredProb()
    title <- sprintf("Class Probability (%s)\n%s", 
                     "Test", getLabel(model))
    
    fig <- plotClassProbs(predProb, main=title)
    return(fig)
  })
  
  output$trainConfMatPlot <- renderPlot({
    performance <- trainPredProb()
    confusion   <- confusionMatrix(performance$pred, performance$obs)
    plotConfusionMat(confusion)
  })
  
  output$testConfMatPlot <- renderPlot({
    performance <- testPredProb()
    confusion   <- confusionMatrix(performance$pred, performance$obs)
    plotConfusionMat(confusion)
  })
  
  output$predClassPlot <-  renderPlot({
    model <- selectedPredNewModel()
    newData <- newData()
    classProbs <- NULL
    
    validate(
      need(!is.null(newData), "Please upload a dataset for prediction!")
    )
    
    if (class(model) == "rfe") {
      classProbs <- predict(model, newData)
      classProbs$pred <- NULL
    }
    if (class(model) == "train") {
      classProbs <- predict(model, newData, type="prob")
    }
    fig <- plotPredictedClasses(classProbs)
    return(fig)
  })
  

  ## Plot functions (Model Comparison
  output$splomCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "splom")
    return(fig)
  })
  
  output$dotCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "dot")
    return(fig)
  })
  
  output$bwCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "bw")
    return(fig)
  })
  
  output$densityCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "density")
    return(fig)
  })
  
  output$parallelCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "parallel")
    return(fig)
  })
  
  output$diffCompPlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=2, "Please select at least two models for comparison.")
    )
    
    fig <- plotModelComparison(models, "diff")
    return(fig)
  })
  
  output$runtimePlot <- renderPlot({
    models <- lapply(input$multiModels, getModel, models)
    validate(
      need(length(models)>=1, "Please select at least one model.")
    )
    
    fig <- plotRuntimes(models, as.logical(input$logScale))
    return(fig)
  })
  
  
  ## Dashboard Boxes
  output$runtimeBox <- renderUI({
    uiOutput <- box(title="Runtime Comparison", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    checkboxInput("logScale", "Set log Scale", FALSE),
                    plotOutput("runtimePlot"))
    return(uiOutput)
  })
  
  output$PCABox <- renderUI({
    testData <- testDataMatrix()
    uiOutput.train <- box(title="Prediction on Training Dataset", 
                         status = "primary", solidHeader = TRUE,
                         collapsible = TRUE,
                         plotlyOutput("trainPCAPlot"),
                         bsButton("trainPCAPlotPopover", label = "", icon = icon("question"),
                                  style = "info", size = "extra-small"),
                         bsPopover(id = "trainPCAPlotPopover", title = "Principal Components",
                                   content = getContent("PCAPlotPopover"),
                                   placement = "right",
                                   trigger = "hover",
                                   options = list(container = "body")))
    uiOutput.test <- box(title="Prediction on Test Dataset", 
                         status = "primary", solidHeader = TRUE,
                         collapsible = TRUE,
                         plotlyOutput("testPCAPlot"),
                         bsButton("testPCAPlotPopover", label = "", icon = icon("question"),
                                  style = "info", size = "extra-small"),
                         bsPopover(id = "testPCAPlotPopover", title = "Principal Components",
                                   content = getContent("PCAPlotPopover"),
                                   placement = "right",
                                   trigger = "hover",
                                   options = list(container = "body")))
    
    uiOutput <- NULL
    if(is.null(testData)) {
      uiOutput <- uiOutput.train
    } else {
        uiOutput <- list(uiOutput.train, uiOutput.test)
    }
    return(uiOutput)
  })
  
  output$modelPerfComparisonBox <- renderUI({
    
    uiOutput <- box(title="Model Comparison", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    tabsetPanel(
                      tabPanel("Scatter", plotOutput("splomCompPlot"),
                               bsButton("splomCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "splomCompPlotPopover", title = "Scatterplot Matrix",
                                         content = getContent("splomCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))),
                      tabPanel("Dot", plotOutput("dotCompPlot"),
                               bsButton("dotCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "dotCompPlotPopover", title = "Dot Plot",
                                         content = getContent("dotCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))),
                      tabPanel("BW", plotOutput("bwCompPlot"),
                               bsButton("bwCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "bwCompPlotPopover", title = "Box Plot",
                                         content = getContent("bwCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))),
                      tabPanel("Density", plotOutput("densityCompPlot"),
                               bsButton("densityCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "densityCompPlotPopover", title = "Density Plot",
                                         content = getContent("densityCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))),
                      tabPanel("Parallel", plotOutput("parallelCompPlot"),
                               bsButton("parallelCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "parallelCompPlotPopover", title = "Parallel Plot",
                                         content = getContent("parallelCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))),
                      tabPanel("Diff", plotOutput("diffCompPlot"),
                               bsButton("diffCompPlotPopover", label = "", icon = icon("question"),
                                        style = "info", size = "extra-small"),
                               bsPopover(id = "diffCompPlotPopover", title = "Diff Plot",
                                         content = getContent("diffCompPlotPopover"),
                                         placement = "right",
                                         trigger = "hover",
                                         options = list(container = "body"))
                               )
                    ))
    return(uiOutput)
  })
  
  output$featureSelectionBox <- renderUI({
    model <- selectedModel() 
    uiOutput <- NULL
    if(class(model)=="rfe") {
      uiOutput <- box(title="Feature Selection via Recursive Feature Elimination", 
                      status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tabsetPanel(
                        tabPanel("Estimation", plotOutput("featurePlot"),
                                 bsButton("featurePlotPopover", label = "", icon = icon("question"),
                                          style = "info", size = "extra-small"),
                                 bsPopover(id = "featurePlotPopover", title = "Variable Performance Profile",
                                           content = getContent("featurePlotPopover"),
                                           placement = "right",
                                           trigger = "hover",
                                           options = list(container = "body"))
                                 ),
                        tabPanel("Scatter", plotOutput("xyPlot"),
                                 bsButton("xyFeaturePlotPopover", label = "", icon = icon("question"),
                                          style = "info", size = "extra-small"),
                                 bsPopover(id = "xyFeaturePlotPopover", title = "Variable Performance Profile",
                                           content = getContent("xyFeaturePlotPopover"),
                                           placement = "right",
                                           trigger = "hover",
                                           options = list(container = "body"))
                                 ),
                        tabPanel("Density", plotOutput("densityPlot"),
                                 bsButton("densityPlotPopover", label = "", icon = icon("question"),
                                          style = "info", size = "extra-small"),
                                 bsPopover(id = "densityPlotPopover", title = "Variable Performance Profile",
                                           content = getContent("densityPlotPopover"),
                                           placement = "right",
                                           trigger = "hover",
                                           options = list(container = "body")))
                      ))
    }
    return(uiOutput)
  })
  
  output$varImpBox <- renderUI({
    model <- selectedModel()
    uiOutput <- box(title="Feature Importance",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("varImpPlot"),
                bsButton("varImpPlotPopover", label = "", icon = icon("question"),
                         style = "info", size = "extra-small"),
                bsPopover(id = "varImpPlotPopover", title = "Histogram of Class Probabilities",
                          content = getContent("varImpPlotPopover"),
                          placement = "right",
                          trigger = "hover",
                          options = list(container = "body")))
    return(uiOutput)
  })
  
  output$predPerfBox <- renderUI({
    model <- selectedPredModel()
    
    uiPerfPlot <- tabsetPanel(tabPanel("Training", 
                                       plotOutput("trainPerfPlot"),
                                       bsButton("trainPerfPlotPopover", label = "", icon = icon("question"),
                                                style = "info", size = "extra-small"),
                                       bsPopover(id = "trainPerfPlotPopover", title = "Histogram of Class Probabilities",
                                                 content = getContent("classProbPlotPopover"),
                                                 placement = "right",
                                                 trigger = "hover",
                                                 options = list(container = "body"))))
    
    if(!is.null(testDataMatrix())) {
      uiPerfPlot <- tabsetPanel(
        tabPanel("Training", plotOutput("trainPerfPlot"),
                 bsButton("trainPerfPlotPopover", label = "", icon = icon("question"),
                          style = "info", size = "extra-small"),
                 bsPopover(id = "trainPerfPlotPopover", title = "Histogram of Class Probabilities",
                           content = getContent("classProbPlotPopover"),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body"))),
        tabPanel("Test", plotOutput("testPerfPlot"),
                 bsButton("testPerfPlotPopover", label = "", icon = icon("question"),
                          style = "info", size = "extra-small"),
                 bsPopover(id = "testPerfPlotPopover", title = "Histogram of Class Probabilities",
                           content = getContent("classProbPlotPopover"),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body"))))
    }
    uiOutput <- box(title="Class Probabilities", width=12,
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    uiPerfPlot)
    return(uiOutput)
  })
  
  output$confMatrixBox <- renderUI({
    model <- selectedPredModel()
    
    uiConfusion <- tabsetPanel(tabPanel("Training", 
                                        plotOutput("trainConfMatPlot"),
                                        wellPanel(htmlOutput("trainOverallStats"))))
    if(!is.null(testDataMatrix())) {
      uiConfusion <- tabsetPanel(
        tabPanel("Training", plotOutput("trainConfMatPlot"),
                 wellPanel(htmlOutput("trainOverallStats"))),
        tabPanel("Test", plotOutput("testConfMatPlot"),
                 wellPanel(htmlOutput("testOverallStats"))))
    }
    uiOutput <- box(title="Confusion Matrices", width=12,
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    uiConfusion)
    return(uiOutput)
  })
  
  output$predNewBox <- renderUI({
    uiPlot <- plotOutput("predClassPlot")
    uiOutput <- box(title="Class Probabilities ", width=12,
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    uiPlot,
                    bsButton("predClassPlotPopover", label = "", icon = icon("question"),
                             style = "info", size = "extra-small"),
                    bsPopover(id = "predClassPlotPopover", title = "Diff Plot",
                              content = getContent("predClassPlotPopover"),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")))
    return(uiOutput)
  })
  
  
  ## Information Tables
  output$resampTableFS <- renderDataTable({
    model <- selectedModel()
    
    metrics <- input$metrics
    validate(
      need(!is.null(metrics), "Please select at least one performance metric.")
    )
    
    if (is.null(model))
      return(NULL)
    
    colSelection <- c("Variables", metrics)
    
    df <- model$results[, colSelection]
    df <- apply(df, 2, function(x) round(x, 4))
    
    
    return(df)
  }, options = list(pageLength = 16))
  
  output$resampTableParams <- renderDataTable({
    model <- selectedModel()
    metrics <- input$metrics
    
    validate(
      need(!is.null(metrics), "Please select at least one performance metric.")
    )
    
    if (is.null(model))
      return(NULL)
    
    
    tuneValues <- ifelse(class(model) == "rfe",
                         colnames(model$fit$results)[!(colnames(model$fit$results) %in% metrics)],
                         colnames(model$results)[!(colnames(model$results) %in% metrics)])
    df <- NULL
    if (class(model) == "rfe") {df <- model$fit$results}
    if (class(model) == "train") {df <- model$results}
    
    colSelection <- c(tuneValues, metrics)
    df <- df[, colSelection]
    df <- apply(df, 2, function(x) round(x, 4))
    
    return(df)
  }, options = list(pageLength = 16))
  
  output$resampTable <- renderUI({
    model <- selectedModel()
    
    uiOutput <- tabsetPanel(tabPanel("Parameter Estimation", DT::dataTableOutput("resampTableParams")))
    if(class(model) == "rfe") {
      uiOutput <- tabsetPanel(
        tabPanel("Feature Selection", DT::dataTableOutput("resampTableFS")),
        tabPanel("Parameter Estimation", DT::dataTableOutput("resampTableParams")))
    }
    
    return(uiOutput)
  })
  
  output$modelDescr <- renderText({
    label <- input$selectModel
    model <- getModel(label, models)
    
    RFE <- ifelse(class(model) == "rfe", "Yes", "No")
    innerResamplMethod <- caret:::resampName(model)
    outerResamplMethod <- "None"
    
    if (class(model) == "rfe") {
      innerResamplMethod <- caret:::resampName(model$fit)
      outerResamplMethod <- caret:::resampName(model)
      model <- model$fit
    }
    
    nSamples <- dim(model$trainingData)[1]
    nFeatures <- dim(model$trainingData)[2] - 1
    classes <- levels(model$trainingData$.outcome)
    preProcessing <- ifelse (is.null(model$preProc), 
                             "No pre-processing", model$preProc)
    
    string <- HTML("<b><h3>Model Description</h3></b>",
                   "<b>Samples:</b> ", nSamples, "<br>",
                   "<b>Features used in training:</b> ", nFeatures, "<br>",
                   "<b>Classes:</b> ", paste(classes, collapse = ", "), "<br>",
                   "<b>Preprocessing:</b> ", preProcessing, "<br>",
                   "<b>Recursive Feature Selection:</b> ", RFE, "<br>",
                   "<b>Outer Resampling Method (Feature Selection):</b> ", outerResamplMethod, "<br>",
                   "<b>Inner Resampling Method (Parameter Tuning):</b> ", innerResamplMethod, "<br>"
    )
    
    return(string)
  })
  
  output$trainOverallStats <- renderText({
    performance <- trainPredProb()
    confusion   <- confusionMatrix(performance$pred, performance$obs)
    accuracy <- confusion$overall[1]
    kappa <- confusion$overall[2]
    accuracyLower <- confusion$overall[3]
    accuracyUpper <- confusion$overall[4]
    accuracyNull <- confusion$overall[5]
    accuracyPValue <- confusion$overall[6]
    mcnemarPValue <- confusion$overall[7]
    
    string <- HTML("<b><h3>Overall Statistics</h3></b>",
                   "<br>",
                   "<b>Accuracy :</b> ", round(accuracy, 4), "<br>",
                   "<b>95% CI :</b> (", round(accuracyLower, 4), ", ", round(accuracyUpper, 4), ")", "<br>",
                   "<b>No Information Rate :</b> ", round(accuracyNull, 4), "<br>",
                   "<b>P-Value [Acc > NIR] :</b> < ", round(accuracyPValue, 4), "<br>",
                   "<b>Kappa :</b> ", round(kappa, 4), "<br>",
                   "<b>Mcnemar's Test P-Value :</b> ", round(mcnemarPValue, 4), "<br>")
    return(string)
  })
  
  output$testOverallStats <- renderText({
    performance <- testPredProb()
    confusion   <- confusionMatrix(performance$pred, performance$obs)
    accuracy <- confusion$overall[1]
    kappa <- confusion$overall[2]
    accuracyLower <- confusion$overall[3]
    accuracyUpper <- confusion$overall[4]
    accuracyNull <- confusion$overall[5]
    accuracyPValue <- confusion$overall[6]
    mcnemarPValue <- confusion$overall[7]
    
    string <- HTML("<b><h3>Overall Statistics</h3></b>",
                   "<br>",
                   "<b>Accuracy :</b> ", round(accuracy, 4), "<br>",
                   "<b>95% CI :</b> (", round(accuracyLower, 4), ", ", round(accuracyUpper, 4), ")", "<br>",
                   "<b>No Information Rate :</b> ", round(accuracyNull, 4), "<br>",
                   "<b>P-Value [Acc > NIR] :</b> < ", round(accuracyPValue, 4), "<br>",
                   "<b>Kappa :</b> ", round(kappa, 4), "<br>",
                   "<b>Mcnemar's Test P-Value :</b> ", round(mcnemarPValue, 4), "<br>")
    return(string)
  })
})
