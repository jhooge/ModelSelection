library(plotly)
library(reshape2)
library(pROC)
library(caret)
library(gridExtra)
library(dplyr)
library(caret)

source("helpers.R")


#' Gets the label for a caret model
#' 
#' @param model A trained caret model (train or rfe)
#'
#' @return A human readable model label (character)
getLabel <- function(model) {
  label <- model$fit$modelInfo$label
  if (is.null(label)) {
    label <- model$modelInfo$label
  }
  return(label)
}


plotSkewness <- function(X) {
  X <- as.data.frame(X) ## in case it is just a numeric vector
  skewValues <- as.data.frame(sapply(X, skewness, na.rm=TRUE, type=1))
  skewValues$variable <- rownames(skewValues)
  rownames(skewValues) <- NULL
  colnames(skewValues) <- c("skewnessValue", "variable")
  skewValues$variable <- as.factor(skewValues$variable)
  
  if (ncol(X) <= 50){
    fig <- ggplot(skewValues, aes(x=variable, y=skewnessValue)) +
      geom_bar(stat="identity", position="dodge") +
      geom_hline(yintercept = 2, linetype = "dashed", color="red") +
      geom_hline(yintercept = -2, linetype = "dashed", color="red") +
      ylab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  } else {
    fig <- ggplot(skewValues, aes(x=skewnessValue)) +
      geom_density() +
      geom_point(aes(x=skewnessValue, y = 0.0005),
                 alpha = 0.25, size=4, colour="darkgray") +
      geom_vline(xintercept = 2, linetype = "dashed", color="red") +
      geom_vline(xintercept = -2, linetype = "dashed", color="red") +
      annotate("text", x = Inf, y = Inf, label = sprintf("n=%i", ncol(X)),
               vjust=1.8, hjust=1.2) +
      xlab(NULL) +
      ylab(NULL) +
      theme_bw()
  }
  return(fig)
}

plotClassification <- function(trainData, testData, model) {
  predProb  <- extractProb(list(model), testX=testData[, predictors(model)], testY=testClasses)
  trainPred <- subset(predProb, subset = dataType=="Training")
  testPred  <- subset(predProb, subset = dataType=="Test")
  
  df_train <- trainData
  df_trainPCA <- prcomp(t(df_train))
  df_trainPCA <- df_trainPCA$rotation[, 1:3]
  df_trainPCA <- as.data.frame(df_trainPCA)
  
  df_trainPCA$obs <- trainPred$obs
  df_trainPCA$pred <- trainPred$pred
  df_trainPCA$isCorrect <- as.factor(trainPred$obs == trainPred$pred)
  
  df_test <- testData
  df_testPCA <- prcomp(t(df_test))
  df_testPCA <- df_testPCA$rotation[, 1:3]
  df_testPCA <- as.data.frame(df_testPCA)
  
  df_testPCA$obs <- testPred$obs
  df_testPCA$pred <- testPred$pred
  df_testPCA$isCorrect <- as.factor(testPred$obs == testPred$pred)
  
  trainPredPlot <- ggplot(data=df_trainPCA, aes(x=PC1, y=PC2)) +
    geom_point(aes(color=pred, shape=isCorrect, size=9), alpha=0.5) +
    ggtitle("Training Predictions") +
    theme_bw()
  testPredPlot <- ggplot(data=df_testPCA, aes(x=PC1, y=PC2)) +
    geom_point(aes(color=pred, shape=isCorrect, size=9), alpha=0.5) +
    ggtitle("Test Predictions") +
    theme_bw()
  fig <- grid.arrange(trainPredPlot, testPredPlot, nrow=2,
                      top = sprintf("Classification Result: %s", model$method))
  return(fig)
}

plotVarImp <- function(model, scale=T, ...) {
  vimp <- NULL
  fig  <- NULL
  
  switch(class(model),
         train={vimp <- varImp(model, scale, ...)$importance},
         rfe={vimp <- varImp(model$fit, scale, ...)$importance}
  )
  
  if (ncol(vimp) > 1) {
    vimp$Mean <- apply(vimp, 1, mean)
    vimp <- vimp[with(vimp, order(-Mean)), ]
    vimp$Mean <- NULL
    vimp$Variable <- factor(rownames(vimp), levels=rev(rownames(vimp)))
    vimp.long <- melt(vimp, id.vars = "Variable")
    colnames(vimp.long) <- c("Variable", "Class", "Importance")
    
    fig <- ggplot(vimp.long, aes(x=Variable, y=Importance)) + 
      geom_bar(stat="identity", aes(fill=Importance)) +
      coord_flip() +
      facet_wrap(~Class, ncol=2) +
      theme_bw()
  } else {
    vimp$Variable <- factor(rownames(vimp), levels=rev(rownames(vimp)))
    fig <- ggplot(vimp, aes(Variable, Overall)) + 
      geom_bar(stat="identity", aes(fill=Overall)) +
      coord_flip() +
      theme_bw()
  }
  return(fig)
}

plotModelComparison <- function(models, type) {
  resamples <- resamplModels(models)
  difValues <- diff(resamples)
  
  switch(type,
         splom={fig <- splom(resamples)},
         dot={fig <- dotplot(resamples)},
         bw={fig <- bwplot(resamples)},
         density={fig <- densityplot(resamples)},
         parallel={fig <- parallelplot(resamples)},
         diff={fig <- dotplot(difValues)}
  )
  
  return(fig)
}

plotConfusionMat <- function(confMat) {
  
  confMat.table <- as.data.frame(confMat$table)
  maxFreq <- max(confMat.table$Freq)
  
  statsByClass <- as.data.frame(confMat$byClass)
  statsByClass$Class <- sapply(strsplit(rownames(statsByClass), "\\:"), `[`, 2)
  rownames(statsByClass) <- NULL
  statsByClass <- melt(statsByClass)
  
  fig1 <- ggplot(confMat.table, aes(Prediction, Reference)) + 
    geom_tile(aes(fill = Freq), colour = "white") +
    geom_text(aes(fill = Freq, label = round(Freq, 2))) +
    scale_fill_gradient(low = "white", high = "#3C8DBC", limits=c(0, maxFreq)) +
    theme_bw()
  
  fig2 <- ggplot(statsByClass, aes(Class, variable)) + 
    geom_tile(aes(fill = value), colour = "white") +
    geom_text(aes(fill = value, label = round(value, 2))) +
    scale_fill_gradient(low = "white", high = "#3C8DBC", limits=c(0,1)) +
    theme_bw()
  
  fig <- grid.arrange(fig1, fig2, ncol=2)
  return(fig)
}

plotPredictedClasses <- function(classProbs) {
  classProbs$Sample <- rownames(classProbs)
  rownames(classProbs) <- NULL
  classProbs <- melt(classProbs)
  colnames(classProbs) <- c("Sample", "Class", "Probability")
  classProbs$Sample <- as.numeric(classProbs$Sample)
  
  fig <- ggplot(classProbs, aes(Sample, Class)) + 
    geom_tile(aes(fill = Probability), colour = "white") +
    geom_text(aes(fill = Probability, label = round(Probability, 2))) +
    scale_fill_gradient(low = "white", high = "#3C8DBC", limits=c(0,1)) +
    coord_fixed(ratio=1) +
    theme_bw()
  return(fig)
}

plotRuntimes <- function(models, log=FALSE) {
  runtimes <- as.data.frame(sapply(models, getRuntime))
  runtimes$Model <- rownames(runtimes)
  colnames(runtimes) <- c("Runtime", "Model")
  fig <- NULL
  
  if (log) {
    fig <- ggplot(data=runtimes, aes_string("Model", "Runtime")) +
      geom_bar(stat="identity", aes(fill=Runtime)) +
      scale_y_log10() +
      coord_flip() +
      ylab("Runtime [sec]") + 
      theme_bw()
  } else {
    fig <- ggplot(data=runtimes, aes_string("Model", "Runtime")) +
      geom_bar(stat="identity", aes(fill=Runtime)) +
      coord_flip() +
      ylab("Runtime [sec]") + 
      theme_bw()
  }
  return(fig)
}

plot3dPCA <- function(model, data) {
  data <- select(data, -Class)
  
  df <- probabilities(model)
  classes <- colnames(df)[!(colnames(df) %in% c("obs", "pred", "model", "dataType", "object"))]
  df <- df[ ,!(colnames(df) %in% c("model", "dataType", "object"))]
  df$maxProb <- apply(df[, classes], 1, max)
  df$Classified <- as.factor(ifelse(df$obs == df$pred, "Correct", "Incorrect"))
  df$Sample <- rownames(df)
  rownames(df) <- NULL
  
  df.pca <- prcomp(t(data))
  df.pca <- df.pca$rotation[, 1:3]
  df.pca <- as.data.frame(df.pca)
  df.combined <- cbind(df, df.pca)
  
  symbols  <- c("circle", "x")
  df.combined$maxProb <- round(df.combined$maxProb, 1)
  df.combined$symbols <- symbols[df.combined$Classified]
  df.combined$size    <- df.combined$maxProb
  df.combined$hovertext <- paste(
    "Sample: ", df.combined$Sample, "<br>",
    "PC1: ", round(df.combined$PC1, 4), "<br>",
    "PC2: ", round(df.combined$PC2, 4), "<br>",
    "PC3: ", round(df.combined$PC3, 4), "<br>",
    "Observed: ", df.combined$obs, "<br>",
    "Predicted: ", df.combined$pred, "<br>",
    "p(c|x) = ", round(df.combined$maxProb, 2),
    sep = "")
  
  fig <- plot_ly(data = df.combined, type="scatter3d", mode="markers", text = ~hovertext, hoverinfo = 'text') %>%
    add_trace(data = df.combined, x = ~PC1, y = ~PC2, z = ~PC3,
              type = "scatter3d",
              text = ~hovertext, hoverinfo = 'text',
              mode = "markers",
              color = ~pred,
              marker = list(size = ~size*15, symbol=~symbols), showlegend=F) %>%
    add_trace(data = df.combined, x = ~PC1, y = ~PC2, z = ~PC3,
              type = "scatter3d",
              mode = "markers",
              color = ~pred,
              visible="legendonly", showlegend=T, legendgroup="color",
              marker = list(size = 14)) %>%
    add_trace(data = df.combined, x = ~PC1, y = ~PC2, z = ~PC3,
              type = "scatter3d",
              mode = "markers",
              marker = list(size = ~size*15),
              visible="legendonly", showlegend=F, legendgroup="size") %>%
    layout(scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")))
  ## I'm unable to generate the legend for the two classification types
  # %>%
  # add_trace(data = df.combined, x = PC1, y=PC2, z=PC3, 
  #           type = "scatter3d", 
  #           mode = "markers",
  #           color=Classified, colors="#000000",
  #           marker = list(size = size*15, symbol=symbols),
  #           showlegend=T, legendgroup="symbols") 
  # %>%
  # layout(legend=list(traceorder="grouped+reversed", tracegroupgap =30),
  #        xaxis=list(range=c(0,12)),
  #        yaxis=list(range=c(0,6)))
  
  return(fig)
}

plotParamPerformance <- function(model, label=NULL, type="train") {
  fig <- NULL
  
  switch(class(model),
         train={fig <- ggplot(model) + ggtitle(label) + theme_bw()},
         rfe={fig <- ggplot(model$fit) + ggtitle(label) + theme_bw()})
  return(fig)
}

plotFeaturePerformance.none <- function(model, label=NULL) {
  stopifnot(class(model)=="rfe")
  fig <- NULL
  if(class(model) == "rfe") { ## Feature Selection by RFE
    fig <- ggplot(model) + ggtitle(label) + theme_bw()
  }
  return(fig)
}

plotFeaturePerformance.density <- function(model, label=NULL) {
  stopifnot(class(model)=="rfe")
  fig <- NULL
  if(class(model) == "rfe") { ## Feature Selection by RFE
    fig <- densityplot(model, main=label,
                       pch = "|", adjust = 1.25, as.table = TRUE)
  }
  return(fig)
}

plotFeaturePerformance.xy <- function(model, label=NULL) {
  stopifnot(class(model)=="rfe")
  
  fig <- NULL
  if(class(model) == "rfe") { ## Feature Selection by RFE
    fig <- xyplot(model, main=label,
                  type = c("g", "p", "smooth"))
  }
  return(fig)
}

plotFeaturePerformance <- function(model, label, type="none") {
  stopifnot(class(model)=="rfe")
  stopifnot(type %in% c("none", "xy", "density"))
  
  args <- list(model=model, label=label)
  
  fig <- NULL
  switch(type,
         none={fig <- do.call(plotFeaturePerformance.none, args)},
         density={fig <- do.call(plotFeaturePerformance.density, args)},
         xy={fig <- do.call(plotFeaturePerformance.xy, args)})
}