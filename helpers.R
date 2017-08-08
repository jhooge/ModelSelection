library(kernlab)

getLabel <- function(model) {
  label <- model$fit$modelInfo$label
  if (is.null(label)) {
    label <- model$modelInfo$label
  }
  return(label)
}

getModel <-  function(label, models) {
  labels <- sapply(models, getLabel)
  i <- which(labels == label)
  model <- models[[i]]
  return(model)
}

getRuntime <- function(model) {
  runtime <- model$times$everything[3]
  
  if (class(model)=="rfe") {
    names(runtime) <- model$fit$modelInfo$label
  }
  
  if (class(model)=="train") {
    names(runtime) <- model$modelInfo$label
  }
  
  return(runtime)
}

resamplModels <- function(models) {
  classes <- sapply(models, class)
  labels <- sapply(models, getLabel)
  models.FS  <- models[classes == "train"]
  models.RFE <- lapply(models[classes == "rfe"], function(x) x$fit)
  models.all <- append(models.FS, models.RFE)
  names(models.all) <- labels
  resamples <- resamples(models.all)
  return(resamples)
}

## Compute performance on holdout set
probabilities <- function(model, testX=NULL, testY=NULL, ...) {
  preds <- predictors(model)
  
  if (class(model) == "rfe") {
    model <- model$fit
  }
  
  predProb <- NULL
  if (is.null(testX) | is.null(testY)) {
    predProb <- extractProb(list(model))
  } else {
    predProb <- extractProb(list(model), testX[, preds], testY)
  }
  return(predProb)
}

getContent <- function(id) {
  content <- list(splomCompPlotPopover=includeText("doc/splomCompPlotPopover.html"),
                  parallelCompPlotPopover=includeText("doc/parallelCompPlotPopover.html"),
                  dotCompPlotPopover=includeText("doc/dotCompPlotPopover.html"),
                  bwCompPlotPopover=includeText("doc/bwCompPlotPopover.html"),
                  densityCompPlotPopover=includeText("doc/densityCompPlotPopover.html"),
                  diffCompPlotPopover=includeText("doc/diffCompPlotPopover.html"),
                  parameterPlotPopover=includeText("doc/parameterPlotPopover.html"),
                  classProbPlotPopover=includeText("doc/classProbPlotPopover.html"),
                  varImpPlotPopover=includeText("doc/varImpPlotPopover.html"),
                  featurePlotPopover=includeText("doc/featurePlotPopover.html"),
                  xyFeaturePlotPopover=includeText("doc/xyFeaturePlotPopover.html"),
                  densityPlotPopover=includeText("doc/densityPlotPopover.html"),
                  PCAPlotPopover=includeText("doc/pcaPlotPopover.html")
                  )
  return(content[[id]])
}