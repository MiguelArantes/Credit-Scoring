source("utils.R")

credit.scoring <- function(formula, dataset, ds_pca, family = 'gaussian') {
  if(!missing(ds_pca)){
  # FIXME: working in this!
  dataset  <- predict(pca, newdata = dataset)
  }

  model <- stats::glm(formula, data = dataset, family = family)

  return(model)
}

