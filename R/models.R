source("utils.R", chdir = TRUE)

credit.scoring <-
  function(set_formula,
           dataset,
           set_direction = "both",
           set_steps = 100) {

    model <-
      step(
        glm(formula = set_formula, data = dataset, family = binomial("logit")),
        direction = set_direction,
        steps = set_steps
      )

    return(model)
  }

select.pca <- function(dataset, rm_vars) {
  if (!missing(rm_vars)) {
    dataset$test[rm_vars] <- NULL
  }

  class_ds <- lapply(dataset, class)

  if (length(class_ds) > 0) {
    dataset <-
      dataset[names(class_ds[class_ds == "numeric" |
                               class_ds == "integer"])]

    ds_pca <- stats::prcomp(dataset,
                            center = TRUE,
                            scale. = TRUE)

    return(ds_pca)
  } else {
    return(NULL)
  }
}

predict.pca <- function(dataset,ds_pca,rm_vars) {
  
  class_ds <- lapply(dataset, class)

  ds <- dataset[names(class_ds[class_ds == "numeric" |
                                 class_ds == "integer"])]

  dataset_pca  <- predict(ds_pca, newdata = log_ds)

  dataset <-
    cbind(dataset[names(class_ds[!(class_ds == "numeric" |
                                     class_ds == "integer") | rm_vars])], dataset_pca)

  return(dataset)
}

set.clusters <- function(dataset, min_nc = 5, max_nc = 14){
  dataset <- dataset$prediction
  
  nb <- NbClust::NbClust(dataset, diss=NULL, distance = "euclidean", 
                min.nc=min_nc, max.nc=max_nc, method = "kmeans", 
                index = "all", alphaBeale = 0.1)
  
  nc <- mode.R(nb$Best.nc[1,1:6])
  
  ic <- classInt::classIntervals(dataset, n = nc, style = "jenks")
  
  return(list(number_of_clusters = nc, cluster_prediction = ic))
}