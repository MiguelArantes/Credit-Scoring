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

  if(nrow(dataset)>2500){
  valid_ind <- sample(seq_len(nrow(dataset)), size = 2500)
  dataset <- dataset[valid_ind, ]
  }
  
  dataset <- dataset$prediction
  
  nb <- NbClust::NbClust(dataset, diss=NULL, distance = "euclidean", 
                min.nc=min_nc, max.nc=max_nc, method = "kmeans", 
                index = "all", alphaBeale = 0.1)
  
  nc <- mode.R(nb$Best.nc[1,1:6])
  
  ic <- classInt::classIntervals(dataset, n = nc, style = "jenks")
  
  return(list(number_of_clusters = nc, cluster_prediction = ic))
}

set.levels <- function(dataset, clusters){
  
  levels <- data.table::data.table(
    level = 1:clusters$number_of_clusters,
    min_level = c(0,
                 clusters$cluster_prediction$brks[2:(clusters$number_of_clusters)]),
    max_level = c(clusters$cluster_prediction$brks[2:(clusters$number_of_clusters)], 1)
  )
  
  data.table::setkey(dataset,'prediction')
  data.table::setkey(levels,'min_level','max_level')
  prediction <-
    levels[dataset, roll = Inf][, `:=` (prediction = round(min_level,4),
                                               min_level = NULL,
                                               max_level = NULL)]
  
  return(list(levels_intervals = levels, prediction = prediction))
}