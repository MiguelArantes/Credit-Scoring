source("utils.R")

credit.scoring <-
  function(formula,
           dataset,
           ds_pca,
           family = binomial("logit"),
           direction = "both",
           steps = 100) {
    if (!missing(ds_pca)) {
    dataset  <- predict.pca(dataset,ds_pca)
    }

    model <-
      step(
        glm(formula, data = dataset, family = family),
        direction = direction,
        steps = steps
      )

    return(model)
  }


predict.pca <- function(dataset,ds_pca) {
  class_ds <- lapply(dataset, class)

  log_ds <-
    log(dataset[names(class_ds[class_ds == "numeric" |
                                 class_ds == "integer"])])

  dataset_pca  <- predict(ds_pca, newdata = log_ds)

  dataset <-
    cbind(dataset[names(class_ds[!(class_ds == "numeric" |
                                     class_ds == "integer")])], dataset_pca)

  return(dataset)
}
