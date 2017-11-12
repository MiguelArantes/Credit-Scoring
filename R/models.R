source("utils.R", chdir = TRUE)

credit.scoring <-
  function(response,
           dataset,
           pca = FALSE,
           family = binomial("logit"),
           direction = "both",
           steps = 100) {

    if (pca) {
    model_pca <- select.pca(dataset = dataset, rm_vars = response)
    dataset  <- predict.pca(dataset = dataset, ds_pca = model_pca, rm_vars = response)
    }

    model <-
      step(
        glm(formula = paste(response,"~ ."), data = dataset, family = family),
        direction = direction,
        steps = steps
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
