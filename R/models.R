source("utils.R")

credit.scoring <-
  function(formula, dataset, ds_pca,
           family = binomial("logit"),
           direction = "both",
           steps = 100) {
    if (!missing(ds_pca)) {
      # FIXME: working in this!
      dataset  <- predict(ds_pca, newdata = dataset)
    }

    model <-
      step(
        glm(formula, data = dataset, family = family),
        direction = direction,
        steps = steps
      )

    return(model)
  }
