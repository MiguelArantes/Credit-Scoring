source("utils.R")

sample.data <- function(dataset, perc_size = 0.75, seed){
  if(missing(seed)){
  seed  <- random.seed.gen()
  }

  smp_size <- floor(perc_size * nrow(dataset))

  set.seed(seed)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]

  return(list(train = train, test = test))
}

select.pca <- function(dataset){
  class_ds <- lapply(dataset, class)
  
  log_ds <- log(dataset[names(class_ds[class_ds == "numeric"])])
  
  ds_pca <- stats::prcomp(log_ds,
                   center = TRUE,
                   scale. = TRUE) 
  
  return(ds_pca)
}