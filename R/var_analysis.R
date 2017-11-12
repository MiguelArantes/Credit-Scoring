source("utils.R", chdir = TRUE)

sample.data <- function(dataset, perc_test = 0.75, perc_valid = 0, seed) {
  if (missing(seed)) {
    seed  <- random.seed.gen()
  }

  smp_size <- floor(perc_size * nrow(dataset))

  set.seed(seed)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

  train <- dataset[train_ind,]
  test <- dataset[-train_ind,]

  return(list(train = train, test = test))
}
