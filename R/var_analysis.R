source("seed_generator.R")

sample.data <- function(dataset, perc_size = 0.75, seed){
  if(missing(seed)){
  seed  <- random.seed.gen()
  }

  smp_size <- floor(0.75 * nrow(mtcars))

  set.seed(seed)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]

  return(list(train = train, test = test))
}
