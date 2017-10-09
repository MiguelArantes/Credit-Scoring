random.seed.gen <- function(n = 1, min = 0, max = 10 ^ 5) {
  seed <- sample(min:max, n, replace = TRUE)
  
  return(seed)
}

set.dataset <- function(dataset, type) {
  if (is.list(dataset) &&
      length(dataset) == 2 &&
      as.logical(min(c("train", "test") %in% names(dataset)))) {
    dataset <- dataset[type]
  }
  
  return(dataset)
}