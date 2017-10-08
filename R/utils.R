random.seed.gen <- function(n = 1, min = 0, max = 10^5){
  seed <- sample(min:max, n, replace=TRUE)

  return(seed)
}
