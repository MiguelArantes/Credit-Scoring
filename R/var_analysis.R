source("utils.R", chdir = TRUE)

sample.data <-
  function(dataset,
           perc_test = 0.75,
           perc_valid = 0,
           seed) {
    
    if(sum(perc_test+perc_valid) > 1){
      stop("not valid total percentages split selection")
    }
    
    if (missing(seed)) {
      seed  <- random.seed.gen()
    }
    
    smp_size <- floor(perc_test * nrow(dataset))
    set.seed(seed)
    train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
    train <- dataset[train_ind, ]
    pre_test <- dataset[-train_ind, ]
    
    if (perc_valid > 0) {
      smp_size <- floor(perc_valid * nrow(dataset))
      valid_ind <- sample(seq_len(nrow(pre_test)), size = smp_size)
      validation <- pre_test[valid_ind, ]
      test <- pre_test[-valid_ind, ]
      
      return(list(
        train = train,
        test = test,
        validation = validation
      ))
    } else{
      test  <- pre_test
      return(list(train = train, test = test))
    }
    
  }
