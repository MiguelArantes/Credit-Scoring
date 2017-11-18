#!/usr/bin/env Rscript

source("models.R", chdir = TRUE)

testthat::test_that("models - credit.scoring", {
  
  sample_data <- readRDS("../data/sampledata.Rds")
  
  model_glm <-
    credit.scoring(set_formula = 'fraud ~ escolaridade + sexo + idade + morada + trabalho + motivo_sinistro',
                   dataset = sample_data$train)
})