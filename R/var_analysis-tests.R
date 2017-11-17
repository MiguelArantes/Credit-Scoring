#!/usr/bin/env Rscript

source("var_analysis.R", chdir = TRUE)

testthat::test_that("SamuraiX - DatasetFillMissing", {
  
  dataset <- data.table::data.table(readr::read_csv("../data/dataset.csv"))
  
  sample_data <-
    sample.data(dataset, perc_test = 0.5, perc_valid = 0.25, seed = 1)
  
  testthat::expect_is(sample_data, "list")
  
  testthat::expect_length(sample_data, 3)
  
  testthat::expect_null(sample_data$empty)
  
  testthat::expect_equal(
    sample_data$train[1],
    data.table::data.table(
      trabalho = "Cozinheiro",
      escolaridade = "Secondary education",
      motivo_sinistro = "Colisão do veículo",
      id = 807,
      sexo = "woman",
      idade = 55,
      morada = "Lisboa",
      fraud = "no"
    )
  )
  
  testthat::expect_equal(
    sample_data$test[1],
    data.table::data.table(
      trabalho = "Bancario",
      escolaridade = "Bachelor",
      motivo_sinistro = "Colisão do veículo",
      id = 874,
      sexo = "woman",
      idade = 74,
      morada = "Setubal",
      fraud = "no"
    )
  )
  
  testthat::expect_equal(
    sample_data$validation[1],
    data.table::data.table(
      trabalho = "Empregado de mesa",
      escolaridade = "Secondary education",
      motivo_sinistro = "Colisão do veículo",
      id = 331,
      sexo = "woman",
      idade = 18,
      morada = "Vila Real",
      fraud = "no"
    )
  )
})
