context("Check simulations")
source("simulate_reads.R")
test_that("true seq creation function works", {
  expect_true(all(colSums(make_true_seqs(10, 5)) > 0))
  expect_true(all(colSums(make_true_seqs(20, 3)) < 3))
  expect_true(all(make_true_seqs(15, 4) %in% c(0,1)))
  
  #new tests for question 3
  expect_equal(rowSums(make_true_seqs( n_snps = 8, n_true_seqs = 4, num_nonzero_per_site = 8)), 8)
  expect_true( rowSums(make_true_seqs( n_snps = 8, n_true_seqs = 4, num_nonzero_per_site = 100)) < 100)
})