context("Test edition metadata retrieval")

test_that("Basic edition retrieval works", {
  results <- guardian_editions("test", "uk")
  expect_equal(length(results), 4)
  expect_equal(names(results), c("status", "userTier", "total", "results"))
})
