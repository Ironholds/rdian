context("Test section metadata retrieval")

test_that("Basic section retrieval works", {
  results <- guardian_sections("test", "business")
  expect_equal(length(results), 4)
  expect_equal(names(results), c("status", "userTier", "total", "results"))
})
