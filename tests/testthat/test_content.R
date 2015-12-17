context("Test content can be retrieved")

test_that("Basic content queries work", {
  
  result <- guardian_content("test", "debate AND economy")
  expect_equal(length(result), 9)
  expect_equal(names(result), c("status", "userTier", "total", "startIndex", "pageSize", "currentPage", 
                                "pages", "orderBy", "results"))
})

test_that("More complex queries (specifying tags and dates) work", {
  result <- guardian_content("test", "debate AND economy", from_date = "2014-01-01", tag = "politics/politics")
  expect_equal(length(result), 9)
  expect_equal(names(result), c("status", "userTier", "total", "startIndex", "pageSize", "currentPage", 
                                "pages", "orderBy", "results"))
})
