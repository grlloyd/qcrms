context("test-do-variability_list_RSD")

test_that("Check sonsistency of doRSD function output", {
  out <- do_variability_list (testData$data, testData$class)
  expect_equal(out , testData$doRSD)
})
