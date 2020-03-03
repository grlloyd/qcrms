context("test-dopca")

test_that("dopca creates correct plot object", {
  out <- doPCA (Data = testData$prepareData_mv_impute_glog_pqn$Data,
    classes = testData$prepareData_mv_impute_glog_pqn$classes,
    qc_label="QC", PQN=TRUE, glogScaling=TRUE, scale=FALSE, mv_impute=TRUE,
    loadings=TRUE)
  expect_true(is(out[[1]], "ggplot"))
  expect_true(is(out[[2]], "ggplot"))
})

test_that("dopca creates correct plot object without sample labels", {
  out <- doPCA (Data = testData$prepareData_mv_impute_glog_pqn$Data,
    classes=testData$prepareData_mv_impute_glog_pqn$classes,
    qc_label="QC", PQN=TRUE, glogScaling=TRUE, scale=FALSE, mv_impute=TRUE,
    loadings=FALSE, labels="none")
  expect_true (is(out, "ggplot"))
})
