context("test-preparedata")

test_that("Prepare data returns expected output, mv imputation only", {
  out <- prepareData(Data=testData$data, classes=testData$class,
              blank = "Blank", PQN=F, mv_impute = T, glogScaling = F,
              qc_label = "QC", ignorelabel = "Removed")
  
  # Remove which method was used for RSD calculations
  out$RSD$variability_method <- NULL
  
  expect_equal(out$RSD, testData$prepareData_mv_impute$RSD)
  expect_equal(out$classes, testData$prepareData_mv_impute$classes)
  expect_equal(as.data.frame(out$Data), testData$prepareData_mv_impute$Data)
})

test_that("Prepare data returns expected output, mv imputation and glog", {
  out <- prepareData(Data=testData$data, classes=testData$class,
                     blank = "Blank", PQN=F, mv_impute = T, glogScaling = T,
                     qc_label = "QC", ignorelabel = "Removed")
  # Remove which method was used for RSD calculations
  out$RSD$variability_method <- NULL
  expect_equal(out$RSD, testData$prepareData_mv_impute_glog$RSD)
  expect_equal(out$classes, testData$prepareData_mv_impute_glog$classes)
  expect_equal(as.data.frame(out$Data), testData$prepareData_mv_impute_glog$Data)
})

test_that("Prepare data returns expected output, mv imputation, glog and pqn", {
  out <- prepareData(Data=testData$data, classes=testData$class,
                     blank = "Blank", PQN=T, mv_impute = T, glogScaling = T,
                     qc_label = "QC", ignorelabel = "Removed")
  # Remove which method was used for RSD calculations
  out$RSD$variability_method <- NULL
  expect_equal(as.data.frame(out$Data), testData$prepareData_mv_impute_glog_pqn$Data)
  expect_equal(out$RSD, testData$prepareData_mv_impute_glog_pqn$RSD)
  expect_equal(out$classes, testData$prepareData_mv_impute_glog_pqn$classes)
})

test_that("If all values are missing the row or columns will be removed", {
  Data <- testData$data
  Data[7, ] <- NA
  out <- prepareData(Data=Data, classes=testData$class,
                     blank = "Blank", PQN=F, mv_impute = T, glogScaling = F,
                     qc_label = NULL, ignorelabel = "Removed")
  # Remove which method was used for RSD calculations
  out$RSD$variability_method <- NULL
  expect_equal(as.data.frame(out$Data), testData$prepareData_test_NA)
})

