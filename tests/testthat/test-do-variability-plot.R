context("test-dorsdplot")

test_that("That violin plot of RSD values is created", {
  out <- do_variability_plot (list_object = testData$doRSD, plotTitle = "Test", subtitle = "sub title", ylim=200)
  expect_equal(out, testData$do_variability_plot_rsd)
})
