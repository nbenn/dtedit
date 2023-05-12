library(shinytest2)

test_that("edit_iris works", {

  expect_s3_class(
    test_app(system.file(package = "dtedit2", "edit_iris")),
    "testthat_results"
  )
})
