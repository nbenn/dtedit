test_that("type mapping", {

  dat <- datasets::iris

  iris_types <- map_types(dat)

  expect_named(iris_types, names(dat))
  expect_identical(iris_types, c(rep("numericInput", 4L), "selectInput"),
                   ignore_attr = "names")
})

test_that("modal field builder", {

  dat <- datasets::iris

  fields <- build_modal_fields(dat)

  expect_named(fields, names(dat))

  for (field in fields) {
    expect_s3_class(field, "shiny.tag")
  }
})
