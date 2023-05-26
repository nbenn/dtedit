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
    expect_type(field, "closure")
  }

  sl <- unique(dat$Sepal.Length)

  args <- list(
    Sepal.Length = list(
      choices = split(as.character(sl), floor(sl)),
      selected = sl[1L]
    )
  )

  dat$Sepal.Length <- as.factor(dat$Sepal.Length)

  fields <- build_modal_fields(dat,
    cols = c("Sepal.Length", "Petal.Length", "Petal.Width"),
    args = args
  )

  expect_named(fields, c("Sepal.Length", "Petal.Length", "Petal.Width"))
})
