library(shinytest2)

test_that("{shinytest2} recording: edit_iris", {

  app <- AppDriver$new(name = "edit_iris", height = 721, width = 899)

  app$set_inputs(species = "versicolor")
  app$set_inputs(sub_irisdt_rows_selected = 3, allow_no_input_binding_ = TRUE)

  app$click("sub_iris_rm")
  app$click("sub_iris_delete")

  app$expect_values(export = "sub_iris", screenshot_args = FALSE)
})
