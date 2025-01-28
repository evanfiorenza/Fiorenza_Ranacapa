library(shinytest2)

test_that("Initial snapshot values are consistent", {
  app <- AppDriver$new(name = "init")
  app$expect_values()
})
