context("button group input")

test_that("id argument", {
  expect_missing_id_error(buttonGroupInput())
  expect_error(buttonGroupInput(id = 2, labels = "LABEL"))
  expect_silent(buttonGroupInput(id = "ID", labels = "LABELS"))
})

test_that("labels argument", {
  expect_error(buttonGroupInput(id = "ID", labels = c("A", "B"), values = "C"))
  expect_silent(buttonGroupInput(id = "ID", labels = c("A", "B")))
  expect_silent(buttonGroupInput(id = "ID", labels = c("A", "B"), values = c("C", "D")))
})

test_that("has dependencies", {
  expect_dependencies(buttonGroupInput("ID"))
})
