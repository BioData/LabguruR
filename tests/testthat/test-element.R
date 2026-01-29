# Tests for element functions

test_that("labguru_add_element requires experiment_procedure_id", {
  expect_error(labguru_add_element())
})

test_that("labguru_add_element requires exactly one of data, img_id, or rscript", {
  expect_error(
    labguru_add_element(experiment_procedure_id = 1, server = "https://test.labguru.com", token = "fake"),
    "One of"
  )
})

test_that("labguru_add_element rejects multiple content types", {
  expect_error(
    labguru_add_element(data = "<p>test</p>", img_id = 1, experiment_procedure_id = 1, server = "https://test.labguru.com", token = "fake"),
    "One of"
  )
})

test_that("labguru_add_element validates data type", {
  expect_error(
    labguru_add_element(data = 123, experiment_procedure_id = 1, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_element validates experiment_procedure_id type", {
  expect_error(
    labguru_add_element(data = "<p>test</p>", experiment_procedure_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_add_element validates return option", {
  expect_error(
    labguru_add_element(data = "<p>test</p>", experiment_procedure_id = 1, return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_element has correct default for return", {
  args <- formals(labguru_add_element)
  expect_equal(args$return, "id")
})

test_that("labguru_list_elements has correct defaults", {
  args <- formals(labguru_list_elements)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
  expect_null(args$experiment_id)
})

test_that("labguru_list_elements validates get_cols option", {
  expect_error(
    labguru_list_elements(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_element requires element_id", {
  expect_error(labguru_get_element())
})

test_that("labguru_get_element validates element_id type", {
  expect_error(
    labguru_get_element(element_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
