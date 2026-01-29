# Tests for experiment procedure functions

test_that("labguru_add_experiment_procedure requires name", {
  expect_error(labguru_add_experiment_procedure())
})

test_that("labguru_add_experiment_procedure requires experiment_id", {
  expect_error(
    labguru_add_experiment_procedure(name = "Test", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_experiment_procedure validates name type", {
  expect_error(
    labguru_add_experiment_procedure(name = 123, experiment_id = 1, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_experiment_procedure validates experiment_id type", {
  expect_error(
    labguru_add_experiment_procedure(name = "Test", experiment_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_add_experiment_procedure validates return option", {
  expect_error(
    labguru_add_experiment_procedure(name = "Test", experiment_id = 1, return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_experiment_procedure has correct default for return", {
  args <- formals(labguru_add_experiment_procedure)
  expect_equal(args$return, "id")
})

test_that("labguru_list_experiment_procedures has correct defaults", {
  args <- formals(labguru_list_experiment_procedures)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
  expect_null(args$experiment_id)
})

test_that("labguru_list_experiment_procedures validates get_cols option", {
  expect_error(
    labguru_list_experiment_procedures(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_experiment_procedure requires experiment_procedure_id", {
  expect_error(labguru_get_experiment_procedure())
})

test_that("labguru_get_experiment_procedure validates experiment_procedure_id type", {
  expect_error(
    labguru_get_experiment_procedure(experiment_procedure_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
