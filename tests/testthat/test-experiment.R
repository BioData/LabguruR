# Tests for experiment functions

test_that("labguru_add_experiment requires title", {
  expect_error(labguru_add_experiment())
})

test_that("labguru_add_experiment requires project_id", {
  expect_error(
    labguru_add_experiment(title = "Test", folder_id = 1, server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_experiment requires folder_id", {
  expect_error(
    labguru_add_experiment(title = "Test", project_id = 1, server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_experiment validates title type", {
  expect_error(
    labguru_add_experiment(title = 123, project_id = 1, folder_id = 1, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_experiment validates return option", {
  expect_error(
    labguru_add_experiment(title = "Test", project_id = 1, folder_id = 1, return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_experiment has correct default for return", {
  args <- formals(labguru_add_experiment)
  expect_equal(args$return, "id")
})

test_that("labguru_list_experiments has correct defaults", {
  args <- formals(labguru_list_experiments)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
  expect_null(args$folder_id)
})

test_that("labguru_list_experiments validates get_cols option", {
  expect_error(
    labguru_list_experiments(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_experiment requires experiment_id", {
  expect_error(labguru_get_experiment())
})

test_that("labguru_get_experiment validates experiment_id type", {
  expect_error(
    labguru_get_experiment(experiment_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_add_attachment_to_experiment requires file", {
  expect_error(labguru_add_attachment_to_experiment())
})

test_that("labguru_add_attachment_to_experiment requires experiment_id", {
  expect_error(
    labguru_add_attachment_to_experiment(file = "test.png", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_attachment_to_experiment validates file exists", {
  expect_error(
    labguru_add_attachment_to_experiment(file = "nonexistent_file.png", experiment_id = 1, server = "https://test.labguru.com", token = "fake"),
    "file|exist"
  )
})
