# Tests for folder functions

test_that("labguru_add_folder requires title", {
  expect_error(labguru_add_folder())
})

test_that("labguru_add_folder requires project_id", {
  expect_error(
    labguru_add_folder(title = "Test", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_folder validates title type", {
  expect_error(
    labguru_add_folder(title = 123, project_id = 1, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_folder validates project_id type", {
  expect_error(
    labguru_add_folder(title = "Test", project_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_add_folder validates return option", {
  expect_error(
    labguru_add_folder(title = "Test", project_id = 1, return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_folder has correct default for return", {
  args <- formals(labguru_add_folder)
  expect_equal(args$return, "id")
})

test_that("labguru_list_folders has correct defaults", {
  args <- formals(labguru_list_folders)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
  expect_null(args$project_id)
})

test_that("labguru_list_folders validates get_cols option", {
  expect_error(
    labguru_list_folders(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_folder requires folder_id", {
  expect_error(labguru_get_folder())
})

test_that("labguru_get_folder validates folder_id type", {
  expect_error(
    labguru_get_folder(folder_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
