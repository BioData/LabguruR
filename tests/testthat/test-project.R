# Tests for project functions

test_that("labguru_add_project requires title", {
  expect_error(labguru_add_project())
})

test_that("labguru_add_project validates title type", {
  expect_error(
    labguru_add_project(title = 123, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_project validates return option", {
  expect_error(
    labguru_add_project(title = "Test", return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_project has correct default for return", {
  args <- formals(labguru_add_project)
  expect_equal(args$return, "id")
})

test_that("labguru_list_projects has correct defaults", {
  args <- formals(labguru_list_projects)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
})

test_that("labguru_list_projects validates page type", {
  expect_error(
    labguru_list_projects(page = "one", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_list_projects validates get_cols option", {
  expect_error(
    labguru_list_projects(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_project requires project_id", {
  expect_error(labguru_get_project())
})

test_that("labguru_get_project validates project_id type", {
  expect_error(
    labguru_get_project(project_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
