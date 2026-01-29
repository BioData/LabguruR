# Tests for comment functions

test_that("labguru_add_comment requires comment", {
  expect_error(labguru_add_comment())
})

test_that("labguru_add_comment requires commentable_id", {
  expect_error(
    labguru_add_comment(comment = "Test", commentable_type = "experiment", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_comment requires commentable_type", {
  expect_error(
    labguru_add_comment(comment = "Test", commentable_id = 1, server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_add_comment validates comment type", {
  expect_error(
    labguru_add_comment(comment = 123, commentable_id = 1, commentable_type = "experiment", server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_add_comment validates commentable_id type", {
  expect_error(
    labguru_add_comment(comment = "Test", commentable_id = "abc", commentable_type = "experiment", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_add_comment validates commentable_type", {
  expect_error(
    labguru_add_comment(comment = "Test", commentable_id = 1, commentable_type = "invalid_type", server = "https://test.labguru.com", token = "fake"),
    "Invalid entity type"
  )
})

test_that("labguru_add_comment accepts valid commentable_type values", {
  # Test that valid types don't trigger "Invalid entity type" error
  # They will fail on API call, but that's expected
  valid_types <- c("experiment", "project", "plasmid")
  for (type in valid_types) {
    err <- tryCatch(
      labguru_add_comment(comment = "Test", commentable_id = 1, commentable_type = type, server = "https://test.labguru.com", token = "fake"),
      error = function(e) e$message
    )
    expect_false(grepl("Invalid entity type", err))
  }
})

test_that("labguru_add_comment validates return option", {
  expect_error(
    labguru_add_comment(comment = "Test", commentable_id = 1, commentable_type = "experiment", return = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_add_comment has correct default for return", {
  args <- formals(labguru_add_comment)
  expect_equal(args$return, "id")
})

test_that("labguru_list_comments has correct defaults", {
  args <- formals(labguru_list_comments)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
})

test_that("labguru_list_comments validates page type", {
  expect_error(
    labguru_list_comments(page = "one", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_list_comments validates get_cols option", {
  expect_error(
    labguru_list_comments(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_get_comment requires comment_id", {
  expect_error(labguru_get_comment())
})

test_that("labguru_get_comment validates comment_id type", {
  expect_error(
    labguru_get_comment(comment_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
