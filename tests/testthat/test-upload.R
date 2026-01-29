# Tests for file upload functions

test_that("labguru_upload_file requires file", {
  expect_error(labguru_upload_file())
})

test_that("labguru_upload_file requires title", {
  expect_error(
    labguru_upload_file(file = "test.png", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_upload_file validates file exists", {
  expect_error(
    labguru_upload_file(file = "nonexistent_file.png", title = "Test", server = "https://test.labguru.com", token = "fake"),
    "file|exist|find"
  )
})

test_that("labguru_upload_file validates title type", {
  # Create a temp file for testing

  tmp <- tempfile(fileext = ".png")
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_error(
    labguru_upload_file(file = tmp, title = 123, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_upload_visualization requires file", {
  expect_error(labguru_upload_visualization())
})

test_that("labguru_upload_visualization requires title", {
  expect_error(
    labguru_upload_visualization(file = "test.png", dataset_id = 1, server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_upload_visualization requires dataset_id", {
  expect_error(
    labguru_upload_visualization(file = "test.png", title = "Test", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_upload_visualization validates file exists", {
  expect_error(
    labguru_upload_visualization(file = "nonexistent_file.png", title = "Test", dataset_id = 1, server = "https://test.labguru.com", token = "fake"),
    "file|exist|find"
  )
})

test_that("labguru_upload_visualization validates dataset_id type", {
  # Create a temp file for testing
  tmp <- tempfile(fileext = ".png")
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_error(
    labguru_upload_visualization(file = tmp, title = "Test", dataset_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
