# Tests for dataset functions

test_that("labguru_upload_dataset requires dataset", {
  expect_error(labguru_upload_dataset())
})

test_that("labguru_upload_dataset requires name", {
  expect_error(
    labguru_upload_dataset(dataset = mtcars, server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_upload_dataset validates dataset is data.frame", {
  expect_error(
    labguru_upload_dataset(dataset = "not a dataframe", name = "test", server = "https://test.labguru.com", token = "fake"),
    "data.frame|dataframe"
  )
})

test_that("labguru_upload_dataset validates name type", {
  expect_error(
    labguru_upload_dataset(dataset = mtcars, name = 123, server = "https://test.labguru.com", token = "fake"),
    "character"
  )
})

test_that("labguru_list_datasets has correct defaults", {
  args <- formals(labguru_list_datasets)
  expect_equal(args$page, 1)
  expect_equal(args$get_cols, "limited")
})

test_that("labguru_list_datasets validates page type", {
  expect_error(
    labguru_list_datasets(page = "one", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_list_datasets validates get_cols option", {
  expect_error(
    labguru_list_datasets(get_cols = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_download_dataset requires dataset_id", {
  expect_error(labguru_download_dataset())
})

test_that("labguru_download_dataset validates dataset_id type", {
  expect_error(
    labguru_download_dataset(dataset_id = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})
