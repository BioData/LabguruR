# Tests for IC50 functions

test_that("labguru_ic50_upload requires table", {
  expect_error(labguru_ic50_upload())
})

test_that("labguru_ic50_upload requires name", {
  expect_error(
    labguru_ic50_upload(table = mtcars, results_dir = ".", img_pdf = "img", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_ic50_upload requires results_dir", {
  expect_error(
    labguru_ic50_upload(table = mtcars, name = "Test", img_pdf = "img", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_ic50_upload requires img_pdf", {
  expect_error(
    labguru_ic50_upload(table = mtcars, name = "Test", results_dir = ".", server = "https://test.labguru.com", token = "fake")
  )
})

test_that("labguru_ic50_upload validates table is data.frame", {
  expect_error(
    labguru_ic50_upload(table = "not a dataframe", name = "Test", results_dir = ".", img_pdf = "img", server = "https://test.labguru.com", token = "fake"),
    "data.frame|dataframe"
  )
})

test_that("labguru_ic50_upload validates img_pdf option", {
  expect_error(
    labguru_ic50_upload(table = mtcars, name = "Test", results_dir = ".", img_pdf = "invalid", server = "https://test.labguru.com", token = "fake"),
    "[Mm]ust be one of"
  )
})

test_that("labguru_ic50_analysis returns NULL when ic50 package not loaded", {
  expect_message(
    result <- labguru_ic50_analysis(plate_id = 1, server = "https://test.labguru.com", token = "fake"),
    "ic50 package"
  )
  expect_null(result)
})

test_that("labguru_download_plate requires plate", {
  expect_error(labguru_download_plate())
})

test_that("labguru_download_plate validates plate type", {
  expect_error(
    labguru_download_plate(plate = "abc", server = "https://test.labguru.com", token = "fake"),
    "integer|numeric"
  )
})

test_that("labguru_download_plate has correct default for dir", {
  args <- formals(labguru_download_plate)
  expect_equal(args$dir, "./plate")
})
