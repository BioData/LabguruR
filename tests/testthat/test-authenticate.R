# Tests for authentication functions

test_that("labguru_authenticate requires email", {
  expect_error(labguru_authenticate())
})
test_that("labguru_authenticate requires password", {
  expect_error(labguru_authenticate(email = "test@example.com"))
})

test_that("labguru_authenticate validates email format", {
  expect_error(
    labguru_authenticate(email = "invalid", password = "pass"),
    "valid email"
  )
})

test_that("labguru_set_token requires token", {
  expect_error(labguru_set_token())
})

test_that("labguru_set_token accepts valid token", {
  # Should not error with valid inputs

  expect_silent(
    labguru_set_token(token = "test_token", server = "https://test.labguru.com", set_sys = FALSE)
  )
})

test_that("labguru_set_token returns token when set_sys = FALSE", {
  result <- labguru_set_token(token = "my_token", server = "https://test.labguru.com", set_sys = FALSE)
  expect_equal(result, "my_token")
})

test_that("labguru_valid_token runs without error", {
  expect_output(labguru_valid_token(token = "test_token"), "not implemented")
})

test_that("labguru_sys_variables runs without error", {
  expect_output(labguru_sys_variables(), "LABGURU")
})
