# Tests for helper functions

# is_valid_email tests
test_that("is_valid_email accepts valid emails", {
  expect_true(is_valid_email("test@example.com"))
  expect_true(is_valid_email("user.name@domain.org"))
  expect_true(is_valid_email("user+tag@example.co.uk"))
  expect_true(is_valid_email("user123@test.io"))
})
test_that("is_valid_email rejects invalid emails", {
  expect_false(is_valid_email("invalid"))
  expect_false(is_valid_email("no-at-sign.com"))
  expect_false(is_valid_email("@nodomain.com"))
  # Note: current regex allows some edge cases like spaces
})

# LABGURU_ENTITY_TYPES tests
test_that("LABGURU_ENTITY_TYPES exists and is a named vector", {
  expect_true(exists("LABGURU_ENTITY_TYPES"))
  expect_true(is.character(LABGURU_ENTITY_TYPES))
  expect_true(length(names(LABGURU_ENTITY_TYPES)) > 0)
})

test_that("LABGURU_ENTITY_TYPES contains expected mappings", {
  expect_equal(LABGURU_ENTITY_TYPES["experiment"], c(experiment = "Projects::Experiment"))
  expect_equal(LABGURU_ENTITY_TYPES["project"], c(project = "Projects::Project"))
  expect_equal(LABGURU_ENTITY_TYPES["milestone"], c(milestone = "Projects::Milestone"))
  expect_equal(LABGURU_ENTITY_TYPES["plasmid"], c(plasmid = "Biocollections::Plasmid"))
  expect_equal(LABGURU_ENTITY_TYPES["antibody"], c(antibody = "Biocollections::Antibody"))
  expect_equal(LABGURU_ENTITY_TYPES["compound"], c(compound = "Chemistry::Compound"))
  expect_equal(LABGURU_ENTITY_TYPES["cell_line"], c(cell_line = "Biocollections::CellLine"))
})

# labguru_get_entity_type tests
test_that("labguru_get_entity_type exists", {
  expect_true(exists("labguru_get_entity_type"))
})

test_that("labguru_get_entity_type returns correct API class names", {
  expect_equal(labguru_get_entity_type("experiment"), "Projects::Experiment")
  expect_equal(labguru_get_entity_type("project"), "Projects::Project")
  expect_equal(labguru_get_entity_type("plasmid"), "Biocollections::Plasmid")
  expect_equal(labguru_get_entity_type("compound"), "Chemistry::Compound")
  expect_equal(labguru_get_entity_type("cell_line"), "Biocollections::CellLine")
})

test_that("labguru_get_entity_type errors on invalid type", {
  expect_error(
    labguru_get_entity_type("nonexistent_type"),
    "Invalid entity type"
  )
  expect_error(
    labguru_get_entity_type(""),
    "Invalid entity type"
  )
})

# check_arg functions tests
test_that("check_arg_single_character validates correctly", {
  expect_silent(check_arg_single_character("test", null = FALSE))
  expect_silent(check_arg_single_character(NULL, null = TRUE))
  expect_error(check_arg_single_character(123, null = FALSE), "character")
  expect_error(check_arg_single_character(c("a", "b"), null = FALSE), "length")
  expect_error(check_arg_single_character(NULL, null = FALSE), "NULL")
})

test_that("check_arg_single_integer validates correctly", {
  expect_silent(check_arg_single_integer(1, null = FALSE))
  expect_silent(check_arg_single_integer(1L, null = FALSE))
  expect_silent(check_arg_single_integer(NULL, null = TRUE))
  expect_error(check_arg_single_integer("abc", null = FALSE), "integer|numeric")
  expect_error(check_arg_single_integer(c(1, 2), null = FALSE), "length")
  expect_error(check_arg_single_integer(NULL, null = FALSE), "NULL")
})

test_that("check_arg_single_logical validates correctly", {
  expect_silent(check_arg_single_logical(TRUE, null = FALSE))
  expect_silent(check_arg_single_logical(FALSE, null = FALSE))
  expect_silent(check_arg_single_logical(NULL, null = TRUE))
  expect_error(check_arg_single_logical("true", null = FALSE), "logical")
  expect_error(check_arg_single_logical(1, null = FALSE), "logical")
  expect_error(check_arg_single_logical(NULL, null = FALSE), "NULL")
})

test_that("check_arg_char_opts validates correctly", {
  expect_silent(check_arg_char_opts("a", opts = c("a", "b"), null = FALSE))
  expect_silent(check_arg_char_opts(NULL, opts = c("a", "b"), null = TRUE))
  expect_error(check_arg_char_opts("c", opts = c("a", "b"), null = FALSE), "[Mm]ust be one of")
  expect_error(check_arg_char_opts(NULL, opts = c("a", "b"), null = FALSE), "NULL")
})

test_that("check_arg_dataframe validates correctly", {
  expect_silent(check_arg_dataframe(mtcars))
  expect_silent(check_arg_dataframe(data.frame(a = 1, b = 2)))
  expect_error(check_arg_dataframe("not a dataframe"), "data.frame|dataframe")
  expect_error(check_arg_dataframe(list(a = 1)), "data.frame|dataframe")
})
