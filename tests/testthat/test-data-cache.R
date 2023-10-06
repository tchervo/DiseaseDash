test_that("cache write input checking", {
  expect_error(write_cache(mtcars, "error"))
})

test_that("cache reading input checking", {
  expect_error(read_cache("error"))

  # Vaccination data is not supported yet so there should be a stop
  expect_error(read_cache("vax"))
})

test_that("cache checking input checking", {
  expect_error(check_cache("error"))
})
