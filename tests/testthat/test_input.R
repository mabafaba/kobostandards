library(testthat)
context("check_input() overall")


testthat::test_that("check_input: issue format standardised",{
  expect_true(output_of_check_is_standardised(check_input(data,questions,choices)))

})

testthat::test_that("check_input_questionnaire: no issues on good input",{
  expect_true(
    nrow(kobostandards::check_input(data,questions,choices))==0
  )
})


testthat::test_that("check_input: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input(NULL,NULL)},NA)
  expect_error({kobostandards::check_input(NA,NULL)},NA)
  expect_error({kobostandards::check_input(NA,NA)},NA)
  expect_error({kobostandards::check_input(c(1:10),letters[1:10])},NA)
  expect_error({kobostandards::check_input(data.frame(x=runif(100)),data.frame(x=runif(100)),data.frame(x=runif(300)))},NA)

})
