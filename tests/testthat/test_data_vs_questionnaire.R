context("check_data_match_questionnaire")

testthat::test_that("check_data_match_questionnaire: issue format standardised",{

  expect_true(
    output_of_check_is_standardised(
    kobostandards::check_data_match_questionnaire(data,questions,choices)
    )
    )

})

testthat::test_that("check_data_match_questionnaire: no issues on good input",{
  expect_true(
    nrow(kobostandards:::check_data_match_questionnaire(data,questions,choices))==0
  )
})


testthat::test_that("check_data_match_questionnaire: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_data_match_questionnaire(NULL,NULL,NULL)},NA)
  expect_error({kobostandards::check_data_match_questionnaire(NA,NA,NULL)},NA)
  expect_error({kobostandards::check_data_match_questionnaire(NULL,NA,NA)},NA)
  expect_error({kobostandards::check_data_match_questionnaire(data.frame(x=runif(100)),
                                                              data.frame(x=runif(100)),
                                                              data.frame(x=runif(300)))},NA)

})

testthat::test_that("check_data_match_questionnaire: skip test on bad input",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_true(nrow(kobostandards::check_data_match_questionnaire(NULL,NULL,NULL))==1)

})
