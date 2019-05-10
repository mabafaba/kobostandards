context("check_input_match_data_questionnaire()")

testthat::test_that("check_input_match_data_questionnaire: issue format standardised",{

  expect_true(
    output_of_check_is_standardised(
    kobostandards::check_input_match_data_questionnaire(data,questions,choices)
    )
    )

})

testthat::test_that("check_input_match_data_questionnaire: no issues on good input",{
  expect_true(
    nrow(kobostandards:::check_input_match_data_questionnaire(data,questions,choices))==0
  )
})

testthat::test_that("check_input_match_data_questionnaire: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input_match_data_questionnaire(NULL,NULL,NULL)},NA)
  expect_error({kobostandards::check_input_match_data_questionnaire(NA,NA,NULL)},NA)
  expect_error({kobostandards::check_input_match_data_questionnaire(NULL,NA,NA)},NA)
  expect_error({kobostandards::check_input_match_data_questionnaire(data.frame(x=runif(100)),
                                                              data.frame(x=runif(100)),
                                                              data.frame(x=runif(300)))},NA)
})

testthat::test_that("check_input_match_data_questionnaire: skip test on bad input",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_true(nrow(kobostandards::check_input_match_data_questionnaire(NULL,NULL,NULL))==1)

})

testthat::test_that("check_data_values_in_choices: finds data choice not in questionnaire",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  data_fake_choices<-data
  data_fake_choices$denselypopulated
  data_fake_choices$denselypopulated<-as.character(data_fake_choices$denselypopulated)
  data_fake_choices$denselypopulated[sample(1:nrow(data_fake_choices))]<-"not_a_choice"
  issues<-check_data_values_in_choices(data_fake_choices,questionnaire = koboquest:::load_questionnaire(data,questions,choices))
  testthat::expect_true(nrow(issues)==1)
  testthat::expect_true(grepl("not_a_choice",issues$comment))

})






















