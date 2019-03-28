context("input checker - questionnaire")

testthat::test_that("check_input_questionnaire: issue format standardised",{

  expect_true(output_of_check_is_standardised(check_input_questionnaire(questions,choices)))

})

testthat::test_that("check_input_questionnaire: no issues on good input",{
  expect_true(
    nrow(kobostandards::check_input_questionnaire(questions,choices))==0
    )
})


testthat::test_that("check_input_questionnaire: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input_questionnaire(NULL,NULL)},NA)
  expect_error({kobostandards::check_input_questionnaire(NA,NULL)},NA)
  expect_error({kobostandards::check_input_questionnaire(NA,NA)},NA)
  expect_error({kobostandards::check_input_questionnaire(data.frame(x=runif(100)),data.frame(x=runif(300)))},NA)

})

testthat::test_that("check_input_questionnaire: skip test on bad input",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_true(nrow(check_questions_types(NULL))==1)

})

testthat::test_that("check_questions_types: no issues on good input",{
  expect_true(nrow(check_questions_types(questions))==0)

})

testthat::test_that("check_input_questionnaire: correctly catch bad types",{
  questions_bad_types<-questions
  questions_bad_types$type<-as.character(questions_bad_types$type)
  questions_bad_types$type[1:7]<-c("select_on","select_multiplef","select multiple","select",NA,"notatype",0)
  expect_true(nrow(check_questions_types(questions_bad_types))==7)

})

