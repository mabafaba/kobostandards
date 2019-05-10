context("check_input_analysisplan()")

# library(testthat)
# setwd("./tests/testthat")
# source("./helper_data.R")
# source("./helper_general.R")
factor_to_character<-function(x){if(is.factor(x)){return(as.character(x))}else{return(x)}}
analysisplan<- purrr::map(analysisplan,factor_to_character) %>% (tibble::as_tibble)

testthat::test_that(" issue format standardised",{

  expect_true(
    output_of_check_is_standardised(
      kobostandards::check_input_analysisplan(analysisplan)
    )
  )

})






testthat::test_that("no issues on good input",{
  issues <- (analysisplan)
  expect_no_issues(check_fun = c(check_input_analysisplan,
                     kobostandards:::check_input_analysisplan_standard_variables,
                     kobostandards:::check_input_analysisplan_has_dependent_variable,
                     kobostandards:::check_input_analysisplan_has_independent_variable,
                     kobostandards:::check_input_analysisplan_legal_hypothesis_types,
                     kobostandards:::check_analysisplan_legal_var_types),

                   analysisplan,
                   label = c("full analysisplan check",
                             "standard variables",
                             "has_dependent_variable",
                             "has_independent_variable",
                             "hypothesis_types",
                             "legal_var_types")
                             )

})




testthat::test_that(" never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input_analysisplan(NULL)},NA)
  expect_error({kobostandards::check_input_analysisplan(NA)},NA)
  expect_error({kobostandards::check_input_analysisplan("just a string")},NA)
  expect_error({kobostandards::check_input_analysisplan(data.frame(x=runif(100)))},NA)

})





testthat::test_that("illegal hypothesis types",{
  bad_types<-analysisplan
  bad_types$hypothesis.type<-as.character(  bad_types$hypothesis.type)
  bad_types$hypothesis.type[c(1,4,6)]<-c("some_illegal_type","another_illegal_type","third_illegal_type")
  expect_true(
    nrow(kobostandards:::check_input_analysisplan_legal_hypothesis_types(bad_types))==3,
    info = paste0(nrow(issues), "issue(s) found instead of 3: ", paste(issues$issue,collapse="\n"))
  )
})

testthat::test_that(" missing independent var values",{
  no_independent<-analysisplan
  no_independent$independent.variable<-as.character(no_independent$independent.variable)
  no_independent$independent.variable[1:4]<-c("",NA," ","NA")
  expect_true(
    nrow(kobostandards:::check_input_analysisplan_has_independent_variable(no_independent))==1
  )
})




testthat::test_that(" missing dependent variable ",{
  no_dependent<-analysisplan
  no_dependent$dependent.variable<-as.character(no_dependent$dependent.variable)
  no_dependent$dependent.variable[1:5]<-c("",NA," ","NA","asdf")


  expect_issues(list(check_input_analysisplan),
                num_issues = 1,
                issue_pattern = "missing required values for analysis plan column:  dependent.variable",
                analysisplan = no_dependent,
                label = "missing dependent variable"
                  )

})




testthat::test_that(" illegal var types ",{

  illegal_dependent_type<-analysisplan
  illegal_dependent_type$dependent.variable.type<-as.character(illegal_dependent_type$dependent.variable.type)
  illegal_dependent_type$dependent.variable.type[c(1,2,3)]<-c("numerix","asdf","")


  expect_true(nrow(kobostandards:::check_analysisplan_legal_var_types(illegal_dependent_type))==3)

  expect_true(all(grepl("dependent.variable.type",kobostandards:::check_input_analysisplan(illegal_dependent_type)$issue)))


  illegal_independent_type<-analysisplan
  illegal_independent_type$independent.variable.type[c(1,5,7)]<-c("numerix","asdf","")

  expect_true(nrow(check_input_analysisplan(illegal_independent_type))==3)
  expect_true(all(grepl("independent.variable.type",check_input_analysisplan(illegal_independent_type)$issue)))


})










# testthat::test_that(" skip test on bad input",{
#   # expect no error [ achieved with "expect_error({},NA)" ]:
#   expect_true(nrow(kobostandards::check_input_analysisplan(NULL))==1)
#
# })
#
#
#
# testthat::test_that(" strata duplicates",{
#
#   # duplicate strata
#   sf_duplicate_strata<-analysisplan
#   sf_duplicate_strata[3,1]<-"a"
#   testthat::expect_true(nrow(check_input_analysisplan(sf_duplicate_strata))==1)
#   testthat::expect_match(check_input_analysisplan(sf_duplicate_strata)$issue,"duplicate")
# })
#
# testthat::test_that(" two columns",{
#
#   # two columns only
#   sf_threecol<-analysisplan
#   sf_threecol$third<-"a"
#   testthat::expect_true(nrow(check_input_analysisplan(sf_threecol))==1)
#   testthat::expect_match(check_input_analysisplan(sf_threecol)$issue,"two columns")
#
#
# })
#
# testthat::test_that(" column types",{
#
#
#   # one numeric, one char/factor column
#   sf_wrongtypes<-analysisplan
#   sf_wrongtypes[3,2]<-"XXX"
#   testthat::expect_true(nrow(check_input_analysisplan(sf_wrongtypes))==1)
#   testthat::expect_match(check_input_analysisplan(sf_wrongtypes)$issue,"numeric")
#
# })
#
#
# testthat::test_that(" no NA",{
#
#   # no NAs
#   sf_na<-analysisplan
#   sf_na[3,2]<-NA
#   sf_na[2,1]<-NA
#   testthat::expect_true(nrow(check_input_analysisplan(sf_na))==1)
#   testthat::expect_match(check_input_analysisplan(sf_na)$issue,"NA")
#
#   # no NAs unless all NAs
#   sf_na[3,]<-NA
#   sf_na[2,]<-NA
#   testthat::expect_true(nrow(check_input_analysisplan(sf_na))==0)
# })
#
# testthat::test_that(" no 0 populations",{
#
#   # pop 0:
#
#   sf_0pop<-analysisplan
#   sf_0pop[2,2]<-0
#   sf_0pop[,2]<-as.numeric(sf_0pop[,2])
#   testthat::expect_match(check_input_analysisplan(sf_0pop)$issue,"0|zero")
#
#
#
# })
#
#
#
#
#
#
#
#
#
#
#
#
#
#
