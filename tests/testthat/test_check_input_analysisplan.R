context("check_input_analysisplan()")

# library(testthat)
# setwd("./tests/testthat")
# source("./helper_data.R")
# source("./helper_general.R")



testthat::test_that("check_input_analysisplan: issue format standardised",{

  expect_true(
    output_of_check_is_standardised(
      kobostandards::check_input_analysisplan(analysisplan)
    )
  )

})


testthat::test_that("check_input_analysisplan: no issues on good input",{
  expect_true(
    nrow(kobostandards:::check_input_analysisplan(analysisplan))==0
  )
})










#
#
testthat::test_that("check_input_analysisplan: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input_analysisplan(NULL)},NA)
  expect_error({kobostandards::check_input_analysisplan(NA)},NA)
  expect_error({kobostandards::check_input_analysisplan("just a string")},NA)
  expect_error({kobostandards::check_input_analysisplan(data.frame(x=runif(100)))},NA)

})

# testthat::test_that("check_input_analysisplan: skip test on bad input",{
#   # expect no error [ achieved with "expect_error({},NA)" ]:
#   expect_true(nrow(kobostandards::check_input_analysisplan(NULL))==1)
#
# })
#
#
#
# testthat::test_that("check_input_analysisplan: strata duplicates",{
#
#   # duplicate strata
#   sf_duplicate_strata<-analysisplan
#   sf_duplicate_strata[3,1]<-"a"
#   testthat::expect_true(nrow(check_input_analysisplan(sf_duplicate_strata))==1)
#   testthat::expect_match(check_input_analysisplan(sf_duplicate_strata)$issue,"duplicate")
# })
#
# testthat::test_that("check_input_analysisplan: two columns",{
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
# testthat::test_that("check_input_analysisplan: column types",{
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
# testthat::test_that("check_input_analysisplan: no NA",{
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
# testthat::test_that("check_input_analysisplan: no 0 populations",{
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
