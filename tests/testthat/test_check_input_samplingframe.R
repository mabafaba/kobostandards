context("check_input_samplingframe()")


# library(testthat)
# setwd("./tests/testthat")
# source("./helper_data.R")
# source("./helper_general.R")
samplingframe<-data.frame(strata=letters[1:5],pop=rep(100,5))

testthat::test_that("check_input_samplingframe: issue format standardised",{

  expect_true(
    output_of_check_is_standardised(
      kobostandards::check_input_samplingframe(samplingframe)
    )
  )

})

testthat::test_that("check_input_samplingframe: no issues on good input",{
  expect_true(
    nrow(kobostandards:::check_input_samplingframe(samplingframe))==0
  )
})


testthat::test_that("check_input_samplingframe: never throws an error",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_error({kobostandards::check_input_samplingframe(NULL)},NA)
  expect_error({kobostandards::check_input_samplingframe(NA)},NA)
  expect_error({kobostandards::check_input_samplingframe("just a string")},NA)
  expect_error({kobostandards::check_input_samplingframe(data.frame(x=runif(100)))},NA)

})

testthat::test_that("check_input_samplingframe: skip test on bad input",{
  # expect no error [ achieved with "expect_error({},NA)" ]:
  expect_true(nrow(kobostandards::check_input_samplingframe(NULL))==1)

})



testthat::test_that("check_input_samplingframe: strata duplicates",{

  # duplicate strata
  sf_duplicate_strata<-samplingframe
  sf_duplicate_strata[3,1]<-"a"
  testthat::expect_true(nrow(check_input_samplingframe(sf_duplicate_strata))==1)
  testthat::expect_match(check_input_samplingframe(sf_duplicate_strata)$issue,"duplicate")
})

testthat::test_that("check_input_samplingframe: two columns",{

  # two columns only
  sf_threecol<-samplingframe
  sf_threecol$third<-"a"
  testthat::expect_true(nrow(check_input_samplingframe(sf_threecol))==1)
  testthat::expect_match(check_input_samplingframe(sf_threecol)$issue,"two columns")


})

testthat::test_that("check_input_samplingframe: column types",{


  # one numeric, one char/factor column
  sf_wrongtypes<-samplingframe
  sf_wrongtypes[3,2]<-"XXX"
  testthat::expect_true(nrow(check_input_samplingframe(sf_wrongtypes))==1)
  testthat::expect_match(check_input_samplingframe(sf_wrongtypes)$issue,"numeric")

})


testthat::test_that("check_input_samplingframe: no NA",{

  # no NAs
  sf_na<-samplingframe
  sf_na[3,2]<-NA
  sf_na[2,1]<-NA
  testthat::expect_true(nrow(check_input_samplingframe(sf_na))==1)
  testthat::expect_match(check_input_samplingframe(sf_na)$issue,"NA")

  # no NAs unless all NAs
  sf_na[3,]<-NA
  sf_na[2,]<-NA
  testthat::expect_true(nrow(check_input_samplingframe(sf_na))==0)
})

testthat::test_that("check_input_samplingframe: no 0 populations",{

  # pop 0:

  sf_0pop<-samplingframe
  sf_0pop[2,2]<-0
  sf_0pop[,2]<-as.numeric(sf_0pop[,2])
  testthat::expect_match(check_input_samplingframe(sf_0pop)$issue,"0|zero")



})














