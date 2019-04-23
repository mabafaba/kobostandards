rm(list=ls())
setwd("./tests/testthat/")
source("helper_data.R")
library(kobostandards)
library(koboquest)

q<-load_questionnaire(data,questions,choices)




iss<-check_input_analysisplan(samplingframe)


#
#
