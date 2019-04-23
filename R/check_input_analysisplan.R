#' check analysisplan
#' (this one is for analysisplan inputs to the hypegrammaR package specifically)
#' @param analysisplan the analysisplan as a data.frame
#' @return list of issues (see ?new_issues() for details)
#' @export
check_input_analysisplan<-function(analysisplan){

  if(!is.data.frame(analysisplan)){
    add_issues(issues)<-new_issues("could not check analysisplan - not a data.frame",severity = "critical")
    return(issues)
  }
  if(nrow(analysisplan)==0){
    add_issues(issues)<-new_issues("could not check analysisplan - 0 rows",severity = "critical")
    return(issues)
  }
  issues<-check_input_analysisplan_standard_variables(analysisplan)


}


analysisplan_expected_column_names <- c("repeat.for.variable", "research.question",
                           "sub.research.question", "hypothesis", "independent.variable",
                           "dependent.variable", "hypothesis.type", "independent.variable.type",
                           "dependent.variable.type")

check_input_analysisplan_standard_variables<-function(analysisplan){
  if(!is.data.frame(analysisplan)){return(new_issues("could not check analysisplan standard variables (not a data frame)",affected_files = "analysisplan",severity = "critical"))}

  expected_colnames_not_found <- analysisplan_expected_column_names[!(analysisplan_expected_column_names %in%
                                                      colnames(analysisplan))]

  new_issues(rep("could not find standardised column names in analysis plan",length(expected_colnames_not_found)),
             affected_files="analysisplan",severity = "critical",comment=paste("missing columns: ",expected_colnames_not_found))
  }
