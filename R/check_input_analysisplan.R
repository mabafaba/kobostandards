#' check analysisplan
#' (this one is for analysisplan inputs to the hypegrammaR package specifically)
#' @param analysisplan the analysisplan as a data.frame
#' @return list of issues (see ?new_issues() for details)
#' @export
check_input_analysisplan<-function(analysisplan){

  issues<-new_issues()

  if(!is.data.frame(analysisplan)){
    add_issues(issues)<-new_issues("could not check analysisplan - not a data.frame",severity = "critical")
    return(issues)
  }

  analysisplan<-df_remove_empty_rows(analysisplan)

  if(nrow(analysisplan)==0){
    add_issues(issues)<-new_issues("could not check analysisplan - 0 rows with values",severity = "critical")
    return(issues)
  }

  add_issues(issues)<-check_input_analysisplan_standard_variables(analysisplan)
  add_issues(issues)<-check_input_analysisplan_has_dependent_variable(analysisplan)
  add_issues(issues)<-check_input_analysisplan_has_independent_variable(analysisplan)
  add_issues(issues)<-check_input_analysisplan_legal_hypothesis_types(analysisplan)
  add_issues(issues)<-check_analysisplan_legal_var_types(analysisplan)

  issues


}



analysisplan_expected_column_names <- c("repeat.for.variable", "research.question",
                           "sub.research.question", "hypothesis", "independent.variable",
                           "dependent.variable", "hypothesis.type", "independent.variable.type",
                           "dependent.variable.type")

check_input_analysisplan_standard_variables<-function(analysisplan){

  if(!is.data.frame(analysisplan)){return(new_issues("could not check analysisplan standard variables (not a data frame)",affected_files = "analysisplan",severity = "critical"))}

  analysisplan<-df_remove_empty_rows(analysisplan)

  expected_colnames_not_found <- analysisplan_expected_column_names[!(analysisplan_expected_column_names %in%
                                                      colnames(analysisplan))]

  new_issues(rep("could not find standardised column names in analysis plan",length(expected_colnames_not_found)),
             affected_files="analysisplan",severity = "critical",comment=paste("missing columns: ",expected_colnames_not_found))
  }



check_input_analysisplan_has_dependent_variable<-function(analysisplan){

  check_analysisplan_variable_no_empty_values(analysisplan,"dependent.variable")

}



check_input_analysisplan_has_independent_variable<-function(analysisplan){

  lacking_columns<-new_issues_df_missing_variables(analysisplan,
                                                   c("dependent.variable","independent.variable","hypothesis.type")
                                                   )


  if(!nrow(lacking_columns)==0){
    lacking_columns$severity<-"critical"
    lacking_columns$affected_files = "analysisplan"
    return(lacking_columns)
  }
  analysisplan<-df_remove_empty_rows(analysisplan)

  must_have_independent<-analysisplan[["hypothesis.type"]] %in% c("group_difference", "change", "correlation")

  check_analysisplan_variable_no_empty_values(analysisplan[must_have_independent,],"independent.variable")


}






check_analysisplan_variable_no_empty_values<-function(analysisplan,variable){

  assertthat::assert_that(assertthat::is.string(variable))
  if(!is.data.frame(analysisplan)){
    return(new_issues(issue = "analysisplan is not a data.frame",affected_files = "analysisplan",severity = "critical",
                      comment=paste("could not check if no missing values in",variable)))
  }

  if(!(variable %in% names(analysisplan))){
    return(new_issues(issue = paste("analysisplan does not contain a column called", variable),affected_files = "analysisplan",severity = "critical",comment=paste("could not check if no missing values in",variable)))
  }

  analysisplan<-df_remove_empty_rows(analysisplan)

  missing<-which((analysisplan[[variable]] %in% c(NA,""," ","NA","N/A")) &
                               apply(analysisplan,1,function(x){!all(is.na(x))}))
  if(length(missing)==0){return(new_issues())}
  new_issues(issue = paste("missing required values for analysis plan column: ",variable),affected_files = "analysisplan",severity = "critical",
             comment = paste("analysisplan rows:",paste0(missing,collapse=", ")))
}


check_input_analysisplan_legal_hypothesis_types<-function(analysisplan){
  col_exists<-new_issues_df_missing_variables(analysisplan,"hypothesis.type",affected_files="analysisplan",severity="critical")
  if(nrow(col_exists)!=0){return(col_exists)}

  analysisplan<-df_remove_empty_rows(analysisplan)
  illegal_types <- analysisplan$hypothesis.type[!(analysisplan$hypothesis.type %in% c("direct_reporting","group_difference","change","correlation","limit"))] %>% as.character
  illegal_types<-unique(illegal_types)
  if(length(illegal_types)==0){return(new_issues())}

  new_issues(paste("illegal hypothesis type: ",illegal_types),affected_files = "analysisplan",severity = "critical",
             comment = paste("allowed types are:",paste("direct_reporting","group_difference","change","correlation","limit",sep=", ")))


}




check_analysisplan_legal_var_types<-function(analysisplan){


  analysisplan<-df_remove_empty_rows(analysisplan)

  issues <- new_issues_df_missing_variables(
                      df = analysisplan,
                      variables = c("dependent.variable.type","independent.variable.type"),
                      affected_files="analysisplan",
                      affected_variables="ALL",
                      severity="critical")

  if(nrow(issues)!=0){return(issues)}

  add_issues(issues)<-new_issues_illegal_values(df = analysisplan,
                                                variable = "dependent.variable.type",
                                                legal_values = c("categorical","numerical"),
                                                affected_files="analysisplan",
                                                affected_variables="ALL",
                                                severity="critical")

  should_independent_var_type_be_empty <- (analysisplan[["independent.variable"]] %in% c(NA, "", " "))


  add_issues(issues)<-new_issues_illegal_values(analysisplan[!should_independent_var_type_be_empty,],
                                                variable = "independent.variable.type",
                                                legal_values = c("categorical","numerical"),
                                                affected_files="analysisplan",
                                                affected_variables="ALL",
                                                severity="critical")
  add_issues(issues)<-new_issues_illegal_values(analysisplan[should_independent_var_type_be_empty,],
                                                variable = "independent.variable.type",
                                                legal_values = c(NA, "", " "),
                                                affected_files="analysisplan",
                                                affected_variables="ALL",
                                                severity="minor",
                                                comment = "independent.variable.type provided, but independent.variable is empty")


  issues
}










df_remove_empty_rows<-function(df){
  value_is_empty<-function(x) {
    if(is.null(x)){return(TRUE)}
    return(x %in% c("",NA,"N/A","NA"))
  }

  df<-lapply(df,function(x){
    x[value_is_empty(x)]<-NA
    x
  }) %>% as.data.frame(stringsAsFactors=F)

  rows_empty<-apply(df,1,function(x){all(value_is_empty(x))})
  df[!rows_empty,,drop=F]

}






