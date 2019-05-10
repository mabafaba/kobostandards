
#' Check if questionnaire choice sheet format is good
#' @param choices kobo choices sheet as data frame
#' @return data frame with issues (see `new_issues()` for details)
#' @export
check_input_questionnaire_choices<-function(choices){

  issues<-new_issues()

  if(!is.data.frame(choices)){
    add_issues(issues)<-new_issues(issue = "choices is not a data frame",
                                   affected_files = "choices",
                                   affected_variables = "ALL",
                                   severity = "critical",
                                   comment = "could not check questionnaire choices")
  }

  add_issues(issues)<-check_choices_headers(choices)

  issues
}





check_choices_headers<-function(choices){
  new_issues_df_missing_variables(choices,c("name","list_name"),
                              affected_files = "choices",
                              affected_variables = "ALL",
                              severity = "critical")
}







