#' Check if questionnaire format is good
#' @param questions kobo questions sheet as data frame
#' @param choices kobo choices sheet as data frame
#' @return data frame with issues (see `new_issues()` for details)
#' @export
check_input_questionnaire<-function(questions,choices){

  issues<-check_input_questionnaire_questions(questions)
  add_issues(issues)<-check_input_questionnaire_choices(choices)
  # add_issues(issues)<-check_input_questionnaire_questions_vs_choices(questions,choices)
  return(issues)
}









