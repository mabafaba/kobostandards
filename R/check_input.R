#' Check if Assessment files are in order
#'
#' @param data the dataset ( from load_data() )
#' @param questionnaire the questionnaire ( from load_questionnaire() )
#' @param samplingframe the samplingframe ( from load_samplingframe() )
#' @param analysisplan
#' @export
check_input<-function(data=NULL,questions=NULL,choices=NULL,samplingframe=NULL,analysisplan=NULL){

  data_issues<-check_input_data(data)
  questions_issues<-check_input_questionnaire_questions(questions)
  choices_issues<-check_input_questionnaire_choices(choices)
  data_questionnaire_issues<-check_input_match_data_questionnaire(data,questions,choices)

  rbind(data_issues,questions_issues,choices_issues,data_questionnaire_issues)

}
