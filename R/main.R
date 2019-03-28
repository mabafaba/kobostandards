

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
             choices_issues<-check_input_questionnaire_choices(questions)
  data_questionnaire_issues<-check_data_match_questionnaire(data,questions,choices)

  rbind(data_issues,questions_issues,choices_issues,data_questionnaire_issues)

}



#' Check if data format is good
#' @param data data as data frame
#' @return data frame with issues (see `new_issues()` for details)
#' @export
check_input_data<-function(data){
  # data<-data.frame(a=runif(10),b=sample(1:10),a=c(21:30))
  # names(data)<-c("a","b","a")
  issues<-new_issues()

  if(!is.data.frame(data)){
    add_issues(issues)<-new_issues("data is not a data.frame",affected_files = "data",affected_variables = "ALL",severity = "critical",comment = "all other data checks could not be performed")
    return(issues)
    }

  if(any(duplicated(names(data)))){
    non_unique_data_headers<-unique(names(data)[duplicated(names(data))])
    add_issues(issues)<-new_issues(rep("duplicate data column header",length(non_unique_data_headers)),
                                   "data",
                                   non_unique_data_headers,
                                   severity = "critical")
  }


  return(issues)



}


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



#' check_input_questionnaire_match_data<-function(questionnaire,data){
#'
#'   select_ones<-names(data)[sapply(names(data),questionnaire$question_is_select_one)]
#'   select_multiples<-names(data)[sapply(names(data),questionnaire$question_is_select_multiple)]
#'   numeric<-names(data)[sapply(names(data),questionnaire$question_is_numeric)]
#'
#'   d_not_found_in_q<-check_questionnaire_data_questions_found(questionnaire,data)
#'
#'
#'   select_one_questions<-questionnaire$question_is_select_one(names(data))
#'
#'   questionnaire$question_is_skipped(data,"asdf")
#'
#'   koboquest:::extract_all_varnames_from_condition()
#' }
#'
#'
#' check_questionnaire_data_questions_found<-function(questionnaire,data){
#'   names_found<-sapply(names(data),questionnaire$question_in_questionnaire)
#'   names_not_found<-names(names_found)[!names_found]
#'   not_found_but_is_sm_choice<-sapply(names_not_found,questionnaire$question_is_sm_choice)
#'   names_not_found<-names_not_found[!not_found_but_is_sm_choice]
#'   issues<-tibble(identifier=names_not_found,issue="data column not found in questionnaire")
#'   return(issues)
#' }
#'
#'
#'
#'
#'
#'
#' check_input_samplingframe_match_data<-function(samplingframe,data){
#' }
#'
#' check_input_samplingframe_match_questionnaire<-function(samplingframe,data){}
#'
#' check_input_questionnaire_match_analysisplan<-function(samplinframe,data){}
#'
#' check_input_analysisplan_match_data<-function(samplinframe,data){}
#'
#'
#'
#'
#'
