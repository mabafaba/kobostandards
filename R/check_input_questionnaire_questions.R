
#' Check if questionnaire questions sheet format is good
#' @param questions kobo questions sheet as data frame
#' @return data frame with issues (see `new_issues()` for details)
#' @export
check_input_questionnaire_questions<-function(questions){


  issues<-new_issues()
  if(!is.data.frame(questions)){
    add_issues(issues)<-new_issues(issue = "questions are not a data frame",
                                   affected_files = "questions",
                                   affected_variables = "ALL",
                                   severity = "critical")
  }


  if(!("type"%in%names(questions))){
    add_issues(issues)<-new_issues(issue = "questions do not contain column 'type'",
                                   affected_files = "questions",
                                   affected_variables = "ALL",
                                   severity = "critical")
  }
  if(!("name"%in%names(questions))){
    add_issues(issues)<-new_issues(issue = "questions do not contain column 'name'",
                                   affected_files = "questions",
                                   affected_variables = "ALL",
                                   severity = "critical")
  }
  if(!(length(grep("label",names(questions)))>0)){
    add_issues(issues)<-new_issues(issue = "questions do not contain a column with the word 'label'",
                                   affected_files = "questions",
                                   affected_variables = "ALL",
                                   severity = "minor")
  }

  add_issues(issues)<-check_questions_types(questions)
  add_issues(issues)<-check_questions_relevant(questions)
  add_issues(issues)<-check_questions_alphanumeric_lowercase_names(questions)



  return(issues)

}





check_questions_alphanumeric_lowercase_names<-function(questions){
  if(is.null(questions)){return(new_issues())}
  if(!is.data.frame(questions)){return(new_issues())}

  question_names <- as.character(questions$name[!(questions$name %in% c(NA,""," "))])
  invalid_names<-question_names[question_names!=xlsformfill:::to_alphanumeric_lowercase(question_names)]
  if(length(invalid_names)==0){
    return(new_issues())
  }
  new_issues(rep("question name should contain only lower case letters, numbers, dots and underscores",length(invalid_names))
                             ,affected_files = "questionnaire questions",
                             affected_variables = invalid_names,severity = "problematic")
}


check_questions_types<-function(questions){

  default_issue_when_skipped<-new_issues("question types could not be checked",
                                         "questions",
                                         "ALL",
                                         "critical",
                                         "'questions' format has another critical issue that made this check impossible")

  if(is.null(questions)){return(default_issue_when_skipped)}
  if(!is.data.frame(questions)){return(default_issue_when_skipped)}
  if(!("type" %in% colnames(questions))){return(default_issue_when_skipped)}

  questions<-questions[apply(questions,1,function(x){!all(is.na(x)|x=="")}),]
  illegal_rows<-which(!questionnaire_questions_is_type_legal(as.character(questions$type)))
  new_issues(issue = rep("illegal question type",length(illegal_rows)),
             affected_files = "questions",
             affected_variables = questions$name[illegal_rows],
             severity = "problematic",
             comment = paste("illegal type value:",as.character(questions$type[illegal_rows]))
  )
}


check_questions_relevant<-function(questions){

  if(!is.data.frame(questions)){
    return(
      new_issues("'relevant' skip logic could not be checked","questions","ALL","critical","questions not a data frame or no 'relevant' column")
    )
  }

  if(is.null(questions$relevant)){
    return(new_issues("no 'relevant' column; + 'relevant' skip logic could not be tested","questions","ALL","critical",""))
  }

  condition_vars<-lapply(questions$relevant,koboquest:::extract_all_varnames_from_condition)
  condition_vars_found<-lapply(condition_vars,function(x){x %in% questions$name})


  condition_vars<-do.call(rbind,
                          mapply(function(vars,found,id){if(length(vars)==0){return(NULL)}
                            return(tibble::tibble(vars,found,id))
                          },condition_vars,condition_vars_found,1:length(condition_vars)) )

  if(is.null(condition_vars)){return(new_issues())}
  condition_vars_not_found<-condition_vars[!condition_vars$found,]

  vars_not_found_issues<-new_issues(issue=paste0("variable '",condition_vars_not_found[,"vars"],"' used in 'relevant' condition not found in question 'name'"),
                                    affected_files = 'questions',
                                    severity = "critical",
                                    affected_variables = as.character(questions$name)[condition_vars_not_found$id],
                                    comment = "skip logic will not be calculated correctly")


  # evaluate conditions on fake data
  ## prepare fake data
  fakevars<-condition_vars$vars[!is.na(condition_vars$vars)] %>% unique
  fakedata<-purrr::map(fakevars,function(x){sample(1:10)})
  names(fakedata)<-fakevars
  fakedata<-tibble::as_tibble(fakedata)
  q_relevant<-q
  q_relevant<-questions[!(questions$relevant %in% c(NA,""," ")),]
  q_relevant$conditions_as_r<-koboquest:::rify_condition(q_relevant$relevant)
  # run conditions
  not<-function(x){!x}
  attach(fakedata)
  did_condition_evaluate<-do.call(rbind,lapply(q_relevant$conditions_as_r,function(condition){
    tryCatch({
      eval(parse(text = condition))
      return(tibble::tibble(success=TRUE,message=""))
    },error=function(e){
      return(tibble::tibble(success=F,message=e$message))
    })
  }))

  detach(fakedata)

  # format results
  did_condition_evaluate$condition<-q_relevant$relevant
  evaluation_failed<-data.frame(did_condition_evaluate[!did_condition_evaluate$success,],q_relevant[!did_condition_evaluate$success,])

  eval_issues<-kobostandards:::new_issues(rep("failed to evaluate 'relevant' condition",
                                              nrow(evaluation_failed)),
                                          affected_files = "questions",
                                          affected_variables = evaluation_failed$name,
                                          severity = "critical",
                                          comment = paste0("condition that failed: '",evaluation_failed$relevant,"'.  --- evaluation failed with error:  ",
                                                           evaluation_failed$message))

  add_issues(vars_not_found_issues)<-eval_issues
  vars_not_found_issues
}
