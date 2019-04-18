

check_data_match_questionnaire<-function(data,questions,choices){

  could_not_check_issue<-new_issues("could not check data against questionnaire",
                                    affected_variables = "ALL",
                                    severity = "critical")

  if(any(sapply(list(data,questions,choices),is.null))){
    could_not_check_issue$comment<- "One or more of data, questions or choices was NULL"
    return(could_not_check_issue)
  }

  if(!all(sapply(list(data,questions,choices),is.data.frame))){
    could_not_check_issue$comment<- "one or more of data, questions or choices was not a data frame"
    return(could_not_check_issue)
  }

  questionnaire<-tryCatch({
  koboquest::load_questionnaire(data,
                                   questions,
                                   choices,
                                   choices.label.column.to.use = "label..English")
  },error=function(e){
    could_not_check_issue$message<-paste0("loading questionnaire failed with error: ",e$message)
    return(could_not_check_issue)
  })

  if(is.data.frame(questionnaire)){
    return(could_not_check_issue)
  }

  issues<-check_all_variables_in_questionnaire(names(data),questionnaire)

  add_issues(issues)<-check_data_types_match_questionnaire(data, questionnaire)
  add_issues(issues)<-        check_data_values_in_choices(data, questionnaire)
}


check_all_variables_in_questionnaire<-function(data_colnames,questionnaire){
  # data_colnames<-names(data)
  # questionnaire<-load_questionnaire(data,questions,choices)
  is_primary_question_column<-sapply(data_colnames,questionnaire$question_in_questionnaire)
  is_select_multiple_choice_column<-sapply(data_colnames,questionnaire$question_is_sm_choice)
  missing<-!(is_primary_question_column | is_select_multiple_choice_column)


  if(!any(missing)){
    return(new_issues())
    }else{

    return(new_issues("data column not found in questionnaire",
                      "data/questions",
                      data_colnames[missing],
                      "minor",
                      "can not get data types, skiplogic, choices and labels from questionaire. select_multiple treated like select_one's"
                      )
    )
  }

}

check_data_types_match_questionnaire<-function(data,q){

  types<-  sapply(as.character(questions$name[!is.na(questions$name)]),function(var){

    if(var %in% names(data)){

        if(q$question_is_numeric(var)) return("numeric")
        if(q$question_is_select_one(var)) return("select_one")
        if(q$question_is_select_multiple(var)) return("select_multiple")
        if(q$question_is_sm_choice(var)) return("sm_choice")

      }
    return(NA)
  })

types<-types[!is.na(types)]
match<-mapply(compare_question_type,data[,names(types)],types)

missmatch<-match!="no missmatch identified"
if(!any(missmatch)){return(new_issues())}
return(new_issues(issue = rep("data type does not match type defined in questionnaire",
                      length(which(missmatch))),
                  affected_files = "data/questions",
                  affected_variables = names(types)[missmatch],
                  severity = "critical",
                  comment = match[missmatch])
                  )


}


compare_question_type<-function(x,type){
  if(all(is.na(x))){return("no missmatch identified")}
  if(type %in% c("select_one")){
    if(!(class(x) %in% c("factor","character","numeric","integer"))){return(paste0("question is 'select_one' but data is class:",class(x)))}
  }
  if(type %in% c("select_multiple")){
    if(!(class(x) %in% c("factor","character","numeric","integer"))){return(paste0("question is 'select_multiple' but data is class:",class(x)))}
  }

  if(type %in% c("numeric")){
    if(!is.numeric(x)){return(paste0("question is numeric but data is class:",class(x)))}
  }
  return("no missmatch identified")
}


check_data_values_in_choices<-function(data,questionnaire){
  rq<-questionnaire$raw_questionnaire()
  categorical_vars<-sapply(names(data),questionnaire$question_is_categorical)
  categorical_vars<-names(categorical_vars[categorical_vars])
  ### "categorical_vars": vector of s1 and sm variable names

  var_row_in_q<-match(categorical_vars,rq$questions$name)
  var_types<-rq$questions$type[var_row_in_q] %>% as.character
  var_types<-var_types %>%
    gsub("[[:space:]]*\\b*select_one\\b*[[:space:]]*","",.) %>%
    gsub("[[:space:]]*\\b*select_multiple\\b*[[:space:]]*","",.)
  ### "var_types": vector of variable "type" (i.e. "yes_no"). Parallel to "categorical_vars"

  lost_choices<-lapply(1:length(categorical_vars),function(var_i){

    these_choices<-as.character(rq$choices[var_types[var_i]==rq$choices$list_name,"name"])
    unique_in_data<-unique(data[[categorical_vars[var_i]]] %>% as.character %>% strsplit(" ") %>% unlist)

    values_not_in_questionnaire<-unique_in_data[!(unique_in_data %in% c(these_choices,NA))]
    return(tibble::tibble(values_not_in_questionnaire=values_not_in_questionnaire,variable=rep(categorical_vars[var_i])))
    }) %>% do.call(rbind,.)

    kobostandards:::new_issues(issue=rep("select_one or select_multiple values in data that are not listed in the questionnaire (data values split by on \" \" (spaces))",nrow(lost_choices)),affected_files="data/questionnaire",affected_variables=lost_choices$variable,severity="minor",comment = paste("value: ",lost_choices$values_not_in_questionnaire))
}
