check_input_match_data_analysisplan<-function(data,analysisplan){
  if(!is.data.frame(data)){
    return(new_issues("could not check match between data and analysis plan - data is not a data.frame",severity="critical",affected_files="data/analysisplan"))
  }
  if(!is.data.frame(analysisplan)){
    return(new_issues("could not check match between data and analysis plan - analysisplan is not a data.frame",severity="critical",affected_files="data/analysisplan"))
  }

  issues<-new_issues()

  add_issues(issues)<-check_analysisplan_vars_in_data(data,analysisplan)

}





check_analysisplan_vars_in_data<-function(data,analysisplan){
  ap_vars<-get_all_analysisplan_variables(analysisplan)
  ap_vars_not_found<-ap_vars[!(ap_vars %in% names(data))]
  ap_vars_not_found<-ap_vars_not_found[!(ap_vars_not_found %in% c(NA,""," "))]
  new_issues(rep("variable named in analysis plan that doesn't exist in data",length(ap_vars_not_found)),
             affected_files = "data/analysisplan",
             affected_variables = ap_vars_not_found,severity = "critical")
}



get_all_analysisplan_variables<-function(analysisplan){
  issues<-new_issues()
  analysisplan<-purrr::map(analysisplan,as.character) %>% (tibble::as_tibble)
  ap_columns_with_variable_names<-c("repeat.for.variable","independent.variable","dependent.variable")
  var_cols_in_ap<-ap_columns_with_variable_names[ap_columns_with_variable_names%in%names(analysisplan)]

  if(length(var_cols_in_ap)!=length(ap_columns_with_variable_names)){
    add_issues(issues)<-new_issues("could not check if analysisplan variables in data - missing analysisplan column",
                                   severity="critical",
                                   comment = paste("missing columns:",ap_columns_with_variable_names[!(ap_columns_with_variable_names%in%var_cols_in_ap)]))
  }
  if(length(var_cols_in_ap)==0){return(issues)}
  vars<-analysisplan[,var_cols_in_ap]  %>% unlist %>% unname %>% unique
  vars[!is.na(vars)]
}
