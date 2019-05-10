




new_issues_illegal_values<-function(df,variable,legal_values,...){
  if(!any(names(df)==variable)){
    return(new_issues(paste0("missing column: ",variable)))
  }

  values<-unique(as.character(df[[variable]]))
  illegal_values<-values[which(!(values %in% legal_values))]
  if(length(illegal_values)==0){
    return(new_issues())
  }

  illegal_values[is.na(illegal_values)]<-"NA"
  issue_names<-paste0("illegal value in '" ,variable, "': '",illegal_values,"'")
  new_issues(issue_names,...)

}







new_issues_df_missing_variables <- function(df,variables, ...){

  if(!is.data.frame(df)){return(new_issues("not a data.frame",...))}
  missing_vars<-variables[!(variables %in% names(df))]
  if(length(missing_vars)==0){return(new_issues())}
  new_issues(issue = paste("column missing: ", missing_vars),...)
}
