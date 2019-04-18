




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

