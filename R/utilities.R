#' adding issues together
#'
#' @examples
#' # use me like you'd use `names()`:
#' add_issues(existing_list_of_issues)<-another_list_of_issues
#' # is equivalent to:
#' existing_list_of_issues<-rbind(existing_df_with_issues,another_df_with_issues)
`add_issues<-`<-function(x,value){
  rbind(x,value)
}


#' create a df with new issues in standard format
#'
#' all parameters except 'issue' are optional
#' each paramter must be a vector, they all must have the same length length
#' 'severity' must be one of the predefined levels
new_issues<-function(issue=character(0),
                     affected_files=character(length(issue)),
                     affected_variables=character(length(issue)),
                     severity=factor(length(issue),levels = c("minor",
                                                              "problematic",
                                                              "critical")),
                     comment=character(length(issue))
){
  tibble::tibble(issue=as.character(issue),
                 affected_files=as.character(affected_files),
                 affected_variables=as.character(affected_variables),
                 severity=factor(as.character(severity),levels=c("minor",
                                                                 "problematic",
                                                                 "critical")),
                 comment=comment)
}




to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}
