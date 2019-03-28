
output_of_check_is_standardised<-function(x){
  output_expected_colnames<-c("issue","affected_files","affected_variables","severity","comment")

  if(is.null(x)){return(FALSE)}
  if(!is.data.frame(x)){return(FALSE)}
  if(ncol(x)!=length(output_expected_colnames)){return(FALSE)}
  if(!all(colnames(x)==output_expected_colnames)){return(FALSE)}
  if(!is.character(x[["issue"]])){return(FALSE)}
  if(!is.character(x[["affected_files"]])){return(FALSE)}
  if(!is.character(x[["comment"]])){return(FALSE)}
  if(!is.factor(x[["severity"]])){return(FALSE)}
  if(!all(levels(x[["severity"]])==c("minor","problematic","critical"))){return(FALSE)}

  return(TRUE)
}
