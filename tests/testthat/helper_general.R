
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





expect_no_issues<-function(check_fun,...,label){

  no_issues_info<-function(issues){ paste0(nrow(issues), " issue(s) found: ", paste(issues$issue,collapse="\n"))}

  if(length(check_fun)!=length(label)){stop("in test utils: check_fun and label must be same lengths")}

  for(i in 1:length(check_fun)){

    issues<-check_fun[[i]](...)
    expect_true(
      nrow(issues)==0,
      info = no_issues_info(issues),
      label = label[i]
    )
  }
}




expect_issues<-function(check_fun,num_issues,issue_pattern=".",...,label){



  if(!length(unique(sapply(list(check_fun,label),length)))==1){"testutil expect_issues: inputs must have same length"}

  for(i in 1:length(check_fun)){

    issues<-check_fun[[i]](...)



    expect_true(
      nrow(issues)==num_issues,
      info = paste0(nrow(issues),"/" ,num_issues," issue(s) found: ", paste(issues$issue,collapse="\n")),
      label = label[i]
    )

    if(nrow(issues)>0){

      for(thisissue in 1:nrow(issues)){

        expect_true(
          grepl(issue_pattern,issues[thisissue,"issue"]),
          info = paste0(issue_pattern," not in:'", issues[thisissue,"issue"],"'"),
          label = c("issue pattern:",label[i])
        )

      }

    }


  }



}







