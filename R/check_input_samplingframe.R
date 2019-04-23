#' check sampling frame data structure
#' @samplingframe the sampling frame as a data.frame
#' @return list of issues (see ?new_issues() for details)
#' @export
check_input_samplingframe<-function(samplingframe){
 issues<-kobostandards:::new_issues()

 # if could not check custom issue
 could_not_check_issue<-kobostandards:::new_issues("could not check samplingframe",affected_files="samplingframe",severity="critical")

 if(!is.data.frame(samplingframe)){
   could_not_check_issue$comment<-"samplingframe not a data.frame"
   return(could_not_check_issue)
   }
 if(nrow(samplingframe)==0){
   could_not_check_issue$comment<-"samplingframe has no rows"
   return(could_not_check_issue)
 }
 if(ncol(samplingframe)==0){
   could_not_check_issue$comment<-"samplingframe has no columns"
   return(could_not_check_issue)
 }


 if(ncol(samplingframe)!=2){
   add_issues(issues)<-new_issues("samplingframe should only have two columns",affected_files = "samplingframe",severity = "minor")
 }


  add_issues(issues)<-check_input_samplingframe_stratum_column(samplingframe)
  add_issues(issues)<-check_input_samplingframe_population_column(samplingframe)
  add_issues(issues)<-check_input_samplingframe_noNA(samplingframe)


issues
 }


check_input_samplingframe_population_column<-function(samplingframe){
  # find possible population columns
  sf_classes<-sapply(samplingframe,class)
  sf_classes<-gsub("factor","categorical",sf_classes)
  sf_classes<-gsub("integer|decimal|numeric|double","numeric",sf_classes)
  stratum_col<-sf_classes[sf_classes=="categorical"][1] %>% names
  population_col<-sf_classes[sf_classes=="numeric"][1] %>% names

  issues<-new_issues()

  # if not sure about population column or none found, return custom issue:
  if(length(which(sf_classes=="numeric"))>1){
    add_issues(issues)<-new_issues("samplingframe has more than one numerical (population) column",affected_files = "samplingframe",severity = "critical")
  }

  if(length(which(sf_classes=="numeric"))<1){
    add_issues(issues)<-new_issues("samplingframe has no purely numerical (population) column - skipping any further checks on population column",affected_files = "samplingframe",severity = "critical")
    return(issues)
  }




  if(any(as.numeric(samplingframe[,population_col])<=0,na.rm = T)){
    samplingframe[1:3,2]<-0
    strata_0_pop<-samplingframe[which(as.numeric(samplingframe[,population_col])<=0),stratum_col]
    add_issues(issues)<-kobostandards:::new_issues(rep("no stratum can have 0 or negative population",
                                                       length(strata_0_pop)),
                                                   affected_files = "samplingframe",
                                                   severity = "critical",
                                                   comment = paste("strata with population <= 0 :",
                                                                   strata_0_pop)
    )
  }
return(issues)
  }



check_input_samplingframe_stratum_column<-function(samplingframe){
  sf_classes<-sapply(samplingframe,class)
  sf_classes<-gsub("factor","categorical",sf_classes)
  sf_classes<-gsub("integer|decimal|numeric|double","numeric",sf_classes)
  stratum_col<-sf_classes[sf_classes=="categorical"][1] %>% names
  population_col<-sf_classes[sf_classes=="numeric"][1] %>% names

  if(!is.data.frame(samplingframe)){return(new_issues("could not check samplingframe stratum column",comment="samplingframe not a data.frame"))}

  if(length(which(sf_classes=="categorical"))>1){
    return(new_issues("samplingframe has more than one categorical column (stratum names) - skipping further checks on stratum column",severity="critical"))
  }

  if(length(which(sf_classes=="categorical"))<1){
    return(new_issues("samplingframe has no categorical column (stratum names) - skipping further checks on stratum column",severity="critical"))
  }


  samplingframe<-samplingframe[apply(samplingframe,1,
        function(x) !all(is.na(x))
  ),]

  issues<-new_issues()

  if(length(stratum_col)>0){
    stratum_col<-stratum_col[1]
    if(any(duplicated(samplingframe[,stratum_col]))){
      duplicated_strata_names <- samplingframe[,stratum_col][duplicated(samplingframe[,stratum_col])] %>% unique
      add_issues(issues)<-kobostandards:::new_issues(
        rep("duplicate stratum name in samplingframe",
            length(duplicated_strata_names)),
        affected_files = "samplingframe",
        severity = "critical",
        comment = paste("non-unique stratum name:",duplicated_strata_names))
    }
  }
return(issues)
}


check_input_samplingframe_noNA<-function(samplingframe){
  samplingframe<-samplingframe[apply(samplingframe,1,function(x){!all(is.na(x))}),,drop=F]
  issues<-new_issues()
if(any(is.na(unlist(samplingframe)))){
  add_issues(issues)<-new_issues("samplingframe must not contain NAs",affected_files = "samplingframe",severity = "critical")
}
  return(issues)
  }
