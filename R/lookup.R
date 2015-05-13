#' lookup
#'
#' Search for row numbers in a data frame that most closely match value
#' 
#' A helper function
#'
#' @param mydata Input data frame
#' 
#' @param value to find.
#'
#' @return The row number that most closely matches
#'
#' @keywords keywords
#'
#' @export
lookup<-function(mydata,value){
  num<-seq(1:length(value))
  for(i in 1:length(value)){
    num[i]<-which(abs(mydata-value[i])==min(abs(mydata-value[i])))
  }
  return(num)
}