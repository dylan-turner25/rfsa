#' Cleans columns names of the FSA MYA Price files
#'
#' @param df a data frame
#' @param year the year the data frame corresponds to
#'
#' @return returns a new set of column names that have been cleaned
#'
#' @noRd
#' @keywords internal
rename_mya_cols <- function(df,year){
  replacement <- 0
  for(y in (year):(year-10)){
    colnames(df)
    pattern <- paste0(y,"/",substr(y+1,3,4))
    colnames(df) <- gsub(pattern, paste0("T - ",replacement),colnames(df))
    replacement <- replacement + 1
  }
  colnames(df) <- gsub("Prices","Price",colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) or Final \\(F\\)","Final",colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) Final \\(F\\)","Final",colnames(df))

  return(colnames(df))
}
