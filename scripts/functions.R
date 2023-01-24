#' Determines the ideal maximum value for an axis
#'
#' @description Determines the ideal maximum value for an axis by adding to the maximum vale of a column in a data a (chosen) percentage
#' 
#' @details Useful for automatically setting the maximum limits in an axis

#' 
#' Arguments
#' @param df The name of the dataframe
#' @param colname The name of the column which contains the values in a string format
#' @param percentage_added Optionally, default 10. A value which indicates what percentage to increase the maximum value by. 
#' For example, 5 would refer to 5%. 
#' @returns A numeric value


determine_max_limit_axis <- function(df, colname, percentage_added = 10) {
  values = df[, colname]
  maximum_value = max(values) + (max(values) * percentage_added/100)
  return(maximum_value)
}