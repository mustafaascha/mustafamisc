#' ACS-NSQIP CPT codes to a variable
#'
#' ACS-NSQIP has a lot of CPT codes for each observation. This function
#' examines each CPT code and returns the code name if it is present,
#' and "No" if the code is not present.
#'
#' @param df This is the name of the NSQIP dataframe.
#' @param code This is the CPT code of interest
#'
#' @export
#'
#' @return Returns the CPT code if present in any NSQIP variable for
#'  that observation, "No" otherwise
CPTtoVar <- function(df, code) {
  ifelse(df$CPT == code |
           df$CONCPT1 == code |
           df$CONCPT1 == code |
           df$CONCPT2 == code |
           df$CONCPT3 == code |
           df$CONCPT4 == code |
           df$CONCPT5 == code |
           df$CONCPT6 == code |
           df$CONCPT7 == code |
           df$CONCPT8 == code |
           df$CONCPT9 == code |
           df$CONCPT10 == code |
           df$OTHERCPT1 == code |
           df$OTHERCPT2 == code |
           df$OTHERCPT3 == code |
           df$OTHERCPT4 == code |
           df$OTHERCPT5 == code |
           df$OTHERCPT6 == code |
           df$OTHERCPT7 == code |
           df$OTHERCPT8 == code |
           df$OTHERCPT9 == code |
           df$OTHERCPT10 == code , CPT, "No")
}
