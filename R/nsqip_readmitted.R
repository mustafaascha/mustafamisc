#' ACS-NSQIP Readmission identification
#'
#' There are several variables to identify if a patient
#'  was readmitted or returned to the hospital. This function
#'  identifies if the patient was readmitted.
#'
#'  @param df This is the NSQIP dataframe
#'
#'  @export
#'
#'  @return Characters, "Readmitted" if readmitted and "Not Readmitted" if not.
nsqip_readmit <- function(df){
  with(df,
  ifelse(READMISSION == "Yes" |
           READMISSION1 == "Yes" |
           READMISSION2 == "Yes" |
           READMISSION3 == "Yes" |
           READMISSION4 == "Yes" |
           READMISSION5 == "Yes" |
           READMRELATED1 == "Yes" |
           READMRELATED2 == "Yes" |
           READMRELATED3 == "Yes" |
           READMRELATED4 == "Yes" |
           READMRELATED5 == "Yes" |
           UNPLANREADMISSION == "Yes" |
           UNPLANNEDREADMISSION1 == "Yes" |
           UNPLANNEDREADMISSION2 == "Yes" |
           UNPLANNEDREADMISSION3 == "Yes" |
           UNPLANNEDREADMISSION4 == "Yes" |
           UNPLANNEDREADMISSION5 == "Yes",
         "Readmitted", "Not Readmitted")
  )
}


