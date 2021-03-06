% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nsqip_CPT_extract.R
\name{nsqip_CPT_extract}
\alias{nsqip_CPT_extract}
\title{ACS-NSQIP CPT codes to a variable}
\usage{
nsqip_CPT_extract(df, code)
}
\arguments{
\item{df}{This is the name of the NSQIP dataframe.}

\item{code}{This is the CPT code of interest}
}
\value{
Character, returns the CPT code if present in any NSQIP variable for
 that observation and "No" otherwise
}
\description{
ACS-NSQIP has a lot of CPT codes for each observation. This function
examines each CPT code and returns the code name if it is present,
and "No" if the code is not present.
}

