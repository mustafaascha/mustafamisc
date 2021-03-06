% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_summary.R
\name{glm_significant}
\alias{glm_significant}
\title{Identify which rows to highlight}
\usage{
glm_significant(model, p)
}
\arguments{
\item{model}{This is a glm model}

\item{p}{Integer between 0 and 1, this is the alpha level that sets significance.}
}
\value{
Logical
}
\description{
This function returns a logical indicating whether the p-value
 reached significance, allowing the pander emphasize_x functions to
 do their jobs more easily.
}

