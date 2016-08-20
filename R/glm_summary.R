#' A concise (if incomplete) summary of glm results
#'
#' Rather than describe the model adequately, "cut to the chase"
#'
#' @param model This is a glm model.
#'
#' @export
#'
#' @return data.frame of Odds Ratios, 95% confidence intervals, and p-values
glm_summary <- function(model){
  cbind(
    exp(cbind(
      OR = coef(model),
      confint(model))),
    P = summary(model)$coefficients[,4])
}

#' Identify which rows to highlight
#'
#' This function returns a logical indicating whether the p-value
#'  reached significance, allowing the pander emphasize_x functions to
#'  do their jobs more easily.
#'
#' @param model This is a glm model
#' @param p Integer between 0 and 1, this is the alpha level that sets significance.
#'
#' @export
#'
#' @return Logical
glm_significant <- function(model, p) {
  model <-    cbind(
    exp(cbind(
      OR = coef(model),
      confint(model))),
    P = summary(model)$coefficients[,4])
  which(model[,4] <= p)
}
