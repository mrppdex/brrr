#' Mock Benefit-Risk Data
#'
#' @description
#' A dataset containing mock data for benefit-risk assessment. This data is used
#' in examples and tests to demonstrate the functionality of the package.
#'
#' @format A data frame with 15 rows and 12 columns:
#' \describe{
#'   \item{endpoint}{The endpoint of the study.}
#'   \item{treatment}{The treatment arm of the study.}
#'   \item{placebo}{The placebo arm of the study.}
#'   \item{estimator}{Estimator name.}
#'   \item{axis_number}{The number of the axis for separating the axes.}
#'   \item{value}{The value of the estimator.}
#'   \item{lower}{The lower bound of the confidence interval.}
#'   \item{upper}{The upper bound of the confidence interval.}
#'   \item{reversed}{A logical indicating if the axis is reversed.}
#'   \item{logscale}{A logical indicating if the axis is in log scale.}
#'   \item{logbase}{The base of the logarithm.}
#'   \item{col3}{A character column for the third column in the plot.}
#' }
"mock_data"

#' Mock Benefit-Risk Data for Risks
#'
#' @description
#' A dataset containing mock data for the risk component of a benefit-risk assessment.
#' This data is used in examples and tests.
#'
#' @format A data frame with 10 rows and 13 columns:
#' \describe{
#'   \item{endpoint}{The endpoint of the study.}
#'   \item{treatment}{The treatment arm of the study.}
#'   \item{placebo}{The placebo arm of the study.}
#'   \item{estimator}{Estimator name.}
#'   \item{axis_number}{The number of the axis for separating the axes.}
#'   \item{value}{The value of the estimator.}
#'   \item{lower}{The lower bound of the confidence interval.}
#'   \item{upper}{The upper bound of the confidence interval.}
#'   \item{reversed}{A logical indicating if the axis is reversed.}
#'   \item{logscale}{A logical indicating if the axis is in log scale.}
#'   \item{logbase}{The base of the logarithm.}
#'   \item{txt_val}{A character column with text values.}
#' }
"mock_data_risks"