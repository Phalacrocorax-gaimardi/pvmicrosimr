#' calibration parameters for agents.
#'
#' This version assumes pv_data_oo i.e. owner-occupiers of non-apartments only. Generated from pvcalibrater.
#'
#'
#' @format A data frame with 7499 rows and 6 variables:
#' \describe{
#'   \item{ID}{agent ID}
#'   \item{w_q9_1}{weight for financial term}
#'   \item{w_qsp21}{weight for social term}
#'   \item{w_theta}{weight for barrier}
#'   \item{q9_1}{bi-monthly bill category}
#'   \item{qsp21}{social answercode}
#' }
#' @source
"agents_init"
