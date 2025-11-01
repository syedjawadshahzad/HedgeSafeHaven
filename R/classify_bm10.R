#' Classify Hedge and Safe-Haven Behavior (BM10 Framework)
#'
#' @description
#' Classifies an asset’s hedge and safe-haven properties based on the
#' coefficients estimated from the \code{\link{hedge_safehaven_bm10}} function
#' following the Baur and McDermott (2010) approach.
#'
#' @param res_df A data frame returned by \code{hedge_safehaven_bm10()},
#'   containing the columns:
#'   \itemize{
#'     \item \code{Hedge} — labels for coefficient levels ("c0", "0.10", "0.05", "0.01")
#'     \item \code{Coefficient_Sum} — estimated coefficient or cumulative hedge ratio
#'     \item \code{p_value} — two-sided p-value for the estimate
#'   }
#' @param tol Numeric tolerance used for comparing small values to zero.
#'   Defaults to \code{1e-8}.
#'
#' @details
#' The function interprets the results of equations (1a)–(1c) from
#' Baur and McDermott (2010) and determines:
#'
#' \itemize{
#'   \item \strong{Hedge classification:}
#'     \itemize{
#'       \item Strong hedge — if \eqn{c_0 < 0} and significant (\eqn{p < 0.10})
#'       \item Weak hedge — if \eqn{c_0 \approx 0} or not significant
#'       \item Not a hedge — otherwise
#'     }
#'   \item \strong{Safe-haven classification:}
#'     \itemize{
#'       \item Strong safe haven — all coefficients (\eqn{c_0, c_1, c_2, c_3})
#'         are negative and significant
#'       \item Weak safe haven — negative and significant only at one
#'         tail level (10\%, 5\%, or 1\%), reported explicitly
#'       \item Not a safe haven — otherwise
#'     }
#' }
#'
#' @return
#' A character string summarizing the conclusion, e.g.:
#' \preformatted{
#' "Selected asset is a weak hedge and safe haven for 5%."
#' }
#'
#' @references
#' Baur, D. G., & McDermott, T. K. (2010).
#' *Is Gold a Safe Haven? International Evidence.*
#' Journal of Banking & Finance, 34(8), 1886–1898.
#'
#' @seealso \code{\link{hedge_safehaven_bm10}}
#'
#' @examples
#' \dontrun{
#' res <- hedge_model_results(hedgedata$SP, hedgedata$GLD)
#' classify_bm10(res)
#' }
#'
#' @export

classify_bm10 <- function(res_df, tol = 1e-8) {
  # extract values
  getv <- function(key) res_df$Coefficient_Sum[res_df$Hedge == key]
  getp <- function(key) res_df$p_value[res_df$Hedge == key]

  c0    <- getv("c0");    p0  <- getp("c0")
  sum10 <- getv("0.10");  p10 <- getp("0.10")
  sum05 <- getv("0.05");  p05 <- getp("0.05")
  sum01 <- getv("0.01");  p01 <- getp("0.01")

  if (any(is.na(c(c0,p0,sum10,p10,sum05,p05,sum01,p01))))
    stop("Input doesn't contain required rows: c0, 0.10, 0.05, 0.01")

  # ---- Hedge classification ----
  c123 <- sum01 - c0
  joint_ok <- c123 <= (max(c0, 0) + tol)  # not jointly positive beyond c0

  hedge_class <- if (c0 < -tol && p0 < 0.1 && joint_ok) {
    "strong hedge"
  } else if ((abs(c0) <= tol || p0 >= 0.1) && joint_ok) {
    "weak hedge"
  } else {
    "not a hedge"
  }

  # ---- Safe haven classification ----
  safehaven_class <- if ((abs(c0) <= tol || p0 >= 0.1) && (abs(sum10) <= tol || p0 >= 0.1) && (abs(sum05) <= tol || p0 >= 0.1) && (abs(sum01) <= tol || p0 >= 0.1)) {
    "weak safe haven"
  } else if ((sum10 < -tol && p10 < 0.10) && (sum05 < -tol && p05 < 0.10) && (sum01 < -tol && p01 < 0.10) && (c0 < -tol && p0 < 0.10)) {
    "strong safe haven"
  } else if (sum10 < -tol && p10 < 0.10)  {
    "safe haven for 10%"
  } else if (sum05 < -tol && p05 < 0.10) {
    "safe haven for 5%"
  } else if (sum01 < -tol && p01 < 0.10) {
    "safe haven for 1%"
  } else {
  "not a safe haven"
  }

  # ---- Final text output ----
  paste("Selected asset is a", hedge_class, "-", safehaven_class, ".")
}
