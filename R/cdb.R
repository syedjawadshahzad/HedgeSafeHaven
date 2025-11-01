#' Conditional Diversification Benefit (CDB)
#'
#' @description
#' Compute the CDB (Christoffersen et al., 2012; 2018) for a two–asset
#' portfolio at confidence level `p`, using normal–theory VaR/ES
#' with plug-in means and standard deviations.
#'
#' @param x Numeric vector of returns for asset X.
#' @param y Numeric vector of returns for asset Y.
#' @param p Scalar in (0, 1): confidence level (e.g., `0.05` for 5% ES/VaR).
#' @param w Scalar in \[0, 1]: portfolio weight on X (Y is 1 - w).
#'
#' @details
#' The function computes
#' \deqn{ \mathrm{CDB}(w,p) = \frac{ w\,\mathrm{ES}_X(p) + (1-w)\,\mathrm{ES}_Y(p) - \mathrm{ES}_P(p) }
#'                                    { w\,\mathrm{ES}_X(p) + (1-w)\,\mathrm{ES}_Y(p) - \mathrm{VaR}_P(p) } }
#' where ES and VaR are computed via `cvar::ES()` and `cvar::VaR()` using
#' `stats::qnorm` (normal approximation). Portfolio mean/SD use plug-in means/SDs
#' under an independence assumption (no covariance term).
#'
#' @return
#' A single numeric value giving the Conditional Diversification Benefit for the
#' specified weight and confidence level.
#'
#' @references
#' Christoffersen, P., Errunza, V., Jacobs, K., & Langlois, H. (2012).
#' *Is the potential for international diversification disappearing? A dynamic copula approach.*
#' Review of Financial Studies, 25(12), 3711–3751.  
#' Christoffersen, P., Jacobs, K., Jin, X., & Langlois, H. (2018).
#' *Dynamic dependence and diversification in corporate credit.*
#' Review of Finance, 22(2), 521–560.
#'
#' @seealso [cvar::ES()], [cvar::VaR()]
#'
#' @examples
#' x <- rnorm(1000, 0.0005, 0.02)
#' y <- rnorm(1000, 0.0003, 0.015)
#' cdb(x, y, p = 0.05, w = 0.3)
#'
#' @importFrom stats sd qnorm
#' @export
cdb <- function(x, y, p, w) {
  # ---- Input validation ----
  if (!is.numeric(x) || !is.numeric(y))
    stop("`x` and `y` must be numeric vectors.")
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1)
    stop("`p` must be a scalar in (0,1).")
  if (!is.numeric(w) || length(w) != 1L || w < 0 || w > 1)
    stop("`w` must be a scalar in [0,1].")

  # ---- Plug-in moments ----
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  sd_x   <- sd(x,   na.rm = TRUE)
  sd_y   <- sd(y,   na.rm = TRUE)

  # ---- Marginal ES ----
  es_x <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_x, sd = sd_x)
  es_y <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_y, sd = sd_y)

  # ---- Portfolio mean and SD (no covariance term) ----
  mean_p <- w * mean_x + (1 - w) * mean_y
  sd_p   <- sqrt((w * sd_x)^2 + ((1 - w) * sd_y)^2)

  # ---- Portfolio ES and VaR ----
  es_p  <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_p, sd = sd_p)
  var_p <- cvar::VaR(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_p, sd = sd_p)

  # ---- Conditional Diversification Benefit ----
  numerator   <- (w * es_x) + ((1 - w) * es_y) - es_p
  denominator <- (w * es_x) + ((1 - w) * es_y) - var_p
  cdb_value   <- numerator / denominator

  as.numeric(cdb_value)
}
