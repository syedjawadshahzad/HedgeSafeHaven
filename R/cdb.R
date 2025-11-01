#' Christoffersen–Diebold–Bakshi (CDB) diversification benefit
#'
#' @description
#' Compute the CDB diversification benefit (Christoffersen et al., 2012; 2018)
#' for a two–asset portfolio at confidence level `p`, using normal–theory VaR/ES
#' with plug-in means and standard deviations.
#'
#' @param x Numeric vector of returns for asset X.
#' @param y Numeric vector of returns for asset Y.
#' @param p Scalar in (0, 1): confidence level (e.g., `0.05` for 5% ES/VaR).
#'
#' @details
#' For weights `w ∈ {0.05, 0.10, 0.20}`, the function computes
#' ```
#' CDB(w,p) = { w*ES_X(p) + (1-w)*ES_Y(p) - ES_P(p) } /
#'            { w*ES_X(p) + (1-w)*ES_Y(p) - VaR_P(p) }
#' ```
#' where `ES` and `VaR` are computed via `cvar::ES()` and `cvar::VaR()` using
#' `stats::qnorm` (normal approximation), with portfolio mean/SD formed from the
#' inputs’ plug-in mean and standard deviation.
#'
#' @return
#' A `1 x 3` numeric matrix of CDB values for weights 0.05, 0.10, 0.20.
#' Column names are `"w05"`, `"w10"`, `"w20"`.
#'
#' @references
#' Christoffersen, P., Errunza, V., Jacobs, K., & Jin, X. (2012).
#' *Is the potential for international diversification disappearing?*
#' Review of Financial Studies, 25(12), 3711–3751.  
#' Christoffersen, P., Langlois, H., & Laurent, S. (2018).
#' *Long-Run Portfolio Choice and the Value of Diversification.*
#'
#' @seealso [cvar::ES()], [cvar::VaR()]
#'
#' @examples
#' x <- rnorm(1000, 0.0005, 0.02)
#' y <- rnorm(1000, 0.0003, 0.015)
#' cdb(x, y, p = 0.05)
#'
#' @importFrom stats sd qnorm
#' @export

cdb <- function(x, y, p) {
  if (!is.numeric(x) || !is.numeric(y)) stop("`x` and `y` must be numeric vectors.")
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1) stop("`p` must be a scalar in (0,1).")

  # Fixed portfolio weights
  weights <- c(0.05, 0.10, 0.20)

  # Plug-in moments
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  sd_x   <- sd(x,   na.rm = TRUE)
  sd_y   <- sd(y,   na.rm = TRUE)

  # ES for marginals (right tail / losses)
  es_x <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_x, sd = sd_x)
  es_y <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_y, sd = sd_y)

  out <- numeric(length(weights))

  for (i in seq_along(weights)) {
    w <- weights[i]

    # Portfolio plug-in mean/stdev under independence proxy (no covariance term by design here)
    mean_p <- w * mean_x + (1 - w) * mean_y
    sd_p   <- sqrt((w * sd_x)^2 + ((1 - w) * sd_y)^2)

    # ES and VaR for portfolio
    es_p  <- cvar::ES(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_p, sd = sd_p)
    var_p <- cvar::VaR(qnorm, p_loss = p, lower.tail = FALSE, mean = mean_p, sd = sd_p)

    # CDB definition
    numerator   <- (w * es_x) + ((1 - w) * es_y) - es_p
    denominator <- (w * es_x) + ((1 - w) * es_y) - var_p
    out[i] <- numerator / denominator
  }

  res <- matrix(out, nrow = 1L)
  colnames(res) <- c("w05", "w10", "w20")
  res
}
