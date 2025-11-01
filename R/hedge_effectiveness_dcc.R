#' Hedge Ratio Summary, Conditional HE, and Optimal Hedging Weight (aDCC–GJR–GARCH)
#'
#' @description
#' Fits a bivariate asymmetric DCC (aDCC) model with GJR–GARCH(1,1) margins and
#' reports: (i) the time-varying hedge ratio summary (mean/min/max),
#' (ii) a single hedging effectiveness (HE) value based on **conditional**
#' variances, and (iii) the **optimal portfolio weight of the hedging asset**
#' averaged over time (weights clipped to \[0,1\]).
#'
#' @param hedged Numeric vector or `zoo`/`xts` series of returns for the asset
#'   being hedged (asset 1).
#' @param hedge Numeric vector or `zoo`/`xts` series of returns for the
#'   hedging asset (asset 2).
#' @param arma_order Integer length-2; ARMA order in the univariate mean
#'   (default `c(1, 0)`).
#' @param garch_order Integer length-2; GARCH order in the univariate variance
#'   (default `c(1, 1)`).
#' @param univariate_dist Character; univariate error distribution for
#'   \code{rugarch::ugarchspec()} (default `"norm"`).
#' @param dcc_dist Character; DCC distribution for \code{rmgarch::dccspec()}
#'   (default `"mvnorm"`).
#' @param dcc_model Character; DCC model type (default `"aDCC"`).
#'
#' @details
#' Let \eqn{\Sigma_t} be the conditional covariance matrix from the DCC fit with
#' elements \eqn{\Sigma_{11,t} = \sigma^2_{1,t}}, \eqn{\Sigma_{22,t} = \sigma^2_{2,t}},
#' and \eqn{\Sigma_{12,t}}. The **time-varying hedge ratio** is
#' \deqn{\beta_t = \Sigma_{12,t} / \Sigma_{22,t}.}
#'
#' The **conditional hedging effectiveness (HE)** is reported as a single value:
#' \deqn{HE \;=\; 1 - \frac{\overline{\operatorname{Var}_t(y_1 - \beta_t y_2)}}%
#'                         {\overline{\sigma^2_{1,t}}}
#'      \;=\; 1 - \frac{\overline{\sigma^2_{1,t} - \Sigma_{12,t}^2/\sigma^2_{2,t}}}
#'                         {\overline{\sigma^2_{1,t}}},}
#' where the bar denotes the time average.
#'
#' The **optimal portfolio weight** of the hedged/spot asset \eqn{S} versus the
#' hedging asset \eqn{F} follows:
#' \deqn{w_{SF,t} \;=\; \frac{\sigma^2_{F,t} - \Sigma_{SF,t}}
#'                            {\sigma^2_{S,t} - 2\Sigma_{SF,t} + \sigma^2_{F,t}},}
#' clipped to \eqn{[0,1]}. The function returns \code{OPW} as the **mean weight
#' of the hedging asset**, i.e. \eqn{\overline{1 - w_{SF,t}}}.
#'
#' @return A one-row \code{data.frame} with:
#' \describe{
#'   \item{\code{beta_mean}}{Mean hedge ratio \eqn{\bar{\beta}}}
#'   \item{\code{beta_min}}{Minimum hedge ratio}
#'   \item{\code{beta_max}}{Maximum hedge ratio}
#'   \item{\code{HE}}{Single conditional hedging effectiveness value}
#'   \item{\code{OPW}}{Mean optimal portfolio weight of the **hedging** asset (clipped)}
#' }
#'
#' @references
#' Basher, S. A., & Sadorsky, P. (2016).
#' Hedging emerging market stock prices with oil, gold, VIX, and bonds:
#' A comparison between DCC, ADCC and GO-GARCH.
#' \emph{Energy Economics}, 54, 235–247.
#'
#' @seealso \code{\link[rugarch]{ugarchspec}},
#'   \code{\link[rmgarch]{dccspec}}, \code{\link[rmgarch]{dccfit}},
#'   \code{\link[rmgarch]{rcov}}
#'
#' @examples
#' \dontrun{
#' # Hedge stock (SP) with gold (GLD)
#' hedge_effectiveness_dcc(hedgedata$SP, hedgedata$GLD)
#' }
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom zoo as.zoo coredata
#' @importFrom stats pnorm quantile setNames
hedge_effectiveness_dcc <- function(
  hedged,
  hedge,
  arma_order = c(1, 0),
  garch_order = c(1, 1),
  univariate_dist = "norm",
  dcc_dist = "mvnorm",
  dcc_model = "aDCC"
) {
  # Coerce and align
  x1 <- zoo::as.zoo(hedged)
  x2 <- zoo::as.zoo(hedge)
  xy <- na.omit(merge(x1, x2))
  if (NROW(xy) < 10L) stop("Not enough overlapping non-missing observations.")

  y <- cbind(
    as.numeric(zoo::coredata(xy[, 1])),
    as.numeric(zoo::coredata(xy[, 2]))
  )

  # Univariate GJR-GARCH(1,1) spec with ARMA in mean
  garch_spec <- rugarch::ugarchspec(
    mean.model     = list(armaOrder = arma_order),
    variance.model = list(garchOrder = garch_order, model = "gjrGARCH"),
    distribution.model = univariate_dist
  )

  # aDCC(1,1) spec
  dcc_spec <- rmgarch::dccspec(
    uspec       = rugarch::multispec(replicate(2, garch_spec)),
    dccOrder    = c(1, 1),
    distribution = dcc_dist,
    model        = dcc_model
  )

  # Fit DCC (no parallel by default for portability)
  fit <- rmgarch::dccfit(dcc_spec, data = y)

  # Time-varying covariance matrices: 2x2xT
  Sigma <- rmgarch::rcov(fit) 

  v1 <- as.numeric(Sigma[1,1,])
  v2 <- as.numeric(Sigma[2,2,])
  c12 <- as.numeric(Sigma[1,2,])
    
  beta_t <- c12 / v2
  port_var_t <- v1 - (c12^2) / v2
  HE <- 1 - mean(port_var_t, na.rm = TRUE) / mean(v1, na.rm = TRUE)
  OPW <- (v2 - c12)/(v1 - 2*c12 + v2)
  OPW <- mean(pmin(pmax(OPW, 0), 1))

  out <- data.frame(
    beta_mean = mean(beta_t, na.rm = TRUE),
    beta_min  = min(beta_t,  na.rm = TRUE),
    beta_max  = max(beta_t,  na.rm = TRUE),
    HE  = HE, OPW = 1 - OPW
  )
  rownames(out) <- NULL
  out
}
