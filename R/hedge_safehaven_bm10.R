#' Hedge Coefficient Estimation (Baur & McDermott, 2010)
#'
#' @description
#' Estimates the time‐varying hedge ratio and its tail behavior following
#' the model of **Baur & McDermott (2010)**:
#'
#' \deqn{
#' \begin{aligned}
#' r_{\mathrm{Gold},t} &= a + b_t r_{\mathrm{Stock},t} + e_t \quad &(1a)\\
#' b_t &= c_0 + c_1 D(r_{\mathrm{Stock},q_{10}}) + c_2 D(r_{\mathrm{Stock},q_{5}}) + c_3 D(r_{\mathrm{Stock},q_{1}}) \quad &(1b)\\
#' h_t &= \pi + \alpha e_{t-1}^2 + \beta h_{t-1} \quad &(1c)
#' \end{aligned}
#' }
#'
#' where \eqn{r_{\mathrm{Gold},t}} is the return of the hedging asset
#' (e.g., gold), \eqn{r_{\mathrm{Stock},t}} is the return of the market
#' (hedged) asset, and \eqn{D(r_{\mathrm{Stock},q_i})} is a dummy equal to 1
#' when the market return lies below its \eqn{i\%} quantile.
#'
#' @param hedged Numeric vector or `zoo` series of returns for the hedged asset
#'   (e.g., stock market). This is the driver in the mean equation.
#' @param hedge Numeric vector or `zoo` series of returns for the hedging asset
#'   (e.g., gold).
#' @param dist Conditional distribution for the GARCH model (default `"norm"`).
#'
#' @details
#' The mean equation incorporates interactions of the hedged asset returns
#' with quantile dummies representing 10\%, 5\%, and 1\% lower tails.
#' These interaction terms allow the hedge ratio \eqn{b_t} to vary during
#' market downturns. The conditional variance follows an
#' \eqn{sGARCH(1,1)} process.
#'
#' The function returns the estimated base hedge coefficient (\eqn{c_0})
#' and cumulative sums for the 10\%, 5\%, and 1\% quantile levels.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{Hedge}{Quantile level ("c0", "0.10", "0.05", "0.01").}
#'   \item{Coefficient_Sum}{Estimated hedge coefficient (or sum up to that level).}
#'   \item{p_value}{Associated two‐sided p‐value.}
#' }
#'
#' @references
#' Baur, D. G., & McDermott, T. K. (2010).
#' *Is Gold a Safe Haven? International Evidence.*
#' Journal of Banking & Finance, 34(8), 1886–1898.
#'
#' @examples
#' \dontrun{
#' data(hedgedata)
#' hedge_safehaven_bm10(hedgedata$SP, hedgedata$GLD)
#' }
#'
#' @importFrom rugarch ugarchspec ugarchfit
#' @importFrom zoo as.zoo 
#' @export

hedge_safehaven_bm10 <- function(hedged, hedge, dist = "norm") {
  x <- as.zoo(hedged)   # rm: hedged asset
  y <- as.zoo(hedge)    # dependent/hedging target
  dat <- na.omit(merge(y, x)); colnames(dat) <- c("y", "rm")
  rm <- zoo::coredata(dat$rm)

  q10 <- as.numeric(quantile(rm, 0.10, na.rm = TRUE))
  q05 <- as.numeric(quantile(rm, 0.05, na.rm = TRUE))
  q01 <- as.numeric(quantile(rm, 0.01, na.rm = TRUE))

  D10 <- as.numeric(rm <= q10)
  D05 <- as.numeric(rm <= q05)
  D01 <- as.numeric(rm <= q01)

  X <- cbind(rm = rm,
             rm_D10 = rm * D10,
             rm_D05 = rm * D05,
             rm_D01 = rm * D01)

  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE,
                          external.regressors = as.matrix(X)),
    distribution.model = dist
  )
  fit <- ugarchfit(spec, data = zoo::coredata(dat$y), solver = "hybrid")

  cn <- names(rugarch::coef(fit))
  names_map <- setNames(grep("^mxreg", cn, value = TRUE),
                        c("rm", "rm_D10", "rm_D05", "rm_D01"))

  res_c0  <- .lincombo_ugarch(fit, c(names_map["rm"]))
  res_010 <- .lincombo_ugarch(fit, c(names_map["rm"], names_map["rm_D10"]))
  res_005 <- .lincombo_ugarch(fit, c(names_map["rm"], names_map["rm_D10"], names_map["rm_D05"]))
  res_001 <- .lincombo_ugarch(fit, c(names_map["rm"], names_map["rm_D10"],
                                     names_map["rm_D05"], names_map["rm_D01"]))

  data.frame(
    Hedge = c("c0", "0.10", "0.05", "0.01"),
    Coefficient_Sum = c(res_c0$estimate, res_010$estimate, res_005$estimate, res_001$estimate),
    p_value = c(res_c0$p, res_010$p, res_005$p, res_001$p),
    row.names = NULL
  )
}

.lincombo_ugarch <- function(fit, terms) {
  co <- rugarch::coef(fit); V <- rugarch::vcov(fit)
  idx <- match(terms, names(co))
  if (anyNA(idx)) stop("Unknown term(s): ", paste(terms[is.na(idx)], collapse = ", "))
  L <- rep(0, length(co)); L[idx] <- 1; names(L) <- names(co)
  est <- as.numeric(crossprod(L, co))
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  list(estimate = est, p = p)
}