#' Daily log returns for S&P 500, Bitcoin, and Gold
#'
#' `hedgedata` provides daily log returns for three major financial assets:
#' S&P 500 (`SP`), Bitcoin (`BTC`), and Gold (`GLD`), obtained from Yahoo Finance.
#' The data are stored as a wide-format `zoo` object with a `Date` index.
#'
#' @format A `zoo` object indexed by `Date` with 3 numeric columns:
#' \describe{
#'   \item{SP}{Daily log return of the S\eqn{\&}P 500 index (Yahoo symbol `^GSPC`).}
#'   \item{BTC}{Daily log return of Bitcoin priced in USD (Yahoo symbol `BTC-USD`).}
#'   \item{GLD}{Daily log return of Gold (front-month futures, Yahoo symbol `GC=F`).}
#' }
#'
#' @details
#' **Data construction process**
#'
#' The dataset was created using the following R workflow:
#'
#' ```r
#' library(quantmod)
#' library(zoo)
#'
#' symbols <- c("^GSPC", "BTC-USD", "GC=F")
#' names_vec <- c("SP", "BTC", "GLD")
#'
#' from <- as.Date("2015-01-01")
#' to <- Sys.Date()
#'
#' # Download daily closing prices
#' get_close <- function(sym, from, to) {
#'   x <- getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)
#'   Cl(x)
#' }
#'
#' prices <- do.call(merge, lapply(symbols, get_close, from = from, to = to))
#' colnames(prices) <- names_vec
#'
#' # Compute daily log returns
#' hedgedata <- diff(log(prices))
#' hedgedata <- na.omit(hedgedata)
#'
#' # Save into package
#' usethis::use_data(hedgedata, overwrite = TRUE)
#' ```
#'
#' - Log returns are computed as \eqn{\log(P_t / P_{t-1})}.
#' - Data are daily and aligned by calendar date.
#' - Missing values (due to non-trading days) are left as `NA` before `na.omit()`.
#'
#' @source Yahoo Finance:
#' \itemize{
#'   \item S\eqn{\&}P 500: `^GSPC`
#'   \item Bitcoin (USD): `BTC-USD`
#'   \item Gold: `GC=F`
#' }
#'
#' @seealso \pkg{zoo}, \pkg{quantmod}, \pkg{usethis}
#'
#' @examples
#' data(hedgedata)
#' head(hedgedata)
#'
#' # Plot all assets' log returns
#' plot(hedgedata, main = "Daily Log Returns: S&P 500, Bitcoin, and Gold")
"hedgedata"
