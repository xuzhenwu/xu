# ----------------------------------------------------------------------------------------------
# runoff signatures
# ----------------------------------------------------------------------------------------------


#' Compute runoff signatures
#'
#' @param Q
#' @param Date
#' @param Months_sum
#' @param Months_win
#'
#' @return
#' @export
#'
#' @examples
runoff_signatures <- function(Q, Date, Months_sum = 6:8, Months_win = c(12, 1, 2)){

  dt <- data.table(Q, Date, Month = month(Date))
  n <- length(Q)

  # low flow
  ZFR <- length(Q[Q==0])/n

  # Pecentile
  logQ <- log(Q)
  Q1 <- quantile(Q, 0.01)
  Q10 <- quantile(Q, 0.10) # low flow
  Q33 <- quantile(Q, 0.33)
  Q50 <- quantile(Q, 0.50)
  Q66 <- quantile(Q, 0.66)
  Q90 <- quantile(Q, 0.90)
  Q95 <- quantile(Q, 0.95)
  Q99 <- quantile(Q, 0.99)  # high flow

  # Mean
  Qm <- mean(Q)
  logQm <- mean(logQ)
  Qs <- dt[Month %in% Months_sum]$Q %>% mean()
  Qw <- dt[Month %in% Months_win]$Q %>% mean()

  # Flow dynamics
  Qstd <- sd(Q)
  if(n < 100){
    Q1 <- Q95 <- Q99 <- CI <- NA
  }else{
    CI <- (Q90 - Q1)/(Q99 - Q1)
  }
  CV <- Qstd / Qm
  IQR <- quantile(Q, 3/4) - quantile(Q, 1/4)

  # Slope of part of curve between the 66% and 33%  flow non-exceedance values of
  # stream flow normalized by their means
  Slope <- (Q66 - Q33)/ (Q66 + Q33) * 2

  return(data.table(
    Q1, Q10, Q50, Q99,
    Qm, logQm, Qs, Qw,
    Qstd, CI, CV, IQR, Slope
  ))
}
