# ----------------------------------------------------------------------------------------------
# indicators to evaluate model simulation
# ----------------------------------------------------------------------------------------------


#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
NSE <- function(Qsim, Qobs){

  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  Qsim <- data$Qsim
  Qobs <- data$Qobs

  # compute
  Qobs_mean <- mean(Qobs)
  Emod <- sum((Qsim - Qobs)^2)
  Eref <- sum((Qobs - Qobs_mean)^2)
  if (Emod == 0 & Eref == 0) {
    NSE <- 0
  } else {
    NSE <- (1 - Emod / Eref)
  }
  NSE
}



#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
logNSE <- function(Qsim, Qobs){

  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  data <- data[Qsim > 0 & Qobs > 0] # exclude 0, as it produce -inf
  Qsim <- log(data$Qsim) #log transform
  Qobs <- log(data$Qobs)

  # compute
  Qobs_mean <- mean(Qobs)
  Emod <- sum((Qsim - Qobs)^2)
  Eref <- sum((Qobs - Qobs_mean)^2)
  if (Emod == 0 & Eref == 0) {
    NSE <- 0
  } else {
    NSE <- (1 - Emod / Eref)
  }
  NSE
}


#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
RMSE <- function(Qsim, Qobs){
  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  Qsim <- data$Qsim
  Qobs <- data$Qobs

  sqrt(mean((Qsim - Qobs)^2))
}



#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
Bias <- function(Qsim, Qobs){

  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  Qsim <- data$Qsim
  Qobs <- data$Qobs

  # BIAS
  sumQobs <- sum(Qobs)
  sumQsim <- sum(Qsim)
  if (sumQobs == 0) {
    BIAS <- 0
  } else {
    BIAS <- (sumQsim - sumQobs) / sumQobs
  }
  BIAS
}



#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
ABias <- function(Qsim, Qobs){

  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  Qsim <- data$Qsim
  Qobs <- data$Qobs

  # BIAS
  sumQobs <- sum(Qobs)
  sumQsim <- sum(Qsim)
  if (sumQobs == 0) {
    BIAS <- 0
  } else {
    BIAS <- sum(abs(Qsim - Qobs)) / sumQobs # abs
  }
  BIAS
}


#' Title
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
KGE <- function(Qsim, Qobs){
  R <- cor(Qsim, Qobs)
  KGE <- 1 - sqrt((R - 1)^2 + (mean(Qsim)/mean(Qobs) - 1)^2 + (sd(Qsim)/sd(Qobs) - 1)^2)
}


#' Model performance
#'
#' Calculate Statistics include: NSE, logNSE, RMSE, R, R2, Bias, ABias
#'
#' @param Qsim
#' @param Qobs
#'
#' @return
#' @export
#'
#' @examples
model_peformance <- function(Qsim, Qobs){

  # exclude NA
  data <- data.table(Qsim, Qobs)
  data <- na.omit(data)
  Qsim <- data$Qsim
  Qobs <- data$Qobs

  data.table(NSE = NSE(Qsim, Qobs),
             RMSE = RMSE(Qsim, Qobs),
             logNSE = logNSE(Qsim, Qobs),
             R = cor(Qsim, Qobs),
             R2 = cor(Qsim, Qobs)^2,
             Bias = Bias(Qsim, Qobs),
             ABias = ABias(Qsim, Qobs),
             KGE = KGE(Qsim, Qobs)
  )
}


