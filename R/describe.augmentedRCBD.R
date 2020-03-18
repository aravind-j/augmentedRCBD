### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2020, ICAR-NBPGR.
#
# augmentedRCBD is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# augmentedRCBD is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

#' Compute Descriptive Statistics from \code{augmentedRCBD} Output
#'
#' \code{describe.augmentedRCBD} computes descriptive statistics from the
#' adjusted means in an object of class \code{augmentedRCBD}.
#'
#' \code{describe.augmentedRCBD} computes the following descriptive statistics
#' from the adjusted means in an object of class \code{augmentedRCBD}.
#'
#' \itemize{ \item Count \item Mean \item Standard deviation \item Standard
#' error \item Minimum \item Maximum \item Skewness statistic along with p-value
#' from D'Agostino test of skewness (D'Agostino, 1970). \item Kurtosis statistic
#' along with p-value from Anscombe-Glynn test of kurtosis (Anscombe and Glynn,
#' 1983). }
#'
#' @param aug An object of class \code{augmentedRCBD}.
#'
#' @return A list with the following descriptive statistics:  \item{Count}{The
#'   number of treatments/genotypes.} \item{Mean}{The mean value.}
#'   \item{Std.Error}{The standard error.} \item{Std.Deviation}{The standard
#'   deviation.} \item{Min}{The minimum value} \item{Max}{The maximum value}
#'   \item{Skewness(statistic)}{The skewness estimator.}
#'   \item{Skewness(p.value)}{The p-value from D'Agostino test of skewness.}
#'   \item{Kurtosis(statistic)}{The kurtosis estimator.}
#'   \item{Kurtosis(p.value)}{The p-value from Anscombe-Glynn test of kurtosis.}
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#' @references
#'
#' \insertRef{dagostino_transformation_1970}{augmentedRCBD}
#'
#' \insertRef{anscombe_distribution_1983}{augmentedRCBD}
#'
#' @importFrom methods is
#' @importFrom moments agostino.test
#' @importFrom moments anscombe.test
#' @importFrom stats sd
#' @export
#'
#'@examples
#' # Example data
#' blk <- c(rep(1,7),rep(2,6),rep(3,7))
#' trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
#'         70, 75, 74)
#' y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
#'         240, 268, 287, 226, 395, 450)
#' data <- data.frame(blk, trt, y1, y2)
#' # Convert block and treatment to factors
#' data$blk <- as.factor(data$blk)
#' data$trt <- as.factor(data$trt)
#' # Results for variable y1
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' # Results for variable y2
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y2, method.comp = "lsd",
#'                      alpha = 0.05, group = TRUE, console = TRUE)
#'
#' # Descriptive statistics
#' describe.augmentedRCBD(out1)
#' describe.augmentedRCBD(out2)
describe.augmentedRCBD <- function(aug) { # all treatments (test + checks)

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class "augmentedRCBD"')
  }

  adjmeans <- aug$Means$`Adjusted Means`
  Mean <- mean(adjmeans)
  Count <- length(adjmeans)
  Skewness <- moments::agostino.test(adjmeans, alternative = "two.sided")
  Kurtosis <- moments::anscombe.test(adjmeans, alternative = "two.sided")
  Range <- range(adjmeans)
  stddev <- sd(adjmeans)
  stderror <- stddev / sqrt(Count)

  out <- list(Count = Count, Mean = Mean, `Std.Error` = stderror,
              `Std.Deviation` = stddev, Min = Range[1], Max = Range[2],
              `Skewness(statistic)` = Skewness$statistic,
              `Skewness(p.value)` = Skewness$p.value,
              `Kurtosis(statistic)` = Kurtosis$statistic,
              `Kurtosis(p.value)` = Kurtosis$p.value)
  return(out)
}
