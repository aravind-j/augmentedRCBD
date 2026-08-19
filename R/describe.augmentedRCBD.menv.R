### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2024, ICAR-NBPGR.
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

#' Compute Descriptive Statistics from \code{augmentedRCBD.menv} Output
#'
#' \code{describe.augmentedRCBD.menv} computes descriptive statistics from the
#' adjusted means in an object of class \code{augmentedRCBD.menv}.
#'
#' \code{describe.augmentedRCBD.menv} computes the following descriptive
#' statistics from the adjusted means (averaged across environments) in an
#' object of class \code{augmentedRCBD.menv}.
#'
#' \itemize{ \item Count \item Mean \item Standard deviation \item Standard
#' error \item Minimum \item Maximum \item Skewness statistic along with p-value
#' from D'Agostino test of skewness (D'Agostino, 1970). \item Kurtosis statistic
#' along with p-value from Anscombe-Glynn test of kurtosis (Anscombe and Glynn,
#' 1983). }
#'
#' @param aug An object of class \code{augmentedRCBD.menv}.
#' @param ... Unused
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
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.menv}}
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
#' @examples
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Scenario 1: Test treatments are replicated across all environments
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Example data
#' blk1 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
#'           7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
#' trt1 <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10,
#'           1, 2, 3, 4, 8, 11, 5, 1, 2, 3, 4, 12, 9, 1, 2, 3, 4, 7, 6, 10,
#'           1, 2, 3, 4, 7, 9, 12, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 8, 11, 10)
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
#'         78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
#'         76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
#'         78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
#' env1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'           2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
#'           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
#' data1 <- data.frame(env1, blk1, trt1, y1)
#' chks1 <- c(1, 2, 3, 4)
#'
#' # Convert block, treatment and environment to factors
#' data1$blk1 <- as.factor(data1$blk1)
#' data1$trt1 <- as.factor(data1$trt1)
#' data1$env1 <- as.factor(data1$env1)
#'
#' # Contingency tables of factors
#' table(data1$env1, data1$trt1)
#' table(data1$env1, data1$blk1)
#' table(data1$blk1, data1$trt1)
#'
#' # Results
#' out1 <- augmentedRCBD.menv(block = data1$blk1, treatment = data1$trt1,
#'                            env = data1$env1, y = data1$y1, checks = chks1,
#'                            scenario = 1, method.comp = "lsd", alpha = 0.05,
#'                            group = TRUE, console = TRUE)
#'
#' # Descriptive statistics
#' describe(out1)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Test treatments are not replicated across all environments
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Example data
#' blk2 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
#'           7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
#' trt2 <- c(1, 2, 3, 4, 7, 10, 11, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 12,
#'           1, 2, 3, 4, 16, 19, 13, 1, 2, 3, 4, 20, 17, 1, 2, 3, 4, 15, 14, 18,
#'           1, 2, 3, 4, 22, 25, 27, 1, 2, 3, 4, 21, 23, 1, 2, 3, 4, 24, 26, 28)
#' y2 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
#'         78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
#'         76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
#'         78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
#' env2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'           2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
#'           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
#' data2 <- data.frame(env2, blk2, trt2, y2)
#' chks2 <- c(1, 2, 3, 4)
#'
#' # Convert block, treatment and environment to factors
#' data2$blk2 <- as.factor(data2$blk2)
#' data2$trt2 <- as.factor(data2$trt2)
#' data2$env2 <- as.factor(data2$env2)
#'
#' # Contingency tables of factors
#' table(data2$env2, data2$trt2)
#' table(data2$env2, data2$blk2)
#' table(data2$blk2, data2$trt2)
#'
#' # Results
#' out2 <- augmentedRCBD.menv(block = data2$blk2, treatment = data2$trt2,
#'                            env = data2$env2, y = data2$y2, checks = chks2,
#'                            scenario = 2, method.comp = "lsd", alpha = 0.05,
#'                            group = TRUE, console = TRUE)
#'
#' # Descriptive statistics
#' describe(out2)
#'
describe.augmentedRCBD.menv <- function(aug, ...) {

  if (!is(aug, "augmentedRCBD.menv")) {
    stop('"aug" is not of class "augmentedRCBD.menv".')
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
