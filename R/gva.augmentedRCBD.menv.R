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

#' Perform Genetic Variability Analysis on \code{augmentedRCBD.menv} Output
#'
#' \code{gva.augmentedRCBD.menv} performs genetic variability analysis on an
#' object of class \code{augmentedRCBD.menv}. \loadmathjax
#'
#' \code{gva.augmentedRCBD.menv} performs genetic variability analysis from the
#' ANOVA results in an object of class \code{augmentedRCBD.menv} and computes
#' several variability estimates. The analysis is based on the mean squares
#' averaged across environments as reported in the combined ANOVA table.
#'
#'
#' @import mathjaxr
#' @importFrom methods is
#' @importFrom grDevices col2rgb
#' @importFrom Rdpack reprompt
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
#' gva(out1)
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
#' gva(out2)
#'
gva.augmentedRCBD.menv <- function(aug, k = 2.063, ...) {

  if (!is(aug, "augmentedRCBD.menv")) {
    stop('"aug" is not of class "augmentedRCBD.menv".')
  }



  out <- NULL
  return(out)

}
