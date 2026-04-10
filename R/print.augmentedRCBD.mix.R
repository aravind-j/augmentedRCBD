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

#' Prints summary of \code{augmentedRCBD.mix} object
#'
#' \code{print.augmentedRCBD.mix} prints to console the summary of an object of
#' class \code{augmentedRCBD.mix} including the augmented design details, ANOVA
#' (Treatment adjusted), ANOVA (Block adjusted), treatment means, coefficient of
#' variation, overall adjusted mean, critical differences and standard errors.
#' The treatment/genotype groups along with the grouping method are also printed
#' if they were computed.
#'
#' @param x An object of class \code{augmentedRCBD.mix}.
#' @param ... Unused
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.mix}}
#'
#' @export
print.augmentedRCBD.mix <- function(x, ...){

  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

  cat("\nAugmented Design Details\n")
  cat("========================\n")
  print(x$Details)

  cat("\nModel Formula\n")
  cat("=========================\n")
  cat(deparse(formula(x$Model)))

  cat("\nModel Details\n")
  cat("=========================\n")
  print(summary(x$Model))

  cat("\nModel Diagnostics\n")
  cat("=========================\n")
  print(data.frame(x$`Model Diagnostics`))

  cat("\nANOVA, Fixed Effects\n")
  cat("=========================\n")
  print(x$`ANOVA, Fixed Effects`)

  cat("\nLRT, Random Effects\n")
  cat("=========================\n")
  print(x$`LRT, Random Effects`)

  cat("\nModel Formula\n")
  cat("=========================\n")
  print(x$Means)

}
