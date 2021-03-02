### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2021, ICAR-NBPGR.
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

#' Prints summary of \code{augmentedRCBD.bulk} object
#'
#' \code{print.augmentedRCBD.bulk} prints to console the summary of an object of
#' class \code{augmentedRCBD.bulk} including the augmented design details,
#' trait-wise mean sum of squares from ANOVA (Treatment adjusted) and ANOVA
#' (Block adjusted), adjusted means, coefficient of variation, overall adjusted
#' means critical differences, standard errors, descriptive statistics,
#' frequency distribution plots, genetic variability statistics and plots of
#' genetic variability parameters.
#'
#' @param x An object of class \code{augmentedRCBD.bulk}.
#' @param ... Unused
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.bulk}}
#'
#' @export
print.augmentedRCBD.bulk <- function(x, ...){

  cat("\nAugmented Design Details\n")
  cat("========================\n")
  Details <- x$Details
  b <- Details$`Number of blocks`
  ntr <- Details$`Number of treatments`
  checks <- Details$`Check treatments`
  ltests <- Details$`Number of test treatments`
  ntraits <- Details$`Number of Traits`
  traits <- Details$Traits

  Details <- t(data.frame(`Number of blocks` = b, `Number of treatments` = ntr,
                  `Number of check treatments` = length(checks),
                  `Number of test treatments` = ltests,
                  `Check treatments` =  paste(checks, collapse = ", "),
                  `Number of traits` = ntraits,
                  Traits = paste(traits, collapse = ", ")))
  rownames(Details) <- gsub("\\.", " ", rownames(Details))
  colnames(Details) <- c("")
  print(Details)
  cat("\nANOVA, Treatment Adjusted\n")
  cat("=========================\n")
  print(x$`ANOVA, Treatment Adjusted`)
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  print(x$`ANOVA, Block Adjusted`)
  cat("\nCoefficient of Variation\n")
  cat("========================\n")
  print(x$CV)
  cat("\n\nOverall Adjusted Mean\n")
  cat("=====================\n")
  print(x$`Overall adjusted mean`)
  cat("\n\nStandard Errors\n")
  cat("===================\n")
  print(x$`Std. Errors`)
  cat("\n\nCritical Difference\n")
  cat("===================\n")
  cat(paste("alpha =", x$alpha))
  print(x$CD)
  cat("\n\nDescriptive Statistics\n")
  cat("===================\n")
  print(x$`Descriptive statistics`)
  cat("\n\nGenetic Variability Analysis\n")
  cat("===================\n")
  print(x$`Genetic variability analysis`)
  cat("\n\nWarning Messages\n")
  cat("===================\n")
  if (!is.null(x$warnings$Model)) {
    cat(paste(x$warnings$Model), sep = "\n")
  }
  if (!is.null(x$warnings$`Freq. dist`)) {
    cat(paste(x$warnings$`Freq. dist`), sep = "\n")
  }
  cat("\nTreatment Means\n")
  cat("===============\n")
  print(x$Means)
}
