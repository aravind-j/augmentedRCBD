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

#' Prints summary of \code{augmentedRCBD} object
#'
#' \code{print.augmentedRCBD} prints to console the summary of an object of
#' class \code{augmentedRCBD} including the augmented design details, ANOVA
#' (Treatment adjusted), ANOVA (Block adjusted), treatment means, coefficient of
#' variation, overall adjusted mean, critical differences and standard errors. The treatment/genotype
#' groups along with the grouping method are also printed if they were computed.
#'
#' @param x An object of class \code{augmentedRCBD}.
#' @param ... Unused
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#'
#' @export
print.augmentedRCBD <- function(x, ...){

  cat("\nAugmented Design Details\n")
  cat("========================\n")
  Details <- x$Details
  b <- Details$`Number of blocks`
  ntr <- Details$`Number of treatments`
  checks <- Details$`Check treatments`
  ltests <- Details$`Number of test treatments`

  Details <- t(data.frame(`Number of blocks` = b, `Number of treatments` = ntr,
                  `Number of check treatments` = length(checks),
                  `Number of test treatments` = ltests,
                  `Check treatments` =  paste(checks, collapse = ", ")))
  rownames(Details) <- gsub("\\.", " ", rownames(Details))
  colnames(Details) <- c("")
  print(Details)
  cat("\nANOVA, Treatment Adjusted\n")
  cat("=========================\n")
  print(x$`ANOVA, Treatment Adjusted`)
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  print(x$`ANOVA, Block Adjusted`)
  cat("\nTreatment Means\n")
  cat("===============\n")
  print(x$Means)
  cat("\nCoefficient of Variation\n")
  cat("========================\n")
  cat(x$CV)
  cat("\n\nOverall Adjusted Mean\n")
  cat("=====================\n")
  cat(x$`Overall adjusted mean`)
  cat("\n\nStandard Errors\n")
  cat("===================\n")
  print(x$`Std. Errors`)
  if (!is.null(x$Groups)) {
    cat("\nTreatment Groups\n")
    cat("==================\n")
    cat(paste("\nMethod : ", x$`Comparison method`, "\n\n", sep = ""))
    print(x$Groups)
  }
}
