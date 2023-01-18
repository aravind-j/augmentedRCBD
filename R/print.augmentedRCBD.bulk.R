### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2022, ICAR-NBPGR.
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

  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

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
  dcols <- setdiff(colnames(x$`ANOVA, Treatment Adjusted`),
                   paste(traits, "_Pr(>F)", sep = ""))
  x$`ANOVA, Treatment Adjusted` <- x$`ANOVA, Treatment Adjusted`[, dcols]
  x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                        "_Mean.Sq",
                                        sep = "")] <-
    lapply(x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                                 "_Mean.Sq",
                                                 sep = "")],
           round.conditional, digits = round.digits)
  colnames(x$`ANOVA, Treatment Adjusted`) <-
    gsub("(^.+)(_sig$)", "", colnames(x$`ANOVA, Treatment Adjusted`))
  colnames(x$`ANOVA, Treatment Adjusted`) <-
    gsub("_Mean.Sq", "", colnames(x$`ANOVA, Treatment Adjusted`))
  cat(paste(rep(" ", max(nchar(x$`ANOVA, Treatment Adjusted`$Source)) +
                  max(nchar(x$`ANOVA, Treatment Adjusted`$Df)) + 5),
            collapse = ""), "Mean.Sq\n")
  print(x$`ANOVA, Treatment Adjusted`)
  cat("ⁿˢ P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  dcols <- setdiff(colnames(x$`ANOVA, Block Adjusted`),
                   paste(traits, "_Pr(>F)", sep = ""))
  x$`ANOVA, Block Adjusted` <- x$`ANOVA, Block Adjusted`[, dcols]
  x$`ANOVA, Block Adjusted`[, paste(traits,
                                        "_Mean.Sq",
                                        sep = "")] <-
    lapply(x$`ANOVA, Block Adjusted`[, paste(traits,
                                                 "_Mean.Sq",
                                                 sep = "")],
           round.conditional, digits = round.digits)
  colnames(x$`ANOVA, Block Adjusted`) <-
    gsub("(^.+)(_sig$)", "", colnames(x$`ANOVA, Block Adjusted`))
  colnames(x$`ANOVA, Block Adjusted`) <-
    gsub("_Mean.Sq", "", colnames(x$`ANOVA, Block Adjusted`))
  cat(paste(rep(" ", max(nchar(x$`ANOVA, Block Adjusted`$Source)) +
                  max(nchar(x$`ANOVA, Block Adjusted`$Df)) + 5),
            collapse = ""), "Mean.Sq\n")
  print(x$`ANOVA, Block Adjusted`)
  cat("ⁿˢ P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  cat("\nCoefficient of Variation\n")
  cat("========================\n")
  x$CV$CV <- round.conditional(x$CV$CV, digits = round.digits)
  print(x$CV)
  cat("\n\nOverall Adjusted Mean\n")
  cat("=====================\n")
  x$`Overall adjusted mean`$Overall.adjusted.mean <-
    round.conditional(x$`Overall adjusted mean`$Overall.adjusted.mean,
                      digits = round.digits)
  print(x$`Overall adjusted mean`)
  cat("\n\nStandard Errors\n")
  cat("===================\n")
  x$`Std. Errors`[, traits] <- lapply(x$`Std. Errors`[, traits, drop = FALSE],
                                      round.conditional, digits = round.digits)
  print(x$`Std. Errors`)
  cat("\n\nCritical Difference\n")
  cat("===================\n")
  cat(paste("alpha =", x$alpha, "\n"))
  x$CD[, traits] <- lapply(x$CD[, traits, drop = FALSE],
                           round.conditional, digits = round.digits)
  print(x$CD)
  cat("\n\nDescriptive Statistics\n")
  cat("===================\n")
  desc <- c("Mean", "Std.Error", "Std.Deviation", "Min",
            "Max", "Skewness", "Kurtosis")
  x$`Descriptive statistics`[, desc] <-
    apply(x$`Descriptive statistics`[, desc], MARGIN = 2,
                           FUN = round.conditional)
  descols <- c("Trait", "Count", "Mean", "Std.Error",
               "Std.Deviation", "Min", "Max", "Skewness", "Skewness_sig",
               "Kurtosis", "Kurtosis_sig")
  print(x$`Descriptive statistics`[, descols])
  cat("ⁿˢ P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  cat("\n\nGenetic Variability Analysis\n")
  cat("===================\n")
  gvap <- c("Mean", "PV", "GV", "EV", "GCV", "PCV",  "ECV", "hBS", "GA", "GAM")
  x$`Genetic variability analysis`[, gvap] <-
    apply(x$`Genetic variability analysis`[, gvap], MARGIN = 2,
          FUN = round.conditional)
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
  x$Means[, traits] <- lapply(x$Means[, traits, drop = FALSE],
                              round.conditional, digits = round.digits)
  print(x$Means)
}
