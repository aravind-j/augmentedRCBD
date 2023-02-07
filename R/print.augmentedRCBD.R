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

  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

  wstring1 <- "Test treatments are replicated"
  wstring2 <- "Negative adjusted means were generated for the following"

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
  cat("\n")
  if (any(grepl(wstring1, x$warnings))) {
    dups <- x$Means[!(x$Means$Treatment %in% checks), ]$Treatment
    dups <- dups[duplicated(dups)]
    dups <- x$Means[x$Means$Treatment %in% dups, c("Treatment", "Block")]
    rownames(dups) <- NULL
    warning("Following test treatments are replicated.", call. = FALSE,
            immediate. = TRUE)
    print(dups)
  }
  cat("\nANOVA, Treatment Adjusted\n")
  cat("=========================\n")
  print(x$`ANOVA, Treatment Adjusted`)
  if (any(!grepl(paste(c(wstring1, wstring2), collapse = "|"), x$warnings))) {
    warning(x$warnings[!grepl(paste(c(wstring1, wstring2), collapse = "|"),
                              x$warnings)],
            call. = FALSE, immediate. = TRUE)
  }
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  print(x$`ANOVA, Block Adjusted`)
  if (any(!grepl(paste(c(wstring1, wstring2), collapse = "|"), x$warnings))) {
    warning(x$warnings[!grepl(paste(c(wstring1, wstring2), collapse = "|"),
                              x$warnings)],
            call. = FALSE, immediate. = TRUE)
  }
  cat("\nCoefficient of Variation\n")
  cat("========================\n")
  cat(x$CV)
  cat("\n\nOverall Adjusted Mean\n")
  cat("=====================\n")
  cat(x$`Overall adjusted mean`)
  cat("\n\nStandard Errors\n")
  cat("===================\n")
  print(x$`Std. Errors`)
  if (!is.null(x$x$warnings)) {
    cat("\n\nWarning Messages\n")
    cat("===================\n")
    cat("\n\n[Model]\n")
    cat(paste(x$x$warnings), sep = "\n")
  }
  cat("\nTreatment Means\n")
  cat("===============\n")
  x$Means[, c("Means", "SE", "Min", "Max", "Adjusted Means")] <-
    lapply(x$Means[, c("Means", "SE", "Min", "Max", "Adjusted Means")],
           round.conditional, digits = round.digits)
  print(x$Means)
  cat("\n")
  if (any(grepl(wstring2, x$warnings))) {
    warning(x$warnings[grepl(wstring2, x$warnings)],
            call. = FALSE, immediate. = TRUE)
  }
  if (!is.null(x$Comparisons)) {
    cat("\nComparisons\n")
    cat("==================\n")
    cat(paste("\nMethod : ", x$`Comparison method`, "\n\n", sep = ""))
    x$Comparisons[, c("estimate", "SE")] <-
      lapply(x$Comparisons[, c("estimate", "SE")],
             round.conditional, digits = round.digits)
    x$Comparisons[, c("t.ratio", "p.value")] <-
      lapply(x$Comparisons[, c("t.ratio", "p.value")],
             round.conditional, digits = max(round.digits, 3))
    print(x$Comparisons)
  }
  if (!is.null(x$Groups)) {
    cat("\nTreatment Groups\n")
    cat("==================\n")
    cat(paste("\nMethod : ", x$`Comparison method`, "\n\n", sep = ""))
    x$Groups[, c("Adjusted Means", "SE", "lower.CL", "upper.CL")] <-
      lapply(x$Groups[, c("Adjusted Means", "SE", "lower.CL", "upper.CL")],
             round.conditional, digits = round.digits)
    print(x$Groups)
  }
}
