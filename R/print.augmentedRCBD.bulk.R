### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2023, ICAR-NBPGR.
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

  wstring1 <- "Test treatments are replicated"
  wstring2 <- "Negative adjusted means were generated for the following"

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
  cat("\n")
  if (any(grepl(wstring1, unlist(x$warnings)))) {
    dups <- x$Means[!(x$Means$Treatment %in% checks), ]$Treatment
    dups <- dups[duplicated(dups)]
    dups <- x$Means[x$Means$Treatment %in% dups, c("Treatment", "Block")]
    rownames(dups) <- NULL
    warning("Following test treatments are replicated.", call. = FALSE,
            immediate. = TRUE)
    print(dups, row.names = FALSE)
  }
  cat("\nANOVA, Treatment Adjusted\n")
  cat("=========================\n")
  dcols <- setdiff(colnames(x$`ANOVA, Treatment Adjusted`),
                   paste(traits, "_Pr(>F)", sep = ""))
  x$`ANOVA, Treatment Adjusted` <- x$`ANOVA, Treatment Adjusted`[, dcols]
  x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                        "_Mean.Sq",
                                        sep = "")] <-
    sapply(x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                                 "_Mean.Sq",
                                                 sep = "")],
           round.conditional, digits = round.digits)
  x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                        "_sig",
                                        sep = "")] <-
    sapply(x$`ANOVA, Treatment Adjusted`[, paste(traits,
                                                 "_sig",
                                                 sep = "")],
           function(sig) ifelse(sig == "ns", "\u207f\u02e2", sig))
  colnames(x$`ANOVA, Treatment Adjusted`) <-
    gsub("(^.+)(_sig$)", "", colnames(x$`ANOVA, Treatment Adjusted`))
  colnames(x$`ANOVA, Treatment Adjusted`) <-
    gsub("_Mean.Sq", "", colnames(x$`ANOVA, Treatment Adjusted`))
  cat(paste(rep(" ", max(nchar(x$`ANOVA, Treatment Adjusted`$Source)) +
                  max(nchar(x$`ANOVA, Treatment Adjusted`$Df)) + 5),
            collapse = ""), "Mean.Sq\n")
  print(x$`ANOVA, Treatment Adjusted`, row.names = FALSE)
  cat("\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  if (any(!grepl(paste(c(wstring1, wstring2), collapse = "|"),
                 x$warnings$Model))) {
    warn_wlist(x$warnings$Model[!grepl(paste(c(wstring1, wstring2),
                                             collapse = "|"),
                                       x$warnings$Model)])
  }
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  dcols <- setdiff(colnames(x$`ANOVA, Block Adjusted`),
                   paste(traits, "_Pr(>F)", sep = ""))
  x$`ANOVA, Block Adjusted` <- x$`ANOVA, Block Adjusted`[, dcols]
  x$`ANOVA, Block Adjusted`[, paste(traits,
                                    "_Mean.Sq",
                                    sep = "")] <-
    sapply(x$`ANOVA, Block Adjusted`[, paste(traits,
                                             "_Mean.Sq",
                                             sep = "")],
           round.conditional, digits = round.digits)
  x$`ANOVA, Block Adjusted`[, paste(traits,
                                    "_sig",
                                    sep = "")] <-
    sapply(x$`ANOVA, Block Adjusted`[, paste(traits,
                                             "_sig",
                                             sep = "")],
           function(sig) ifelse(sig == "ns", "\u207f\u02e2", sig))
  colnames(x$`ANOVA, Block Adjusted`) <-
    gsub("(^.+)(_sig$)", "", colnames(x$`ANOVA, Block Adjusted`))
  colnames(x$`ANOVA, Block Adjusted`) <-
    gsub("_Mean.Sq", "", colnames(x$`ANOVA, Block Adjusted`))
  cat(paste(rep(" ", max(nchar(x$`ANOVA, Block Adjusted`$Source)) +
                  max(nchar(x$`ANOVA, Block Adjusted`$Df)) + 5),
            collapse = ""), "Mean.Sq\n")
  print(x$`ANOVA, Block Adjusted`, row.names = FALSE)
  cat("\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  if (any(!grepl(paste(c(wstring1, wstring2), collapse = "|"),
                 x$warnings$Model))) {
    warn_wlist(x$warnings$Model[!grepl(paste(c(wstring1, wstring2),
                                             collapse = "|"),
                                       x$warnings$Model)])
  }
  cat("\nCoefficient of Variation\n")
  cat("========================\n")
  x$CV$CV <- round.conditional(x$CV$CV, digits = round.digits)
  print(x$CV, row.names = FALSE)
  cat("\n\nOverall Adjusted Mean\n")
  cat("=====================\n")
  x$`Overall adjusted mean`$Overall.adjusted.mean <-
    round.conditional(x$`Overall adjusted mean`$Overall.adjusted.mean,
                      digits = round.digits)
  print(x$`Overall adjusted mean`, row.names = FALSE)
  cat("\n\nStandard Errors\n")
  cat("===============\n")
  x$`Std. Errors`[, traits] <- lapply(x$`Std. Errors`[, traits, drop = FALSE],
                                      round.conditional, digits = round.digits)
  print(x$`Std. Errors`, row.names = FALSE)
  cat("\n\nCritical Difference\n")
  cat("===================\n")
  cat(paste("alpha =", x$alpha, "\n"))
  x$CD[, traits] <- lapply(x$CD[, traits, drop = FALSE],
                           round.conditional, digits = round.digits)
  print(x$CD, row.names = FALSE)
  cat("\n\nDescriptive Statistics\n")
  cat("======================\n")
  desc <- c("Mean", "Std.Error", "Std.Deviation", "Min",
            "Max", "Skewness", "Kurtosis")
  x$`Descriptive statistics`[, desc] <-
    apply(x$`Descriptive statistics`[, desc], MARGIN = 2,
          FUN = round.conditional)
  x$`Descriptive statistics`[, c("Kurtosis_sig", "Skewness_sig")] <-
    apply(x$`Descriptive statistics`[, c("Kurtosis_sig", "Skewness_sig")],
          MARGIN = 2, FUN = function(sig) ifelse(sig == "ns",
                                                 "\u207f\u02e2", sig))
  descols <- c("Trait", "Count", "Mean", "Std.Error",
               "Std.Deviation", "Min", "Max", "Skewness", "Skewness_sig",
               "Kurtosis", "Kurtosis_sig")
  print(x$`Descriptive statistics`[, descols], row.names = FALSE)
  cat("\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01\n")
  cat("\n\nGenetic Variability Analysis\n")
  cat("============================\n")
  gvap <- c("Mean", "PV", "GV", "EV", "GCV", "PCV",  "ECV", "hBS", "GA", "GAM")
  x$`Genetic variability analysis`[, gvap] <-
    apply(x$`Genetic variability analysis`[, gvap], MARGIN = 2,
          FUN = round.conditional)
  cat(paste("k =", x$k, "\n"))
  if (!is.null(x$warnings$GVA)) {
    gwstring1 <- "may not be appropriate for this trait"
    gwstring2 <- "Negative GV detected"

    if (any(grepl(paste(c(gwstring1, gwstring2), collapse = "|"),
                  x$warnings$GVA))) {
      new_trait <- x$`Genetic variability analysis`$Trait
      gwhltv <- rep("", length(new_trait))

      if (grepl(gwstring1, x$warnings$GVA)) {
        gwhlt1 <- names(sapply(x$warnings$GVA,
                               function(gvaw) any(grepl(gwstring1, gvaw))))
        gwhltv[which(new_trait %in% gwhlt1)] <-
          paste( gwhltv[which(new_trait %in% gwhlt1)], "\u2020", sep = "")
      }
      if (grepl(gwstring2, x$warnings$GVA)) {
        gwhlt2 <- names(sapply(x$warnings$GVA,
                               function(gvaw) any(grepl(gwstring2, gvaw))))
        gwhltv[which(new_trait %in% gwhlt2)] <-
          paste( gwhltv[which(new_trait %in% gwhlt2)], "\u2021", sep = "")
      }
      gwhltv <- stringi::stri_pad_right(gwhltv, width = max(nchar(gwhltv)))

      new_trait <- paste(stringi::stri_pad_right(x$`Genetic variability analysis`$Trait,
                                                 width = max(nchar(x$`Genetic variability analysis`$Trait))),
                         gwhltv)
      x$`Genetic variability analysis`$Trait <- new_trait

      print(x$`Genetic variability analysis`, row.names = FALSE)
      cat("\n")

      if (grepl(gwstring1, x$warnings$GVA)) {
        warning("\n\u2020 P-value for \"Treatment: Test\" is > 0.05. ",
            "Genetic variability analysis may not be appropriate for this trait.\n",
            call. = FALSE, immediate. = TRUE)
      }
      if (grepl(gwstring1, x$warnings$GVA)) {
        warning("\n\u2021 Negative GV detected.",
            "\n GCV, GCV category, hBS, hBS category, GA, GAM and\n GAM category could not be computed.",
            call. = FALSE, immediate. = TRUE)
      }
    }

  } else {
    print(x$`Genetic variability analysis`, row.names = FALSE)
  }
  cat("\n\nWarning Messages\n")
  cat("================\n")
  if (!is.null(x$warnings$Model)) {
    cat("\n\n[Model]\n")
    print_wlist(x$warnings$Model)
  }
  if (!is.null(x$warnings$`Freq. dist`)) {
    cat("\n\n[Frequency Distribution]\n")
    print_wlist(x$warnings$`Freq. dist`)
  }
  if (!is.null(x$warnings$GVA)) {
    cat("\n\n[GVA]\n")
    print_wlist(x$warnings$GVA)
  }
  cat("\nTreatment Means\n")
  cat("===============\n")
  x$Means[, traits] <- lapply(x$Means[, traits, drop = FALSE],
                              round.conditional, digits = round.digits)
  print(x$Means, row.names = FALSE)
  if (any(grepl(wstring2, x$warnings))) {
    warn_wlist(x$warnings$Model[grepl(wstring2, x$warnings$Model)])
  }
}


warn_wlist <- function(wlist) {
  invisible(sapply(seq_along(wlist), function(i) {
    cat("\n")
    warning(strwrap(prefix = "\n", initial = "",
                    x = c(paste("<", names(wlist)[i], ">", sep = ""),
                          trimws(unlist(strsplit(wlist[[i]], "\n"))))),
            call. = FALSE, immediate. = TRUE)
  }, simplify = TRUE, USE.NAMES = FALSE))
}

print_wlist <- function(wlist) {
  invisible(sapply(seq_along(wlist), function(i) {
    cat("<", names(wlist)[i], ">", sep = "")
    cat("\n")
    cat(wlist[[i]])
    cat("\n")
  }, simplify = TRUE, USE.NAMES = FALSE))
}


