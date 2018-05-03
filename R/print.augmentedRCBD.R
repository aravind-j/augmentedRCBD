#' Prints summary of \code{augmentedRCBD} object.
#'
#' \code{print.augmentedRCBD} prints to console the summary of an object of
#' class \code{augmentedRCBD} including the augmented design details, ANOVA
#' (Treatment adjusted), ANOVA (Block adjusted), Treatment means, Coefficient of
#' variation, overall adjusted mean and standard errors. The treatment/genotype
#' groups along with the grouping method are also printed if they were computed.
#'
#' @param x An object of class \code{augmentedRCBD}.
#' @param ... Unused
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#'
#' @export
print.augmentedRCBD <- function(x, ...){

  cat("\nAugmented design details\n")
  cat("========================\n")
  print(Details)
  cat("\nANOVA, Treatment Adjusted\n")
  cat("=========================\n")
  print(A1)
  cat("\nANOVA, Block Adjusted\n")
  cat("=====================\n")
  print(A2)
  cat("\nTreatment means\n")
  cat("===============\n")
  print(Means)
  cat("\nCoefficient of variation\n")
  cat("========================\n")
  cat(CV)
  cat("\n\nOverall adjusted mean\n")
  cat("=====================\n")
  cat(`Overall Adjusted mean`)
  cat("\n\nStandard errors\n")
  cat("===================\n")
  print(SECD)
  if (group) {
    cat("\nTreatment groups\n")
    cat("==================\n")
    cat(paste("\nMethod : ", method.comp, "\n\n", sep = ""))
    print(Groups)
  }
}

