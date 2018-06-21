### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015, ICAR-NBPGR.
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

#' Generate MS Word Report from \code{augmentedRCBD} Output
#'
#' \code{report.augmentedRCBD} generates a tidy report from an object of
#' class \code{augmentedRCBD} as docx MS word file using the
#' \code{\link[officer]{officer}} package.
#'
#' @param aug An object of class \code{augmentedRCBD}.
#' @param target The path to the docx file to be created.
#'
#' @export
#' @import officer
#' @import flextable
#' @importFrom dplyr mutate_if
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom methods is
#' @importFrom stringi stri_pad_right
#' @importFrom graphics plot
#'
#' @seealso \code{\link[officer]{officer}}, \code{\link[flextable]{flextable}}
#'
#' @examples
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
#' # Results for variable y1 (checks inferred)
#' out <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = FALSE)
#'
#' \dontrun{
#' report.augmentedRCBD(out, "augmentedRCBD output.docx")
#' }
#'
report.augmentedRCBD <- function(aug, target){

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class "augmentedRCBD"')
  }

  if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)){
    stop(target, " should have '.docx' extension.")
  }

  augreport <- read_docx(file.path(system.file(package = "augmentedRCBD"),
                                   "template.docx"))

  augreport <- body_add_par(augreport, value = "augmentedRCBD", style = "Title")
  augreport <- body_add_toc(augreport, level = 2)

  # Details
  augreport <- body_add_par(augreport, value = "Details", style = "heading 1")

  Details <- t(data.frame(`Number of blocks` = aug$Details$`Number of blocks`,
                          `Number of treatments` = aug$Details$`Number of treatments`,
                          `Number of check treatments` = aug$Details$`Number of check treatments`,
                          `Number of test treatments` = aug$Details$`Number of test treatments`,
                          `Check treatments` =  paste(aug$Details$`Check treatments`,
                                                      collapse = ", ")))
  Details <- data.frame(Details)
  Details <- cbind(gsub("\\.", " ", rownames(Details)), Details)
  colnames(Details) <- c("Item", "Details")

  Details <- regulartable(data = data.frame(Details))
  Details <- autofit(Details)
  augreport <- body_add_flextable(augreport, Details)

  # ANOVA, TA
  augreport <- body_add_par(augreport, value = "ANOVA, Treatment Adjusted",
                            style = "heading 1")
  if (is.data.frame(aug$`ANOVA, Treatment Adjusted`)){
    anovata <- aug$`ANOVA, Treatment Adjusted`
  } else {
    anovata <- data.frame(aug$`ANOVA, Treatment Adjusted`[[1]])
    anovata <- cbind(Source = trimws(rownames(anovata)), anovata)
  }
  colnames(anovata) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                         "F value", "Pr(>F)")
  anovata$Df <- as.character(anovata$Df)
  anovata <- dplyr::mutate_if(anovata, is.numeric, round.conditional)
  anovata <- autofit(regulartable(anovata))
  anovata <- bold(anovata, part = "header")
  augreport <- body_add_flextable(augreport, anovata)

  # ANOVA, BA
  augreport <- body_add_par(augreport, value = "ANOVA, Block Adjusted",
                            style = "heading 1")
  if (is.data.frame(aug$`ANOVA, Block Adjusted`)){
    anovaba <- aug$`ANOVA, Block Adjusted`
  } else {
    anovaba <- data.frame(aug$`ANOVA, Block Adjusted`[[1]])
    anovaba <- cbind(Source = trimws(rownames(anovaba)), anovaba)
  }
  colnames(anovaba) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                         "F value", "Pr(>F)")
  anovaba$Df <- as.character(anovaba$Df)
  anovaba <- dplyr::mutate_if(anovaba, is.numeric, round.conditional)
  anovaba <- autofit(regulartable(anovaba))
  anovaba <- bold(anovaba, part = "header")
  augreport <- body_add_flextable(augreport, anovaba)

  # Std. Errors
  augreport <- body_add_par(augreport,
                            value = "Standard Errors and Critical Differences",
                            style = "heading 1")
  se <- aug$`Std. Errors`
  se <- dplyr::mutate_if(se, is.numeric, round.conditional)
  se <- autofit(regulartable(se))
  se <- bold(se, part = "header")
  augreport <- body_add_flextable(augreport, se)

  # Overall adjusted mean
  augreport <- body_add_par(augreport, value = "Overall Adjusted Mean",
                            style = "heading 1")
  augreport <- body_add_par(augreport,
                            value = as.character(round.conditional(aug$`Overall adjusted mean`)),
                            style = "Normal")

  # Coefficient of variation
  augreport <- body_add_par(augreport, value = "Coefficient of Variation",
                            style = "heading 1")
  augreport <- body_add_par(augreport,
                            value = as.character(round.conditional(aug$CV)),
                            style = "Normal")

  # Means
  augreport <- body_add_par(augreport, value = "Means", style = "heading 1")
  Means <- aug$Means
  Means <- dplyr::mutate_if(Means, is.numeric, round.conditional)
  Means <- autofit(regulartable(Means))
  Means <- bold(Means, part = "header")
  augreport <- body_add_flextable(augreport, Means)

  # Freq dist
  augreport <- body_add_par(augreport, value = "Frequency Distribution",
                            style = "heading 1")
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 6, height = 4, units = 'in', res = 300)
  plot(freqdist.augmentedRCBD(aug, xlab = ""))
  dev.off()
  augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
  rm(src)

  # Desc stat
  descout <- data.frame(describe.augmentedRCBD(aug))[1, ]
  descout$Skewness.p.value. <- ifelse(descout$Skewness.p.value. <= 0.01, "**",
                                      ifelse(descout$Skewness.p.value. <= 0.05,
                                             "*", "ns"))
  descout$Kurtosis.p.value. <- ifelse(descout$Kurtosis.p.value. <= 0.01, "**",
                                      ifelse(descout$Kurtosis.p.value. <= 0.05,
                                             "*", "ns"))

  desc <- c("Mean", "Std.Error", "Std.Deviation", "Min",
            "Max", "Skewness.statistic.", "Kurtosis.statistic.")
  descout[, desc] <- apply(descout[, desc], MARGIN = 2, FUN = round.conditional)
  colnames(descout) <- c("Count", "Mean", "Std.Error", "Std.Deviation",
                         "Min", "Max", "Skewness", "Skewness_sig", "Kurtosis",
                         "Kurtosis_sig")
  descout$Skewness <- paste(descout$Skewness,
                            stringi::stri_pad_right(descout$Skewness_sig, 3),
                            sep = " ")
  descout$Kurtosis <- paste(descout$Kurtosis,
                            stringi::stri_pad_right(descout$Kurtosis_sig, 3),
                            sep = " ")
  descout <- descout[, c("Count", "Mean", "Std.Error", "Std.Deviation",
                         "Min", "Max", "Skewness", "Kurtosis")]
  descout <- data.frame(t(descout))
  descout <- cbind(Statistic = rownames(descout), descout)
  rownames(descout) <- NULL
  colnames(descout) <- c("Statistic", "Value")

  descout <- autofit(regulartable(descout))
  descout <- bold(descout, part = "header")
  augreport <- body_add_flextable(augreport, descout)

  augreport <- body_add_par(augreport,
                            value = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
                            style = "Normal")

  # GVA
  gvaout <- data.frame(gva.augmentedRCBD(aug))
  gvaout <- dplyr::mutate_if(gvaout, is.numeric, round.conditional)
  gvaout <- data.frame(t(gvaout))
  gvaout <- cbind(Statistic = rownames(gvaout), gvaout)
  rownames(gvaout) <- NULL
  colnames(gvaout) <- c("Statistic", "Value")

  gvaout <- autofit(regulartable(gvaout))
  gvaout <- bold(gvaout, part = "header")
  augreport <- body_add_flextable(augreport, gvaout)

  # Comparisons
  if (!is.null(aug$Comparisons)) {
    augreport <- body_add_par(augreport, value = "Comparisons",
                              style = "heading 1")
    augreport <- body_add_par(augreport,
                              value = paste("Comparison method:",
                                            aug$`Comparison method`),
                              style = "Normal")
    cmp <- aug$Comparisons
    cmp <- dplyr::mutate_if(cmp, is.numeric, round.conditional)
    cmp <- autofit(regulartable(cmp))
    cmp <- bold(cmp, part = "header")
    augreport <- body_add_flextable(augreport, cmp)

    augreport <- body_add_par(augreport,
                              value = "* P \u2264 0.05; ** P \u2264 0.01",
                              style = "Normal")
  }

  # Groups
  if (!is.null(aug$Groups)) {
    augreport <- body_add_par(augreport, value = "Groups",
                              style = "heading 1")
    augreport <- body_add_par(augreport,
                              value = paste("Comparison method:",
                                            aug$`Comparison method`),
                              style = "Normal")
    gps <- aug$Groups
    gps <- dplyr::mutate_if(gps, is.numeric, round.conditional)
    gps <- autofit(regulartable(gps))
    gps <- bold(gps, part = "header")
    augreport <- body_add_flextable(augreport, gps)
  }

  augreport <- body_add_par(augreport,
                            value = "################## The End ##################",
                            style = "Center text")

  print(augreport, target = target)

}
