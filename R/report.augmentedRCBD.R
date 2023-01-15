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

#' Generate MS Word or Excel Report from \code{augmentedRCBD} Output
#'
#' \code{report.augmentedRCBD} generates a tidy report from an object of class
#' \code{augmentedRCBD} as docx MS word file using the
#' \code{\link[officer]{officer}} package or xlsx MS excel file using the
#' \code{\link[openxlsx]{openxlsx}} package.
#'
#' @param aug An object of class \code{augmentedRCBD}.
#' @param target The path to the report file to be created.
#' @param file.type The file type of the report. Either \code{"word"} for MS
#'   Word report file or \code{"excel"} for MS Excel report file.
#'
#' @export
#' @import officer
#' @import flextable
#' @import openxlsx
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
#' \donttest{
#' report.augmentedRCBD(aug = out,
#'                      target = file.path(tempdir(),
#'                                         "augmentedRCBD output.docx"),
#'                      file.type = "word")
#' report.augmentedRCBD(aug = out,
#'                      target = file.path(tempdir(),
#'                                         "augmentedRCBD output.xlsx"),
#'                      file.type = "excel")
#' }
#'
report.augmentedRCBD <- function(aug, target, file.type = c("word", "excel")){

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class "augmentedRCBD"')
  }

  file.type <- match.arg(file.type)

  if (file.type == "word") {
    if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)) {
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
    se <- cbind(Comparison = row.names(se), se)
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
    augreport <- body_add_par(augreport, value = "Descriptive Statistics",
                              style = "heading 1")
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
    augreport <- body_add_par(augreport, value = "Genetic Variability Analysis",
                              style = "heading 1")
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

  if (file.type == "excel") {
    if (!grepl(x = target, pattern = "\\.(xlsx)$", ignore.case = TRUE)) {
      stop(target, " should have '.xlsx' extension.")
    }



  # Create workbook
  wb <- createWorkbook()
  options(openxlsx.borders = "#TopBottomLeftRight")
  options(openxlsx.borderStyle = "thin")
  modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

  hs <- createStyle(halign = "left", valign = "bottom")

  num.base <- "0.00"
  numstyle <- createStyle(numFmt = num.base)
  ssstyle <- createStyle(numFmt = paste(num.base, '"*"'))
  dsstyle <- createStyle(numFmt = paste(num.base, '"**"'))
  nsstyle <- createStyle(numFmt = paste(num.base, '"ⁿˢ"'))

  # Index
  index <- c("Details", "ANOVA, Treatment Adjusted", "ANOVA, Block Adjusted",
             "SEs and CDs", "Overall Adjusted Mean", "Coefficient of Variation",
             "Means", "Frequency Distribution", "Descriptive Statistics",
             "Genetic Variability Analysis", "Comparisons", "Groups")
  index <- data.frame(`Sl.No` = seq_along(index),
                      Sheets = index)

  addWorksheet(wb, sheetName = "Index", gridLines = FALSE)
  insertImage(wb, sheet = "Index",
              file = system.file("extdata", "augmentedRCBD.png",
                                 package = "augmentedRCBD"),
              startCol = "A", startRow = 2,
              width = 1, height = 1.1)
  writeData(wb, sheet = "Index", x = "https://aravind-j.github.io/augmentedRCBD",
            startCol = "C", startRow = 4, borders = "none")
  writeData(wb, sheet = "Index", x = "https://github.com/aravind-j/augmentedRCBD",
            startCol = "C", startRow = 5, borders = "none")
  writeData(wb, sheet = "Index", x = "https://CRAN.R-project.org/package=augmentedRCBD",
            startCol = "C", startRow = 6, borders = "none")
  writeDataTable(wb, sheet = "Index", x = index,
            startCol = "B", startRow = 9, colNames = TRUE, rowNames = FALSE,
            headerStyle = hs, tableStyle = "TableStyleLight1",
            withFilter = FALSE, bandedRows = FALSE)
  addStyle(wb,  sheet = "Index", style = createStyle(halign = "right"),
           rows = 9, cols = 2, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "Index", cols = 1:3, widths = "auto")

  # Details
  Details <- t(data.frame(`Number of blocks` = aug$Details$`Number of blocks`,
                          `Number of treatments` = aug$Details$`Number of treatments`,
                          `Number of check treatments` = aug$Details$`Number of check treatments`,
                          `Number of test treatments` = aug$Details$`Number of test treatments`,
                          `Check treatments` =  paste(aug$Details$`Check treatments`,
                                                      collapse = ", ")))
  Details <- data.frame(Details)
  Details <- cbind(gsub("\\.", " ", rownames(Details)), Details)
  colnames(Details) <- c("Item", "Details")

  addWorksheet(wb, sheetName = "Details", gridLines = FALSE)
  writeDataTable(wb, sheet = "Details",
            x = Details,
            colNames = TRUE, rowNames = FALSE, headerStyle = hs,
            tableStyle = "TableStyleLight1", withFilter = FALSE,
            bandedRows = FALSE)
  setColWidths(wb, sheet = "Details", cols = 1:ncol(Details), widths = "auto")

  # ANOVA, TA
  if (is.data.frame(aug$`ANOVA, Treatment Adjusted`)){
    anovata <- aug$`ANOVA, Treatment Adjusted`
  } else {
    anovata <- data.frame(aug$`ANOVA, Treatment Adjusted`[[1]])
    anovata <- cbind(Source = trimws(rownames(anovata)), anovata)
  }
  colnames(anovata) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                         "F value", "Pr(>F)")

  addWorksheet(wb, sheetName = "ANOVA, Treatment Adjusted", gridLines = FALSE)
  writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted",
                 x = anovata,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = numstyle,
           rows = 2:6, cols = 3:6, stack = FALSE, gridExpand = TRUE)
  addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
           style = createStyle(halign = "right"),
           rows = 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "ANOVA, Treatment Adjusted",
               cols = 1:ncol(anovata), widths = "auto")

  # ANOVA, BA
  if (is.data.frame(aug$`ANOVA, Block Adjusted`)){
    anovaba <- aug$`ANOVA, Block Adjusted`
  } else {
    anovaba <- data.frame(aug$`ANOVA, Block Adjusted`[[1]])
    anovaba <- cbind(Source = trimws(rownames(anovaba)), anovaba)
  }
  colnames(anovaba) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                         "F value", "Pr(>F)")

  addWorksheet(wb, sheetName = "ANOVA, Block Adjusted", gridLines = FALSE)
  writeDataTable(wb, sheet = "ANOVA, Block Adjusted",
                 x = anovaba,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = numstyle,
           rows = 2:7, cols = 3:6, stack = FALSE, gridExpand = TRUE)
  addStyle(wb,  sheet = "ANOVA, Block Adjusted",
           style = createStyle(halign = "right"),
           rows = 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "ANOVA, Block Adjusted",
               cols = 1:ncol(anovaba), widths = "auto")


  # Std. Errors
  se <- aug$`Std. Errors`
  se <- cbind(Comparison = row.names(se), se)

  addWorksheet(wb, sheetName = "SEs and CDs", gridLines = FALSE)
  writeDataTable(wb, sheet = "SEs and CDs",
                 x = se,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "SEs and CDs", style = numstyle,
           rows = 2:5, cols = 2:3, stack = FALSE, gridExpand = TRUE)
  addStyle(wb,  sheet = "SEs and CDs", style = createStyle(halign = "right"),
           rows = 1, cols = 2:3, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "SEs and CDs",
               cols = 1:ncol(se), widths = "auto")

  # Overall adjusted mean
  addWorksheet(wb, sheetName = "Overall Adjusted Mean", gridLines = FALSE)
  writeData(wb, sheet = "Overall Adjusted Mean",
            x = aug$`Overall adjusted mean`,
            startCol = "A", startRow = 1, borders = "none")
  addStyle(wb,  sheet = "Overall Adjusted Mean", style = numstyle,
           rows = 1, cols = 1, stack = FALSE)

  # Coefficient of variation
  addWorksheet(wb, sheetName = "Coefficient of Variation", gridLines = FALSE)
  writeData(wb, sheet = "Coefficient of Variation",
            x = aug$CV,
            startCol = "A", startRow = 1, borders = "none")
  addStyle(wb,  sheet = "Coefficient of Variation", style = numstyle,
           rows = 1, cols = 1, stack = FALSE)

  # Means
  Means <- aug$Means

  addWorksheet(wb, sheetName = "Means", gridLines = FALSE)
  writeDataTable(wb, sheet = "Means",
                 x = Means,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "Means", style = numstyle,
           rows = 2:(nrow(Means) + 1), cols = c(3:4, 6:8),
           stack = FALSE, gridExpand = TRUE)
  addStyle(wb,  sheet = "Means", style = createStyle(halign = "right"),
           rows = 1, cols = 3:8, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "Means",
               cols = 1:ncol(Means), widths = "auto")

  # Freq dist
  addWorksheet(wb, sheetName = "Frequency Distribution", gridLines = FALSE)
  plot(freqdist.augmentedRCBD(aug, xlab = ""))
  insertPlot(wb, sheet = "Frequency Distribution",
             xy = c("B", 2))
  dev.off()

  # Desc stat
  descout <- data.frame(describe.augmentedRCBD(aug))[1, ]

  descout_sub <- descout[, c("Skewness.p.value.", "Kurtosis.p.value.")]
  descout <- descout[, c("Count", "Mean", "Std.Error", "Std.Deviation",
                         "Min", "Max", "Skewness.statistic.",
                         "Kurtosis.statistic.")]
  colnames(descout) <- gsub("\\.statistic\\.", "", colnames(descout))
  descout <- data.frame(t(descout))
  descout <- cbind(Statistic = rownames(descout), descout)
  rownames(descout) <- NULL
  colnames(descout) <- c("Statistic", "Value")

  addWorksheet(wb, sheetName = "Descriptive Statistics", gridLines = FALSE)
  writeDataTable(wb, sheet = "Descriptive Statistics",
                 x = descout,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "Descriptive Statistics", style = numstyle,
           rows = 2:7, cols = 2, stack = FALSE, gridExpand = TRUE)
  addStyle(wb,  sheet = "Descriptive Statistics",
           style = if(descout_sub$Skewness.p.value. <= 0.01) {
             dsstyle } else {
               if (descout_sub$Skewness.p.value. <= 0.05) {
                 ssstyle
               } else {
                 nsstyle
               }
             },
           rows = 8, cols = 2, stack = FALSE)
  addStyle(wb,  sheet = "Descriptive Statistics",
           style = if(descout_sub$Kurtosis.p.value. <= 0.01) {
             dsstyle } else {
               if (descout_sub$Kurtosis.p.value. <= 0.05) {
                 ssstyle
               } else {
                 nsstyle
               }
             },
           rows = 9, cols = 2, stack = FALSE)
  addStyle(wb,  sheet = "Descriptive Statistics",
           style = createStyle(halign = "right"),
           rows = 1, cols = 2, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "Descriptive Statistics",
               cols = 1:ncol(descout), widths = "auto")
  writeData(wb, sheet = "Descriptive Statistics", xy = c("A", 10),
            x = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
            borders = "none")

  # GVA
  gvaout <- data.frame(gva.augmentedRCBD(aug))
  gvaout <- data.frame(t(gvaout))
  gvaout <- cbind(Statistic = rownames(gvaout), gvaout)
  rownames(gvaout) <- NULL
  colnames(gvaout) <- c("Statistic", "Value")

  stat_cat <- c("GCV.category", "PCV.category", "hBS.category", "GAM.category")
  gvaout_sub <- gvaout[gvaout$Statistic %in% stat_cat, ]
  gvaout[gvaout$Statistic %in% stat_cat, ]$Value <- "0"
  gvaout$Value <- as.numeric(gvaout$Value)

  addWorksheet(wb, sheetName = "Genetic Variability Analysis",
               gridLines = FALSE)
  writeDataTable(wb, sheet = "Genetic Variability Analysis",
                 x = gvaout,
                 colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                 tableStyle = "TableStyleLight1", withFilter = FALSE,
                 bandedRows = FALSE)
  addStyle(wb,  sheet = "Genetic Variability Analysis", style = numstyle,
           rows = c(2:6, 8, 10:11, 13:14), cols = 2,
           stack = FALSE, gridExpand = TRUE)
  writeData(wb, sheet = "Genetic Variability Analysis",
            x = gvaout_sub[gvaout_sub == "GCV.category", ]$Value,
            xy = c("B", 7), borders = "none")
  writeData(wb, sheet = "Genetic Variability Analysis",
            x = gvaout_sub[gvaout_sub == "PCV.category", ]$Value,
            xy = c("B", 9), borders = "none")
  writeData(wb, sheet = "Genetic Variability Analysis",
            x = gvaout_sub[gvaout_sub == "hBS.category", ]$Value,
            xy = c("B", 12), borders = "none")
  writeData(wb, sheet = "Genetic Variability Analysis",
            x = gvaout_sub[gvaout_sub == "GAM.category", ]$Value,
            xy = c("B", 15), borders = "none")
  addStyle(wb,  sheet = "Genetic Variability Analysis",
           style = createStyle(halign = "right"),
           rows = 1:15, cols = 2, stack = TRUE, gridExpand = TRUE)
  setColWidths(wb, sheet = "Genetic Variability Analysis",
               cols = 1:ncol(gvaout), widths = "auto")

  # Comparisons
  if (!is.null(aug$Comparisons)) {

    cmp <- aug$Comparisons

    addWorksheet(wb, sheetName = "Comparisons",
                 gridLines = FALSE)
    writeData(wb, sheet = "Comparisons", xy = c("A", 1),
              x = paste("Comparison method:",
                        aug$`Comparison method`), borders = "none")
    writeDataTable(wb, sheet = "Comparisons",
                   x = cmp, xy = c("A", 2),
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", bandedRows = FALSE)
    addStyle(wb,  sheet = "Comparisons", style = numstyle,
             rows = 3:(nrow(cmp) + 2), cols = c(2:3, 5:6),
             stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "Comparisons", style = createStyle(halign = "right"),
             rows = 2, cols = 2:6, stack = TRUE, gridExpand = TRUE)
    writeData(wb, sheet = "Comparisons", xy = c("A", nrow(cmp) + 3),
              x = "* P \u2264 0.05; ** P \u2264 0.01", borders = "none")
  }

  # Groups
  if (!is.null(aug$Groups)) {
    addWorksheet(wb, sheetName = "Groups",
                 gridLines = FALSE)
    writeData(wb, sheet = "Groups", xy = c("A", 1),
              x = paste("Comparison method:",
                        aug$`Comparison method`), borders = "none")
    gps <- aug$Groups

    writeDataTable(wb, sheet = "Groups",
                   x = gps, xy = c("A", 2),
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", bandedRows = FALSE)
    addStyle(wb,  sheet = "Groups", style = numstyle,
             rows = 3:(nrow(gps) + 2), cols = c(2:3, 5:6),
             stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "Groups", style = createStyle(halign = "right"),
             rows = 2, cols = 2:6, stack = TRUE, gridExpand = TRUE)
  }

  saveWorkbook(wb = wb, file = target,
               overwrite = TRUE)
  }

  message(paste("File created at", target))

}
