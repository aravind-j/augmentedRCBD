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

#' Generate MS Word Report from \code{augmentedRCBD.bulk} Output
#'
#' \code{report.augmentedRCBD.bulk} generates a tidy report from an object of
#' class \code{augmentedRCBD.bulk} as docx MS word file using the
#' \code{\link[officer]{officer}} package.
#'
#' @param aug.bulk An object of class \code{augmentedRCBD.bulk}.
#' @param target The path to the docx file to be created.
#'
#' @export
#' @import officer
#' @import flextable
#' @import ggplot2
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom methods is
#' @importFrom stats qnorm
#' @importFrom graphics plot
#'
#' @seealso \code{\link[officer]{officer}}, \code{\link[flextable]{flextable}}
#'
#' @examples
#' # Example data
#' blk <- c(rep(1,7),rep(2,6),rep(3,7))
#' trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
#'
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
#'         70, 75, 74)
#' y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
#'         240, 268, 287, 226, 395, 450)
#' dataf <- data.frame(blk, trt, y1, y2)
#'
#' bout <- augmentedRCBD.bulk(data = dataf, block = "blk",
#'                            treatment = "trt", traits = c("y1", "y2"),
#'                            checks = NULL, alpha = 0.05, describe = TRUE,
#'                            freqdist = TRUE, gva = TRUE,
#'                            check.col = c("brown", "darkcyan",
#'                                          "forestgreen", "purple"),
#'                            console = FALSE)
#'
#' \donttest{
#' report.augmentedRCBD.bulk(bout, file.path(tempdir(),
#'                           "augmentedRCBD bulk output.docx"))
#' }
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.bulk}}
#'
#'
report.augmentedRCBD.bulk <- function(aug.bulk, target,
                                      file.type = c("word", "excel")){

  if (!is(aug.bulk, "augmentedRCBD.bulk")) {
    stop('"aug.bulk" is not of class "augmentedRCBD.bulk"')
  }

  file.type <- match.arg(file.type)

  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

  if (file.type == "word") {
    if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)) {
      stop(target, " should have '.docx' extension.")
    }

    augreport <- read_docx(file.path(system.file(package = "augmentedRCBD"),
                                     "template.docx"))

    augreport <- body_add_par(augreport, value = "augmentedRCBD", style = "Title")
    augreport <- body_add_toc(augreport, level = 2)

    traits <- aug.bulk$Details$Traits
    ntraits <- aug.bulk$Details$`Number of Traits`

    # Details
    augreport <- body_add_par(augreport, value = "Details", style = "heading 1")

    Details <- t(data.frame(`Number of blocks` = aug.bulk$Details$`Number of blocks`,
                            `Number of treatments` = aug.bulk$Details$`Number of treatments`,
                            `Number of check treatments` = aug.bulk$Details$`Number of check treatments`,
                            `Number of test treatments` = aug.bulk$Details$`Number of test treatments`,
                            `Check treatments` =  paste(aug.bulk$Details$`Check treatments`, collapse = ", "),
                            `Number of Traits` = aug.bulk$Details$`Number of Traits`,
                            `Traits` = paste(aug.bulk$Details$Traits, collapse = ", ")))
    Details <- data.frame(Details)
    Details <- cbind(gsub("\\.", " ", rownames(Details)), Details)
    colnames(Details) <- c("Item", "Details")

    Details <- regulartable(data = data.frame(Details))
    Details <- autofit(Details)
    augreport <- body_add_flextable(augreport, Details)

    merge_mss_sig <- function(i) {
      paste(round.conditional(anovatable[, paste(traits[i], "_Mean.Sq",
                                              sep = "")],
                              digits = round.digits),
            stringi::stri_pad_right(anovatable[, paste(traits[i], "_sig",
                                                    sep = "")], 3))
    }

    # ANOVA, TA
    augreport <- body_add_par(augreport, value = "ANOVA, Treatment Adjusted",
                              style = "heading 1")
    anovata <- aug.bulk$`ANOVA, Treatment Adjusted`
    # anovata$Df <- as.character(anovata$Df)
    colnames(anovata) <- make.names(colnames(anovata), unique = TRUE)
    anovatable <- anovata
    anovata[, traits] <- lapply(1:length(traits),
                                merge_mss_sig)
    anovata[, c(paste(traits, "_Mean.Sq", sep = ""),
                paste(traits, "_sig", sep = ""))] <- NULL
    anovatable <- NULL
    anovata <- autofit(flextable(anovata))
    anovata <- bold(anovata, part = "header")
    anovata <- add_header_row(anovata,
                   colwidths = c(1, 1, ntraits),
                   values = c("", "", "Mean.Sq"))
    augreport <- body_add_flextable(augreport, anovata)

    augreport <- body_add_par(augreport,
                              value = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
                              style = "Normal")
    # ANOVA, BA
    augreport <- body_add_par(augreport, value = "ANOVA, Block Adjusted",
                              style = "heading 1")
    anovaba <- aug.bulk$`ANOVA, Block Adjusted`
    colnames(anovaba) <- make.names(colnames(anovaba), unique = TRUE)
    anovatable <- anovaba
    anovaba[, traits] <- lapply(1:length(traits),
                                merge_mss_sig)
    anovaba[, c(paste(traits, "_Mean.Sq", sep = ""),
                paste(traits, "_sig", sep = ""))] <- NULL
    anovatable <- NULL
    anovaba <- autofit(flextable(anovaba))
    anovaba <- bold(anovaba, part = "header")
    anovaba <- add_header_row(anovaba,
                              colwidths = c(1, 1, ntraits),
                              values = c("", "", "Mean.Sq"))
    augreport <- body_add_flextable(augreport, anovaba)

    augreport <- body_add_par(augreport,
                              value = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
                              style = "Normal")

    # Std. error
    augreport <- body_add_par(augreport, value = "Standard Errors",
                              style = "heading 1")
    SE <- aug.bulk$`Std. Errors`
    colnames(SE) <- make.names(colnames(SE), unique = TRUE)
    SE <- autofit(flextable(SE))
    SE <- bold(SE, part = "header")
    augreport <- body_add_flextable(augreport, SE)

    # CD
    augreport <- body_add_par(augreport,
                              value = paste("Critical Difference (",
                                            aug.bulk$alpha * 100, "%)", sep = ""),
                              style = "heading 1")
    CD <- aug.bulk$CD
    colnames(CD) <- make.names(colnames(CD), unique = TRUE)
    CD <- autofit(flextable(CD))
    CD <- bold(CD, part = "header")
    augreport <- body_add_flextable(augreport, CD)

    # CV
    augreport <- body_add_par(augreport, value = "Coefficient of Variance",
                              style = "heading 1")
    CV <- aug.bulk$CV
    CV <- autofit(flextable(CV))
    CV <- bold(CV, part = "header")
    augreport <- body_add_flextable(augreport, CV)

    # Overall adj. mean
    augreport <- body_add_par(augreport, value = "Overall Adjusted Mean",
                              style = "heading 1")
    oadjmean <- aug.bulk$`Overall adjusted mean`
    oadjmean <- autofit(flextable(oadjmean))
    oadjmean <- bold(oadjmean, part = "header")
    augreport <- body_add_flextable(augreport, oadjmean)

    # Check statistics
    augreport <- body_add_par(augreport, value = "Check Statistics",
                              style = "heading 1")
    for (i in seq_along(aug.bulk$`Check statistics`)) {
      augreport <- body_add_par(augreport, value = names(aug.bulk$`Check statistics`)[i],
                                style = "heading 2")
      chkout <- autofit(flextable(aug.bulk$`Check statistics`[[i]]))
      chkout <- bold(chkout, part = "header")
      augreport <- body_add_flextable(augreport, chkout)
    }


    # Descriptive statistics
    if (!is.null(aug.bulk$`Descriptive statistics`)){
      augreport <- body_add_par(augreport, value = "Descriptive Statistics",
                                style = "heading 1")
      descout <- aug.bulk$`Descriptive statistics`
      descout <- autofit(flextable(descout))
      descout <- bold(descout, part = "header")
      augreport <- body_add_flextable(augreport, descout)

      augreport <- body_add_par(augreport,
                                value = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
                                style = "Normal")
    }

    # Frequency distribution
    if (!is.null(aug.bulk$`Frequency distribution`)) {
      augreport <- body_add_par(augreport, value = "Frequency Distribution",
                                style = "heading 1")

      for (i in 1:aug.bulk$Details$`Number of Traits`) {
        augreport <- body_add_par(augreport, value = aug.bulk$Details$Traits[i],
                                  style = "heading 2")
        src <- tempfile(fileext = ".png")
        png(filename = src, width = 6, height = 4, units = 'in', res = 300)
        plot(aug.bulk$`Frequency distribution`[[aug.bulk$Details$Traits[i]]])
        dev.off()
        augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
        rm(src)
      }
    }

    # GVA
    if (!is.null(aug.bulk$`Genetic variability analysis`)) {
      augreport <- body_add_par(augreport, value = "Genetic Variability Analysis",
                                style = "heading 1")
      GVA <- aug.bulk$`Genetic variability analysis`
      GVA <- autofit(flextable(GVA))
      GVA <- bold(GVA, part = "header")
      augreport <- body_add_flextable(augreport, GVA)
    }

    # GVA plots
    if (any(unlist(lapply(aug.bulk$`GVA plots`, function(x) !is.null(x))))) {

      augreport <- body_add_par(augreport, value = "Genetic Variablity Analysis Plots",
                                style = "heading 1")

      if (!is.null(aug.bulk$`GVA plots`$`Phenotypic and Genotypic CV`)) {
        augreport <- body_add_par(augreport, value = "Phenotypic and Genotypic Coefficient of Variability",
                                  style = "heading 2")

        src <- tempfile(fileext = ".png")
        png(filename = src, width = 6, height = 4, units = 'in', res = 300)
        plot(aug.bulk$`GVA plots`$`Phenotypic and Genotypic CV`)
        dev.off()
        augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
        rm(src)
      }

      if (!is.null(aug.bulk$`GVA plots`$`Broad sense heritability`)) {
        augreport <- body_add_par(augreport, value = "Broad Sense Heritability",
                                  style = "heading 2")

        src <- tempfile(fileext = ".png")
        png(filename = src, width = 6, height = 4, units = 'in', res = 300)
        plot(aug.bulk$`GVA plots`$`Broad sense heritability`)
        dev.off()
        augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
        rm(src)
      }

      if (!is.null(aug.bulk$`GVA plots`$`Genetic advance over mean`)) {
        augreport <- body_add_par(augreport, value = "Genetic Advance Over Mean",
                                  style = "heading 2")

        src <- tempfile(fileext = ".png")
        png(filename = src, width = 6, height = 4, units = 'in', res = 300)
        plot(aug.bulk$`GVA plots`$`Genetic advance over mean`)
        dev.off()
        augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
        rm(src)
      }
    }

    # Adjusted Means
    augreport <- body_add_par(augreport, value = "Adjusted Means",
                              style = "heading 1")
    adj.means <- aug.bulk$Means
    colnames(adj.means) <- make.names(colnames(adj.means), unique = TRUE)
    adj.means <- autofit(flextable(adj.means))
    adj.means <- bold(adj.means, part = "header")
    augreport <- body_add_flextable(augreport, adj.means)

    # Warnings
    if (!all( unlist(lapply(aug.bulk$warnings, is.null)))) {
      augreport <- body_add_par(augreport, value = "Warnings",
                                style = "heading 1")

      if (!is.null(aug.bulk$warnings$Model)) {
        augreport <- body_add_par(augreport, value = "Model",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$Model)) {
          augreport <- body_add_par(augreport,
                                    value = aug.bulk$warnings$Model[i],
                                    style = "Code")
        }
      }

      if (!is.null(aug.bulk$warnings$`Freq. dist`)) {
        augreport <- body_add_par(augreport, value = "Frequency Distribution",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$`Freq. dist`)) {
          augreport <- body_add_par(augreport,
                                    value = aug.bulk$warnings$`Freq. dist`[i],
                                    style = "Code")
        }
      }

      if (!is.null(aug.bulk$warnings$GVA)) {
        augreport <- body_add_par(augreport, value = "Genetic Variablity Analysis",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$GVA)) {
          augreport <- body_add_par(augreport,
                                    value = aug.bulk$warnings$GVA[i],
                                    style = "Code")
        }
      }
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

    ntraits <- aug.bulk$Details$`Number of Traits`
    traits <- aug.bulk$Details$Traits
    nchecks <- aug.bulk$Details$`Number of check treatments`
    ntreats <- aug.bulk$Details$`Number of treatments`

    # Index
    index <- c("Details", "ANOVA, Treatment Adjusted", "ANOVA, Block Adjusted",
               "Standard Errors", "Critical Difference", "Coefficient of Variation",
               "Overall Adjusted Mean", "Check Statistics",
               "Descriptive Statistics", "Frequency Distribution",
               "Genetic Variability Analysis", "GVA Plots", "Adjusted Means",
               "Warnings")
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
    Details <- t(data.frame(`Number of blocks` = aug.bulk$Details$`Number of blocks`,
                            `Number of treatments` = aug.bulk$Details$`Number of treatments`,
                            `Number of check treatments` = aug.bulk$Details$`Number of check treatments`,
                            `Number of test treatments` = aug.bulk$Details$`Number of test treatments`,
                            `Check treatments` =  paste(aug.bulk$Details$`Check treatments`, collapse = ", "),
                            `Number of Traits` = aug.bulk$Details$`Number of Traits`,
                            `Traits` = paste(aug.bulk$Details$Traits, collapse = ", ")))
    Details <- data.frame(Details)
    Details <- cbind(gsub("\\.", " ", rownames(Details)), Details)
    colnames(Details) <- c("Item", "Details")

    addWorksheet(wb, sheetName = "Details", gridLines = FALSE)
    writeDataTable(wb, sheet = "Details", x = Details,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    setColWidths(wb, sheet = "Details", cols = 1:ncol(Details), widths = "auto")

    # ANOVA, TA
    anovata <- aug.bulk$`ANOVA, Treatment Adjusted`
    colnames(anovata) <- make.names(colnames(anovata), unique = TRUE)

    addWorksheet(wb, sheetName = "ANOVA, Treatment Adjusted", gridLines = FALSE)
    writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted", x = anovata,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(numFmt = "0"),
             rows = 2:6, cols = 2, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted",
                 cols = 1:ncol(anovata), widths = "auto")
    writeData(wb, sheet = "ANOVA, Treatment Adjusted", xy = c("A", 7),
              x = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")

    # ANOVA, BA
    anovaba <- aug.bulk$`ANOVA, Block Adjusted`
    colnames(anovaba) <- make.names(colnames(anovaba), unique = TRUE)

    addWorksheet(wb, sheetName = "ANOVA, Block Adjusted", gridLines = FALSE)
    writeDataTable(wb, sheet = "ANOVA, Block Adjusted", x = anovaba,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(numFmt = "0"),
             rows = 2:7, cols = 2, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "ANOVA, Block Adjusted",
                 cols = 1:ncol(anovaba), widths = "auto")
    writeData(wb, sheet = "ANOVA, Block Adjusted", xy = c("A", 8),
              x = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")

    # Std. Error
    SE <- aug.bulk$`Std. Errors`
    SE[, traits] <- lapply(SE[, traits], as.numeric)
    colnames(SE) <- make.names(colnames(SE), unique = TRUE)

    addWorksheet(wb, sheetName = "Standard Errors", gridLines = FALSE)
    writeDataTable(wb, sheet = "Standard Errors", x = SE,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Standard Errors", style = numstyle,
             rows = 2:5, cols = 2:(ntraits + 1), stack = FALSE,
             gridExpand = TRUE)
    addStyle(wb,  sheet = "Standard Errors",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2:(ntraits + 1), stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Standard Errors",
                 cols = 1:ncol(SE), widths = "auto")

    # CD
    CD <- aug.bulk$CD
    CD[, traits] <- lapply(CD[, traits], as.numeric)
    colnames(CD) <- make.names(colnames(CD), unique = TRUE)

    addWorksheet(wb, sheetName = "Critical Difference", gridLines = FALSE)
    writeDataTable(wb, sheet = "Critical Difference", x = CD,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Critical Difference", style = numstyle,
             rows = 2:5, cols = 2:(ntraits + 1), stack = FALSE,
             gridExpand = TRUE)
    addStyle(wb,  sheet = "Critical Difference",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2:(ntraits + 1), stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Critical Difference",
                 cols = 1:ncol(CD), widths = "auto")

    # CV
    CV <- aug.bulk$CV
    CV$CV <- as.numeric(CV$CV)
    colnames(CV) <- make.names(colnames(CV), unique = TRUE)

    addWorksheet(wb, sheetName = "Coefficient of Variance", gridLines = FALSE)
    writeDataTable(wb, sheet = "Coefficient of Variance", x = CV,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Coefficient of Variance", style = numstyle,
             rows = 2:(ntraits + 1), cols = 2, stack = FALSE,
             gridExpand = TRUE)
    addStyle(wb,  sheet = "Coefficient of Variance",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Coefficient of Variance",
                 cols = 1:ncol(CV), widths = "auto")

    # Overall adj. mean
    oadjmean <- aug.bulk$`Overall adjusted mean`
    oadjmean$Overall.adjusted.mean <- as.numeric(oadjmean$Overall.adjusted.mean)
    colnames(oadjmean) <- make.names(colnames(oadjmean), unique = TRUE)

    addWorksheet(wb, sheetName = "Overall Adjusted Mean", gridLines = FALSE)
    writeDataTable(wb, sheet = "Overall Adjusted Mean", x = oadjmean,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Overall Adjusted Mean", style = numstyle,
             rows = 2:(ntraits + 1), cols = 2, stack = FALSE,
             gridExpand = TRUE)
    addStyle(wb,  sheet = "Overall Adjusted Mean",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Overall Adjusted Mean",
                 cols = 1:ncol(oadjmean), widths = "auto")


    # Check statistics
    addWorksheet(wb, sheetName = "Check Statistics", gridLines = FALSE)
    indexdf <- data.frame(i = sort(rep(1:ntraits, nchecks+3)),
                          rows  = rep(1:(nchecks+3), ntraits))
    indexdf$index <- seq_along(indexdf$i)

    for (i in seq_along(aug.bulk$`Check statistics`)) {
      row1 <- indexdf[indexdf$i == i & indexdf$rows == 1, ]$index
      writeData(wb, sheet = "Check Statistics",
                x = names(aug.bulk$`Check statistics`)[i],
                startCol = "A", startRow = row1, borders = "none")
      writeDataTable(wb, sheet = "Check Statistics",
                     x = aug.bulk$`Check statistics`[[i]],
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE, startCol = "A",
                     startRow = row1 + 1)
      addStyle(wb,  sheet = "Check Statistics", style = numstyle,
               rows = (row1 + 1):(row1 + nchecks + 1), cols = 3:4,
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Check Statistics",
               style = createStyle(halign = "right"),
               rows = row1 + 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
      rm(row1)
    }
    setColWidths(wb, sheet = "Check Statistics",
                 cols = 1:ncol(aug.bulk$`Check statistics`[[1]]),
                 widths = "auto")
    rm(indexdf)

    # Descriptive statistics
    if (!is.null(aug.bulk$`Descriptive statistics`)){
      descout <- aug.bulk$`Descriptive statistics`

      addWorksheet(wb, sheetName = "Descriptive Statistics", gridLines = FALSE)
      writeDataTable(wb, sheet = "Descriptive Statistics", x = descout,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      addStyle(wb,  sheet = "Descriptive Statistics", style = numstyle,
               rows = 2:(ntraits + 1), cols = 2, stack = FALSE,
               gridExpand = TRUE)
      addStyle(wb,  sheet = "Descriptive Statistics",
               style = createStyle(halign = "right"),
               rows = 1, cols = 3:7, stack = TRUE, gridExpand = TRUE)
      setColWidths(wb, sheet = "Descriptive Statistics",
                   cols = 1:ncol(descout), widths = "auto")
      writeData(wb, sheet = "Descriptive Statistics", xy = c("A", ntraits + 2),
                x = "ns P > 0.05; * P <= 0.05; ** P <= 0.01",
                borders = "none")
    }

    # Frequency distribution
    if (!is.null(aug.bulk$`Frequency distribution`)) {
      addWorksheet(wb, sheetName = "Frequency Distribution", gridLines = FALSE)
      indexdf <- data.frame(i = sort(rep(1:ntraits, 25)),
                            rows  = rep(1:(25), ntraits))
      indexdf$index <- seq_along(indexdf$i)

      for (i in seq_along(aug.bulk$`Frequency distribution`)) {
        row1 <- indexdf[indexdf$i == i & indexdf$rows == 1, ]$index
        writeData(wb, sheet = "Frequency Distribution",
                  x = names(aug.bulk$`Frequency distribution`)[i],
                  startCol = "A", startRow = row1, borders = "none")
        plot(aug.bulk$`Frequency distribution`[[i]])
        insertPlot(wb, sheet = "Frequency Distribution",
                   xy = c("B", row1))
        dev.off()
        rm(row1)
      }
      setColWidths(wb, sheet = "Frequency Distribution",
                   cols = 1, widths = "auto")
      rm(indexdf)
    }

    # GVA
    if (!is.null(aug.bulk$`Genetic variability analysis`)) {
      GVA <- aug.bulk$`Genetic variability analysis`

      addWorksheet(wb, sheetName = "Genetic Variability Analysis",
                   gridLines = FALSE)
      writeDataTable(wb, sheet = "Genetic Variability Analysis", x = GVA,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      addStyle(wb,  sheet = "Genetic Variability Analysis", style = numstyle,
               rows = 2:(ntraits + 1), cols = c(2:6, 8, 10:11, 13:14),
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Genetic Variability Analysis",
               style = createStyle(halign = "right"),
               rows = 1, cols = c(2:6, 8, 10:11, 13:14), stack = TRUE,
               gridExpand = TRUE)
      setColWidths(wb, sheet = "Genetic Variability Analysis",
                   cols = 1:ncol(GVA), widths = "auto")
    }

    # GVA plots
    if (any(unlist(lapply(aug.bulk$`GVA plots`, function(x) !is.null(x))))) {

      addWorksheet(wb, sheetName = "GVA Plots", gridLines = FALSE)

      if (!is.null(aug.bulk$`GVA plots`$`Phenotypic and Genotypic CV`)) {
        writeData(wb, sheet = "GVA Plots",
                  x = "Phenotypic and Genotypic Coefficient of Variability",
                  startCol = "A", startRow = 1, borders = "none")
        plot(aug.bulk$`GVA plots`$`Phenotypic and Genotypic CV`)
        insertPlot(wb, sheet = "GVA Plots",
                   xy = c("A", 2))
        dev.off()
      }

      if (!is.null(aug.bulk$`GVA plots`$`Broad sense heritability`)) {
        writeData(wb, sheet = "GVA Plots",
                  x = "Broad Sense Heritability",
                  startCol = "A", startRow = 26, borders = "none")
        plot(aug.bulk$`GVA plots`$`Broad sense heritability`)
        insertPlot(wb, sheet = "GVA Plots",
                   xy = c("A", 27))
        dev.off()
      }

      if (!is.null(aug.bulk$`GVA plots`$`Genetic advance over mean`)) {
        writeData(wb, sheet = "GVA Plots",
                  x = "Genetic Advance Over Mean",
                  startCol = "A", startRow = 51, borders = "none")
        plot(aug.bulk$`GVA plots`$`Genetic advance over mean`)
        insertPlot(wb, sheet = "GVA Plots",
                   xy = c("A", 52))
        dev.off()
      }
    }

    # Adjusted Means
    adj.means <- aug.bulk$Means
    colnames(adj.means) <- make.names(colnames(adj.means), unique = TRUE)

    addWorksheet(wb, sheetName = "Adjusted Means", gridLines = FALSE)
    writeDataTable(wb, sheet = "Adjusted Means", x = adj.means,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Adjusted Means", style = numstyle,
             rows = 2:(ntreats + 1), cols = 2:(ntraits + 1), stack = FALSE,
             gridExpand = TRUE)
    addStyle(wb,  sheet = "Adjusted Means",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2:(ntraits + 1), stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Adjusted Means",
                 cols = 1:ncol(adj.means), widths = "auto")

    # Warnings
    if (!all( unlist(lapply(aug.bulk$warnings, is.null)))) {
      addWorksheet(wb, sheetName = "Warnings", gridLines = FALSE)

      row1 <- 1

      if (!is.null(aug.bulk$warnings$Model)) {
        writeData(wb, sheet = "Warnings", x = "Model",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = aug.bulk$warnings$Model,
                  startCol = "A", row1 + 1, borders = "none")
        row1 <- row1 + 2 + length(aug.bulk$warnings$Model)
      }

      if (!is.null(aug.bulk$warnings$`Freq. dist`)) {
        writeData(wb, sheet = "Warnings", x = "Frequency Distribution",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = aug.bulk$warnings$`Freq. dist`,
                  startCol = "A", row1 + 1, borders = "none")
        row1 <- row1 + 2 + length(aug.bulk$warnings$`Freq. dist`)
      }

      if (!is.null(aug.bulk$warnings$GVA)) {
        writeData(wb, sheet = "Warnings", x = "Genetic Variablity Analysis",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = aug.bulk$warnings$GVA,
                  startCol = "A", row1 + 1, borders = "none")
      }
    }

    saveWorkbook(wb = wb, file = target, overwrite = TRUE)

  }

  message(paste("File created at", target))

}
