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

#' Generate MS Word or Excel Report from \code{augmentedRCBD.bulk} Output
#'
#' \code{report.augmentedRCBD.bulk} generates a tidy report from an object of
#' class \code{augmentedRCBD.bulk} as docx MS word file using the
#' \code{\link[officer]{officer}} package or xlsx MS excel file using the
#' \code{\link[openxlsx]{openxlsx}} package.
#'
#' @param aug.bulk An object of class \code{augmentedRCBD.bulk}.
#' @param target The path to the docx file to be created.
#' @param file.type The file type of the report. From v0.2.0, only
#'   \code{"excel"} (MS Excel report) is supported. Generation of MS Word
#'   reports is no longer supported.
#'
#' @export
#' @import officer
#' @import flextable
#' @import ggplot2
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom grDevices dev.new
#' @importFrom methods is
#' @importFrom stats qnorm
#' @importFrom graphics plot
#' @importFrom utils capture.output citation stack
#'
#' @note The raw values in the \code{augmentedRCBD} object are retained with
#'   full precision. They are only formatted to display 2 decimal places in the
#'   Excel report.
#'
#'   If values such as adjusted means are required for downstream analysis,
#'   export the raw values directly from R or use the Excel report, which
#'   preserves the underlying values.
#'
#'   The default number of displayed decimal places can be changed using the
#'   \code{augmentedRCBD.round.digits} global option. For example,
#'   \code{setOption(augmentedRCBD.round.digits = 3)} sets the number of decimal
#'   places to 3.
#'
#'   Values are not rounded to zero; instead, they are rounded to the nearest
#'   decimal place specified by \code{augmentedRCBD.round.digits}. F value, t
#'   ratio and p values are not rounded to less than 3 decimal places.
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
#' report.augmentedRCBD.bulk(
#'   aug.bulk = bout,
#'   target = file.path(tempdir(),
#'                      "augmentedRCBD bulk output.xlsx"),
#'   file.type = "excel")
#' }
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.bulk}}
#'
#'
report.augmentedRCBD.bulk <- function(aug.bulk, target,
                                      file.type = c("excel")){

  # Checks ----
  if (!is(aug.bulk, "augmentedRCBD.bulk")) {
    stop('"aug.bulk" is not of class "augmentedRCBD.bulk".')
  }

  file.type <- match.arg(file.type)

  # Settings ----
  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

  # Prepare warnings ----
  wstring1 <- "test treatment(s) are replicated"
  wstring2 <- "Negative adjusted means were generated for the following"

  if (!is.null(aug.bulk$warnings$`Missing values`)) {
    misswarn_list <-  aug.bulk$warnings$`Missing values`
  }

  if (any(grepl(wstring1, aug.bulk$warnings$Model, fixed = TRUE))) {
    modwarn1_list <-
      lapply(aug.bulk$warnings$Model, function(modwarn) {
        modwarn[grepl(wstring1, modwarn, fixed = TRUE)]
      })
    modwarn1_list <- modwarn1_list[lapply(modwarn1_list, length) > 0]
    modwarn1_list <- lapply(modwarn1_list, function(x) {
      gsub(pattern = "test treatment(s) are replicated.",
           replacement = "test treatment(s) are replicated:",
           x = x, fixed = T)
    })
  }

  if (any(grepl(wstring2, aug.bulk$warnings$Model, fixed = TRUE))) {
    modwarn2_list <-
      lapply(aug.bulk$warnings$Model, function(modwarn) {
        modwarn[grepl(wstring2, modwarn, fixed = TRUE)]
      })
    modwarn2_list <- modwarn2_list[lapply(modwarn2_list, length) > 0]
    modwarn2_list <- lapply(modwarn2_list, function(x) {
      gsub(pattern = "following treatment(s)",
           replacement = "following treatment(s):",
           x = x, fixed = T)
    })
  }

  if (exists("misswarn_list") & !exists("modwarn1_list")) {
    detwarn_list <- misswarn_list
  }

  if (!exists("misswarn_list") & exists("modwarn1_list")) {
    detwarn_list <- modwarn1_list
  }

  if (exists("misswarn_list") & exists("modwarn1_list")) {
    detwarn_list <- c(misswarn_list, modwarn1_list)
    detwarn_list <- tapply(detwarn_list, names(detwarn_list),
                           function(x) unlist(x, FALSE, FALSE))
  }

  # WORD ----

  if (file.type == "word") {
    stop('The "word" file type is no longer supported. ',
         'Use file.type = "excel" instead.')
  }

  # EXCEL ----

  if (file.type == "excel") {
    if (!grepl(x = target, pattern = "\\.(xlsx)$", ignore.case = TRUE)) {
      stop(target, " should have '.xlsx' extension.")
    }

    # Create workbook
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

    hs <- createStyle(halign = "left", valign = "bottom")

    num.base <- "0.00"
    numstyle <- createStyle(numFmt = num.base)
    ssstyle <- createStyle(numFmt = paste(num.base, '"*"'))
    dsstyle <- createStyle(numFmt = paste(num.base, '"**"'))
    nsstyle <- createStyle(numFmt = paste(num.base, '"\u207f\u02e2"'))
    csstyle <- createStyle(numFmt = paste(num.base, '"\u2020"'))
    padstyle <- createStyle(numFmt = paste(num.base, '"\u00A0""\u00A0"'))

    ntraits <- aug.bulk$Details$`Number of Traits`
    traits <- aug.bulk$Details$Traits
    nchecks <- aug.bulk$Details$`Number of check treatments`
    ntreats <- aug.bulk$Details$`Number of treatments`

    ## Index ----
    index <- c("Details", "ANOVA, Treatment Adjusted",
               "ANOVA, Block Adjusted",
               "Standard Errors", "Critical Difference",
               "Coefficient of Variation", "Overall Adjusted Mean",
               "Check Statistics")
    if (!is.null(aug.bulk$`Descriptive statistics`)) {
      index <- c(index,"Descriptive Statistics")
    }
    if (!is.null(aug.bulk$`Frequency distribution`)) {
      index <- c(index,"Frequency Distribution")
    }
    if (!is.null(aug.bulk$GVA)) {
      index <- c(index,"Genetic Variability Analysis", "GVA Plots")
    }
    index <- c(index, "Adjusted Means", "Warnings")
    index <- data.frame(`Sl.No` = seq_along(index),
                        Sheets = index)

    addWorksheet(wb, sheetName = "Index", gridLines = FALSE)
    insertImage(wb, sheet = "Index",
                file = system.file("extdata", "augmentedRCBD.png",
                                   package = "augmentedRCBD"),
                startCol = "A", startRow = 2,
                width = 1, height = 1.1)
    writeData(wb, sheet = "Index",
              x = "https://aravind-j.github.io/augmentedRCBD",
              startCol = "C", startRow = 4, borders = "none")
    writeData(wb, sheet = "Index",
              x = "https://github.com/aravind-j/augmentedRCBD",
              startCol = "C", startRow = 5, borders = "none")
    writeData(wb, sheet = "Index",
              x = "https://CRAN.R-project.org/package=augmentedRCBD",
              startCol = "C", startRow = 6, borders = "none")
    writeDataTable(wb, sheet = "Index", x = index,
                   startCol = "B", startRow = 9, colNames = TRUE,
                   rowNames = FALSE,
                   headerStyle = hs, tableStyle = "TableStyleLight1",
                   withFilter = FALSE, bandedRows = FALSE)
    addStyle(wb,  sheet = "Index", style = createStyle(halign = "right"),
             rows = 9, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Index", cols = 1:3, widths = "auto")

    citout <- capture.output(citation("augmentedRCBD"))
    writeData(wb, sheet = "Index", x = citout,
              startCol = "B", startRow = 25, borders = "none")
    setColWidths(wb, sheet = "Index", cols = 2, widths = 5)

    traits <- aug.bulk$Details$Trait
    ntraits <- length(traits)

    ## Details ----
    Details <- aug.bulk$Details

    addWorksheet(wb, sheetName = "Details", gridLines = FALSE)
    writeDataTable(wb, sheet = "Details", x = Details,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    setColWidths(wb, sheet = "Details", cols = 1:ncol(Details),
                 widths = "auto")

    setColWidths(wb, sheet = "Details", cols = 1,
                 widths = max(nchar(Details$Trait)) + 5)

    if (exists("detwarn_list")) {
      warn <- lapply(seq_along(detwarn_list), function(i) {
        c(paste("<", names(detwarn_list)[[i]], ">", sep = ""),
          detwarn_list[[i]], "")
      })
      warn <- unlist(warn)

      row1 <- nrow(Details) + 3
      writeData(wb, sheet = "Details", x = warn,
                startCol = "A", startRow = row1,
                borders = "none", colNames = FALSE)
      addStyle(wb,  sheet = "Details",
               style = createStyle(fontColour  = "#C00000", wrapText = FALSE,
                                   halign = "left", valign = "top"),
               rows = row1 + seq_along(warn) - 1, cols = 1,
               stack = FALSE, gridExpand = TRUE)

      for (i in row1 + seq_along(warn) - 1) {
        mergeCells(wb = wb, sheet = "Details", cols = 1:6, rows = i)
      }
    }

    anova_warn <- NULL
    if (!is.null(aug.bulk$warnings$Model)) {
      wstring_ind <- lapply(aug.bulk$warnings$Model, function(x) {
        grepl(wstring1, x, fixed = TRUE) | grepl(wstring2, x, fixed = TRUE)
      })
      if (any(!(unlist(wstring_ind)))) {
        anova_warn <-
          lapply(seq_along(aug.bulk$warnings$Model), function(i) {
            aug.bulk$warnings$Model[[i]][-which(wstring_ind[[i]])]
          })
        names(anova_warn) <- names(aug.bulk$warnings$Model)
        anova_warn <- anova_warn[lapply(anova_warn, length) > 0]

        anova_warn <- lapply(seq_along(anova_warn), function(i) {
          c(paste("<", names(anova_warn)[[i]], ">", sep = ""),
            anova_warn[[i]], "")
        })
        anova_warn <- unlist(anova_warn)
      }
    }

    ## ANOVA, TA ----
    anovata <- aug.bulk$`ANOVA, Treatment Adjusted`
    anovata <- anovata[, setdiff(colnames(anovata),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    anovata_sig <- anovata[, c("Source",
                               paste(traits, "_sig", sep = ""))]
    colnames(anovata_sig) <- gsub("_sig", "", colnames(anovata_sig))
    anovata_msq <- anovata[, c("Source",
                               paste(traits, "_Mean.Sq", sep = ""))]
    colnames(anovata_msq) <- gsub("_Mean.Sq", "", colnames(anovata_msq))
    anovata_df <- anovata[, c("Source",
                              paste(traits, "_Df", sep = ""))]
    colnames(anovata_df) <- gsub("_Df", "", colnames(anovata_df))


    addWorksheet(wb, sheetName = "ANOVA, Treatment Adjusted",
                 gridLines = FALSE)

    strtcol = 2

    writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted",
                   x = data.frame(Source = anovata_df[, "Source"]),
                   startRow = 2, startCol = 1,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)

    for (k in seq_along(traits)) {
      writeData(wb, sheet = "ANOVA, Treatment Adjusted",
                x = traits[k], startCol = strtcol, startRow = 1)
      addStyle(wb, sheet = "ANOVA, Treatment Adjusted",
               cols = strtcol, rows = 1,
               style = createStyle(textDecoration = "bold", halign = "center",
                                   valign = "bottom", wrapText = TRUE))
      mergeCells(wb, sheet = "ANOVA, Treatment Adjusted",
                 cols = c(strtcol, strtcol + 1), rows = 1)
      writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted",
                     x = data.frame(Df = anovata_df[, traits[k]]),
                     startRow = 2, startCol = strtcol,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted",
                     x = data.frame(Mean.Sq = anovata_msq[, traits[k]]),
                     startRow = 2, startCol = strtcol + 1,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
               style = createStyle(numFmt = "0"),
               rows = 3:7, cols = strtcol + 1,
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
               style = createStyle(halign = "right"),
               rows = 2, cols = c(strtcol, strtcol + 1),
               stack = TRUE, gridExpand = TRUE)
      for (i in 1:4) {
        col <- which(colnames(anovata_sig) == traits[k])
        if (anovata_sig[i, col] == "*") {
          addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = ssstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
        if (anovata_sig[i, col] == "**") {
          addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = dsstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
        if (anovata_sig[i, col] == "ns") {
          addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = nsstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
      }
      addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = padstyle,
               rows = 7, cols = strtcol + 1, stack = FALSE)
      strtcol <- strtcol + 2
    }
    rm(strtcol)
    setRowHeights(wb, sheet = "ANOVA, Treatment Adjusted",
                  rows = 1, heights = 50)
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted",
                 cols = 1:1 + (ntraits * 2), widths = "auto")
    writeData(wb, sheet = "ANOVA, Treatment Adjusted", xy = c("A", 8),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted", cols = 1,
                 widths = max(nchar(anovata$Source)) + 5)

    if (!is.null(anova_warn)) {
      row1 <- 10
      writeData(wb, sheet = "ANOVA, Treatment Adjusted", x = anova_warn,
                startCol = "A", startRow = row1,
                borders = "none", colNames = FALSE)
      addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
               style = createStyle(fontColour  = "#C00000", wrapText = FALSE,
                                   halign = "left", valign = "top"),
               rows = row1 + seq_along(anova_warn) - 1, cols = 1,
               stack = FALSE, gridExpand = TRUE)
    }


    ## ANOVA, BA ----
    anovaba <- aug.bulk$`ANOVA, Block Adjusted`
    anovaba <- anovaba[, setdiff(colnames(anovaba),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    anovaba_sig <- anovaba[, c("Source",
                               paste(traits, "_sig", sep = ""))]
    colnames(anovaba_sig) <- gsub("_sig", "", colnames(anovaba_sig))
    anovaba_msq <- anovaba[, c("Source",
                               paste(traits, "_Mean.Sq", sep = ""))]
    colnames(anovaba_msq) <- gsub("_Mean.Sq", "", colnames(anovaba_msq))
    anovaba_df <- anovaba[, c("Source",
                              paste(traits, "_Df", sep = ""))]
    colnames(anovaba_df) <- gsub("_Df", "", colnames(anovaba_df))


    addWorksheet(wb, sheetName = "ANOVA, Block Adjusted", gridLines = FALSE)

    strtcol = 2

    writeDataTable(wb, sheet = "ANOVA, Block Adjusted",
                   x = data.frame(Source = anovaba_df[, "Source"]),
                   startRow = 2, startCol = 1,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)

    for (k in seq_along(traits)) {
      writeData(wb, sheet = "ANOVA, Block Adjusted",
                x = traits[k], startCol = strtcol, startRow = 1)
      addStyle(wb, sheet = "ANOVA, Block Adjusted",
               cols = strtcol, rows = 1,
               style = createStyle(textDecoration = "bold", halign = "center",
                                   valign = "bottom", wrapText = TRUE))
      mergeCells(wb, sheet = "ANOVA, Block Adjusted",
                 cols = c(strtcol, strtcol + 1), rows = 1)
      writeDataTable(wb, sheet = "ANOVA, Block Adjusted",
                     x = data.frame(Df = anovaba_df[, traits[k]]),
                     startRow = 2, startCol = strtcol,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      writeDataTable(wb, sheet = "ANOVA, Block Adjusted",
                     x = data.frame(Mean.Sq = anovaba_msq[, traits[k]]),
                     startRow = 2, startCol = strtcol + 1,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      addStyle(wb,  sheet = "ANOVA, Block Adjusted",
               style = createStyle(numFmt = "0"),
               rows = 3:7, cols = strtcol + 1,
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "ANOVA, Block Adjusted",
               style = createStyle(halign = "right"),
               rows = 2, cols = c(strtcol, strtcol + 1),
               stack = TRUE, gridExpand = TRUE)
      for (i in 1:4) {
        col <- which(colnames(anovaba_sig) == traits[k])
        if (anovaba_sig[i, col] == "*") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = ssstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
        if (anovaba_sig[i, col] == "**") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = dsstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
        if (anovaba_sig[i, col] == "ns") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = nsstyle,
                   rows = i + 2, cols = strtcol + 1, stack = FALSE)
        }
      }
      addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = padstyle,
               rows = 7, cols = strtcol + 1, stack = FALSE)
      strtcol <- strtcol + 2
    }
    rm(strtcol)
    setRowHeights(wb, sheet = "ANOVA, Block Adjusted",
                  rows = 1, heights = 50)
    setColWidths(wb, sheet = "ANOVA, Block Adjusted",
                 cols = 1:1 + (ntraits * 2), widths = "auto")
    writeData(wb, sheet = "ANOVA, Block Adjusted", xy = c("A", 8),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")
    setColWidths(wb, sheet = "ANOVA, Block Adjusted", cols = 1,
                 widths = max(nchar(anovaba$Source)) + 5)

    if (!is.null(anova_warn)) {
      row1 <- 10
      writeData(wb, sheet = "ANOVA, Block Adjusted", x = anova_warn,
                startCol = "A", startRow = row1,
                borders = "none", colNames = FALSE)
      addStyle(wb,  sheet = "ANOVA, Block Adjusted",
               style = createStyle(fontColour  = "#C00000", wrapText = FALSE,
                                   halign = "left", valign = "top"),
               rows = row1 + seq_along(anova_warn) - 1, cols = 1,
               stack = FALSE, gridExpand = TRUE)
    }

    ## Std. Error ----
    SE <- aug.bulk$`Std. Errors`
    SE[, traits] <- sapply(SE[, traits], as.numeric)

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
             rows = 1, cols = 2:(ntraits + 1), stack = TRUE,
             gridExpand = TRUE)
    setColWidths(wb, sheet = "Standard Errors",
                 cols = 1:ncol(SE), widths = "auto")

    ## CD ----
    CD <- aug.bulk$CD
    CD[, traits] <- sapply(CD[, traits], as.numeric)

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

    ## CV ----
    CV <- aug.bulk$CV
    CV$CV <- as.numeric(CV$CV)

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

    ## Overall adj. mean ----
    oadjmean <- aug.bulk$`Overall adjusted mean`
    oadjmean$Overall.adjusted.mean <-
      as.numeric(oadjmean$Overall.adjusted.mean)

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


    ## Check statistics ----
    addWorksheet(wb, sheetName = "Check Statistics", gridLines = FALSE)
    indexdf <- lapply(seq_along(aug.bulk$`Check statistics`), function(i) {
      data.frame(i = rep(i, times = nrow(aug.bulk$`Check statistics`[[i]]) + 3),
                 rows = 1:(nrow(aug.bulk$`Check statistics`[[i]]) + 3))
    })
    indexdf <- bind_rows(indexdf)
    indexdf$index <- seq_along(indexdf$i)

    for (i in seq_along(aug.bulk$`Check statistics`)) {
      nchecks <- nrow(aug.bulk$`Check statistics`[[i]])
      row1 <- indexdf[indexdf$i == i & indexdf$rows == 1, ]$index
      writeData(wb, sheet = "Check Statistics",
                x = names(aug.bulk$`Check statistics`)[i],
                startCol = "A", startRow = row1, borders = "none")
      addStyle(wb, sheet = "Check Statistics", rows = row1, cols = 1,
               style = createStyle(textDecoration = "bold"))
      writeDataTable(wb, sheet = "Check Statistics",
                     x = aug.bulk$`Check statistics`[[i]],
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE, startCol = "A",
                     startRow = row1 + 1)
      addStyle(wb,  sheet = "Check Statistics", style = numstyle,
               rows = (row1 + 1):(row1 + nchecks + 1), cols = 3:6,
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Check Statistics",
               style = createStyle(halign = "right"),
               rows = row1 + 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
      rm(row1)
    }
    setColWidths(wb, sheet = "Check Statistics",
                 cols = 1:ncol(aug.bulk$`Check statistics`[[1]]),
                 widths = "auto")
    setColWidths(wb, sheet = "Check Statistics",
                 cols = 5:6,
                 widths = max(unlist(lapply(aug.bulk$`Check statistics`,
                                            function(chkst) {
                                              max(nchar(chkst$Max),
                                                  nchar(chkst$Max),
                                                  3)
                                            }))) + 5)
    rm(indexdf)

    ## Descriptive statistics ----
    if (!is.null(aug.bulk$`Descriptive statistics`)){
      descout <- aug.bulk$`Descriptive statistics`
      descout <- descout[, setdiff(colnames(descout),
                                   c("Skewness_Pr(>F)", "Kurtosis_Pr(>F)"))]
      descout_org <- descout
      descout <- descout[, setdiff(colnames(descout),
                                   c("Skewness_sig", "Kurtosis_sig"))]

      addWorksheet(wb, sheetName = "Descriptive Statistics", gridLines = FALSE)
      writeDataTable(wb, sheet = "Descriptive Statistics", x = descout,
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
      addStyle(wb,  sheet = "Descriptive Statistics",
               style = createStyle(numFmt = "0"),
               rows = 2:(ntraits + 1), cols = 2, stack = FALSE,
               gridExpand = TRUE)
      addStyle(wb,  sheet = "Descriptive Statistics", style = numstyle,
               rows = 2:(ntraits + 1), cols = 3:9, stack = FALSE,
               gridExpand = TRUE)
      addStyle(wb,  sheet = "Descriptive Statistics",
               style = createStyle(halign = "right"),
               rows = 1, cols = 2:9, stack = TRUE, gridExpand = TRUE)
      for (i in 1:ntraits) {
        if (descout_org$Skewness_sig[i] == "*") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = ssstyle,
                   rows = i + 1, cols = 8, stack = FALSE)
        }
        if (descout_org$Skewness_sig[i]  == "**") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = dsstyle,
                   rows = i + 1, cols = 8, stack = FALSE)
        }
        if (descout_org$Skewness_sig[i]  == "ns") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = nsstyle,
                   rows = i + 1, cols = 8, stack = FALSE)
        }
      }
      for (i in 1:ntraits) {
        if (descout_org$Kurtosis_sig[i] == "*") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = ssstyle,
                   rows = i + 1, cols = 9, stack = FALSE)
        }
        if (descout_org$Kurtosis_sig[i]  == "**") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = dsstyle,
                   rows = i + 1, cols = 9, stack = FALSE)
        }
        if (descout_org$Kurtosis_sig[i]  == "ns") {
          addStyle(wb,  sheet = "Descriptive Statistics", style = nsstyle,
                   rows = i + 1, cols = 9, stack = FALSE)
        }
      }
      setColWidths(wb, sheet = "Descriptive Statistics",
                   cols = 1:ncol(descout), widths = "auto")
      writeData(wb, sheet = "Descriptive Statistics", xy = c("A", ntraits + 2),
                x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
                borders = "none")
      setColWidths(wb, sheet = "Descriptive Statistics", cols = 1,
                   widths = max(nchar(descout$Trait)) + 5)
    }

    ## Frequency distribution -----
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
        addStyle(wb, sheet = "Frequency Distribution", rows = row1, cols = 1,
                 style = createStyle(textDecoration = "bold"))
        plot(aug.bulk$`Frequency distribution`[[i]])
        insertPlot(wb, sheet = "Frequency Distribution",
                   xy = c("B", row1))
        dev.off()
        if (!is.null(aug.bulk$warnings$`Freq. dist`[[traits[i]]])) {
          writeData(wb, sheet = "Frequency Distribution",
                    x = aug.bulk$warnings$`Freq. dist`[[traits[i]]],
                    startCol = "J", startRow = row1 + 1, borders = "none")
          addStyle(wb,  sheet = "Frequency Distribution",
                   style = createStyle(fontColour  = "#C00000"),
                   rows = row1 +
                     length(aug.bulk$warnings$`Freq. dist`[[traits[i]]]),
                   cols = 10, stack = FALSE, gridExpand = TRUE)
        }
        rm(row1)
      }
      setColWidths(wb, sheet = "Frequency Distribution",
                   cols = 1, widths = "auto")
      rm(indexdf)
    }

    ## GVA ----
    if (!is.null(aug.bulk$`Genetic variability analysis`)) {
      GVA <- aug.bulk$`Genetic variability analysis`

      if (!is.null(aug.bulk$warnings$GVA)) {
        gwstring1 <- "may not be appropriate for this trait"
        gwstring2 <- "Negative GV detected"
        if (any(grepl(paste(c(gwstring1, gwstring2), collapse = "|"),
                      aug.bulk$warnings$GVA))) {
          new_trait <- GVA$Trait
          gwhltv <- rep("", length(new_trait))

          if (any(grepl(gwstring1, aug.bulk$warnings$GVA))) {
            gwhlt1_match <- sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring1, gvaw)))
            gwhlt1 <- names(gwhlt1_match[gwhlt1_match])
            gwhltv[which(new_trait %in% gwhlt1)] <-
              paste( gwhltv[which(new_trait %in% gwhlt1)], "\u2020", sep = "")
          }
          if (any(grepl(gwstring2, aug.bulk$warnings$GVA))) {
            gwhlt2_match <- sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring2, gvaw)))
            gwhlt2 <- names(gwhlt2_match[gwhlt2_match])
            gwhltv[which(new_trait %in% gwhlt2)] <-
              paste( gwhltv[which(new_trait %in% gwhlt2)], "\u2021", sep = "")
          }
          gwhltv <- stringi::stri_pad_right(gwhltv, width = max(nchar(gwhltv)),
                                            pad = "\u00A0")

          new_trait <-
            paste(stringi::stri_pad_right(GVA$Trait,
                                          width = max(nchar(GVA$Trait)),
                                          pad = "\u00A0"),
                  gwhltv)
          GVA$Trait <- new_trait
        }
      }

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

      if (!is.null(aug.bulk$warnings$GVA)) {
        row1 <- nrow(GVA) + 2
        if (any(grepl(gwstring1, aug.bulk$warnings$GVA))) {
          writeData(wb, sheet = "Genetic Variability Analysis",
                    x =
                      paste(c("\u2020 P-value for \"Treatment: Test\" is ",
                              "> 0.05. ",
                              "Genetic variability analysis may not be ",
                              "appropriate for this trait."),
                            collapse = ""),
                    startCol = "A", startRow = row1, borders = "none")
          addStyle(wb,  sheet = "Genetic Variability Analysis",
                   style = createStyle(fontColour  = "#C00000"),
                   rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
          row1 <- row1 + 1
        }
        if (any(grepl(gwstring2, aug.bulk$warnings$GVA))) {
          writeData(wb, sheet = "Genetic Variability Analysis",
                    x =
                      paste(c("\u2021 Negative GV detected. ",
                              "GCV, GCV category, hBS, hBS category, GA, GAM ",
                              "and GAM category could not be computed."),
                            collapse = ""),
                    startCol = "A", startRow = row1, borders = "none")
          addStyle(wb,  sheet = "Genetic Variability Analysis",
                   style = createStyle(fontColour  = "#C00000"),
                   rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        }
        rm(row1)
      }
      setColWidths(wb, sheet = "Genetic Variability Analysis", cols = 1,
                   widths = max(nchar(GVA$Trait)) + 5)

    }

    ## GVA plots ----
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
        plot(aug.bulk$`GVA plots`$`Broad sense heritability`)
        insertPlot(wb, sheet = "GVA Plots",
                   xy = c("A", 52))
        dev.off()
        # workaround for cleanEx() issue
        # https://github.com/jonathonthill/MMAPPR2/issues/90#issue-367025989
        dev.new()
      }

      addStyle(wb, sheet = "GVA Plots", rows = c(1, 26, 52), cols = 1,
               style = createStyle(textDecoration = "bold"))
    }

    ## Adjusted Means ----
    adj.means <- aug.bulk$Means
    # colnames(adj.means) <- make.names(colnames(adj.means), unique = TRUE)

    addWorksheet(wb, sheetName = "Adjusted Means", gridLines = FALSE)
    writeDataTable(wb, sheet = "Adjusted Means", x = adj.means,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Adjusted Means", style = numstyle,
             rows = 2:(nrow(adj.means) + 1), cols = 3:(ntraits + 2),
             stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "Adjusted Means",
             style = createStyle(halign = "right"),
             rows = 1, cols = 3:(ntraits + 2), stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Adjusted Means",
                 cols = 1:ncol(adj.means), widths = "auto")
    if (any(grepl(wstring2, aug.bulk$warnings))) {

      neg_msg <- "Negative adjusted means"
      if (any(grepl("truncated to zero", aug.bulk$warnings))) {
        neg_msg <- paste(neg_msg, " (",
                         "Truncated to zero",
                         ")", sep = "")
      }
      neg_msg <- paste("\u2020 ", neg_msg, ".", sep = "")

      neg_df <- lapply(aug.bulk$warnings$Model[grepl(wstring2,
                                                     aug.bulk$warnings$Model)],
                       function(x) trimws(unlist(strsplit(x, "\n"))))
      neg_df <- lapply(neg_df, function(x) unlist(strsplit(x[[2]], ", ")))

      neg_traits <- names(neg_df)

      for (i in seq_along(neg_traits)) {
        neg_trts <- neg_df[[neg_traits[i]]]
        neg_index <- which(adj.means$Treatment %in% neg_trts)
        neg_index2 <- which(!(adj.means$Treatment %in% neg_trts))

        addStyle(wb,  sheet = "Adjusted Means", style = csstyle,
                 rows = neg_index + 1,
                 cols = which(colnames(adj.means) == neg_traits[i]),
                 stack = FALSE, gridExpand = TRUE)
        addStyle(wb,  sheet = "Adjusted Means", style = padstyle,
                 rows = neg_index2 + 1,
                 cols = which(colnames(adj.means) == neg_traits[i]),
                 stack = FALSE, gridExpand = TRUE)
      }
      writeData(wb, sheet = "Adjusted Means", x = neg_msg,
                startCol = "A",  startRow = nrow(adj.means) + 2,
                borders = "none")
      setColWidths(wb, sheet = "Adjusted Means",
                   cols = 1, widths = max(nchar(adj.means$Treatment), 9) + 5)

    }

    ## Warnings -----
    if (!all( unlist(lapply(aug.bulk$warnings, is.null)))) {
      addWorksheet(wb, sheetName = "Warnings", gridLines = FALSE)

      row1 <- 1

      if (!is.null(aug.bulk$warnings$`Missing values`)) {
        misswarn <- stack(aug.bulk$warnings$`Missing values`)
        misswarn <- misswarn[, 2:1]
        writeData(wb, sheet = "Warnings", x = "Missing Values",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = misswarn, startCol = "A", startRow = row1 + 1,
                  borders = "none", colNames = FALSE)
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + nrow(misswarn)
      }

      if (!is.null(aug.bulk$warnings$Model)) {
        warn <- stack(aug.bulk$warnings$Model)
        warn <- warn[, 2:1]
        writeData(wb, sheet = "Warnings", x = "Model",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = warn, startCol = "A", startRow = row1 + 1,
                  borders = "none", colNames = FALSE)
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + nrow(warn)
      }

      if (!is.null(aug.bulk$warnings$`Freq. dist`)) {
        fqwarn <- stack(aug.bulk$warnings$`Freq. dist`)
        fqwarn <- fqwarn[, 2:1]
        writeData(wb, sheet = "Warnings", x = "Frequency Distribution",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = fqwarn, startCol = "A", startRow = row1 + 1,
                  borders = "none", colNames = FALSE)
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + nrow(fqwarn)
      }

      if (!is.null(aug.bulk$warnings$GVA)) {
        gvawarn <- stack(aug.bulk$warnings$GVA)
        gvawarn <- gvawarn[, 2:1]
        writeData(wb, sheet = "Warnings", x = "Genetic Variablity Analysis",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = gvawarn, startCol = "A", startRow = row1 + 1,
                  borders = "none", colNames = FALSE)
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
      }
    }

    setColWidths(wb, sheet = "Warnings",
                 cols = 1, widths = max(nchar(traits)) + 5)

    saveWorkbook(wb = wb, file = target, overwrite = TRUE)

  }

  message(paste("File created at", target))

}


wlist2blist <- function(wlist, fp_p = fp_par()) {
  outlist <- lapply(wlist, function(wtext) fpar(ftext(wtext),
                                                fp_p = fp_p))
  attributes(outlist) <- list(class = c("block_list", "block"))
  return(outlist)
}
