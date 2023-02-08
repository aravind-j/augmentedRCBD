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
#' @param k The standardized selection differential or selection intensity
#'   required for computation of Genetic advance. Default is 2.063 for 5\%
#'   selection proportion (see \strong{Details} in
#'  \code{\link[augmentedRCBD]{gva.augmentedRCBD}}). Ignored if
#'  \code{gva = FALSE}.
#' @param check.col The colour(s) to be used to highlight check values in the
#'   plot as a character vector. Must be valid colour values in R (named
#'   colours, hexadecimal representation, index of colours [\code{1:8}] in
#'   default R \code{palette()} etc.).
#'
#' @note The raw values in the \code{augmentedRCBD} object are rounded off to 2
#'   digits in the word and excel reports. However, in case of excel report, the
#'   raw values are present in the cell and are formatted to display only 2
#'   digits.
#'
#'   So, if values such as adjusted means are being used of downstream
#'   analysis, export the raw values from within R or use the excel report.
#'
#'   This default rounding can be changed by setting the global options
#'   \code{augmentedRCBD.round.digits}. For example
#'   \code{setOption(augmentedRCBD.round.digits = 3)} sets the number of decimal
#'   places for rounding to 3.
#'
#'   Values will not be rounded to zero, instead will be rounded to the nearest
#'   decimal place. F value, t ratio and p values are not rounded to less than 3
#'   decimal places.
#'
#' @export
#' @import officer
#' @import flextable
#' @import openxlsx
#' @importFrom dplyr mutate_if
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom methods is
#' @importFrom stringi stri_pad_right stri_trans_totitle
#' @importFrom graphics plot
#' @importFrom utils capture.output citation stack
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
#'                      file.type = "word",
#'                      check.col = c("brown", "darkcyan",
#'                                    "forestgreen", "purple"))
#' report.augmentedRCBD(aug = out,
#'                      target = file.path(tempdir(),
#'                                         "augmentedRCBD output.xlsx"),
#'                      file.type = "excel",
#'                      check.col = c("brown", "darkcyan",
#'                                    "forestgreen", "purple"))
#' }
#'
report.augmentedRCBD <- function(aug, target, file.type = c("word", "excel"),
                                 k = 2.063, check.col = "red"){

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class "augmentedRCBD"')
  }

  file.type <- match.arg(file.type)

  # check.col
  if (!all(iscolour(check.col))) {
    stop('"check.col" specifies invalid colour(s)')
  }

  checks <- aug$Details$`Check treatments`

  if (length(check.col) != 1) {
    if (length(check.col) != length(checks)) {
      stop('"checks" and "check.col" are of unequal lengths')
    }
  }

  round.digits <- getOption("augmentedRCBD.round.digits", default = 2)

  wstring1 <- "Test treatments are replicated"
  wstring2 <- "Negative adjusted means were generated for the following"

  if (file.type == "word") {
    if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)) {
      stop(target, " should have '.docx' extension.")
    }

    suppar <- fp_text(vertical.align = "superscript")

    augreport <- read_docx(file.path(system.file(package = "augmentedRCBD"),
                                     "template.docx"))

    augreport <- body_add_par(augreport, value = "augmentedRCBD",
                              style = "Title")
    augreport <- body_add_toc(augreport, level = 2)

    # Details
    augreport <- body_add_par(augreport, value = "Details",
                              style = "heading 1")

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
    if (any(grepl(wstring1, aug$warnings))) {
      dups <- aug$Means[!(aug$Means$Treatment %in% checks), ]$Treatment
      dups <- dups[duplicated(dups)]
      dups <- aug$Means[aug$Means$Treatment %in% dups, c("Treatment", "Block")]
      rownames(dups) <- NULL
      augreport <- body_add_par(augreport, value = "\r\n", style = "Normal")
      augreport <- body_add_par(augreport,
                                value = "Following test treatments are replicated.",
                                style = "Warning")
      augreport <- body_add_flextable(augreport,
                                      theme_alafoli(autofit(regulartable(dups))))
    }

    # ANOVA, TA
    augreport <- body_add_par(augreport, value = "ANOVA, Treatment Adjusted",
                              style = "heading 1")
    if (is.data.frame(aug$`ANOVA, Treatment Adjusted`)){
      anovata <- aug$`ANOVA, Treatment Adjusted`
    } else {
      anovata <- data.frame(aug$`ANOVA, Treatment Adjusted`[[1]])
      anovata <- cbind(Source = trimws(rownames(anovata)), anovata)
    }
    anovata$sig <- ifelse(anovata$Pr..F. <= 0.01, "**",
                          ifelse(anovata$Pr..F. <= 0.05, "*", "ns"))
    colnames(anovata) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                           "F value", "Pr(>F)", " ")
    anovata$Df <- as.character(anovata$Df)
    anovata[, c("Sum Sq", "Mean Sq")] <-
      lapply(anovata[, c("Sum Sq", "Mean Sq")], round.conditional,
             digits = round.digits)
    anovata[, c("F value", "Pr(>F)")] <-
      lapply(anovata[, c("F value", "Pr(>F)")], round.conditional,
             digits = max(round.digits, 3))
    nsindex <- which(anovata[, 7] == "ns")
    anovata <- autofit(regulartable(anovata))
    if (!is.null(nsindex)) {
      anovata <- compose(anovata, part = "body", i = nsindex, j = 7,
                         value = as_paragraph(as_sup("ns")))
    }
    anovata <- align(anovata, j = 2:6, align = "right", part = "all")
    anovata <- bold(anovata, part = "header")
    augreport <- body_add_flextable(augreport, anovata)
    augreport <- body_add_fpar(augreport,
                               value = fpar(ftext("ns", suppar),
                                            ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))

    # ANOVA, BA
    augreport <- body_add_par(augreport, value = "ANOVA, Block Adjusted",
                              style = "heading 1")
    if (is.data.frame(aug$`ANOVA, Block Adjusted`)){
      anovaba <- aug$`ANOVA, Block Adjusted`
    } else {
      anovaba <- data.frame(aug$`ANOVA, Block Adjusted`[[1]])
      anovaba <- cbind(Source = trimws(rownames(anovaba)), anovaba)
    }
    anovaba$sig <- ifelse(anovaba$Pr..F. <= 0.01, "**",
                          ifelse(anovaba$Pr..F. <= 0.05, "*", "ns"))
    colnames(anovaba) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                           "F value", "Pr(>F)", " ")
    anovaba$Df <- as.character(anovaba$Df)
    anovaba[, c("Sum Sq", "Mean Sq")] <-
      lapply(anovaba[, c("Sum Sq", "Mean Sq")], round.conditional,
             digits = round.digits)
    anovaba[, c("F value", "Pr(>F)")] <-
      lapply(anovaba[, c("F value", "Pr(>F)")], round.conditional,
             digits = max(round.digits, 3))
    nsindex <- which(anovaba[, 7] == "ns")
    anovaba <- autofit(regulartable(anovaba))
    if (!is.null(nsindex)) {
      anovaba <- compose(anovaba, part = "body", i = nsindex, j = 7,
                         value = as_paragraph(as_sup("ns")))
    }
    anovaba <- align(anovaba, j = 2:6, align = "right", part = "all")
    anovaba <- bold(anovaba, part = "header")
    augreport <- body_add_flextable(augreport, anovaba)
    augreport <- body_add_fpar(augreport,
                               value = fpar(ftext("ns", suppar),
                                            ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))
    # Std. Errors
    augreport <- body_add_par(augreport,
                              value = "Standard Errors and Critical Differences",
                              style = "heading 1")
    se <- aug$`Std. Errors`
    se <- cbind(Comparison = row.names(se), se)
    se <- dplyr::mutate_if(se, is.numeric, round.conditional,
                           digits = round.digits)
    se <- autofit(regulartable(se))
    se <- align(se, j = 2:3, align = "right", part = "all")
    se <- bold(se, part = "header")
    augreport <- body_add_flextable(augreport, se)

    # Overall adjusted mean
    augreport <- body_add_par(augreport, value = "Overall Adjusted Mean",
                              style = "heading 1")
    augreport <- body_add_par(augreport,
                              value = as.character(round.conditional(aug$`Overall adjusted mean`,
                                                                     digits = round.digits)),
                              style = "Normal")

    # Coefficient of variation
    augreport <- body_add_par(augreport, value = "Coefficient of Variation",
                              style = "heading 1")
    augreport <- body_add_par(augreport,
                              value = as.character(round.conditional(aug$CV,
                                                                     digits = round.digits)),
                              style = "Normal")

    # Means
    augreport <- body_add_par(augreport, value = "Means", style = "heading 1")
    Means <- aug$Means
    Means[, c("Means", "SE", "Min", "Max", "Adjusted Means")] <-
      lapply(Means[, c("Means", "SE", "Min", "Max", "Adjusted Means")],
             round.conditional, digits = round.digits)
    if (any(grepl(wstring2, aug$warnings))) {
      wstring2_mod <- trimws(unlist(strsplit(aug$warnings[grepl(wstring2,
                                                                aug$warnings)],
                                             "\n")))
      neg_trts <- trimws(unlist(strsplit(wstring2_mod[2], ",")))
      neg_index <- which(Means$Treatment %in% neg_trts)
      Means$x <- ""
      Means[neg_index, ]$x <- "\u2020"
      colnames(Means) <- c("Treatment", "Block", "Means", "SE", "r", "Min",
                           "Max", "Adjusted Means",  " ")
    }
    Means <- autofit(regulartable(Means))
    Means <- align(Means, j = 2:8, align = "right", part = "all")
    Means <- bold(Means, part = "header")
    augreport <- body_add_flextable(augreport, Means)
    if (any(grepl(wstring2, aug$warnings))) {
      neg_msg <- gsub(" were generated for the following treatment\\(s\\)", "",
                      wstring2_mod[1])
      if (!is.na(wstring2_mod[3])) {
        neg_msg <- paste(neg_msg, " (",
                         stri_trans_totitle(gsub("They were ", "",
                                                 wstring2_mod[3]),
                                            type = "sentence"),
                         ")", sep = "")
      }
      neg_msg <- paste("\u2020 ", neg_msg, ".", sep = "")
      augreport <- body_add_par(augreport, value = neg_msg, style = "Normal")
    }

    # Freq dist
    augreport <- body_add_par(augreport, value = "Frequency Distribution",
                              style = "heading 1")
    src <- tempfile(fileext = ".png")
    png(filename = src, width = 6, height = 4, units = 'in', res = 300)
    fqwarn <- NULL
    withCallingHandlers({
      plot(freqdist.augmentedRCBD(aug, xlab = "", check.col = check.col))
    }, warning = function(w) {
      fqwarn <<- append(fqwarn, cli::ansi_strip(w$message))
      invokeRestart("muffleWarning")
    })
    dev.off()
    augreport <- body_add_img(augreport, src = src, width = 6, height = 4)
    rm(src)
    if (!is.null(fqwarn)) {
      augreport <- body_add_par(augreport, value = "\r\n", style = "Normal")
      for (i in seq_along(fqwarn)) {
        augreport <- body_add_par(augreport, value = fqwarn[i],
                                  style = "Warning")
      }
    }

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
    descout[, desc] <- apply(descout[, desc], MARGIN = 2,
                             FUN = round.conditional, digits = round.digits)
    colnames(descout) <- c("Count", "Mean", "Std.Error", "Std.Deviation",
                           "Min", "Max", "Skewness", "Skewness_sig", "Kurtosis",
                           "Kurtosis_sig")
    descout <- rbind(descout[, c("Count", "Mean", "Std.Error", "Std.Deviation",
                                 "Min", "Max", "Skewness", "Kurtosis")],
                     c(rep("", 6),
                       unlist(descout[,
                                      c("Skewness_sig", "Kurtosis_sig")])))

    descout <- data.frame(t(descout))
    descout <- cbind(Statistic = rownames(descout), descout)
    rownames(descout) <- NULL
    colnames(descout) <- c("Statistic", "Value", " ")
    nsindex <- which(descout[, 3] == "ns")
    descout <- autofit(regulartable(descout))
    if (!is.null(nsindex)) {
      descout <- compose(descout, part = "body", i = nsindex, j = 3,
                         value = as_paragraph(as_sup("ns")))
    }
    descout <- align(descout, j = 2, align = "right", part = "all")
    descout <- align(descout, j = 3, align = "left", part = "all")
    descout <- bold(descout, part = "header")
    augreport <- body_add_flextable(augreport, descout)

    augreport <- body_add_fpar(augreport,
                               value = fpar(ftext("ns", suppar),
                                            ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))

    # GVA
    augreport <- body_add_par(augreport, value = "Genetic Variability Analysis",
                              style = "heading 1")
    gvaout <- gva.augmentedRCBD(aug, k = k)
    gvawarn <- NULL
    withCallingHandlers({
      gvaout <- data.frame(gva.augmentedRCBD(aug, k = k))
    }, warning = function(w) {
      gvawarn <<- append(gvawarn, cli::ansi_strip(w$message))
      invokeRestart("muffleWarning")
    })
    gvaout <- data.frame(gvaout)
    gvaout <- dplyr::mutate_if(gvaout, is.numeric, round.conditional,
                               digits = round.digits)
    gvaout <- data.frame(t(gvaout))
    gvaout <- cbind(Statistic = rownames(gvaout), gvaout)
    rownames(gvaout) <- NULL
    gvaout$x <-  c(rep("", 11), rep("*", 3))
    gvaout$x <- ifelse(is.na(gvaout$t.gvaout.), "", gvaout$x)
    colnames(gvaout) <- c("Statistic", "Value", " ")


    gvaout <- autofit(regulartable(gvaout))
    gvaout <- align(gvaout, j = 2, align = "right", part = "all")
    gvaout <- align(gvaout, j = 3, align = "left", part = "all")
    gvaout <- bold(gvaout, part = "header")
    augreport <- body_add_flextable(augreport, gvaout)
    augreport <- body_add_par(augreport, value = paste("* k =", k))
    if (!is.null(gvawarn)) {
      augreport <- body_add_par(augreport, value = "\r\n", style = "Normal")
      for (i in seq_along(gvawarn)) {
        augreport <- body_add_par(augreport, value = gvawarn[i],
                                  style = "Warning")
      }
    }

    # Comparisons
    if (!is.null(aug$Comparisons)) {
      augreport <- body_add_par(augreport, value = "Comparisons",
                                style = "heading 1")
      augreport <- body_add_par(augreport,
                                value = paste("Comparison method:",
                                              aug$`Comparison method`),
                                style = "Normal")
      cmp <- aug$Comparisons
      cmp[, c("estimate", "SE")] <-
        lapply(cmp[, c("estimate", "SE")],
               round.conditional, digits = round.digits)
      cmp[, c("t.ratio", "p.value")] <-
        lapply(cmp[, c("t.ratio", "p.value")],
               round.conditional, digits = max(round.digits, 3))
      cmp <- autofit(regulartable(cmp))
      cmp <- align(cmp, j = 2:6, align = "right", part = "all")
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
      gps[, c("Adjusted Means", "SE", "lower.CL", "upper.CL")] <-
        lapply(gps[, c("Adjusted Means", "SE", "lower.CL", "upper.CL")],
               round.conditional, digits = round.digits)
      gps <- autofit(regulartable(gps))
      gps <- align(gps, j = 2:5, align = "right", part = "all")
      gps <- bold(gps, part = "header")
      augreport <- body_add_flextable(augreport, gps)
    }

    # Warnings
    if (!all(unlist(lapply(list(aug$warnings, fqwarn, gvawarn), is.null)))) {
      augreport <- body_add_par(augreport, value = "Warnings",
                                style = "heading 1")

      if (!is.null(aug$warnings)) {
        augreport <- body_add_par(augreport,
                                  value = "Model",
                                  style = "heading 2")
        warn_mod <- trimws(unlist(strsplit(aug$warnings, "\n")))
        for (i in seq_along(warn_mod)) {
          augreport <- body_add_par(augreport,
                                     value = warn_mod[i],
                                     style = "Code")
        }
      }

      if (!is.null(fqwarn)) {
        augreport <- body_add_par(augreport,
                                  value = "Frequency Distribution",
                                  style = "heading 2")
        for (i in seq_along(fqwarn)) {
          augreport <- body_add_par(augreport, value = fqwarn[i],
                                    style = "Code")
        }
      }

      if (!is.null(gvawarn)) {
        augreport <- body_add_par(augreport,
                                  value = "Genetic Variablity Analysis",
                                  style = "heading 2")
        for (i in seq_along(gvawarn)) {
          augreport <- body_add_par(augreport, value = gvawarn[i],
                                    style = "Code")
        }

      }
    }

    # augreport <- body_add_par(augreport,
    #                           value = "################## The End ##################",
    #                           style = "Center text")

    augreport <- body_add_par(augreport, value = "Citation Info",
                              style = "heading 1")

    citout <- capture.output(citation("augmentedRCBD"))
    citlist <- lapply(citout,
                      function(ctext) fpar(ftext(ctext),
                                           fp_p = fp_par(padding.bottom = 2,
                                                         word_style = "Code")))

    attributes(citlist) <- list(class = c("block_list", "block"))
    augreport <- body_add_blocks(augreport, blocks = citlist)

    print(augreport, target = target)

  }

  if (file.type == "excel") {
    if (!grepl(x = target, pattern = "\\.(xlsx)$", ignore.case = TRUE)) {
      stop(target, " should have '.xlsx' extension.")
    }

    # Create workbook
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

    hs <- createStyle(halign = "left", valign = "bottom")

    num.base <- paste("0.", paste(rep(0, round.digits), collapse = ""), sep = "")
    numstyle <- createStyle(numFmt = num.base)
    num.base.p <- paste("0.", paste(rep(0, max(round.digits, 3)),
                                    collapse = ""), sep = "")
    numstyle.p <- createStyle(numFmt = num.base.p)
    ssstyle <- createStyle(numFmt = paste(num.base, '"*"'))
    dsstyle <- createStyle(numFmt = paste(num.base, '"**"'))
    nsstyle <- createStyle(numFmt = paste(num.base, '"\u207f\u02e2"'))
    csstyle <- createStyle(numFmt = paste(num.base, '"\u2020"'))
    numstyle2 <- createStyle(numFmt = paste(num.base, '"\u00A0""\u00A0"'))

    # Index
    index <- c("Details", "ANOVA, Treatment Adjusted", "ANOVA, Block Adjusted",
               "SEs and CDs", "Overall Adjusted Mean", "Coefficient of Variation",
               "Means", "Frequency Distribution", "Descriptive Statistics",
               "Genetic Variability Analysis")

    if (!is.null(aug$Comparisons)) {
      index <- c(index, "Comparisons")
    }
    if (!is.null(aug$Groups)) {
      index <- c(index, "Groups")
    }

    index <- c(index, "Warnings")

    index <- data.frame(`Sl.No` = seq_along(index), Sheets = index)

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
                   startCol = "B", startRow = 9, colNames = TRUE, rowNames = FALSE,
                   headerStyle = hs, tableStyle = "TableStyleLight1",
                   withFilter = FALSE, bandedRows = FALSE)
    addStyle(wb,  sheet = "Index", style = createStyle(halign = "right"),
             rows = 9, cols = 2, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "Index", cols = 1:3, widths = "auto")

    citout <- capture.output(citation("augmentedRCBD"))
    writeData(wb, sheet = "Index", x = citout,
              startCol = "B", startRow = 25, borders = "none")
    setColWidths(wb, sheet = "Index", cols = 2, widths = 5)

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
    writeDataTable(wb, sheet = "Details", x = Details,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    setColWidths(wb, sheet = "Details", cols = 1:ncol(Details), widths = "auto")
    if (any(grepl(wstring1, aug$warnings))) {
      dups <- aug$Means[!(aug$Means$Treatment %in% checks), ]$Treatment
      dups <- dups[duplicated(dups)]
      dups <- aug$Means[aug$Means$Treatment %in% dups, c("Treatment", "Block")]
      rownames(dups) <- NULL
      writeData(wb, sheet = "Details", xy = c("A", 8),
                x = "Following test treatments are replicated.",
                borders = "none")
      addStyle(wb,  sheet = "Details",
               style = createStyle(fontColour  = "#C00000"),
               rows = 8, cols = 1, stack = FALSE, gridExpand = TRUE)
      writeDataTable(wb, sheet = "Details", x = dups, xy = c("A", 10),
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
    }

    # ANOVA, TA
    if (is.data.frame(aug$`ANOVA, Treatment Adjusted`)){
      anovata <- aug$`ANOVA, Treatment Adjusted`
    } else {
      anovata <- data.frame(aug$`ANOVA, Treatment Adjusted`[[1]])
      anovata <- cbind(Source = trimws(rownames(anovata)), anovata)
    }
    anovata$sig <- ifelse(anovata$Pr..F. <= 0.01, "**",
                          ifelse(anovata$Pr..F. <= 0.05, "*", "\u207f\u02e2"))
    colnames(anovata) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                           "F value", "Pr(>F)", " ")

    addWorksheet(wb, sheetName = "ANOVA, Treatment Adjusted", gridLines = FALSE)
    writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted", x = anovata,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = numstyle,
             rows = 2:6, cols = 3:4, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = numstyle.p,
             rows = 2:6, cols = 5:6, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted",
                 cols = 1:ncol(anovata), widths = "auto")
    writeData(wb, sheet = "ANOVA, Treatment Adjusted",
              xy = c("A", 7),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")

    # ANOVA, BA
    if (is.data.frame(aug$`ANOVA, Block Adjusted`)){
      anovaba <- aug$`ANOVA, Block Adjusted`
    } else {
      anovaba <- data.frame(aug$`ANOVA, Block Adjusted`[[1]])
      anovaba <- cbind(Source = trimws(rownames(anovaba)), anovaba)
    }
    anovaba$sig <- ifelse(anovaba$Pr..F. <= 0.01, "**",
                          ifelse(anovaba$Pr..F. <= 0.05, "*", "\u207f\u02e2"))
    colnames(anovaba) <- c("Source", "Df", "Sum Sq", "Mean Sq",
                           "F value", "Pr(>F)", " ")

    addWorksheet(wb, sheetName = "ANOVA, Block Adjusted", gridLines = FALSE)
    writeDataTable(wb, sheet = "ANOVA, Block Adjusted", x = anovaba,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = numstyle,
             rows = 2:7, cols = 3:4, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = numstyle,
             rows = 2:7, cols = 5:6, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(halign = "right"),
             rows = 1, cols = 2:6, stack = TRUE, gridExpand = TRUE)
    setColWidths(wb, sheet = "ANOVA, Block Adjusted",
                 cols = 1:ncol(anovaba), widths = "auto")
    writeData(wb, sheet = "ANOVA, Block Adjusted",
              xy = c("A", 8),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")

    # Std. Errors
    se <- aug$`Std. Errors`
    se <- cbind(Comparison = row.names(se), se)

    addWorksheet(wb, sheetName = "SEs and CDs", gridLines = FALSE)
    writeDataTable(wb, sheet = "SEs and CDs", x = se,
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
    writeData(wb, sheet = "Coefficient of Variation", x = aug$CV,
              startCol = "A", startRow = 1, borders = "none")
    addStyle(wb,  sheet = "Coefficient of Variation", style = numstyle,
             rows = 1, cols = 1, stack = FALSE)

    # Means
    Means <- aug$Means

    addWorksheet(wb, sheetName = "Means", gridLines = FALSE)
    writeDataTable(wb, sheet = "Means", x = Means,
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
    if (any(grepl(wstring2, aug$warnings))) {
      wstring2_mod <- trimws(unlist(strsplit(aug$warnings[grepl(wstring2,
                                                                aug$warnings)],
                                             "\n")))
      neg_trts <- trimws(unlist(strsplit(wstring2_mod[2], ",")))
      neg_index <- which(Means$Treatment %in% neg_trts)
      neg_index2 <- which(!(Means$Treatment %in% neg_trts))

      addStyle(wb,  sheet = "Means", style = csstyle,
               rows = neg_index + 1,
               cols = 8, stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Means", style = numstyle2,
               rows = neg_index2 + 1,
               cols = 8, stack = FALSE, gridExpand = TRUE)

      neg_msg <- gsub(" were generated for the following treatment\\(s\\)", "",
                            wstring2_mod[1])
      if (!is.na(wstring2_mod[3])) {
        neg_msg <- paste(neg_msg, " (",
                         stri_trans_totitle(gsub("They were ", "",
                                                 wstring2_mod[3]),
                                            type = "sentence"),
                         ")", sep = "")
      }
      neg_msg <- paste("\u2020 ", neg_msg, ".", sep = "")
      writeData(wb, sheet = "Means", x = neg_msg, xy = c("A", nrow(Means) + 2),
                borders = "none")
    }
    setColWidths(wb, sheet = "Means", cols = 1,
                 widths = max(nchar(Means$Treatment) + 7))

    # Freq dist
    addWorksheet(wb, sheetName = "Frequency Distribution", gridLines = FALSE)
    fqwarn <- NULL
    withCallingHandlers({
      plot(freqdist.augmentedRCBD(aug, xlab = "", check.col = check.col))
    }, warning = function(w) {
      fqwarn <<- append(fqwarn, cli::ansi_strip(w$message))
      invokeRestart("muffleWarning")
    })
    insertPlot(wb, sheet = "Frequency Distribution",
               xy = c("A", 2))
    dev.off()
    if (!is.null(fqwarn)) {
      writeData(wb, sheet = "Frequency Distribution", xy = c("A", 25),
                x = fqwarn, borders = "none")
      addStyle(wb,  sheet = "Frequency Distribution",
               style = createStyle(fontColour  = "#C00000"),
               rows = 25:(25 + length(fqwarn)), cols = 1,
               stack = FALSE, gridExpand = TRUE)
    }

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
    writeDataTable(wb, sheet = "Descriptive Statistics", x = descout,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "Descriptive Statistics", style = numstyle,
             rows = 3:7, cols = 2, stack = FALSE, gridExpand = TRUE)
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
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")

    # GVA
    gvawarn <- NULL
    withCallingHandlers({
      gvaout <- data.frame(gva.augmentedRCBD(aug, k = k))
    }, warning = function(w) {
      gvawarn <<- append(gvawarn, cli::ansi_strip(w$message))
      invokeRestart("muffleWarning")
    })
    gvaout <- data.frame(t(gvaout))
    gvaout <- cbind(Statistic = rownames(gvaout), gvaout)
    rownames(gvaout) <- NULL
    gvaout$x <-  c(rep("", 11), rep("*", 3))
    gvaout$x <- ifelse(is.na(gvaout$t.gvaout.), "", gvaout$x)
    colnames(gvaout) <- c("Statistic", "Value", " ")

    stat_cat <- c("GCV.category", "PCV.category", "hBS.category", "GAM.category")
    gvaout_sub <- gvaout[gvaout$Statistic %in% stat_cat, ]
    gvaout[gvaout$Statistic %in% stat_cat, ]$Value <- "0"
    gvaout$Value <- as.numeric(gvaout$Value)

    addWorksheet(wb, sheetName = "Genetic Variability Analysis",
                 gridLines = FALSE)
    writeDataTable(wb, sheet = "Genetic Variability Analysis", x = gvaout,
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
    writeData(wb, sheet = "Genetic Variability Analysis",
              x = paste("* k =", k),
              xy = c("A", 16), borders = "none")
    if (!is.null(gvawarn)) {
      writeData(wb, sheet = "Genetic Variability Analysis", xy = c("A", 18),
                x = gvawarn, borders = "none")
      addStyle(wb,  sheet = "Genetic Variability Analysis",
               style = createStyle(fontColour  = "#C00000"),
               rows = 18:(18 + length(gvawarn) - 1), cols = 1,
               stack = FALSE, gridExpand = TRUE)
    }
    setColWidths(wb, sheet = "Genetic Variability Analysis", cols = 1,
                 widths = max(nchar(gvaout$Statistic) + 7))

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
                     tableStyle = "TableStyleLight1", bandedRows = FALSE,
                     withFilter = FALSE)
      addStyle(wb,  sheet = "Comparisons", style = numstyle,
               rows = 3:(nrow(cmp) + 2), cols = c(2:3, 5:6),
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Comparisons", style = numstyle.p,
               rows = 3:(nrow(cmp) + 2), cols = c(5:6),
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

      writeDataTable(wb, sheet = "Groups", x = gps, xy = c("A", 2),
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", bandedRows = FALSE,
                     withFilter = FALSE)
      addStyle(wb,  sheet = "Groups", style = numstyle,
               rows = 3:(nrow(gps) + 2), cols = c(2:3, 5:6),
               stack = FALSE, gridExpand = TRUE)
      addStyle(wb,  sheet = "Groups", style = createStyle(halign = "right"),
               rows = 2, cols = 2:6, stack = TRUE, gridExpand = TRUE)
    }

    # Warnings
    if (!all( unlist(lapply(list(aug$warnings, fqwarn, gvawarn), is.null)))) {
      addWorksheet(wb, sheetName = "Warnings", gridLines = FALSE)

      row1 <- 1

      if (!is.null(aug$warnings)) {
        warn_mod <- trimws(unlist(strsplit(aug$warnings, "\n")))
        writeData(wb, sheet = "Warnings", x = "Model",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = warn_mod,
                  startCol = "A", row1 + 1, borders = "none")
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + length(warn_mod)
      }

      if (!is.null(fqwarn)) {
        writeData(wb, sheet = "Warnings", x = "Frequency Distribution",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = fqwarn,
                  startCol = "A", row1 + 1, borders = "none")
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + length(fqwarn)
      }
      if (!is.null(gvawarn)) {
        writeData(wb, sheet = "Warnings", x = "Genetic Variablity Analysis",
                  startCol = "A", startRow = row1, borders = "none")
        writeData(wb, sheet = "Warnings",
                  x = gvawarn,
                  startCol = "A", row1 + 1, borders = "none")
        addStyle(wb,  sheet = "Warnings",
                 style = createStyle(textDecoration = "bold"),
                 rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
        row1 <- row1 + 2 + length(gvawarn)
      }


    }

    saveWorkbook(wb = wb, file = target, overwrite = TRUE)
  }

  message(paste("File created at", target))

}
