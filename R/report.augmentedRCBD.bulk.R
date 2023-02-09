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

#' Generate MS Word or Excel Report from \code{augmentedRCBD.bulk} Output
#'
#' \code{report.augmentedRCBD.bulk} generates a tidy report from an object of
#' class \code{augmentedRCBD.bulk} as docx MS word file using the
#' \code{\link[officer]{officer}} package or xlsx MS excel file using the
#' \code{\link[openxlsx]{openxlsx}} package.
#'
#' @param aug.bulk An object of class \code{augmentedRCBD.bulk}.
#' @param target The path to the docx file to be created.
#' @param file.type The file type of the report. Either \code{"word"} for MS
#'   Word report file or \code{"excel"} for MS Excel report file.
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
#'   decimal place.
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
#' report.augmentedRCBD.bulk(aug.bulk = bout,
#'                           target = file.path(tempdir(),
#'                                              "augmentedRCBD bulk output.docx"),
#'                           file.type = "word")
#' report.augmentedRCBD.bulk(aug.bulk = bout,
#'                           target = file.path(tempdir(),
#'                                              "augmentedRCBD bulk output.xlsx"),
#'                           file.type = "excel")
#' }
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.bulk}}
#'
#'
report.augmentedRCBD.bulk <- function(aug.bulk, target,
                                      file.type = c("word", "excel")){

  if (!is(aug.bulk, "augmentedRCBD.bulk")) {
    stop('"aug.bulk" is not of class "augmentedRCBD.bulk".')
  }

  file.type <- match.arg(file.type)

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

    checks <- aug.bulk$Details$`Check treatments`

    Details <- regulartable(data = data.frame(Details))
    Details <- autofit(Details)
    augreport <- body_add_flextable(augreport, Details)
    if (any(grepl(wstring1, unlist(aug.bulk$warnings)))) {
      dups <- aug.bulk$Means[!(aug.bulk$Means$Treatment %in% checks), ]$Treatment
      dups <- dups[duplicated(dups)]
      dups <- aug.bulk$Means[aug.bulk$Means$Treatment %in% dups, c("Treatment", "Block")]
      rownames(dups) <- NULL
      augreport <- body_add_par(augreport, value = "\r\n", style = "Normal")
      augreport <- body_add_par(augreport,
                                value = "Following test treatments are replicated.",
                                style = "Warning")
      augreport <- body_add_flextable(augreport,
                                      theme_alafoli(autofit(regulartable(dups))))
    }

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
    anovata <- anovata[, setdiff(colnames(anovata),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    colnames(anovata) <- make.names(colnames(anovata), unique = TRUE)
    anovata[, paste(traits, "_Mean.Sq", sep = "")] <-
      sapply(anovata[, paste(traits, "_Mean.Sq", sep = "")],
             round.conditional, digits = round.digits)
    mcols <- lapply(traits, function(x) which(grepl(paste("(^", x, "_)(.+$)",
                                                          sep = ""),
                                           colnames(anovata))))
    names(mcols) <- traits
    colnames(anovata) <- gsub("_Mean.Sq", "", colnames(anovata))
    nsindex <- lapply(mcols, function(x) which(anovata[, x[2]] == "ns"))
    anovata <- flextable(anovata)
    for(i in 1:ntraits) {
      if (!is.null(nsindex[[traits[[i]]]])) {
        anovata <- compose(anovata, part = "body", i = nsindex[[traits[[i]]]],
                           j = mcols[[traits[[i]]]][2],
                           value = as_paragraph(as_sup("ns")))
      }
      anovata <- merge_at(anovata, 1, mcols[[traits[i]]], "header")
    }
    anovata <- bold(anovata, part = "header")
    anovata <- add_header_row(anovata,
                   colwidths = c(1, 1, ntraits * 2),
                   values = c("Source", "Df", "Mean.Sq"))
    anovata <- merge_at(anovata, 1:2, 1, "header")
    anovata <- merge_at(anovata, 1:2, 2, "header")
    anovata <- align(anovata, j = 2, align = "right", part = "all")
    anovata <- align(anovata, j = 3:(2 + (ntraits * 2)), align = "center",
                     part = "header")
    anovata <- align(anovata, j = unlist(lapply(mcols, function(x) x[1])),
          align = "right", part = "body")
    anovata <-  align(anovata, j = unlist(lapply(mcols, function(x) x[2])),
          align = "left", part = "body")
    anovata <- autofit(anovata)
    augreport <- body_add_flextable(augreport, anovata)

    augreport <- body_add_fpar(augreport,
                               value = fpar(ftext("ns", suppar),
                                            ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))
    # ANOVA, BA
    augreport <- body_add_par(augreport, value = "ANOVA, Block Adjusted",
                              style = "heading 1")
    anovaba <- aug.bulk$`ANOVA, Block Adjusted`
    anovaba <- anovaba[, setdiff(colnames(anovaba),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    colnames(anovaba) <- make.names(colnames(anovaba), unique = TRUE)
    anovaba[, paste(traits, "_Mean.Sq", sep = "")] <-
      sapply(anovaba[, paste(traits, "_Mean.Sq", sep = "")],
             round.conditional, digits = round.digits)
    mcols <- lapply(traits, function(x) which(grepl(paste("(^", x, "_)(.+$)",
                                                          sep = ""),
                                                    colnames(anovaba))))
    names(mcols) <- traits
    colnames(anovaba) <- gsub("_Mean.Sq", "", colnames(anovaba))
    nsindex <- lapply(mcols, function(x) which(anovaba[, x[2]] == "ns"))
    anovaba <- flextable(anovaba)
    for(i in 1:ntraits) {
      if (!is.null(nsindex[[traits[[i]]]])) {
        anovaba <- compose(anovaba, part = "body", i = nsindex[[traits[[i]]]],
                           j = mcols[[traits[[i]]]][2],
                           value = as_paragraph(as_sup("ns")))
      }
      anovaba <- merge_at(anovaba, 1, mcols[[traits[i]]], "header")
    }
    anovaba <- bold(anovaba, part = "header")
    anovaba <- add_header_row(anovaba,
                              colwidths = c(1, 1, ntraits * 2),
                              values = c("Source", "Df", "Mean.Sq"))
    anovaba <- merge_at(anovaba, 1:2, 1, "header")
    anovaba <- merge_at(anovaba, 1:2, 2, "header")
    anovaba <- align(anovaba, j = 2, align = "right", part = "all")
    anovaba <- align(anovaba, j = 3:(2 + (ntraits * 2)), align = "center",
                     part = "header")
    anovaba <- align(anovaba, j = unlist(lapply(mcols, function(x) x[1])),
                     align = "right", part = "body")
    anovaba <-  align(anovaba, j = unlist(lapply(mcols, function(x) x[2])),
                      align = "left", part = "body")
    anovaba <- autofit(anovaba)
    augreport <- body_add_flextable(augreport, anovaba)

    augreport <- body_add_fpar(augreport,
                               value = fpar(ftext("ns", suppar),
                                            ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))

    # Std. error
    augreport <- body_add_par(augreport, value = "Standard Errors",
                              style = "heading 1")
    SE <- aug.bulk$`Std. Errors`
    colnames(SE) <- make.names(colnames(SE), unique = TRUE)
    SE[, traits] <- lapply(SE[, traits, drop = FALSE],
           round.conditional, digits = round.digits)
    SE <- autofit(flextable(SE))
    SE <- bold(SE, part = "header")
    SE <- align(SE, j = 2:(ntraits + 1), align = "right", part = "all")
    augreport <- body_add_flextable(augreport, SE)

    # CD
    augreport <- body_add_par(augreport,
                              value = paste("Critical Difference (",
                                            aug.bulk$alpha * 100, "%)",
                                            sep = ""),
                              style = "heading 1")
    CD <- aug.bulk$CD
    colnames(CD) <- make.names(colnames(CD), unique = TRUE)
    CD[, traits] <- lapply(CD[, traits, drop = FALSE],
                 round.conditional, digits = round.digits)
    CD <- autofit(flextable(CD))
    CD <- bold(CD, part = "header")
    CD <- align(CD, j = 2:(ntraits + 1), align = "right", part = "all")
    augreport <- body_add_flextable(augreport, CD)

    # CV
    augreport <- body_add_par(augreport, value = "Coefficient of Variance",
                              style = "heading 1")
    CV <- aug.bulk$CV
    CV$CV <- round.conditional(CV$CV, digits = round.digits)
    CV <- autofit(flextable(CV))
    CV <- bold(CV, part = "header")
    CV <- align(CV, j = 2, align = "right", part = "all")
    augreport <- body_add_flextable(augreport, CV)

    # Overall adj. mean
    augreport <- body_add_par(augreport, value = "Overall Adjusted Mean",
                              style = "heading 1")
    oadjmean <- aug.bulk$`Overall adjusted mean`
    oadjmean$Overall.adjusted.mean <-
      round.conditional(oadjmean$Overall.adjusted.mean,
                        digits = round.digits)
    oadjmean <- autofit(flextable(oadjmean))
    oadjmean <- bold(oadjmean, part = "header")
    oadjmean <- align(oadjmean, j = 2, align = "right", part = "all")
    augreport <- body_add_flextable(augreport, oadjmean)

    # Check statistics
    augreport <- body_add_par(augreport, value = "Check Statistics",
                              style = "heading 1")
    for (i in seq_along(aug.bulk$`Check statistics`)) {
      augreport <- body_add_par(augreport,
                                value = names(aug.bulk$`Check statistics`)[i],
                                style = "heading 2")
      chkout <- aug.bulk$`Check statistics`[[i]]
      chkout[, c("Means", "SE", "Min", "Max")] <-
        lapply(chkout[, c("Means", "SE", "Min", "Max")], round.conditional,
               digits = round.digits)
      chkout <- autofit(flextable(chkout))
      chkout <- bold(chkout, part = "header")
      chkout <- align(chkout, j = 3:6, align = "right", part = "all")
      augreport <- body_add_flextable(augreport, chkout)
    }

    # Descriptive statistics
    if (!is.null(aug.bulk$`Descriptive statistics`)){
      augreport <- body_add_par(augreport, value = "Descriptive Statistics",
                                style = "heading 1")
      descout <- aug.bulk$`Descriptive statistics`
      descout <- descout[, setdiff(colnames(descout),
                                   c("Skewness_Pr(>F)", "Kurtosis_Pr(>F)"))]
      descols <- c("Mean", "Std.Error", "Std.Deviation", "Min",
                   "Max", "Skewness", "Kurtosis")
      descout[, descols] <- lapply(descout[, descols], round.conditional,
                                   digits = round.digits)
      nsindex1 <- which(descout[, 9] == "ns")
      nsindex2 <- which(descout[, 11] == "ns")
      descout <- flextable(descout)
      if (!is.null(nsindex1)) {
        descout <- compose(descout, part = "body", i = nsindex1, j = 9,
                           value = as_paragraph(as_sup("ns")))
      }
      if (!is.null(nsindex2)) {
        descout <- compose(descout, part = "body", i = nsindex2, j = 11,
                           value = as_paragraph(as_sup("ns")))
      }
      descout <- merge_at(descout, 1, 8:9, "header")
      descout <- merge_at(descout, 1, 10:11, "header")
      descout <- bold(descout, part = "header")
      descout <- align(descout, j = 2:7, align = "right", part = "all")
      descout <- align(descout, j = c(8, 10), align = "right", part = "body")
      descout <- align(descout, j = c(8, 10), align = "center", part = "header")
      descout <- autofit(descout)
      augreport <- body_add_flextable(augreport, descout)

      augreport <- body_add_fpar(augreport,
                                 value = fpar(ftext("ns", suppar),
                                              ftext(" P > 0.05; * P <= 0.05; ** P <= 0.01")))
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
        if (!is.null(aug.bulk$warnings$`Freq. dist`[[traits[i]]])) {
          wlist <- lapply(aug.bulk$warnings$`Freq. dist`[[traits[i]]],
                          function(wtext) fpar(ftext(wtext),
                                               fp_p = fp_par(padding.bottom = 2,
                                                             word_style = "Warning")))

          attributes(wlist) <- list(class = c("block_list", "block"))
          augreport <- body_add_blocks(augreport, blocks = wlist)
          rm(wlist)
        }

      }
    }

    # GVA
    if (!is.null(aug.bulk$`Genetic variability analysis`)) {
      augreport <- body_add_par(augreport,
                                value = "Genetic Variability Analysis",
                                style = "heading 1")
      GVA <- aug.bulk$`Genetic variability analysis`
      gvacols <- c("Mean", "PV", "GV", "EV", "GCV","PCV","ECV",
                   "hBS","GA", "GAM")
      GVA[, gvacols] <- lapply(GVA[, gvacols], round.conditional,
                               digits = round.digits)

      if (!is.null(aug.bulk$warnings$GVA)) {
        gwstring1 <- "may not be appropriate for this trait"
        gwstring2 <- "Negative GV detected"

        if (any(grepl(paste(c(gwstring1, gwstring2), collapse = "|"),
                      aug.bulk$warnings$GVA))) {
          new_trait <- GVA$Trait
          gwhltv <- rep("", length(new_trait))

          if (grepl(gwstring1, aug.bulk$warnings$GVA)) {
            gwhlt1 <- names(sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring1, gvaw))))
            gwhltv[which(new_trait %in% gwhlt1)] <-
              paste( gwhltv[which(new_trait %in% gwhlt1)], "\u2020", sep = "")
          }
          if (grepl(gwstring2, aug.bulk$warnings$GVA)) {
            gwhlt2 <- names(sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring2, gvaw))))
            gwhltv[which(new_trait %in% gwhlt2)] <-
              paste( gwhltv[which(new_trait %in% gwhlt2)], "\u2021", sep = "")
          }
          gwhltv <- stringi::stri_pad_right(gwhltv, width = max(nchar(gwhltv)),
                                            pad = "\u00A0")

          new_trait <- paste(stringi::stri_pad_right(GVA$Trait,
                                                     width = max(nchar(GVA$Trait)),
                                                     pad = "\u00A0"),
                             gwhltv)
          GVA$Trait <- new_trait
        }

      }

      GVA <- flextable(GVA)
      GVA <- bold(GVA, part = "header")
      GVA <- align(GVA, j = c(2:6, 8, 10:11, 13:14),
                   align = "right", part = "all")
      GVA <- autofit(GVA)
      augreport <- body_add_flextable(augreport, GVA)

      if (!is.null(aug.bulk$warnings$GVA)) {
        if (grepl(gwstring1, aug.bulk$warnings$GVA)) {
          augreport <-
            body_add_par(augreport,
                         value = paste(c("\u2020 P-value for \"Treatment: Test\" is > 0.05. ",
                                         "Genetic variability analysis may not be appropriate for this trait."),
                                       collapse = ""),
                         style = "Warning")
        }
        if (grepl(gwstring2, aug.bulk$warnings$GVA)) {
          augreport <-
            body_add_par(augreport,
                         value = paste(c("\u2021 Negative GV detected. ",
                                         "GCV, GCV category, hBS, hBS category, GA, GAM and GAM category could not be computed."),
                                       collapse = ""),
                         style = "Warning")
        }
      }
    }

    # GVA plots
    if (any(unlist(lapply(aug.bulk$`GVA plots`, function(x) !is.null(x))))) {

      augreport <- body_add_par(augreport,
                                value = "Genetic Variablity Analysis Plots",
                                style = "heading 1")

      if (!is.null(aug.bulk$`GVA plots`$`Phenotypic and Genotypic CV`)) {
        augreport <- body_add_par(augreport,
                                  value = "Phenotypic and Genotypic Coefficient of Variability",
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
        augreport <- body_add_par(augreport,
                                  value = "Genetic Advance Over Mean",
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
    adj.means[, traits] <- sapply(adj.means[, traits], round.conditional,
                                  digits = round.digits)
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

      adj.means[, neg_traits] <-
        sapply(neg_traits,
               function(nt) paste(adj.means[, nt],
                                  ifelse(adj.means$Treatment %in% neg_df[[nt]],
                                         "\u2020", "\u00A0\u00A0"),
                                  sep = "\u00A0"))

    }
    adj.means <- flextable(adj.means)
    adj.means <- bold(adj.means, part = "header")
    adj.means <- align(adj.means, j = 3:(length(traits) + 2),
                 align = "right", part = "all")
    adj.means <- autofit(adj.means)
    augreport <- body_add_flextable(augreport, adj.means)

    if (any(grepl(wstring2, aug.bulk$warnings))) {
      augreport <- body_add_par(augreport, value = neg_msg, style = "Normal")
    }

    # Warnings
    if (!all( unlist(lapply(aug.bulk$warnings, is.null)))) {
      augreport <- body_add_par(augreport, value = "Warnings",
                                style = "heading 1")

      if (!is.null(aug.bulk$warnings$Model)) {
        augreport <- body_add_par(augreport, value = "Model",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$Model)) {
          augreport <- body_add_par(augreport,
                                    value = names(aug.bulk$warnings$Model)[i],
                                    style = "heading 4")
          wlist <- lapply(aug.bulk$warnings$Model[[i]],
                          function(wtext) fpar(ftext(wtext),
                                               fp_p = fp_par(padding.bottom = 2,
                                                             word_style = "Code")))

          attributes(wlist) <- list(class = c("block_list", "block"))
          augreport <- body_add_blocks(augreport, blocks = wlist)
          rm(wlist)
        }
      }

      if (!is.null(aug.bulk$warnings$`Freq. dist`)) {
        augreport <- body_add_par(augreport,
                                  value = "Frequency Distribution",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$`Freq. dist`)) {
          augreport <- body_add_par(augreport,
                                    value = names(aug.bulk$warnings$`Freq. dist`)[i],
                                    style = "heading 4")
          wlist <- lapply(aug.bulk$warnings$`Freq. dist`[[i]],
                          function(wtext) fpar(ftext(wtext),
                                               fp_p = fp_par(padding.bottom = 2,
                                                             word_style = "Code")))

          attributes(wlist) <- list(class = c("block_list", "block"))
          augreport <- body_add_blocks(augreport, blocks = wlist)
          rm(wlist)
        }
      }
      if (!is.null(aug.bulk$warnings$GVA)) {
        augreport <- body_add_par(augreport,
                                  value = "Genetic Variablity Analysis",
                                  style = "heading 2")
        for (i in seq_along(aug.bulk$warnings$GVA)) {
          augreport <- body_add_par(augreport,
                                    value = names(aug.bulk$warnings$GVA)[i],
                                    style = "heading 4")
          wlist <- lapply(aug.bulk$warnings$GVA[[i]],
                          function(wtext) fpar(ftext(wtext),
                                               fp_p = fp_par(padding.bottom = 2,
                                                             word_style = "Code")))

          attributes(wlist) <- list(class = c("block_list", "block"))
          augreport <- body_add_blocks(augreport, blocks = wlist)
          rm(wlist)
        }
      }
    }

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

    # Index
    index <- c("Details", "ANOVA, Treatment Adjusted", "ANOVA, Block Adjusted",
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

    checks <- aug.bulk$Details$`Check treatments`

    addWorksheet(wb, sheetName = "Details", gridLines = FALSE)
    writeDataTable(wb, sheet = "Details", x = Details,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    setColWidths(wb, sheet = "Details", cols = 1:ncol(Details), widths = "auto")
    if (any(grepl(wstring1, unlist(aug.bulk$warnings)))) {
      dups <- aug.bulk$Means[!(aug.bulk$Means$Treatment %in% checks), ]$Treatment
      dups <- dups[duplicated(dups)]
      dups <- aug.bulk$Means[aug.bulk$Means$Treatment %in% dups, c("Treatment", "Block")]
      rownames(dups) <- NULL
      writeData(wb, sheet = "Details", xy = c("A", 10),
                x = "Following test treatments are replicated.",
                borders = "none")
      addStyle(wb,  sheet = "Details",
               style = createStyle(fontColour  = "#C00000"),
               rows = 10, cols = 1, stack = FALSE, gridExpand = TRUE)
      writeDataTable(wb, sheet = "Details", x = dups, xy = c("A", 12),
                     colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                     tableStyle = "TableStyleLight1", withFilter = FALSE,
                     bandedRows = FALSE)
    }
    setColWidths(wb, sheet = "Details", cols = 1,
                 widths = max(nchar(Details$Item)) + 5)

    # ANOVA, TA
    anovata <- aug.bulk$`ANOVA, Treatment Adjusted`
    anovata <- anovata[, setdiff(colnames(anovata),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    colnames(anovata) <- make.names(colnames(anovata), unique = TRUE)
    anovata_sig <- anovata[, c("Source", "Df",
                               paste(traits, "_sig", sep = ""))]
    colnames(anovata_sig) <- gsub("_sig", "", colnames(anovata_sig))
    colnames(anovata) <- gsub("_Mean.Sq", "", colnames(anovata))
    anovata <- anovata[, c("Source", "Df", traits)]

    addWorksheet(wb, sheetName = "ANOVA, Treatment Adjusted", gridLines = FALSE)
    writeData(wb, sheet = "ANOVA, Treatment Adjusted",
              x = "Mean.Sq", startCol = 3, startRow = 1, headerStyle = hs,
              borders = "columns")
    writeDataTable(wb, sheet = "ANOVA, Treatment Adjusted",
                   x = anovata, startRow = 2,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(numFmt = "0"),
             rows = 3:7, cols = 2, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(halign = "right"),
             rows = 2, cols = 2:(ntraits + 2), stack = TRUE, gridExpand = TRUE)
    for(j in seq_along(traits)) {
      for (i in 1:4) {
          col <- which(colnames(anovata) == traits[j])
          if (anovata_sig[i, col] == "*") {
            addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = ssstyle,
                     rows = i + 2, cols = col, stack = FALSE)
          }
          if (anovata_sig[i, col] == "**") {
            addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = dsstyle,
                     rows = i + 2, cols = col, stack = FALSE)
          }
          if (anovata_sig[i, col] == "ns") {
            addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = nsstyle,
                     rows = i + 2, cols = col, stack = FALSE)
          }
      }
    }
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted", style = padstyle,
             rows = 7, cols = 3:(ntraits + 2), stack = FALSE)
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted",
                 cols = 1:ncol(anovata), widths = "auto")
    addStyle(wb,  sheet = "ANOVA, Treatment Adjusted",
             style = createStyle(halign = "center", textDecoration = "bold"),
             rows = 1, cols = 3, stack = FALSE, gridExpand = TRUE)
    mergeCells(wb, sheet = "ANOVA, Treatment Adjusted",
               cols = 3:(ntraits + 2), rows = 1)
    writeData(wb, sheet = "ANOVA, Treatment Adjusted", xy = c("A", 8),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")
    setColWidths(wb, sheet = "ANOVA, Treatment Adjusted", cols = 1,
                 widths = max(nchar(anovata$Source)) + 5)

    # ANOVA, BA
    anovaba <- aug.bulk$`ANOVA, Block Adjusted`
    anovaba <- anovaba[, setdiff(colnames(anovaba),
                                 paste(traits, "Pr(>F)", sep = "_"))]
    colnames(anovaba) <- make.names(colnames(anovaba), unique = TRUE)
    anovaba_sig <- anovaba[, c("Source", "Df",
                               paste(traits, "_sig", sep = ""))]
    colnames(anovaba_sig) <- gsub("_sig", "", colnames(anovaba_sig))
    colnames(anovaba) <- gsub("_Mean.Sq", "", colnames(anovaba))
    anovaba <- anovaba[, c("Source", "Df", traits)]

    addWorksheet(wb, sheetName = "ANOVA, Block Adjusted", gridLines = FALSE)
    writeData(wb, sheet = "ANOVA, Block Adjusted",
              x = "Mean.Sq", startCol = 3, startRow = 1, headerStyle = hs,
              borders = "columns")
    writeDataTable(wb, sheet = "ANOVA, Block Adjusted",
                   x = anovaba, startRow = 2,
                   colNames = TRUE, rowNames = FALSE, headerStyle = hs,
                   tableStyle = "TableStyleLight1", withFilter = FALSE,
                   bandedRows = FALSE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(numFmt = "0"),
             rows = 3:8, cols = 2, stack = FALSE, gridExpand = TRUE)
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(halign = "right"),
             rows = 2, cols = 2:(ntraits + 2), stack = TRUE, gridExpand = TRUE)
    for(j in seq_along(traits)) {
      for (i in 1:5) {
        col <- which(colnames(anovaba) == traits[j])
        if (anovaba_sig[i, col] == "*") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = ssstyle,
                   rows = i + 2, cols = col, stack = FALSE)
        }
        if (anovaba_sig[i, col] == "**") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = dsstyle,
                   rows = i + 2, cols = col, stack = FALSE)
        }
        if (anovaba_sig[i, col] == "ns") {
          addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = nsstyle,
                   rows = i + 2, cols = col, stack = FALSE)
        }
      }
    }
    addStyle(wb,  sheet = "ANOVA, Block Adjusted", style = padstyle,
             rows = 8, cols = 3:(ntraits + 2), stack = FALSE)
    setColWidths(wb, sheet = "ANOVA, Block Adjusted",
                 cols = 1:ncol(anovaba), widths = "auto")
    addStyle(wb,  sheet = "ANOVA, Block Adjusted",
             style = createStyle(halign = "center", textDecoration = "bold"),
             rows = 1, cols = 3, stack = FALSE, gridExpand = TRUE)
    mergeCells(wb, sheet = "ANOVA, Block Adjusted",
               cols = 3:(ntraits + 2), rows = 1)
    writeData(wb, sheet = "ANOVA, Block Adjusted", xy = c("A", 9),
              x = "\u207f\u02e2 P > 0.05; * P <= 0.05; ** P <= 0.01",
              borders = "none")
    setColWidths(wb, sheet = "ANOVA, Block Adjusted", cols = 1,
                 widths = max(nchar(anovaba$Source)) + 5)

    # Std. Error
    SE <- aug.bulk$`Std. Errors`
    SE[, traits] <- sapply(SE[, traits], as.numeric)
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
    CD[, traits] <- sapply(CD[, traits], as.numeric)
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
                                            function(chkst) max(nchar(chkst$Max),
                                                                nchar(chkst$Max),
                                                                3)))) + 5)
    rm(indexdf)

    # Descriptive statistics
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
                   rows = row1 + length(aug.bulk$warnings$`Freq. dist`[[traits[i]]]),
                   cols = 10, stack = FALSE, gridExpand = TRUE)
        }
        rm(row1)
      }
      setColWidths(wb, sheet = "Frequency Distribution",
                   cols = 1, widths = "auto")
      rm(indexdf)
    }

    # GVA
    if (!is.null(aug.bulk$`Genetic variability analysis`)) {
      GVA <- aug.bulk$`Genetic variability analysis`

      if (!is.null(aug.bulk$warnings$GVA)) {
        gwstring1 <- "may not be appropriate for this trait"
        gwstring2 <- "Negative GV detected"
        if (any(grepl(paste(c(gwstring1, gwstring2), collapse = "|"),
                      aug.bulk$warnings$GVA))) {
          new_trait <- GVA$Trait
          gwhltv <- rep("", length(new_trait))

          if (grepl(gwstring1, aug.bulk$warnings$GVA)) {
            gwhlt1 <- names(sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring1, gvaw))))
            gwhltv[which(new_trait %in% gwhlt1)] <-
              paste( gwhltv[which(new_trait %in% gwhlt1)], "\u2020", sep = "")
          }
          if (grepl(gwstring2, aug.bulk$warnings$GVA)) {
            gwhlt2 <- names(sapply(aug.bulk$warnings$GVA,
                                   function(gvaw) any(grepl(gwstring2, gvaw))))
            gwhltv[which(new_trait %in% gwhlt2)] <-
              paste( gwhltv[which(new_trait %in% gwhlt2)], "\u2021", sep = "")
          }
          gwhltv <- stringi::stri_pad_right(gwhltv, width = max(nchar(gwhltv)),
                                            pad = "\u00A0")

          new_trait <- paste(stringi::stri_pad_right(GVA$Trait,
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
        if (grepl(gwstring1, aug.bulk$warnings$GVA)) {
          writeData(wb, sheet = "Genetic Variability Analysis",
                    x = paste(c("\u2020 P-value for \"Treatment: Test\" is > 0.05. ",
                                "Genetic variability analysis may not be appropriate for this trait."),
                              collapse = ""),
                    startCol = "A", startRow = row1, borders = "none")
          addStyle(wb,  sheet = "Genetic Variability Analysis",
                   style = createStyle(fontColour  = "#C00000"),
                   rows = row1, cols = 1, stack = FALSE, gridExpand = TRUE)
          row1 <- row1 + 1
        }
        if (grepl(gwstring2, aug.bulk$warnings$GVA)) {
          writeData(wb, sheet = "Genetic Variability Analysis",
                    x = paste(c("\u2021 Negative GV detected. ",
                                "GCV, GCV category, hBS, hBS category, GA, GAM and GAM category could not be computed."),
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

    # Adjusted Means
    adj.means <- aug.bulk$Means
    colnames(adj.means) <- make.names(colnames(adj.means), unique = TRUE)

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

    # Warnings
    if (!all( unlist(lapply(aug.bulk$warnings, is.null)))) {
      addWorksheet(wb, sheetName = "Warnings", gridLines = FALSE)

      row1 <- 1

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

    saveWorkbook(wb = wb, file = target, overwrite = TRUE)

  }

  message(paste("File created at", target))

}
