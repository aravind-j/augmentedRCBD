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

#' Analysis of Augmented Randomised Complete Block Design for Multiple
#' Traits/Characters
#'
#' \code{augmentedRCBD.bulk} is a wrapper around the functions
#' \code{augmentedRCBD}, \code{describe.augmentedRCBD},
#' \code{freqdist.augmentedRCBD} and \code{gva.augmentedRCBD}. It will carry out
#' these analyses for multiple traits/characters from the input data as a data
#' frame object.
#'
#' @param data The data as a data frame object. The data frame should possess
#'   columns specifying the block, treatment and multiple traits/characters.
#' @param block Name of column specifying the blocks in the design as a
#'   character string.
#' @param treatment Name of column specifying the treatments as a character
#'   string.
#' @param traits Name of columns specifying the multiple traits/characters as a
#'   character vector.
#' @param checks Character vector of the checks present in \code{treatment}
#'   levels. If not specified, checks are inferred from the data on the basis of
#'   number of replications of treatments/genotypes.
#' @param alpha Type I error probability (Significance level) to be used for
#'   multiple comparisons.
#' @param describe If \code{TRUE}, descriptive statistics will be computed.
#'   Default is \code{TRUE}.
#' @param freqdist If \code{TRUE}, frequency distributions be plotted. Default
#'   is \code{TRUE}.
#' @param gva If \code{TRUE}, genetic variability analysis will be done. Default
#'   is \code{TRUE}.
#' @param check.col The colour(s) to be used to highlight check values in the
#'   plot as a character vector. Must be valid colour values in R (named colours,
#'  hexadecimal representation, index of colours [\code{1:8}] in default R
#'  `palette()` etc.).
#' @param console If \code{TRUE}, output will be printed to console. Default is
#'   \code{TRUE}.
#'
#' @return A list of class \code{augmentedRCBD.bulk} containing the following
#'   components:  \item{\code{Details}}{Details of the augmented design used and
#'   the traits/characters.} \item{\code{ANOVA, Treatment Adjusted}}{A data
#'   frame of mean sum of squares of the specified traits from treatment
#'   adjusted ANOVA.} \item{\code{ANOVA, Block Adjusted}}{A data frame of mean
#'   sum of squares of the specified traits from block adjusted ANOVA}
#'   \item{\code{Means}}{A data frame of the adjusted means of the treatments
#'   for the specified traits.} \item{\code{alpha}}{Type I error probability
#'   (Significance level) used.} \item{\code{Std. Errors}}{A data frame of
#'   standard error of difference between various combinations for the specified
#'   traits.} \item{\code{CD}}{A data frame of critical difference (at the
#'   specified alpha) between various combinations for the specified traits.}
#'   \item{\code{Overall adjusted mean}}{A data frame of the overall adjusted
#'   mean for the specified traits.} \item{\code{CV}}{A data frame of the
#'   coefficient of variance for the specified traits.} \item{\code{Descriptive
#'   statistics}}{A data frame of descriptive statistics for the specified
#'   traits.} \item{\code{Frequency distribution}}{A list of ggplot2 plot grobs
#'   of the frequency distribution plots.} \item{\code{Genetic variability
#'   analysis}}{A data frame of genetic variability statistics for the specified
#'   traits.} \item{\code{GVA plots}}{A list of three ggplot2 objects with the
#'   plots for (a) Phenotypic and Genotypic CV, (b) Broad sense heritability and
#'   (c) Genetic advance over mean} \item{\code{warnings}}{A list of warning
#'   messages (if any) captured during model fitting and frequency distribution
#'   plotting.}
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
#'                            console = TRUE)
#'
#' # Frequency distribution plots
#' lapply(bout$`Frequency distribution`, plot)
#'
#' # GVA plots
#' bout$`GVA plots`
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}},
#'   \code{\link[augmentedRCBD]{describe.augmentedRCBD}},
#'   \code{\link[augmentedRCBD]{freqdist.augmentedRCBD}},
#'   \code{\link[augmentedRCBD]{gva.augmentedRCBD}}
#'
#' @import ggplot2
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate_if
#' @importFrom stringi stri_pad_right
#' @importFrom grDevices nclass.FD
#' @importFrom grDevices nclass.scott
#' @importFrom grDevices nclass.Sturges
#' @importFrom stats na.omit
#' @export
augmentedRCBD.bulk <- function(data, block, treatment, traits, checks = NULL,
                               alpha = 0.05, describe = TRUE,
                               freqdist = TRUE, gva = TRUE,
                               check.col = "red", console = TRUE) {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object')
  }
  
  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }
  
  # check if block column present in data
  if (!(block %in% colnames(data))) {
    stop(paste('Column ', block,
               ' specified as the block column is not present in "data"',
               sep = ""))
  }
  # check if treatment column present in data
  if (!(treatment %in% colnames(data))) {
    stop(paste('Column ', treatment,
               ' specified as the treatment column is not present in "data"',
               sep = ""))
  }
  # check if trait columns present in data
  if (FALSE %in% (traits %in% colnames(data))) {
    stop(paste('The following column(s) specified as trait columns not present in "data":\n',
               paste(traits[!(traits %in% colnames(data))], collapse = ", "),
               sep = ""))
  }
  # check for missing values
  missvcols <- unlist(lapply(data[, traits], function(x) TRUE %in% is.na(x)))
  if (TRUE %in% missvcols) {
    stop(paste('The following column(s) in "data" have missing values:\n',
               paste(names(missvcols[missvcols]), collapse = ", ")))
  }
  # check if trait columns are of type numeric/integer
  inttraitcols <- unlist(lapply(data[, traits],
                                function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
  if (TRUE %in% inttraitcols) {
    stop(paste('The following trait column(s) in "data" are not of type numeric:\n',
               paste(names(inttraitcols[inttraitcols]), collapse = ", ")))
  }
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha < 1)')
  }

  # check.col
  if (!all(iscolour(check.col))) {
    stop('"check.col" specifies invalid colour(s)')
  }

  # convert to factor
  data[, block] <- as.factor(as.character(data[, block]))
  data[, treatment] <- as.factor(as.character(data[, treatment]))

  # Fix treatment order so that checks are in the beginning
  if (!missing(checks) && !is.null(checks)) { # i.e. checks are specified
    #if (!is.null(checks)) {
    treatmentorder <- data.frame(table(treatment = data[, treatment],
                                       block = data[, block]))
    treatmentorder[treatmentorder$Freq != 0, ]$Freq <- 1
    treatmentorder <- reshape2::dcast(treatmentorder, treatment ~ block,
                                      value.var = "Freq")
    treatmentorder$Freq <- rowSums(subset(treatmentorder,
                                          select = -c(treatment)))
    treatmentorder <- treatmentorder[, c("treatment", "Freq")]

    nblocks <- length(levels(data[, block]))
    rownames(treatmentorder) <- NULL


    # check if "checks" are present in all the blocks
    if (!(all(treatmentorder[treatmentorder$treatment %in% checks, ]$Freq == nblocks))) {
      print(treatmentorder)
      stop(paste('"checks" are not replicated across all the blocks (',
                 nblocks, ')', sep = ""))
    }

    tests <- levels(data[, treatment])[!(levels(data[, treatment]) %in% checks)]
    if (!all(table(droplevels(data[, treatment][data[, treatment] %in% tests])) == 1)) {
      warning("Test treatments are replicated")
    }

  } else {# i.e. "checks" is not specified
    treatmentorder <- data.frame(table(treatment = data[, treatment],
                                       block = data[, block]))
    treatmentorder[treatmentorder$Freq != 0, ]$Freq <- 1
    treatmentorder <- reshape2::dcast(treatmentorder, treatment ~ block,
                                      value.var = "Freq")
    treatmentorder$Freq <- rowSums(subset(treatmentorder,
                                          select = -c(treatment)))
    treatmentorder <- treatmentorder[, c("treatment", "Freq")]
    treatmentorder <- treatmentorder[with(treatmentorder,
                                          order(-Freq, treatment)), ]
    nblocks <- length(levels(data[, block]))
    rownames(treatmentorder) <- NULL

    # check if the checks can be inferred.
    # i.e. if any treatments are present in all the blocks
    if (!(nblocks %in% treatmentorder$Freq)) {
      print(treatmentorder)
      stop(paste("Checks cannot be inferred as none of the treatments are",
                 "replicated across all the blocks (",
                 nblocks, ")", sep = ""))
    }

    checks <- as.character(treatmentorder[treatmentorder$Freq == nblocks, ]$treatment)
    tests <- as.character(treatmentorder[treatmentorder$Freq != nblocks, ]$treatment)

    tests <- levels(data[, treatment])[!(levels(data[, treatment]) %in% checks)]
    if (!all(table(droplevels(data[, treatment][data[, treatment] %in% tests])) == 1)) {
      warning("Test treatments are replicated")
    }
  }

  if (length(check.col) != 1) {
   if (length(check.col) != length(checks)) {
     stop('"checks" and "check.col" are of unequal lengths')
   }
 }

  output <- vector("list", length(traits))
  names(output) <- traits

  warn <- NULL
  for (i in seq_along(traits)) {

    withCallingHandlers({
      output[[i]] <- augmentedRCBD(block = data[, block],
                                   treatment = data[, treatment],
                                   y = data[, traits[i]], checks = checks,
                                   method.comp = "none", alpha = alpha,
                                   group = FALSE, console = FALSE,
                                   simplify = TRUE)
    }, warning = function(w) {
      warn <<- append(warn, traits[i])
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    cat(paste("\nANOVA for ", traits[i], " computed (", i,  "/",
              length(traits), ")\n", sep = ""))
    gc()
  }

  # Details
  Details <- output[[1]]$Details
  Details <- append(Details, list(`Number of Traits` = length(traits),
                                  Traits = traits))

  # ANOVA table
  anovata <- lapply(output, function(x) x$`ANOVA, Treatment Adjusted`)
  anovaba <- lapply(output, function(x) x$`ANOVA, Block Adjusted`)

  if (!all(unlist(lapply(X = anovata, FUN = is.data.frame)))) {
    anovata <- lapply(anovata, function(x) data.frame(x[[1]]))
    anovata <- lapply(anovata, function(x) cbind(Source = rownames(x), x))
  }
  if (!all(unlist(lapply(X = anovaba, FUN = is.data.frame)))) {
    anovaba <- lapply(anovaba, function(x) data.frame(x[[1]]))
    anovaba <- lapply(anovaba, function(x) cbind(Source = rownames(x), x))
  }

  anovata <- Map(cbind, anovata, Trait = names(anovata))
  anovaba <- Map(cbind, anovaba, Trait = names(anovaba))

  anovata <- lapply(anovata, function(x) dplyr::mutate_if(x, is.factor,
                                                          as.character))
  anovaba <- lapply(anovaba, function(x) dplyr::mutate_if(x, is.factor,
                                                          as.character))

  anovata <- dplyr::bind_rows(anovata)
  anovaba <- dplyr::bind_rows(anovaba)

  anovata$sig <- ifelse(anovata$Pr..F. <= 0.01, "**",
                        ifelse(anovata$Pr..F. <= 0.05, "*", "ns"))
  anovaba$sig <- ifelse(anovaba$Pr..F. <= 0.01, "**",
                        ifelse(anovaba$Pr..F. <= 0.05, "*", "ns"))

  anovata$Source <- trimws(anovata$Source)
  anovaba$Source <- trimws(anovaba$Source)

  anovata$sig[is.na(anovata$sig)] <- ""
  anovaba$sig[is.na(anovaba$sig)] <- ""

  # Round off the MSS values according to value
  anovata$MSS <- round.conditional(anovata$Mean.Sq)
  anovaba$MSS <- round.conditional(anovaba$Mean.Sq)

  anovata$MSS <- paste(anovata$MSS, stringi::stri_pad_right(anovata$sig, 3),
                       sep = " ")
  anovaba$MSS <- paste(anovaba$MSS, stringi::stri_pad_right(anovaba$sig, 3),
                       sep = " ")

  anovataout <- dcast(anovata, Source + Df ~ Trait, value.var = "MSS")
  rm(anovata)

  anovabaout <- dcast(anovaba, Source + Df ~ Trait, value.var = "MSS")
  rm(anovaba)

  anovataout$sl <- c(1, 5, 2, 3, 4)
  anovataout <- dplyr::arrange(anovataout, sl)

  anovabaout$sl <- c(5, 6, 1, 2, 4, 3)
  anovabaout <- dplyr::arrange(anovabaout, sl)

  anovataout$sl <- NULL
  anovabaout$sl <- NULL

  # Adjusted means
  adjmeans <- lapply(output, function(x) x$Means)
  adjmeans <- Map(cbind, adjmeans, Trait = names(adjmeans))
  adjmeans <- lapply(adjmeans, function(x) dplyr::mutate_if(x, is.factor,
                                                            as.character))
  adjmeans <- dplyr::bind_rows(adjmeans)
  adjmeans <- reshape2::dcast(adjmeans, Treatment ~ Trait,
                              value.var = "Adjusted Means",
                              fun.aggregate = mean)
  adjmeans[, traits] <- lapply(adjmeans[, traits], round.conditional)


  # CV
  cvout <- lapply(output, function(x) x$CV)
  cvout <- lapply(cvout, function(x) data.frame(CV = x))
  cvout <- Map(cbind, Trait = names(cvout), cvout)
  cvout <- lapply(cvout, function(x) dplyr::mutate_if(x, is.factor,
                                                      as.character))
  cvout <- dplyr::bind_rows(cvout)
  cvout$CV <- round.conditional(cvout$CV)

  # overall adj mean
  oadjmean <- lapply(output, function(x) x$`Overall adjusted mean`)
  oadjmean <- lapply(oadjmean,
                     function(x) data.frame(Overall.adjusted.mean = x))
  oadjmean <- Map(cbind, Trait = names(oadjmean), oadjmean)
  oadjmean <- lapply(oadjmean, function(x) dplyr::mutate_if(x, is.factor,
                                                            as.character))
  oadjmean <- dplyr::bind_rows(oadjmean)
  oadjmean$Overall.adjusted.mean <- round.conditional(oadjmean$Overall.adjusted.mean)

  # SE and CD
  secd <- lapply(output, function(x) x$`Std. Errors`)
  secd <- Map(cbind, Trait = names(secd), secd)
  secd <- lapply(secd, function(x) cbind(Comparison = rownames(x), x))
  secd <- lapply(secd, function(x) dplyr::mutate_if(x, is.factor, as.character))
  secd <- dplyr::bind_rows(secd)
  secd$`Std. Error of Diff.` <- round.conditional(secd$`Std. Error of Diff.`)
  secd[, grepl("CD \\(", colnames(secd))] <- round.conditional(secd[, grepl("CD \\(", colnames(secd))])

  seout <- reshape2::dcast(secd, Comparison ~ Trait,
                           value.var = "Std. Error of Diff.")
  cdout <- reshape2::dcast(secd, Comparison ~ Trait,
                           value.var = colnames(secd)[grepl("CD \\(", colnames(secd))])

  # Descriptive statistics
  descout <- NULL
  if(describe == TRUE) {
    descout <- vector("list", length(traits))
    names(descout) <- traits

    for (i in seq_along(traits)) {
      descout[[i]] <- describe.augmentedRCBD(output[[traits[i]]])
    }

    descout <- lapply(descout, function(x) data.frame(x)[1, ])
    descout <- Map(cbind, Trait = names(descout), descout)

    descout <- lapply(descout, function(x) dplyr::mutate_if(x, is.factor,
                                                            as.character))
    descout <- dplyr::bind_rows(descout)

    descout$Skewness.p.value. <- ifelse(descout$Skewness.p.value. <= 0.01, "**",
                                        ifelse(descout$Skewness.p.value. <= 0.05,
                                               "*", "ns"))
    descout$Kurtosis.p.value. <- ifelse(descout$Kurtosis.p.value. <= 0.01, "**",
                                        ifelse(descout$Kurtosis.p.value. <= 0.05,
                                               "*", "ns"))
    desc <- c("Mean", "Std.Error", "Std.Deviation", "Min",
              "Max", "Skewness.statistic.", "Kurtosis.statistic.")
    descout[, desc] <- apply(descout[, desc], MARGIN = 2,
                             FUN = round.conditional)

    colnames(descout) <- c("Trait", "Count", "Mean", "Std.Error",
                           "Std.Deviation", "Min", "Max", "Skewness",
                           "Skewness_sig", "Kurtosis", "Kurtosis_sig")
    descout$Skewness <- paste(descout$Skewness,
                              stringi::stri_pad_right(descout$Skewness_sig, 3),
                              sep = " ")
    descout$Kurtosis <- paste(descout$Kurtosis,
                              stringi::stri_pad_right(descout$Kurtosis_sig, 3),
                              sep = " ")
    descout <- descout[, c("Trait", "Count", "Mean", "Std.Error",
                           "Std.Deviation", "Min", "Max", "Skewness",
                           "Kurtosis")]
  }

  # GVA
  gvaout <- NULL
  gvaplot_cvg <- NULL
  gvaplot_hbsg <- NULL
  gvaplot_gamg <- NULL

  if(gva == TRUE) {
    gvaout <- vector("list", length(traits))
    names(gvaout) <- traits

    for (i in seq_along(traits)) {
      gvaout[[i]] <- gva.augmentedRCBD(output[[traits[i]]])
    }

    gvaout <- lapply(gvaout, function(x) data.frame(x))
    gvaout <- Map(cbind, Trait = names(gvaout), gvaout)

    gvaout <- lapply(gvaout, function(x) dplyr::mutate_if(x, is.factor,
                                                          as.character))
    gvaout <- dplyr::bind_rows(gvaout)

    gvaplot <- gvaout

    gvap <- c("Mean", "PV", "GV", "EV", "GCV", "PCV",  "ECV", "hBS", "GA", "GAM")
    gvaout[, gvap] <- apply(gvaout[, gvap], MARGIN = 2, FUN = round.conditional)

    # GVA plot
    themecustom <- theme(axis.text.x = element_text(color = "black", angle = 45,
                                                    hjust = 1),
                         axis.text.y = element_text(color = "black"))
    # PCV GCV
    gvaplot_cv <- reshape2::melt(gvaplot, id.vars = c("Trait"),
                                 measure.vars = c("PCV", "GCV"))
    gvaplot_2 <- gvaplot[, c("Trait", "PCV", "GCV")]
    gvaplot_2$max <- apply(gvaplot_2[, c("PCV", "GCV")], 1, function(x) max(x))
    gvaplot_2$min <- apply(gvaplot_2[, c("PCV", "GCV")], 1, function(x) min(x))

    gvacat <- data.frame(xmin = 0,
                         xmax = 0.10,
                         ymin = c(-Inf, 10, 20),
                         ymax = c(10, 20, Inf),
                         Category = as.factor(c("Low", "Medium", "High")))
    gvacat$Category <- factor(gvacat$Category,
                              levels = c("Low", "Medium", "High"))


    gvaplot_cvg <- ggplot(gvaplot_cv, aes(x = Trait, colour = variable,
                                          group = variable)) +
      geom_hline(yintercept = c(10, 20), color = "black", linetype = 3) +
      geom_segment(data = gvaplot_2, aes(x = Trait, xend = Trait,
                                         y = -Inf, yend = min),
                   inherit.aes = F) +
      geom_segment(data = gvaplot_2, aes(x = Trait, xend = Trait,
                                         y = min, yend = max),
                   inherit.aes = F, size = 2, colour = "gray70") +
      geom_point(aes(y = value)) +
      scale_color_manual("Type", values = c("red", "blue")) +
      scale_y_continuous(breaks = seq(0,
                                      ceiling(max(gvaplot_2[, c("PCV", "GCV")])) + 10,
                                      by = 10)) +
      geom_rect(data = gvacat, aes(xmin = xmin, ymin = ymin,
                                   xmax = xmax, ymax = ymax, fill = Category),
                alpha = 0.5, inherit.aes = FALSE) +
      scale_fill_manual(values = c("gray60", "gray30", "gray5")) +
      ylab("Coefficient of variation") +
      theme_bw() + themecustom

    # hBS
    gvacat2 <- data.frame(xmin = 0,
                          xmax = 0.10,
                          ymin = c(-Inf, 30, 60),
                          ymax = c(30, 60, Inf),
                          Category = as.factor(c("Low", "Medium", "High")))
    gvacat2$Category <- factor(gvacat2$Category,
                               levels = c("Low", "Medium", "High"))
    gvaplot_hbs <- reshape2::melt(gvaplot, id.vars = c("Trait"),
                                  measure.vars = "hBS")

    gvaplot_hbsg <- ggplot(gvaplot_hbs, aes(x = Trait, colour = variable,
                                            group = variable)) +
      geom_hline(yintercept = c(30, 60), color = "black", linetype = 3) +
      geom_segment(data = gvaplot_hbs, aes(x = Trait, xend = Trait, y = -Inf,
                                           yend = value),
                   colour = "black") +
      geom_point(aes(y = value), colour = "black") +
      scale_y_continuous(breaks = seq(0, ceiling(max(gvaplot[, "hBS"])) + 10,
                                      by = 10)) +
      geom_rect(data = gvacat2, aes(xmin = xmin, ymin = ymin,
                                    xmax = xmax, ymax = ymax, fill = Category),
                alpha = 0.5, inherit.aes = FALSE) +
      scale_fill_manual(values = c("gray60", "gray30", "gray5")) +
      ylab("Broad sense heritability") +
      theme_bw() + themecustom

    # GAM
    gvaplot_gam <- reshape2::melt(gvaplot, id.vars = c("Trait"),
                                  measure.vars = "GAM")

    gvaplot_gamg <- ggplot(gvaplot_gam, aes(x = Trait, colour = variable,
                                            group = variable)) +
      geom_hline(yintercept = c(10, 20), color = "black", linetype = 3) +
      geom_segment(data = gvaplot_gam, aes(x = Trait, xend = Trait, y = -Inf,
                                           yend = value),
                   colour = "black") +
      geom_point(aes(y = value), colour = "black") +
      scale_y_continuous(breaks = seq(0, ceiling(max(gvaplot[, "GAM"])) + 10,
                                      by = 10)) +
      geom_rect(data = gvacat, aes(xmin = xmin, ymin = ymin,
                                   xmax = xmax, ymax = ymax, fill = Category),
                alpha = 0.5, inherit.aes = FALSE) +
      scale_fill_manual(values = c("gray60", "gray30", "gray5")) +
      ylab("Genetic advance over mean") +
      theme_bw() + themecustom
  }

  gvaplots <- list(`Phenotypic and Genotypic CV` = gvaplot_cvg,
                   `Broad sense heritability` = gvaplot_hbsg,
                   `Genetic advance over mean` = gvaplot_gamg)

  # Freq Dist
  fqout <- NULL
  fqwarn <- NULL
  if (freqdist == TRUE) {
    fqout <- vector("list", length(traits))
    names(fqout) <- traits

    fqwarn <- data.frame(Trait = traits, Message = NA_character_,
                         stringsAsFactors = F)

    fqwarn <- NULL
    for (i in seq_along(traits)) {

      withCallingHandlers({
        fqout[[i]] <- freqdist.augmentedRCBD(output[[traits[i]]],
                                             xlab = traits[i],
                                             check.col = check.col)
      }, warning = function(w) {
        fqwarn <<- append(fqwarn, traits[i])
        fqwarn <<- append(fqwarn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    }
  }


  out <- list(Details = Details, `ANOVA, Treatment Adjusted` = anovataout,
              `ANOVA, Block Adjusted` = anovabaout, Means = adjmeans,
              alpha = alpha, `Std. Errors` = seout, CD = cdout,
              `Overall adjusted mean` = oadjmean,
              `CV` = cvout, `Descriptive statistics` = descout,
              `Frequency distribution` = fqout,
              `Genetic variability analysis` = gvaout,
              `GVA plots` = gvaplots,
              warnings = list(Model = warn, `Freq. dist` = fqwarn))

  # Set Class
  class(out) <- "augmentedRCBD.bulk"

  if (console == TRUE) {
    print.augmentedRCBD.bulk(out)
  }

  return(out)

}

round.conditional <- function(x, digits = 2){
  x <- ifelse(round(x, digits) != 0,
         as.character(round(x, digits)),
         as.character(signif(x, digits)))
  return(x)

}

iscolour <- function(x) {
  sapply(x, function(x) {
    tryCatch(is.matrix(col2rgb(x)),
             error = function(e) FALSE)
  })
}
