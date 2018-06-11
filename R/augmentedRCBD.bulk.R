#' Analysis of Augmented Randomised Complete Block Design for Multiple
#' Traits/Characters
#'
#' \code{augmentedRCBD.bulk} is a wrapper around the functions
#' \code{augmentedRCBD}, \code{describe.augmentedRCBD},
#' \code{freqdist.augmentedRCBD} and \code{gva.augmentedRCBD}. It will carry out
#' these analyses for multiple traits/characters from the input data as a
#' data.frame object.
#'
#' @param data The data as a data.frame object. The data.frame should possess
#'   columns specifying the block, treatment and multiple traits/characters.
#' @param block Name of column specifying the blocks in the design as a
#'   character string.
#' @param treatment Name of column specifying the treatments as a character
#'   string.
#' @param traits Name of columns specifying the treatments as a character
#'   vector.
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
#'   plot as a character vector.
#'
#' @import reshape2
#' @import dplyr
#' @import ggplot2
#' @export
augmentedRCBD.bulk <- function(data, block, treatment, traits, checks = NULL,
                               alpha = 0.05, describe = TRUE,
                               freqdist = TRUE, gva = TRUE,
                               check.col = "red") {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object')
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
  inttraitcols <- unlist(lapply(data[,traits],
                                function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
  if (TRUE %in% inttraitcols) {
    stop(paste('The following trait column(s) in "data" are not of type numeric:\n',
               paste(names(inttraitcols[inttraitcols]), collapse = ", ")))
  }
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha <1)')
  }

  # convert to factor
  data[, block] <- as.factor(data[, block])
  data[, treatment] <- as.factor(data[, treatment])

  if (!missing(checks) && !is.null(checks)) {
  #if (!is.null(checks)) {
    checks <- as.character("checks")
    # checks are present in treatment levels
    if (FALSE %in% c(checks %in% levels(data[, treatment]))) {
      miss <- paste(checks[!(checks %in% levels(data[, treatment]))], collapse = ", ")
      stop(paste("Following check(s) are not present in treatment levels:\n",
                    miss))
    }
  }


  output <- vector("list", length(traits))
  names(output) <- traits

  warn <- NULL
  for (i in 1:length(traits)) {

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

    print(traits[i])
    gc()
  }

  # Details
  Details <- output[[1]]$Details

  # ANOVA table
  anovata <- lapply(output, function(x) x$`ANOVA, Treatment Adjusted`)
  anovaba <- lapply(output, function(x) x$`ANOVA, Block Adjusted`)

  if(!all(unlist(lapply(X = anovata, FUN = is.data.frame)))){
    anovata <- lapply(anovata, function(x) data.frame(x[[1]]))
    anovata <- lapply(anovata, function(x) cbind(Type = "ANOVA, Treatment Adjusted",
                                                 Source = rownames(x), x))
  } else {
    anovata <- lapply(anovata, function(x) cbind(Type = "ANOVA, Treatment Adjusted",
                                                 x))
  }
  if(!all(unlist(lapply(X = anovaba, FUN = is.data.frame)))){
    anovaba <- lapply(anovaba, function(x) data.frame(x[[1]]))
    anovaba <- lapply(anovaba, function(x) cbind(Type = "ANOVA, Block Adjusted",
                                                 Source = rownames(x), x))
  } else {
    anovaba <- lapply(anovaba, function(x) cbind(Type = "ANOVA, Block Adjusted",
                                                 x))
  }

  anovata <- Map(cbind, anovata, Trait = names(anovata))
  anovaba <- Map(cbind, anovaba, Trait = names(anovaba))


  anovata <- lapply(anovata, function(x) dplyr::mutate_if(x, is.factor, as.character))
  anovaba <- lapply(anovaba, function(x) dplyr::mutate_if(x, is.factor, as.character))

  anovata <- dplyr::bind_rows(anovata)
  anovaba <- dplyr::bind_rows(anovaba)

  anovata$sig <- ifelse(anovata$Pr..F. < 0.01, "**",
                        ifelse(anovata$Pr..F. < 0.05, "*", ""))
  anovaba$sig <- ifelse(anovaba$Pr..F. < 0.01, "**",
                        ifelse(anovaba$Pr..F. < 0.05, "*", ""))

  anovatable <- dplyr::bind_rows(anovata, anovaba)
  anovatable$Source <- trimws(anovatable$Source)
  anovatable$sig[is.na(anovatable$sig)] <- ""

  # Round off the MSS values according to value
  anovatable$MSS <- ifelse(round(anovatable$Mean.Sq, 2) != 0,
                           as.character(round(anovatable$Mean.Sq, 2)),
                           sub("\\.$", "", sub(".0+$", "",
                                               sprintf("%f", signif(anovatable$Mean.Sq, 2)))))


  anovaout <- dcast(anovatable, Type + Source + Df ~ Trait, value.var = "MSS")
  anovasig <- dcast(anovatable, Type + Source + Df ~ Trait, value.var = "sig")
  colnames(anovasig) <- c("Type", "Source",
                          paste(setdiff(colnames(anovasig),c("Type", "Source")),
                                "sig", sep = "_"))

  ind <- rbind(names(anovaout),names(anovasig))
  ind <-  unique(as.vector(ind))

  anovaout <- merge(anovaout, anovasig, by = c("Type", "Source"))
  anovaout <- anovaout[, ind]

  rm(anovasig, anovata, anovaba, anovatable)

  # Adjusted means
  adjmeans <- lapply(output, function(x) x$Means)
  adjmeans <- Map(cbind, adjmeans, Trait = names(adjmeans))
  adjmeans <- lapply(adjmeans, function(x) dplyr::mutate_if(x, is.factor, as.character))
  adjmeans <- dplyr::bind_rows(adjmeans)
  adjmeans <- reshape2::dcast(adjmeans, Treatment ~ Trait,
                              value.var = "Adjusted Means", fun.aggregate = mean)
  adjmeans[, traits] <- lapply(adjmeans[,traits], round.conditional)


  # CV
  cvout <- lapply(output, function(x) x$CV)
  cvout <- lapply(cvout, function(x) data.frame(CV = x))
  cvout <- Map(cbind, Trait = names(cvout), cvout)
  cvout <- lapply(cvout, function(x) dplyr::mutate_if(x, is.factor, as.character))
  cvout <- dplyr::bind_rows(cvout)
  cvout$CV <- round.conditional(cvout$CV)

  # overall adj mean
  oadjmean <- lapply(output, function(x) x$`Overall adjusted mean`)
  oadjmean <- lapply(oadjmean, function(x) data.frame(Overall.adjusted.mean = x))
  oadjmean <- Map(cbind, Trait = names(oadjmean), oadjmean)
  oadjmean <- lapply(oadjmean, function(x) dplyr::mutate_if(x, is.factor, as.character))
  oadjmean <- dplyr::bind_rows(oadjmean)
  oadjmean$Overall.adjusted.mean <- round.conditional(oadjmean$Overall.adjusted.mean)

  # SE and CD
  secd <- lapply(output, function(x) x$`Std. Errors`)
  secd <- Map(cbind, Trait = names(secd), secd)
  secd <- lapply(secd, function(x) cbind(Comparison = rownames(x), x))
  secd <- lapply(secd, function(x) dplyr::mutate_if(x, is.factor, as.character))
  secd <- dplyr::bind_rows(secd)
  secd$`Std. Error of Diff.` <- round.conditional(secd$`Std. Error of Diff.`)
  secd$`CD (5%)` <- round.conditional(secd$`CD (5%)`)

  seout <- reshape2::dcast(secd, Comparison ~ Trait, value.var = "Std. Error of Diff.")
  cdout <- reshape2::dcast(secd, Comparison ~ Trait, value.var = "CD (5%)")

  # Descriptive statistics
  descout <- vector("list", length(traits))
  names(descout) <- traits

  for (i in 1:length(traits)) {
    descout[[i]] <- describe.augmentedRCBD(output[[traits[i]]])
  }

  descout <- lapply(descout, function(x) data.frame(x)[1,])
  descout <- Map(cbind, Trait = names(descout), descout)

  descout <- lapply(descout, function(x) dplyr::mutate_if(x, is.factor, as.character))
  descout <- dplyr::bind_rows(descout)

  descout$Skewness.p.value. <- ifelse(descout$Skewness.p.value. < 0.01, "**",
                        ifelse(descout$Skewness.p.value. < 0.05, "*", ""))
  descout$Kurtosis.p.value. <- ifelse(descout$Kurtosis.p.value. < 0.01, "**",
                                      ifelse(descout$Kurtosis.p.value. < 0.05, "*", ""))
  desc <- c("Mean", "Std.Error", "Std.Deviation", "Min",
            "Max", "Skewness.statistic.", "Kurtosis.statistic.")
  descout[,desc] <- apply(descout[,desc], MARGIN = 2, FUN = round.conditional)

  colnames(descout) <- c("Trait", "Count", "Mean", "Std.Error", "Std.Deviation",
                         "Min", "Max", "Skewness", "Skewness_sig", "Kurtosis",
                         "Kurtosis_sig")

  # GVA
  gvaout <- vector("list", length(traits))
  names(gvaout) <- traits

  for (i in 1:length(traits)) {
    gvaout[[i]] <- gva.augmentedRCBD(output[[traits[i]]])
  }

  gvaout <- lapply(gvaout, function(x) data.frame(x))
  gvaout <- Map(cbind, Trait = names(gvaout), gvaout)

  gvaout <- lapply(gvaout, function(x) dplyr::mutate_if(x, is.factor, as.character))
  gvaout <- dplyr::bind_rows(gvaout)

  gvaplot <- gvaout

  gvap <- c("Mean", "PV", "GV", "EV", "GCV", "PCV",  "ECV", "hBS", "GA", "GAM")
  gvaout[,gvap] <- apply(gvaout[,gvap], MARGIN = 2, FUN = round.conditional)

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
                           Category = as.factor(c("Low","Medium", "High")))
  gvacat$Category <- factor(gvacat$Category,
                            levels = c("Low", "Medium", "High"))


  gvaplot_cvg <- ggplot(gvaplot_cv, aes(x = Trait, colour = variable, group = variable)) +
    geom_hline(yintercept = c(10, 20), color = "black", linetype = 3) +
    geom_segment(data = gvaplot_2, aes(x = Trait, xend = Trait, y = -Inf, yend = min),
                 inherit.aes = F) +
    geom_segment(data = gvaplot_2, aes(x = Trait, xend = Trait, y = min, yend = max),
                 inherit.aes = F, size = 2, colour = "gray70") +
    geom_point(aes(y = value)) +
    scale_color_manual("Type", values = c("red", "blue")) +
    scale_y_continuous(breaks = seq(0, ceiling(max(gvaplot_2[, c("PCV", "GCV")])) + 10 ,
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
                       Category = as.factor(c("Low","Medium", "High")))
  gvacat2$Category <- factor(gvacat2$Category,
                            levels = c("Low", "Medium", "High"))
  gvaplot_hbs <- reshape2::melt(gvaplot, id.vars = c("Trait"),
                                measure.vars = "hBS")

  gvaplot_hbsg <- ggplot(gvaplot_hbs, aes(x = Trait, colour = variable, group = variable)) +
    geom_hline(yintercept = c(30, 60), color = "black", linetype = 3) +
    geom_segment(data = gvaplot_hbs, aes(x = Trait, xend = Trait, y = -Inf, yend = value),
                 colour = "black") +
    geom_point(aes(y = value), colour = "black") +
    scale_y_continuous(breaks = seq(0, ceiling(max(gvaplot[, "hBS"])) + 10 ,
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

  gvaplot_gamg <- ggplot(gvaplot_gam, aes(x = Trait, colour = variable, group = variable)) +
    geom_hline(yintercept = c(10, 20), color = "black", linetype = 3) +
    geom_segment(data = gvaplot_gam, aes(x = Trait, xend = Trait, y = -Inf, yend = value),
                 colour = "black") +
    geom_point(aes(y = value), colour = "black") +
    scale_y_continuous(breaks = seq(0, ceiling(max(gvaplot[, "GAM"])) + 10 ,
                                    by = 10)) +
    geom_rect(data = gvacat, aes(xmin = xmin, ymin = ymin,
                                 xmax = xmax, ymax = ymax, fill = Category),
              alpha = 0.5, inherit.aes = FALSE) +
    scale_fill_manual(values = c("gray60", "gray30", "gray5")) +
    ylab("Genetic advance over mean") +
    theme_bw() + themecustom

  # Freq Dist

  fqout <- vector("list", length(traits))
  names(fqout) <- traits

  fqwarn <- data.frame(Trait = traits, Message = NA_character_,
                       stringsAsFactors = F)

  fqwarn <- NULL
  for (i in 1:length(traits)) {

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



round.conditional <- function(x, digits = 2){

  x <- ifelse(round(x, digits) != 0,
         as.character(round(x, digits)),
         as.character(signif(x, digits)))
  return(x)

}


library(cowplot)
plot_grid(gvaplot_cvg, gvaplot_hbsg, gvaplot_gamg, ncol = 1, align = "v")

rows = 4
cols = 2
perpage = rows*cols
npage <- plyr::round_any(length(fqout)/perpage, 1, ceiling)
FqDistPlotsMulti <- split(fqout, seq.int(1, length(fqout), perpage))

pdf(file = "Freq distribution.pdf", height = 16.5, width =  11.7, onefile=TRUE)
plot_grid(plotlist = FqDistPlotsMulti[[1]], ncol = cols, nrow = rows, scale = 0.9)
plot_grid(plotlist = FqDistPlotsMulti[[2]], ncol = cols, nrow = rows, scale = 0.9)
dev.off()

