#' Analysis of Augmented Randomised Complete Block Design for Multiple
#' Traits/Characters
#'
#' @param data
#'
#' @param block
#' @param treatment
#' @param traits
#' @param checks
#' @param method.comp
#' @param alpha
#' @param describe
#' @param freqdist
#' @param gva
#' @param round.digits
#'
#' @import reshape2
#' @import dplyr
#' @export
augmentedRCBD.bulk <- function(data, block, treatment, traits, checks = NULL,
                               method.comp = c("lsd","tukey"),
                               alpha=0.05, describe = TRUE,
                               freqdist = TRUE, gva = TRUE,
                               round.digits) {

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
  missvcols <- unlist(lapply(data, function(x) TRUE %in% is.na(x)))
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
  # method.comp
  method.comp <- match.arg(method.comp, c("lsd","tukey"), several.ok = FALSE)

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

  for (i in 1:length(traits)) {

    output[[i]] <- augmentedRCBD(block = data[, block],
                                 treatment = data[, treatment],
                                 y = data[, traits[i]], checks = checks,
                                 method.comp = method.comp, alpha = alpha,
                                 group = FALSE, console = FALSE)

  }

  # Details
  Details <- output[[1]]$Details

  # ANOVA table
  anovata <- lapply(output, function(x) x$`ANOVA, Treatment Adjusted`)
  anovaba <- lapply(output, function(x) x$`ANOVA, Block Adjusted`)

  anovata <- lapply(anovata, function(x) data.frame(x[[1]]))
  anovaba <- lapply(anovaba, function(x) data.frame(x[[1]]))

  anovata <- Map(cbind, anovata, Trait = names(anovata))
  anovaba <- Map(cbind, anovaba, Trait = names(anovaba))

  anovata <- lapply(anovata, function(x) cbind(Type = "ANOVA, Treatment Adjusted",
                                               Source = rownames(x), x))
  anovaba <- lapply(anovaba, function(x) cbind(Type = "ANOVA, Block Adjusted",
                                               Source = rownames(x), x))

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


  anovaout <- dcast(anovatable, Type + Source ~ Trait, value.var = "MSS")
  anovasig <- dcast(anovatable, Type + Source ~ Trait, value.var = "sig")
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
  adjmeans$`Adjusted Means` <- ifelse(round(adjmeans$`Adjusted Means`, 2) != 0,
                                      as.character(round(adjmeans$`Adjusted Means`, 2)),
                                      sub("\\.$", "", sub(".0+$", "",
                                                          sprintf("%f", signif(adjmeans$`Adjusted Means`, 2)))))

  adjmeans <- reshape2::dcast(adjmeans, Treatment ~ Trait, value.var = "Adjusted Means")

  # CV
  cvout <- lapply(output, function(x) x$CV)
  cvout <- lapply(cvout, function(x) data.frame(CV = x))
  cvout <- Map(cbind, Trait = names(cvout), cvout)
  cvout <- lapply(cvout, function(x) dplyr::mutate_if(x, is.factor, as.character))
  cvout <- dplyr::bind_rows(cvout)
  cvout$CV <- ifelse(round(cvout$CV, 2) != 0,
                     as.character(round(cvout$CV, 2)),
                     sub("\\.$", "", sub(".0+$", "",
                                         sprintf("%f", signif(cvout$CV, 2)))))
  # overall adj mean
  oadjmean <- lapply(output, function(x) x$`Overall adjusted mean`)
  oadjmean <- lapply(oadjmean, function(x) data.frame(Overall.adjusted.mean = x))
  oadjmean <- Map(cbind, Trait = names(oadjmean), oadjmean)
  oadjmean <- lapply(oadjmean, function(x) dplyr::mutate_if(x, is.factor, as.character))
  oadjmean <- dplyr::bind_rows(oadjmean)
  oadjmean$Overall.adjusted.mean <- ifelse(round(oadjmean$Overall.adjusted.mean, 2) != 0,
                                           as.character(round(oadjmean$Overall.adjusted.mean, 2)),
                                           sub("\\.$", "", sub(".0+$", "",
                                                               sprintf("%f", signif(oadjmean$Overall.adjusted.mean, 2)))))
  # SE and CD
  secd <- lapply(output, function(x) x$`Std. Errors`)
  secd <- Map(cbind, Trait = names(secd), secd)
  secd <- lapply(secd, function(x) cbind(Comparison = rownames(x), x))
  secd <- lapply(secd, function(x) dplyr::mutate_if(x, is.factor, as.character))
  secd <- dplyr::bind_rows(secd)
  secd$`Std. Error of Diff.` <- ifelse(round(secd$`Std. Error of Diff.`, 2) != 0,
                                       as.character(round(secd$`Std. Error of Diff.`, 2)),
                                       sub("\\.$", "", sub(".0+$", "",
                                                           sprintf("%f", signif(secd$`Std. Error of Diff.`, 2)))))
  secd$`CD (5%)` <- ifelse(round(secd$`CD (5%)`, 2) != 0,
                           as.character(round(secd$`CD (5%)`, 2)),
                           sub("\\.$", "", sub(".0+$", "",
                                               sprintf("%f", signif(secd$`CD (5%)`, 2)))))

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
  descout[,desc] <- apply(descout[,desc], MARGIN=2,
        FUN= function(x) ifelse(round(x, 2) != 0,
                                as.character(round(x, 2)),
                                sub("\\.$", "", sub(".0+$", "",
                                                    sprintf("%f", signif(x, 2))))))

  descout <- rename(descout, Skewness = Skewness.statistic., Kurtosis = Kurtosis.statistic.,
                    Skewness_sig = Skewness.p.value., Kurtosis_sig = Kurtosis.p.value.)

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

  gvap <- c("Mean", "PV", "GV", "EV", "GCV", "PCV",  "ECV", "hBS", "GA", "GAM")
  gvaout[,gvap] <- apply(gvaout[,gvap], MARGIN=2,
                         FUN= function(x) ifelse(round(x, 2) != 0,
                                                 as.character(round(x, 2)),
                                                 sub("\\.$", "", sub(".0+$", "",
                                                                     sprintf("%f", signif(x, 2))))))
  # GVA plot

  # Freq Dist

  # Capture error


}



library(dplyr)
library(reshape2)
# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
# trt <- c(1, 2, 3, 14, 7, 11, 12, 1, 2, 3, 4, 5, 9, 13, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
dataf <- data.frame(blk, trt, y1, y2)

block = "blk"
treatment = "trt"
traits = c("y1", "y2")
data = dataf
checks = NULL
method.comp = "lsd"
alpha=0.05
describe = TRUE
freqdist = TRUE
gva = TRUE
#
# augmentedRCBD.bulk(data = dataf, block = "blk", treatment = "trt", traits =  c("y1", "y2"),
#                    checks = NULL, method.comp = "lsd", alpha=0.05,
#                    describe = TRUE, freqdist = TRUE, gva = TRUE)
