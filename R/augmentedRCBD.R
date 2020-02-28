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

#'Analysis of Augmented Randomised Complete Block Design
#'
#'\code{augmentedRCBD} is a function for analysis of variance of an augmented
#'randomised block design (Federer, 1956; Federer, 1961) and the generation as
#'well as comparison of the adjusted means of the treatments/genotypes.
#'
#'This function borrows code from \code{DAU.test} function of \code{agricolae}
#'package (de Mendiburu et al., 2016) as well as from Appendix VIII of Mathur et
#'al., (2008).
#'
#'@note \itemize{ \item Data should preferably be balanced i.e. all the check
#'  genotypes should be present in all the blocks. If not, a warning is issued.
#'  \item  There should not be any missing values. \item The number of test
#'  genotypes can vary within a block. }
#'
#'  In case the large number of treatments or genotypes, it is advisable to
#'  avoid comparisons with the \code{group =  FALSE} argument as it will be
#'  memory and processor intensive. Further it is advised to simplify output
#'  with \code{simplify = TRUE} in order to reduce output object size.
#'
#'@param block Vector of blocks (as a factor).
#'@param treatment Vector of treatments/genotypes (as a factor).
#'@param y Numeric vector of response variable (Trait).
#'@param checks Character vector of the checks present in \code{treatment}
#'  levels. If not specified, checks are inferred from the data on the basis of
#'  number of replications of treatments/genotypes.
#'@param method.comp Method for comparison of treatments (\code{"lsd"} for least
#'  significant difference or \code{"tukey"} for Tukey's honest significant
#'  difference). If \code{"none"}, no comparisons will be made, the ANOVA output
#'  will be given as a data frame and the adjusted means will be computed
#'  directly from treatment and block effects instead of using
#'  \code{\link[emmeans]{emmeans}}.
#'@param alpha Type I error probability (Significance level) to be used for
#'  multiple comparisons.
#'@param group If \code{TRUE}, genotypes will be grouped according to
#'  \code{"method.comp"}. Default is \code{TRUE}.
#'@param console If \code{TRUE}, output will be printed to console. Default is
#'  \code{TRUE}. Default is \code{TRUE}.
#'@param simplify If \code{TRUE}, ANOVA output will be given as a data frame
#'  instead of a \code{summary.aov} object. Default is \code{TRUE}.
#'@param truncate.means If \code{TRUE}, the negative adjusted means will be
#'  truncated to zero. Default is \code{TRUE}.
#'
#'@return A list of class \code{augmentedRCBD} containing the following
#'  components:  \item{\code{Details}}{Details of the augmented design used.}
#'  \item{\code{Means}}{A data frame with the "Means", "Block", "SE", "Mix",
#'  "Max" and "Adjusted Means" for each "Treatment".} \item{\code{ANOVA,
#'  Treatment Adjusted}}{An object of class \code{summary.aov} for ANOVA table
#'  with treatments adjusted.} \item{\code{ANOVA, Block Adjusted}}{An object of
#'  class \code{summary.aov} for ANOVA table with block adjusted.}
#'  \item{\code{Block effects}}{A vector of block effects.}
#'  \item{\code{Treatment effects}}{A vector of treatment effects.}
#'  \item{\code{Std. Errors}}{A data frame of standard error of difference
#'  between various combinations along with critical difference and tukey's
#'  honest significant difference (when \code{method.comp = "tukey"}) at
#'  \code{alpha}.} \item{\code{Overall adjusted mean}}{Overall adjusted mean.}
#'  \item{\code{CV}}{Coefficient of variation.} \item{\code{Comparisons}}{A data
#'  frame of pairwise comparisons of treatments. This is computed only if
#'  argument \code{group} is \code{TRUE}} \item{\code{Groups}}{A data frame with
#'  compact letter display of pairwise comparisons of treatments. Means with at
#'  least one letter common are not significantly different statistically. This
#'  is computed only if argument \code{group} is \code{TRUE} }
#'
#'@import multcompView
#'@importFrom multcomp cld
#'@importFrom emmeans emmeans
#'@importFrom reshape2 dcast
#'@importFrom stats anova
#'@importFrom stats aov
#'@importFrom stats contr.helmert
#'@importFrom stats coef
#'@importFrom stats na.omit
#'@importFrom stats qt
#'@importFrom stats qtukey
#'@importFrom stats sd
#'@importFrom stats contrasts<-
#'@importFrom graphics pairs
#'@importFrom Rdpack reprompt
#'@export
#'
#'@seealso \code{\link[agricolae]{DAU.test}},
#'  \href{https://www.rdocumentation.org/packages/easyanova/versions/5.0/topics/ea1}{\code{ea1}},
#'   \code{\link[emmeans]{emmeans}}, \code{\link[emmeans]{cld.emmGrid}},
#'  \href{https://rdrr.io/rforge/plantbreeding/man/aug.rcb.html}{\code{aug.rcb}}
#'
#'@references
#'
#'
#'
#'\insertRef{federer_augmented_1956}{augmentedRCBD}
#'
#'\insertRef{federer_augmented_1961}{augmentedRCBD}
#'
#'\insertRef{mathur_data_2008}{augmentedRCBD}
#'
#'\insertRef{de_mendiburu_agricolae_2015}{augmentedRCBD}
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
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' # Results for variable y2 (checks inferred)
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                      alpha = 0.05, group = TRUE, console = TRUE)
#'
#' # Results for variable y1 (checks specified)
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE,
#'                       checks = c("1", "2", "3", "4"))
#' # Results for variable y2 (checks specified)
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE,
#'                       checks = c("1", "2", "3", "4"))
#'
#'\dontrun{
#' # Error in case checks not replicated across all blocks
#' # Check 1 and 4 not replicated in all 3 blocks
#' trt <- c(1, 2, 3, 14, 7, 11, 12, 1, 2, 3, 4, 5, 9, 13, 2, 3, 4, 8, 6, 10)
#' data$trt <- as.factor(trt)
#' table(data$trt, data$blk)
#' # Results for variable y1 (checks specified)
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE,
#'                       checks = c("1", "2", "3", "4"))
#'}
#'
#' # Warning in case test treatments are replicated
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE,
#'                       checks = c("2", "3"))
#'
augmentedRCBD <- function(block, treatment, y, checks = NULL,
                          method.comp = c("lsd", "tukey", "none"),
                          alpha=0.05, group=TRUE, console = TRUE,
                          simplify = FALSE, truncate.means = TRUE) {
  # Checks
  # block
  if (!is.factor(block)) {
    stop('"block" should be of class "factor"')
  }
  # treatment
  if (!is.factor(treatment)) {
    stop('"treatment" should be of class "factor"')
  }
  # y
  if (!(is.vector(y, mode = "integer") | is.vector(y, mode = "numeric"))) {
    stop('"y" should be a vector of class "numeric" or "integer"')
  }
  if (!(length(y) == length(treatment) && length(treatment) == length(block))) {
    stop('"block", "treatment" and "y" are of unequal lengths')
  }
  if (TRUE %in% is.na(y)) { # check for missing values
    stop('"y" has missing value(s)')
  }
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha < 1)')
  }
  # method.comp
  method.comp <- match.arg(method.comp, c("lsd", "tukey", "none"),
                           several.ok = FALSE)
  if (method.comp == "none") group <- FALSE
  if (group == FALSE) method.comp <- "none"

  if (!missing(checks) && !is.null(checks)) {
  #if (!is.null(checks)) {
    checks <- as.character(checks)
    # checks are present in treatment levels
    if (FALSE %in% c(checks %in% levels(treatment))) {
      miss <- paste(checks[!(checks %in% levels(treatment))], collapse = ", ")
      stop(paste("Following check(s) are not present in treatment levels:\n",
                    paste(miss, collapse = ", ")))
    }
  }

  # Fix treatment order so that checks are in the beginning
  if (!missing(checks) && !is.null(checks)) { # i.e. checks are specified
  #if (!is.null(checks)) {
    treatmentorder <- data.frame(table(treatment, block))
    treatmentorder[treatmentorder$Freq != 0, ]$Freq <- 1
    treatmentorder <- reshape2::dcast(treatmentorder, treatment ~ block,
                                      value.var = "Freq")
    treatmentorder$Freq <- rowSums(subset(treatmentorder,
                                          select = -c(treatment)))
    treatmentorder <- treatmentorder[, c("treatment", "Freq")]

    nblocks <- length(levels(block))
    rownames(treatmentorder) <- NULL

    # check if "checks" are present in all the blocks
    if (!(all(treatmentorder[treatmentorder$treatment %in% checks, ]$Freq == nblocks))) {
      print(treatmentorder)
      stop(paste('"checks" are not replicated across all the blocks (',
                 nblocks, ')', sep = ""))
    }

    tests <- levels(treatment)[!(levels(treatment) %in% checks)]
    if (!all(table(droplevels(treatment[treatment %in% tests])) == 1)) {
      warning("Test treatments are replicated")
    }

    nworder <- c(levels(treatmentorder$treatment)[levels(treatmentorder$treatment) %in% checks],
                 tests)
    treatment <- factor(treatment, levels = nworder)

  } else {# i.e. "checks" is not specified
    treatmentorder <- data.frame(table(treatment, block))
    treatmentorder[treatmentorder$Freq != 0, ]$Freq <- 1
    treatmentorder <- reshape2::dcast(treatmentorder, treatment ~ block,
                                      value.var = "Freq")
    treatmentorder$Freq <- rowSums(subset(treatmentorder,
                                          select = -c(treatment)))
    treatmentorder <- treatmentorder[, c("treatment", "Freq")]
    treatmentorder <- treatmentorder[with(treatmentorder,
                                          order(-Freq, treatment)), ]
    nworder <- treatmentorder$treatment
    treatment <- factor(treatment, levels = treatmentorder$treatment)

    nblocks <- length(levels(block))
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

    tests <- levels(treatment)[!(levels(treatment) %in% checks)]
    if (!all(table(droplevels(treatment[treatment %in% tests])) == 1)) {
      warning("Test treatments are replicated")
    }
  }


  r <- unique(table(treatment))
  b <- nlevels(block) # no. of blocks
  ntr <- nlevels(treatment)  # no. of treatments

  blockwisechecks <- as.data.frame.matrix(table(treatment, block))
  blockwisechecks <- cbind(treatment = rownames(blockwisechecks),
                           blockwisechecks)
  blockwisechecks <- blockwisechecks[blockwisechecks$treatment %in% checks, ]
  rownames(blockwisechecks) <- NULL

  Details <- list(`Number of blocks` = b, `Number of treatments` = ntr,
                  `Number of check treatments` = length(checks),
                  `Number of test treatments` = length(tests),
                  `Check treatments` =  checks)

  tb <- data.frame(treatment, block)
  tb$block <- as.character(tb$block)
  tb$Block <- ifelse(tb$treatment %in% checks, "", tb$block)
  tb$block <- NULL
  tb <- unique(tb)

  # Get means table
  Means <- tapply(y, treatment, function(x) mean(x, na.rm = TRUE))
  mi <- tapply(y, treatment, function(x) min(x, na.rm = TRUE))
  ma <- tapply(y, treatment, function(x) max(x, na.rm = TRUE))
  n.rep <- tapply(y, treatment, function(x) length(na.omit(x)))
  sds <- tapply(y, treatment, function(x) sd(x, na.rm = TRUE))
  std.err <- sds / sqrt(n.rep)
  Means <- data.frame(Treatment = names(Means), Means, SE = std.err, r = n.rep,
                      Min = mi, Max = ma)
  Means <- merge(Means, tb, by.x = "Treatment", by.y = "treatment")
  Means <- Means[c("Treatment", "Block", "Means", "SE", "r", "Min", "Max")]

  # ANOVA 1 - `ANOVA, Treatment Adjusted`
  options(contrasts = c("contr.helmert", "contr.poly"))
  augmented.aov <- aov(y ~ block + treatment)

  df.check <- length(checks) - 1
  df.treatment <- length(levels(treatment)) - 1

  A1 <- summary(augmented.aov,
                split = list(treatment = list(Check = 1:df.check,
                                              `Test and Test vs. Check` = (df.check + 1):df.treatment)))

  # Calculate adjusted treatment effects
  options(contrasts = c("contr.sum", "contr.poly"))
  augmented3.aov <- aov(y ~ block + treatment)
  co <- coef(augmented3.aov)

  co.treatment <- co[augmented3.aov$assign == 2]
  effects.treatment <- c(co.treatment, -sum(co.treatment))
  names(effects.treatment) <- levels(treatment)
  `Overall adjusted mean` <- co[1]
  names(`Overall adjusted mean`) <- NULL

  # Calculate adjusted block effects
  co.block <- co[augmented3.aov$assign == 1]
  effects.block <- c(co.block, -sum(co.block))
  names(effects.block) <- levels(block)

  # ANOVA 2 - `ANOVA, Block Adjusted`
  contr.augmented <- function(n1, n2){
    m1 <- contr.helmert(n1)
    m2 <- contr.helmert(n2)
    m10 <- cbind(m1, matrix(0, nrow(m1), ncol(m2)))
    m02 <- cbind(matrix(0, nrow(m2), ncol(m1)), m2)
    rbind(m10, m02)
  }

  contrasts(treatment) <- contr.augmented(df.check + 1,
                                          df.treatment - df.check)
  augmented2.aov <- aov(y ~ treatment + block)
  A2 <- summary(augmented2.aov,
                split = list(treatment = list(Check = 1:df.check,
                                              Test = (df.check + 1):(df.treatment - 1),
                                              `Test vs. check` = df.treatment)))

  rownames(A1[[1]])[1] <- "Block (ignoring Treatments)         "
  rownames(A1[[1]])[2] <- "Treatment (eliminating Blocks)      "
  rownames(A1[[1]])[3] <- "  Treatment: Check                  "
  rownames(A1[[1]])[4] <- "  Treatment: Test and Test vs. Check"

  rownames(A2[[1]])[1] <- "Treatment (ignoring Blocks)   "
  rownames(A2[[1]])[2] <- "  Treatment: Check            "
  rownames(A2[[1]])[3] <- "  Treatment: Test             "
  rownames(A2[[1]])[4] <- "  Treatment: Test vs. Check   "
  rownames(A2[[1]])[5] <- "Block (eliminating Treatments)"

  # Adjusted means
  if (method.comp == "none") {
    mean.adj1 <- data.frame(mean.adj = `Overall adjusted mean` + effects.treatment[1:(df.check + 1)])
    mean.adj1$Treatment <- rownames(mean.adj1)
    mean.adj2 <- data.frame(mean.adj = `Overall adjusted mean` + effects.treatment[(df.check + 2):(df.treatment + 1)])
    mean.adj2$Treatment <- rownames(mean.adj2)
    mean.adj <- rbind(mean.adj1, mean.adj2)

    Means <- merge.data.frame(Means, mean.adj, by = "Treatment", all = TRUE)

    colnames(Means) <- c("Treatment", "Block", "Means", "SE", "r",
                         "Min", "Max", "Adjusted Means")

  } else {
    LSMeans <- emmeans::emmeans(augmented3.aov, "treatment")
    LSMeans2 <- summary(LSMeans)[, c("treatment", "emmean")]
    colnames(LSMeans2) <- c("Treatment", "Adjusted Means")

    Means <- merge.data.frame(Means, LSMeans2, by = "Treatment", all = TRUE)

    colnames(Means) <- c("Treatment", "Block", "Means", "SE", "r",
                         "Min", "Max", "Adjusted Means")
  }

  if (simplify == TRUE) {
    A1 <- data.frame(A1[[1]])
    A1 <- cbind(Source = trimws(rownames(A1)), A1)
    A2 <- data.frame(A2[[1]])
    A2 <- cbind(Source = trimws(rownames(A2)), A2)
    rownames(A1) <- NULL
    rownames(A2) <- NULL
    }

  # Grouping of treatments
  Comparison <- NULL
  Groups <- NULL


  if (group == TRUE) {
    if (method.comp == "lsd") adjust <- "none"
    if (method.comp == "tukey") adjust <- "tukey"

    Comparison <- data.frame(summary(pairs(LSMeans, adjust = adjust)))
    Groups <- data.frame(multcomp::cld(LSMeans, adjust = adjust))

    Comparison$sig <- ifelse(Comparison$p.value < 0.001, "***",
                             ifelse(Comparison$p.value < 0.01, "**",
                                    ifelse(Comparison$p.value < 0.05, "*", "")))
    colnames(Groups) <- c("Treatment", "Adjusted Means", "SE", "df",
                          "lower.CL", "upper.CL", "Group")

  }

  # Compute SE and CD for various comparisons
  augmented3.anova <- anova(augmented3.aov)
  MSE <- augmented3.anova[[3]][3]

  CV <- sqrt(MSE) * 100 / mean(augmented3.aov$fitted.values)

  r <- augmented3.anova$Df[1] + 1 # Number of blocks
  c <- length(checks) # Number of check treatments
  t0 <- qt(1 - (alpha / 2), augmented3.aov$df.residual)

  S <- c("Control Treatment Means", "Two Test Treatments (Same Block)",
         "Two Test Treatments (Different Blocks)",
         "A Test Treatment and a Control Treatment")

  SE.check <- sqrt(2 * MSE / r) #Two Control Treatments
  SE.test1 <- sqrt(2 * MSE) #Two Augmented Treatments (Same Block)
  SE.test2 <- sqrt(2 * MSE * (1 + (1 / c))) #Two Augmented Treatments(Different Blocks)
  SE.testcheck <- sqrt(MSE * (1 + (1 / r) + (1 / c) + (1 / (r * c)))) #A Test Treatment and a Control Treatment

  SECD <- data.frame(`Std. Error of Diff.` =  c(SE.check, SE.test1,
                                                SE.test2, SE.testcheck),
                     row.names = S, check.names = FALSE)
  SECD$CD <- t0 * SECD$`Std. Error of Diff.`
  colnames(SECD) <- c("Std. Error of Diff.",
                      paste("CD (", alpha * 100, "%)", sep = ""))

  if (method.comp == "tukey") {
    q0 <- qtukey(p = 1 - alpha, nmeans = nlevels(treatment),
                 df = augmented3.aov$df.residual)

    SECD$THSD <- q0 * SECD$`Std. Error of Diff.`
    colnames(SECD) <- c("Std. Error of Diff.",
                        paste("CD (", alpha * 100, "%)", sep = ""),
                        paste("Tukey HSD (", alpha * 100, "%)", sep = ""))
  }

  rm(augmented.aov, augmented2.aov, augmented3.aov, augmented3.anova)

  # Truncate negative adjusted means
  if (any(Means$`Adjusted Means` < 0)){
    negadjmeans <- which(Means$`Adjusted Means` < 0)
    negadjmeanst <- as.character(Means$Treatment[negadjmeans])

    negmsg <- paste('Negative adjusted means for the following treatment(s)',
                    '\n', paste(negadjmeanst, collapse = ", "))

  if (truncate.means == TRUE) {
    Means$`Adjusted Means`[Means$`Adjusted Means` < 0] <- 0
    Groups$`Adjusted Means`[Groups$`Adjusted Means` < 0] <- 0

    warning(paste(negmsg, '\n',
                  'They were truncated to zero'))
  } else {
    warning(negmsg)
  }
  }

  output <- list(Details = Details, Means = Means,
                 `ANOVA, Treatment Adjusted` = A1,
                 `ANOVA, Block Adjusted` = A2, `Block effects` = effects.block,
                 `Treatment effects` = effects.treatment, `Std. Errors` = SECD,
                 `Overall adjusted mean` = `Overall adjusted mean`,
                 `CV` = CV, `Comparison method` = method.comp,
                 Comparisons = Comparison, Groups = Groups)

  # Set Class
  class(output) <- "augmentedRCBD"

  if (console) {
    print.augmentedRCBD(output)
  }

  return(output)

}
