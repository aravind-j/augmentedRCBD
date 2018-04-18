#' Analysis of Augmented Randomised Complete Block Design
#'
#' \code{augmentedRCBD} is a function for analysis of variance of an augmented
#' randomised block design and the generation as well as comparison of the
#' adjusted means of the treatments (genotypes).
#'
#' This function borrows code from \code{DAU.test} function of \code{agricolae}
#' package (de Mendiburu et al., 2016) as well as from Appendix VIII of Mathur
#' et al., 2008.
#'
#' @note Data should be balanced (all the check genotypes should be present in
#'   all the blocks). There should not be any missing values. The number of test
#'   genotypes can vary within a block.
#'
#' @param block Vector of blocks.
#' @param treatment Vector of treatments(genotypes).
#' @param y Vector of response variable (Trait).
#' @param method Method for comparison of treatments (\code{"lsd"} for least
#'   significant difference or \code{"tukey"} for Tukey's honest significant
#'   difference).
#' @param alpha Type I error probability (Significance level) to be used for
#'   multiple comparisons.
#' @param group If \code{TRUE}, genotypes will be grouped according to
#'   \code{"method"}.
#' @param console If \code{TRUE}, output will be printed to console.
#'
#' @return A list with the following components:  \item{\code{Details}}{Details
#'   of the augmented design used.} \item{\code{Means}}{A data frame with the
#'   "Means", "Block", "SE", "Mix", "Max" and "Adjusted Means" for each
#'   "Treatment".} \item{\code{ANOVA, Treatment Adjusted}}{An object of class
#'   \code{summary.aov} for ANOVA table with treatments adjusted.}
#'   \item{\code{ANOVA, Block Adjusted}}{An object of class \code{summary.aov}
#'   for ANOVA table with block adjusted.} \item{\code{Block effects}}{A vector
#'   of block effects.} \item{\code{Treatment effects}}{A vector of treatment
#'   effects.} \item{\code{Std. Errors}}{A data frame of standard error of
#'   difference between various combinations along with critical difference and
#'   tukey's honest significant difference (when \code{method = "tukey"}) at
#'   \code{alpha}.} \item{\code{Overall Adjusted mean}}{Overall adjusted mean.}
#'   \item{\code{CV}}{Coefficient of variation.} \item{\code{Comparisons}}{A
#'   data frame of pairwise comparisons of treatments.} \item{\code{Groups}}{A
#'   data frame with compact letter display of pairwise comparisons of
#'   treatments. Means with at least one letter common are not significantly
#'   different statistically.}
#'
#' @import lsmeans
#' @import multcompView
#' @export
#'
#' @references
#'
#'  \insertRef{federer_augmented_1956}{augmentedRCBD}
#'
#'  \insertRef{federer_augmented_1961}{augmentedRCBD}
#'
#'  \insertRef{mathur_data_2008}{augmentedRCBD}
#'
#'  \insertRef{de_mendiburu_agricolae:_2015}{augmentedRCBD}
#'
#' @examples
augmentedRCBD <- function(block, treatment, y,
                           method = c("lsd","tukey"),
                           alpha=0.05, group=TRUE, console = TRUE) {
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
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha <1)')
  }
  # method
  method <- match.arg(method, c("lsd","tukey"), several.ok = FALSE)

  # Fix treatment order so that checks are in the beginning
  treatmentorder <- data.frame(table(treatment))
  treatmentorder <- treatmentorder[with(treatmentorder,
                                        order(-Freq, treatment)), ]
  treatment <- factor(treatment, levels = treatmentorder$treatment)

  checks <- as.character(treatmentorder[treatmentorder$Freq != 1,]$treatment)
  tests <- as.character(treatmentorder[treatmentorder$Freq == 1,]$treatment)

  r <- unique(table(treatment))
  b <- nlevels(block) # no. of blocks
  ntr <- nlevels(treatment)  # no. of treatments

  Details <-   t(data.frame(`Number of blocks` = b, `Number of treatments` = ntr,
                            `    Number of check treatments` = length(checks),
                            `    Number of test treatments` = length(tests),
                            `Check treatments` =  paste(checks, collapse = ", "),
                            check.names = FALSE))
  colnames(Details) <- c("")

  tb <- data.frame(treatment, block)
  tb$block <- as.character(tb$block)
  tb$Block <- ifelse(tb$treatment %in% checks, "", tb$block)
  tb$block <- NULL
  tb <- unique(tb)

  # Get means table
  Means <- tapply(y,treatment, function(x) mean(x,na.rm = TRUE))
  mi <- tapply(y, treatment, function(x) min(x,na.rm = TRUE))
  ma <- tapply(y, treatment, function(x) max(x,na.rm = TRUE))
  n.rep <- tapply(y, treatment, function(x) length(na.omit(x)))
  sds <- tapply(y, treatment, function(x) sd(x,na.rm = TRUE))
  std.err <- sds/sqrt(n.rep)
  Means <- data.frame(Treatment = names(Means), Means, SE = sds, r = n.rep,
                      Min = mi, Max = ma)
  Means <- merge(Means, tb, by.x = "Treatment", by.y = "treatment")
  Means <- Means[c("Treatment", "Block", "Means", "SE", "r", "Min", "Max")]

  # ANOVA 1 - `ANOVA, Treatment Adjusted`
  options(contrasts = c("contr.helmert","contr.poly"))
  augmented.aov <- aov(y ~ block + treatment)

  df.check <- length(checks)-1
  df.treatment <- length(levels(treatment))-1

  A1 <- summary(augmented.aov,
                split = list(treatment = list(Check = 1:df.check,
                                              `Test and Test vs. Check` = (df.check+1):df.treatment)))

  # Calculate adjusted treatment effects
  options(contrasts = c("contr.sum", "contr.poly"))
  augmented3.aov <- aov(y ~ block + treatment)
  co <- coef(augmented3.aov)

  co.treatment <- co[augmented3.aov$assign == 2]
  effects.treatment <- c(co.treatment, -sum(co.treatment))
  names(effects.treatment) <- levels(treatment)
  `Overall Adjusted mean` = co[1]
  names(`Overall Adjusted mean`) <- NULL

  # Calculate adjusted block effects
  co.block <- co[augmented3.aov$assign == 1]
  effects.block <- c(co.block, -sum(co.block))
  names(effects.block) <- levels(block)

  # ANOVA 2 - `ANOVA, Block Adjusted`
  contr.augmented <- function(n1,n2){
    m1 <- contr.helmert(n1)
    m2 <- contr.helmert(n2)
    m10 <- cbind(m1,matrix(0,nrow(m1),ncol(m2)))
    m02 <- cbind(matrix(0,nrow(m2),ncol(m1)),m2)
    rbind(m10,m02)
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
  #   mean.adj1 <- data.frame(mean.adj = `Overall Adjusted mean` + effects.treatment[1:(df.check + 1)])
  #   mean.adj1$Treatment <- rownames(mean.adj1)
  #   mean.adj2 <- data.frame(mean.adj = `Overall Adjusted mean` + effects.treatment[(df.check + 2):(df.treatment + 1)])
  #   mean.adj2$Treatment <- rownames(mean.adj2)
  #   mean.adj <- rbind(mean.adj1, mean.adj2)
  #
  #   Means <- merge.data.frame(Means, mean.adj, by = "Treatment", all = TRUE)

  LSMeans <- lsmeans::lsmeans(augmented3.aov, "treatment")
  LSMeans2 <- summary(LSMeans)[, c("treatment", "lsmean")]
  colnames(LSMeans2) <- c("Treatment", "Adjusted Means")

  Means <- merge.data.frame(Means, LSMeans2, by = "Treatment", all = TRUE)

  colnames(Means) <- c("Treatment", "Block", "Means", "SE", "r",
                       "Min", "Max", "Adjusted Means")

  # Grouping of treatments
  Comparison <- NULL
  Groups <- NULL

  if (group==TRUE) {
    if (method == "lsd") adjust = "none"
    if (method == "tukey") adjust = "tukey"

    Comparison <- data.frame(summary(pairs(LSMeans, adjust = adjust)))
    Groups <- data.frame(lsmeans::cld(LSMeans, adjust = adjust))

    Comparison$sig <- ifelse(Comparison$p.value < 0.001, "***",
                             ifelse(Comparison$p.value < 0.01, "**",
                                    ifelse(Comparison$p.value < 0.05, "*", "")))
    colnames(Groups) <- c("Treatment", "Adjusted Means", "SE", "df", "lower.CL", "upper.CL",
                          "Group")
  }

  # Compute SE and CD for various comparisons
  augmented3.anova <- anova(augmented3.aov)
  MSE <- augmented3.anova[[3]][3]

  CV <- sqrt(MSE)*100/mean(augmented3.aov$fitted.values)

  r <- augmented3.anova$Df[1] + 1 # Number of blocks
  c <- length(checks) # Number of check treatments
  t0 <- qt(1-(alpha/2), augmented3.aov$df.residual)

  S <- c("Control Treatment Means", "Two Test Treatments (Same Block)",
         "Two Test Treatments (Different Blocks)",
         "A Test Treatment and a Control Treatment")

  SE.check <- sqrt(2*MSE/r) #Two Control Treatments
  SE.test1 <- sqrt(2*MSE) #Two Augmented Treatments (Same Block)
  SE.test2 <- sqrt(2*MSE*(1+(1/c))) #Two Augmented Treatments(Different Blocks)
  SE.testcheck <- sqrt(MSE*(1+(1/r)+(1/c)-(1/(r*c)))) #A Test Treatment and a Control Treatment

  SECD <- data.frame(`Std. Error of Diff.` =  c(SE.check, SE.test1,
                                                SE.test2, SE.testcheck),
                     row.names = S, check.names = FALSE)
  SECD$CD <- t0*SECD$`Std. Error of Diff.`
  colnames(SECD) <- c("Std. Error of Diff.",
                      paste("CD (", alpha*100, "%)", sep = ""))

  if (method == "tukey") {
    q0 <- qtukey(1-(alpha/2), nlevels(treatment),
                 df = augmented3.aov$df.residual)

    SECD$THSD <- q0*SECD$`Std. Error of Diff.`
    colnames(SECD) <- c("Std. Error of Diff.",
                        paste("CD (", alpha*100, "%)",sep = ""),
                        paste("Tukey HSD (", alpha*100, "%)",sep = ""))
  }

  output <- list(Details = Details, Means = Means, `ANOVA, Treatment Adjusted` = A1,
                 `ANOVA, Block Adjusted` = A2, `Block effects` = effects.block,
                 `Treatment effects` = effects.treatment, `Std. Errors` = SECD,
                 `Overall Adjusted mean` = `Overall Adjusted mean`,
                 `CV` = CV, Comparisons = Comparison, Groups = Groups)

  if (console) {
    cat("\nAugmented design details\n")
    cat("========================\n")
    print(Details)
    cat("\nANOVA, Treatment Adjusted\n")
    cat("=========================\n")
    print(A1)
    cat("\nANOVA, Block Adjusted\n")
    cat("=====================\n")
    print(A2)
    cat("\nTreatment means\n")
    cat("===============\n")
    print(Means)
    cat("\nCoefficient of variation\n")
    cat("========================\n")
    cat(CV)
    cat("\n\nOverall adjusted mean\n")
    cat("=====================\n")
    cat(`Overall Adjusted mean`)
    cat("\n\nStandard errors\n")
    cat("===================\n")
    print(SECD)
    if (group) {
      cat("\nTreatment groups\n")
      cat("==================\n")
      cat(paste("\nMethod : ", method, "\n\n", sep = ""))
      print(Groups)
    }
  }

  return(output)

}
