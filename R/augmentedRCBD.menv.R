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

#' Combined Analysis of Augmented Randomised Complete Block Design in Multiple
#' Environments
#'
#' \code{augmentedRCBD.menv} is an extension of
#' \code{\link[augmentedRCBD]{augmentedRCBD}} designed for the combined/pooled
#' analysis of data from augmented randomised complete block design across
#' multiple environments such as locations and/or seasons.
#' This function performs analysis under the following two scenarios. \describe{
#' \item{\emph{Scenario 1}:}{Replicated test treatments across environments.}
#' \item{\emph{Scenario 2}:}{Non-replicated test treatments across
#' environments.} }
#'
#' The method of analysis is determined by whether the test treatments are
#' replicated across different environments as indicated by the \code{scenario}
#' argument. The design is balanced in terms of the checks being replicated
#' across all the blocks and environments.
#'
#' \describe{ \item{Scenario \code{1}:}{The test treatments are replicated
#' across all environments. Here the Treatment \u00D7 Environment interaction
#' is fully estimable as it is balanced.}\item{Scenario \code{2}:}{The test
#' treatments are replicated across all environments. Here the Treatment
#' \u00D7 Environment interaction is only partially estimable as it is
#' unbalanced..} }
#'
#' @note \itemize{ \item Data should preferably be balanced i.e. all the check
#'   genotypes should be present in all the blocks. If not, a warning is issued.
#'   \item There should not be any missing values. \item The number of test
#'   genotypes can vary within a block. \item When a large number of treatments
#'   or genotypes are involved, the analysis becomes memory intensive because
#'   calculating adjusted means necessitates the creation of a reference grid
#'   matrix by \code{emmeans}, which can grow exponentially in size }
#'
#' @inheritParams augmentedRCBD
#' @param env Vector of environments (as a factor).
#' @param scenario Either \code{1} or \code{2} (see \strong{Details}).
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#'
#' @returns A list of class \code{augmentedRCBD.menv} containing the following
#'   components:  \item{\code{Details}}{Details of the augmented design used.}
#'  \item{\code{Means}}{A data frame with the "Means", "Block", "SE", "Mix",
#'  "Max" and "Adjusted Means" for each "Treatment".} \item{\code{ANOVA,
#'  Treatment Adjusted}}{An object of class \code{summary.aov} for ANOVA table
#'  with treatments adjusted.} \item{\code{ANOVA, Block Adjusted}}{An object of
#'  class \code{summary.aov} for ANOVA table with block adjusted.}
#'   \item{\code{Block effects}}{A vector of block effects.}
#'   \item{\code{Treatment effects}}{A vector of treatment effects.}
#'  \item{\code{Std. Errors}}{A data frame of standard error of difference
#'  between various combinations along with critical difference and tukey's
#'  honest significant difference (when \code{method.comp = "tukey"}) at
#'  \code{alpha}.} \item{\code{Overall adjusted mean}}{Overall adjusted mean.}
#'  \item{\code{CV}}{Coefficient of variation.} \item{\code{Comparison
#'  method}}{The method for comparison of treatments.}
#'  \item{\code{Comparisons}}{A data frame of pairwise comparisons of
#'  treatments. This is computed only if argument \code{group} is \code{TRUE}}
#'  \item{\code{Groups}}{A data frame with compact letter display of pairwise
#'  comparisons of treatments. Means with at least one letter common are not
#'  significantly different statistically. This is computed only if argument
#'  \code{group} is \code{TRUE} } \item{\code{warning}}{A vector of warning
#'  messages (if any) captured during model fitting. }
#' @export
#'
#' @examples
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Scenario 1: Test treatments are replicated across all environments
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Example data
#' blk1 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
#'           7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
#' trt1 <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10,
#'           1, 2, 3, 4, 8, 11, 5, 1, 2, 3, 4, 12, 9, 1, 2, 3, 4, 7, 6, 10,
#'           1, 2, 3, 4, 7, 9, 12, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 8, 11, 10)
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
#'         78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
#'         76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
#'         78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
#' env1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'           2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
#'           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
#' data1 <- data.frame(env1, blk1, trt1, y1)
#' chks1 <- c(1, 2, 3, 4)
#'
#' # Convert block, treatment and environment to factors
#' data1$blk1 <- as.factor(data1$blk1)
#' data1$trt1 <- as.factor(data1$trt1)
#' data1$env1 <- as.factor(data1$env1)
#'
#' # Contingency tables of factors
#' table(data1$env1, data1$trt1)
#' table(data1$env1, data1$blk1)
#' table(data1$blk1, data1$trt1)
#'
#' # Results
#' out1 <- augmentedRCBD.menv(block = data1$blk1, treatment = data1$trt1,
#'                            env = data1$env1, y = data1$y1, checks = chks1,
#'                            scenario = 1, method.comp = "lsd", alpha = 0.05,
#'                            group = TRUE, console = TRUE)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Test treatments are not replicated across all environments
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Example data
#' blk2 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
#'           7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
#' trt2 <- c(1, 2, 3, 4, 7, 10, 11, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 12,
#'           1, 2, 3, 4, 16, 19, 13, 1, 2, 3, 4, 20, 17, 1, 2, 3, 4, 15, 14, 18,
#'           1, 2, 3, 4, 22, 25, 27, 1, 2, 3, 4, 21, 23, 1, 2, 3, 4, 24, 26, 28)
#' y2 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
#'         78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
#'         76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
#'         78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
#' env2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'           2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
#'           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
#' data2 <- data.frame(env2, blk2, trt2, y2)
#' chks2 <- c(1, 2, 3, 4)
#'
#' # Convert block, treatment and environment to factors
#' data2$blk2 <- as.factor(data2$blk2)
#' data2$trt2 <- as.factor(data2$trt2)
#' data2$env2 <- as.factor(data2$env2)
#'
#' # Contingency tables of factors
#' table(data2$env2, data2$trt2)
#' table(data2$env2, data2$blk2)
#' table(data2$blk2, data2$trt2)
#'
#' # Results
#' out2 <- augmentedRCBD.menv(block = data2$blk2, treatment = data2$trt2,
#'                            env = data2$env2, y = data2$y2, checks = chks2,
#'                            scenario = 2, method.comp = "lsd", alpha = 0.05,
#'                            group = TRUE, console = TRUE)
#'
augmentedRCBD.menv <- function(block, treatment, env, y, checks = NULL,
                               method.comp = c("lsd", "tukey", "none"),
                               scenario = c(1, 2),  alpha = 0.05, group = TRUE,
                               console = TRUE, simplify = FALSE,
                               truncate.means = TRUE) {

  aug.debug <- getOption("augmentedRCBD.debug", default = FALSE)

  # Checks ----

  # block
  if (!is.factor(block)) {
    stop('"block" should be of class "factor".')
  }
  # treatment
  if (!is.factor(treatment)) {
    stop('"treatment" should be of class "factor".')
  }
  # environment
  if (!is.factor(env)) {
    stop('"env" should be of class "factor".')
  }
  # y
  if (!(is.vector(y, mode = "integer") |
        is.vector(y, mode = "numeric"))) {
    stop('"y" should be a vector of class "numeric" or "integer".')
  }
  if (!(length(y) == length(treatment) &&
        length(treatment) == length(block))) {
    stop('"block", "treatment" and "y" are of unequal lengths.')
  }
  if (TRUE %in% is.na(y)) { # check for missing values
    stop('"y" has missing value(s).')
  }
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha < 1).')
  }
  # method.comp
  method.comp <- match.arg(method.comp, c("lsd", "tukey", "none"),
                           several.ok = FALSE)
  if (method.comp == "none") group <- FALSE
  if (group == FALSE) method.comp <- "none"

  # scenario
  scenario <- match.arg(scenario, several.ok = FALSE)

  # [TO DO]
  # -  Remove check inference
  # - Check for data balance scenario 1 and 2

  if (!missing(checks) && !is.null(checks)) {
    #if (!is.null(checks)) {
    checks <- as.character(checks)
    # checks are present in treatment levels
    if (FALSE %in% c(checks %in% levels(treatment))) {
      miss <- paste(checks[!(checks %in% levels(treatment))], collapse = ", ")
      stop(paste("Following check(s) are not present in treatment levels:\n",
                 paste(miss, collapse = ", ")))
    }

    tests <- levels(treatment)[!(levels(treatment) %in% checks)]
    test_counts <- table(env = droplevels(env[treatment %in% tests]),
                         tests = droplevels(treatment[treatment %in% tests]))
    test_counts <- data.frame(test_counts)
    # if (!all(test_counts$Freq == 1)) {
    if (any(test_counts$Freq > 1)) {
      rep_tests <- test_counts[test_counts$Freq > 1, ]

      rep_tests_collapsed <- tapply(rep_tests$tests, rep_tests$env,
                                    function(x) {
                                      paste(x, collapse = ", ")
                                    })
      rep_tests_collapsed <- rep_tests_collapsed[!is.na(rep_tests_collapsed)]

      warning(paste("The following test treatment(s) are replicated."),
              '\n',
              paste(paste("env_", names(rep_tests_collapsed), ": ",
                          rep_tests_collapsed, sep = ""),
                    collapse = " \n"))
    }
  } else {
    stop('"checks" are not specified.')
  }

  warn <- NULL

  withCallingHandlers({

    # Fix treatment order so that checks are in the beginning ----

    treatment <- factor(treatment,
                        levels = c(checks,
                                   setdiff(levels(treatment), checks)))

    # Basic details ----
    r <- unique(table(treatment))
    b <- nlevels(block) # no. of blocks
    ntr <- nlevels(treatment)  # no. of treatments
    nenv <- nlevels(env) # no. of environments

    # blockwisechecks <- as.data.frame.matrix(table(treatment, block))
    # blockwisechecks <- cbind(treatment = rownames(blockwisechecks),
    #                          blockwisechecks)
    # blockwisechecks <-
    #   blockwisechecks[blockwisechecks$treatment %in% checks, ]
    # rownames(blockwisechecks) <- NULL

    Details <- list(`Number of blocks` = b, `Number of treatments` = ntr,
                    `Number of environments` = nenv,
                    `Number of check treatments` = length(checks),
                    `Number of test treatments` = length(tests),
                    `Check treatments` =  checks)

    # tb <- data.frame(treatment, block)
    # tb$block <- as.character(tb$block)
    # tb$Block <- ifelse(tb$treatment %in% checks, "", tb$block)
    # tb$block <- NULL
    # tb <- unique(tb)

    # Get means table ----
    Means <- tapply(y, interaction(treatment, sep = "_"),
                    function(x) mean(x, na.rm = TRUE))
    mi <- tapply(y, interaction(treatment, sep = "_"),
                 function(x) min(x, na.rm = TRUE))
    ma <- tapply(y, interaction(treatment, sep = "_"),
                 function(x) max(x, na.rm = TRUE))
    n.rep <- tapply(y, interaction(treatment, sep = "_"),
                    function(x) length(na.omit(x)))
    sds <- tapply(y, interaction(treatment, sep = "_"),
                  function(x) sd(x, na.rm = TRUE))
    std.err <- sds / sqrt(n.rep)
    Means <- data.frame(treatment = names(Means), Means,
                        SE = std.err, r = n.rep,
                        Min = mi, Max = ma)

    Means <- Means[c("treatment", "Means", "SE", "r", "Min", "Max")]

    # ANOVA 1 - `ANOVA, Treatment Adjusted` ----
    # Get helmert contrasts for Type III SS
    options(contrasts = c("contr.helmert", "contr.poly"))

    # Create a factor for 'Block within Environment'
    # block_env <- factor(paste0(env, block))
    block2 <- interaction(env, block, drop = TRUE, sep = "_")

    if (aug.debug) {
      message("Starting Treatment Adjusted ANOVA.")
    }

    # augmented.env.aov <-
    #   aov(y ~ env + treatment + env/block - block + env:treatment)
    # augmented.env.aov <-
    #   aov(y ~ env + treatment + env:block + env:treatment)
    # # (with implicit nesting of block within env)
    # augmented.env.aov <-
    #   aov(y ~ env + treatment + block_env + env:treatment)
    # augmented.env.aov <-
    #   aov(y ~ env + treatment + env:block2 + env:treatment) # [\u2713]
    augmented.env.aov <-
      aov(y ~ env + treatment + env:block2 + env:treatment)

    if (aug.debug) {
      message("Completed Treatment Adjusted ANOVA.")
    }

    df.check <- length(checks) - 1
    df.treatment <- nlevels(treatment) - 1

    A1 <- summary(augmented.env.aov,
                  split = list(treatment = list(
                    Check = 1:df.check,
                    `Test and Test vs. Check` = (df.check + 1):df.treatment)))

    rownames(A1[[1]])[1] <-
      "Environment                                              "
    rownames(A1[[1]])[2] <-
      "Treatment (eliminating Blocks)                           "
    rownames(A1[[1]])[3] <-
      "  Treatment: Check                                       "
    rownames(A1[[1]])[4] <-
      "  Treatment: Test and Test vs. Check                     "
    rownames(A1[[1]])[5] <-
      "Block (within Environment, ignoring Treatments)          "
    rownames(A1[[1]])[6] <-
      "Environment \u00D7 Treatment interaction                      "
    rownames(A1[[1]])[7] <-
      "  Interaction: Check \u00D7 Environment                       "
    rownames(A1[[1]])[8] <-
      "  Interaction: Test and Test vs. Check \u00D7 Environment     "

    if (scenario == 2 & aug.debug == FALSE) {
      A1[[1]] <- A1[[1]][-(7:8), ]
    }

    # Calculate adjusted env, treatment and block effects ----
    options(contrasts = c("contr.sum", "contr.poly"))

    if (aug.debug) {
      message("Starting ANOVA for adjusted effects.")
    }

    if (scenario == 1) {
      augmented3.env.aov <-
        aov(y ~ env + treatment + env:block2 + env:treatment)
    }

    if (scenario == 2) {
      augmented3.env.aov <- aov(y ~ env + treatment + block2)
    }

    if (aug.debug) {
      message("Completed ANOVA for adjusted effects.")
    }

    ## Compute reference grid ----
    # This is memory intensive. As the number of treatment levels increases,
    # the grid size can also explode to out-of-memory size.

    if (aug.debug) {
      message("Starting ref_grid creation.")
    }

    refgrid <-
      emmeans::ref_grid(object = augmented3.env.aov,
                        nesting = "block2 %in% env")

    if (aug.debug) {
      message("Completed ref_grid creation.")
    }

    # emm_suppress_msgs <- c(
    #   "A nesting structure was detected in the fitted model",
    #   "Results may be misleading due to involvement in interactions"
    # )
    #
    # refgrid <-
    #   withCallingHandlers(
    #     emmeans::ref_grid(object = augmented3.env.aov),
    #
    #     message = function(m) {
    #       msg_text <- conditionMessage(m)
    #
    #       if (any(vapply(emm_suppress_msgs, grepl, logical(1), msg_text))) {
    #         invokeRestart("muffleMessage")
    #       } else {
    #         invokeRestart("muffleMessage")
    #       }
    #     }
    #   )

    effects.env <- emm_adj_effects(refgrid, "env")
    effects.treatment <-
      emm_adj_effects(refgrid, "treatment") # averaged over env
    effects.block <- emm_adj_effects(refgrid, "block2")

    `Overall adjusted mean` <- attributes(effects.treatment)$overall

    # ANOVA 2 - `ANOVA, Block Adjusted` ----
    # Get contrast matrix for differentiating between check and test treatments
    contr.augmented <- function(n1, n2){
      m1 <- contr.helmert(n1)
      m2 <- contr.helmert(n2)
      m10 <- cbind(m1, matrix(0, nrow(m1), ncol(m2)))
      m02 <- cbind(matrix(0, nrow(m2), ncol(m1)), m2)
      rbind(m10, m02)
    }

    contrasts(treatment) <- contr.augmented(df.check + 1,
                                            df.treatment - df.check)

    if (aug.debug) {
      message("Starting Treatment Adjusted ANOVA.")
    }

    augmented2.env.aov <- aov(y ~ env + treatment + env:block2 + env:treatment)

    if (aug.debug) {
      message("Completed Treatment Adjusted ANOVA.")
    }

    A2 <- summary(augmented2.env.aov,
                  split = list(
                    treatment = list(
                      Check = 1:df.check,
                      Test = (df.check + 1):(df.treatment - 1),
                      `Test vs. check` = df.treatment)
                  ))

    rownames(A2[[1]])[1] <-
      "Environment                                              "
    rownames(A2[[1]])[2] <-
      "Treatment (ignoring Blocks)                              "
    rownames(A2[[1]])[3] <-
      "  Treatment: Check                                       "
    rownames(A2[[1]])[4] <-
      "  Treatment: Test                                        "
    rownames(A2[[1]])[5] <-
      "  Treatment: Test vs. Check                              "
    rownames(A2[[1]])[6] <-
      "Block (within Environment, eliminating Treatments)       "
    rownames(A2[[1]])[7] <-
      "Environment \u00D7 Treatment interaction                      "
    rownames(A2[[1]])[8] <-
      "  Interaction: Check \u00D7 Environment                       "
    rownames(A2[[1]])[9] <-
      "  Interaction: Test \u00D7 Environment                        "
    rownames(A2[[1]])[10] <-
      "  Interaction: Test vs. Check \u00D7 Environment              "

    if (scenario == 2 & aug.debug == FALSE) {
      A2[[1]] <- A2[[1]][-(8:10), ]
    }

    # Adjusted means ----

    # # Manual ----
    # mean.adj1 <-
    #   data.frame(mean.adj = `Overall adjusted mean` +
    #                effects.treatment[1:(df.check + 1)])
    # mean.adj1$treatment <- rownames(mean.adj1)
    # mean.adj2 <-
    #   data.frame(mean.adj = `Overall adjusted mean` +
    #                effects.treatment[(df.check + 2):(df.treatment + 1)])
    # mean.adj2$treatment <- rownames(mean.adj2)
    # mean.adj <- rbind(mean.adj1, mean.adj2)
    #
    # Means <- merge.data.frame(Means, mean.adj, by = "treatment", all = TRUE)
    # colnames(Means) <- c("treatment", "Means", "SE", "r",
    #                      "Min", "Max", "Adjusted Means")

    ## With emmeans ----
    LSMeans <- emmeans::emmeans(object = refgrid,
                                specs = "treatment")

    LSMeans2 <- summary(LSMeans)[, c("treatment", "emmean")]
    colnames(LSMeans2) <- c("treatment", "Adjusted Means")

    Means <- merge.data.frame(Means, LSMeans2, by = "treatment", all = TRUE)
    colnames(Means) <- c("treatment", "Means", "SE", "r",
                         "Min", "Max", "Adjusted Means")

    if (simplify == TRUE) {
      A1 <- data.frame(A1[[1]])
      A1 <- cbind(Source = trimws(rownames(A1)), A1)
      A2 <- data.frame(A2[[1]])
      A2 <- cbind(Source = trimws(rownames(A2)), A2)
      rownames(A1) <- NULL
      rownames(A2) <- NULL
    }

    # Grouping of treatments ----
    Comparison <- NULL
    Groups <- NULL

    if (group == TRUE) {
      if (method.comp == "lsd") adjust <- "none"
      if (method.comp == "tukey") adjust <- "tukey"

      Comparison <- data.frame(summary(pairs(LSMeans, adjust = adjust)))
      Groups <- data.frame(multcomp::cld(LSMeans, adjust = adjust))

      Comparison$sig <- ifelse(Comparison$p.value < 0.001, "***",
                               ifelse(Comparison$p.value < 0.01, "**",
                                      ifelse(Comparison$p.value < 0.05, "*",
                                             "")))
      colnames(Groups) <- c("Treatment", "Adjusted Means", "SE", "df",
                            "lower.CL", "upper.CL", "Group")
    }

    ## Compute SE and CD for various comparisons ----
    alpha = 0.05

    augmented3.env.anova <- anova(augmented3.env.aov)
    MSE <- augmented3.env.anova[[3]][5]

    CV <- sqrt(MSE) * 100 / mean(augmented3.env.aov$fitted.values)

    r <- nlevels(block2) # Number of blocks
    c <- length(checks) # Number of check treatments
    l <- augmented3.env.anova$Df[1] + 1 # Number of environments
    t0 <- qt(1 - (alpha / 2), augmented3.env.aov$df.residual)

    S <- c("Control Treatment Means", "Two Test Treatments (Same Block)",
           "Two Test Treatments (Different Blocks)",
           "A Test Treatment and a Control Treatment")

    # Two Control Treatments (averaged over environments)
    SE.check <- sqrt(2 * MSE / (r * l))
    # Two Augmented Treatments (Same Block) (averaged over environments)
    SE.test1 <- sqrt(2 * MSE / l)
    # Two Augmented Treatments(Different Blocks) (averaged over environments)
    SE.test2 <- sqrt(2 * MSE * (1 + (1 / c)) / l)
    # A Test Treatment and a Control Treatment (averaged over environments)
    SE.testcheck <- sqrt(MSE * (1 + (1 / r) + (1 / c) + (1 / (r * c))) / l)

    SECD <- data.frame(`Std. Error of Diff.` =  c(SE.check, SE.test1,
                                                  SE.test2, SE.testcheck),
                       row.names = S, check.names = FALSE)
    SECD$CD <- t0 * SECD$`Std. Error of Diff.`
    colnames(SECD) <- c("Std. Error of Diff.",
                        paste("CD (", alpha * 100, "%)", sep = ""))

    if (method.comp == "tukey") {
      q0 <- qtukey(p = 1 - alpha, nmeans = nlevels(treatment),
                   df = augmented3.env.aov$df.residual)

      SECD$THSD <- c((q0 * SECD[1:3,]$`Std. Error of Diff.`) / sqrt(2), 0)
      hm <- 4/(1 + (1 / r) + (1 / c) + (1 / (r * c)))
      SECD[4,]$THSD <- q0 * sqrt(MSE/hm)
      colnames(SECD) <- c("Std. Error of Diff.",
                          paste("CD (", alpha * 100, "%)", sep = ""),
                          paste("Tukey HSD (", alpha * 100, "%)", sep = ""))
    }

    rm(augmented.env.aov, augmented2.env.aov,
       augmented3.env.aov, augmented3.env.anova, refgrid)

    # Truncate negative adjusted means ----
    if (any(Means$`Adjusted Means` < 0)) {
      negadjmeans <- which(Means$`Adjusted Means` < 0)
      negadjmeanst <- as.character(Means$Treatment[negadjmeans])

      negmsg <-
        paste('Negative adjusted means were generated for the',
              'following treatment(s)',
              '\n', paste(negadjmeanst, collapse = ", "))

      if (truncate.means == TRUE) {
        Means$`Adjusted Means`[Means$`Adjusted Means` < 0] <- 0

        if (group == TRUE) {
          Groups$`Adjusted Means`[Groups$`Adjusted Means` < 0] <- 0
        }

        warning(paste(negmsg, '\n',
                      'They were truncated to zero'))
      } else {
        warning(negmsg)
      }
    }

  }, warning = function(w) {
    warn <<- append(warn, cli::ansi_strip(conditionMessage(w)))
    warning(conditionMessage(w), call. = FALSE)
    invokeRestart("muffleWarning")
  })

  output <- list(Details = Details, Means = Means,
                 `ANOVA, Treatment Adjusted` = A1,
                 `ANOVA, Block Adjusted` = A2, `Block effects` = effects.block,
                 `Treatment effects` = effects.treatment, `Std. Errors` = SECD,
                 `Overall adjusted mean` = `Overall adjusted mean`,
                 `CV` = CV, `Comparison method` = method.comp,
                 Comparisons = Comparison, Groups = Groups, warnings = warn)

  # Set Class
  class(output) <- "augmentedRCBD.menv"

  if (console) {
    print.augmentedRCBD.menv(output)
  }

  return(output)

}

emm_adj_effects <- function(grid, fct, ...) {

  # Vector of message patterns to suppress
  suppress_patterns <- c(
    "A nesting structure was detected in the fitted model",
    "Results may be misleading due to involvement in interactions"
  )

  # Use withCallingHandlers so muffleMessage restart is available
  emm <- withCallingHandlers(
    emmeans::emmeans(object = grid, specs = fct, ...),

    message = function(m) {
      msg_text <- conditionMessage(m)

      # If message matches any suppression pattern -> suppress
      if (any(vapply(suppress_patterns, grepl, logical(1), msg_text))) {
        invokeRestart("muffleMessage")
      } else {
        invokeRestart("muffleMessage")
      }
    }
  )

  emm <- summary(emm)
  overall <- mean(emm$emmean)

  eff <- emm$emmean - overall
  names(eff) <- emm[, fct]

  attr(eff, "overall") <- overall

  return(eff)
}
