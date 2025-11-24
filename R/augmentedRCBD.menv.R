

#' @export
augmentedRCBD.menv <- function(block, treatment, env, y, checks = NULL,
                               method.comp = c("lsd", "tukey", "none"),
                               alpha = 0.05, group = TRUE, console = TRUE,
                               simplify = FALSE, truncate.means = TRUE) {

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
  if (!(is.vector(y, mode = "integer") | is.vector(y, mode = "numeric"))) {
    stop('"y" should be a vector of class "numeric" or "integer".')
  }
  if (!(length(y) == length(treatment) && length(treatment) == length(block))) {
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

  # [TO DO]
  # -  Remove check inference
  # - Check for data balance scenarion 1 and 2

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
    # blockwisechecks <- blockwisechecks[blockwisechecks$treatment %in% checks, ]
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
      message("Starting Treatment Adjusted ANOVA")
    }

    # augmented.env.aov <- aov(y ~ env + treatment + env/block - block + env:treatment)
    # augmented.env.aov <- aov(y ~ env + treatment + env:block + env:treatment)
    # augmented.env.aov <- aov(y ~ env + treatment + block_env + env:treatment) # (with implicit nesting of block within env)
    # augmented.env.aov <- aov(y ~ env + treatment + env:block2 + env:treatment) # [\u2713]
    augmented.env.aov <- aov(y ~ env + treatment + env:block2 + env:treatment)

    if (aug.debug) {
      message("Completed Treatment Adjusted ANOVA")
    }

    df.check <- length(checks) - 1
    df.treatment <- nlevels(treatment) - 1

    A1 <- summary(augmented.env.aov,
                  split = list(treatment = list(
                    Check = 1:df.check,
                    `Test and Test vs. Check` = (df.check + 1):df.treatment)))

    rownames(A1[[1]])[1] <- "Environment                                              "
    rownames(A1[[1]])[2] <- "Treatment (eliminating Blocks)                           "
    rownames(A1[[1]])[3] <- "  Treatment: Check                                       "
    rownames(A1[[1]])[4] <- "  Treatment: Test and Test vs. Check                     "
    rownames(A1[[1]])[5] <- "Block (within Environment, ignoring Treatments)          "
    rownames(A1[[1]])[6] <- "Environment \u00D7 Treatment interaction                      "
    rownames(A1[[1]])[7] <- "  Interaction: Check \u00D7 Environment                       "
    rownames(A1[[1]])[8] <- "  Interaction: Test and Test vs. Check \u00D7 Environment     "

    # Calculate adjusted env, treatment and block effects ----
    options(contrasts = c("contr.sum", "contr.poly"))

    if (aug.debug) {
      message("Starting ANOVA for adjusted effects")
    }

    augmented3.env.aov <- aov(y ~ env + treatment + env:block2 + env:treatment)

    if (aug.debug) {
      message("Completed ANOVA for adjusted effects")
    }

    ## Compute reference grid ----
    # This is memory intensive. As the number of treatment levels increases,
    # the grid size can also explode to out-of-memory size.

    if (aug.debug) {
      message("Starting ref_grid creation")
    }

    refgrid <-
      emmeans::ref_grid(object = augmented3.env.aov,
                        nesting = "block2 %in% env")

    if (aug.debug) {
      message("Completed ref_grid creation")
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
      message("Starting Treatment Adjusted ANOVA")
    }

    augmented2.env.aov <- aov(y ~ env + treatment + env:block2 + env:treatment)

    if (aug.debug) {
      message("Completed Treatment Adjusted ANOVA")
    }

    A2 <- summary(augmented2.env.aov,
                  split = list(treatment = list(Check = 1:df.check,
                                                Test = (df.check + 1):(df.treatment - 1),
                                                `Test vs. check` = df.treatment)))

    rownames(A2[[1]])[1] <- "Environment                                              "
    rownames(A2[[1]])[2] <- "Treatment (ignoring Blocks)                              "
    rownames(A2[[1]])[3] <- "  Treatment: Check                                       "
    rownames(A2[[1]])[4] <- "  Treatment: Test                                        "
    rownames(A2[[1]])[5] <- "  Treatment: Test vs. Check                              "
    rownames(A2[[1]])[6] <- "Block (within Environment, eliminating Treatments)       "
    rownames(A2[[1]])[7] <- "Environment \u00D7 Treatment interaction                      "
    rownames(A2[[1]])[8] <- "  Interaction: Check \u00D7 Environment                       "
    rownames(A2[[1]])[9] <- "  Interaction: Test \u00D7 Environment                        "
    rownames(A2[[1]])[10] <- "  Interaction: Test vs. Check \u00D7 Environment              "

    # Adjusted means ----

    ## Manual ----
    # mean.adj1 <- data.frame(mean.adj = `Overall adjusted mean` + effects.treatment[1:(df.check + 1)])
    # mean.adj1$treatment <- rownames(mean.adj1)
    # mean.adj2 <- data.frame(mean.adj = `Overall adjusted mean` + effects.treatment[(df.check + 2):(df.treatment + 1)])
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
      Comparison <- data.frame(summary(pairs(LSMeans, adjust = "tukey")))
      Groups <- data.frame(multcomp::cld(LSMeans, adjust = "tukey"))
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

    SE.check <- sqrt(2 * MSE / (r * l)) #Two Control Treatments (averaged over environments)
    SE.test1 <- sqrt(2 * MSE / l) #Two Augmented Treatments (Same Block) (averaged over environments)
    SE.test2 <- sqrt(2 * MSE * (1 + (1 / c)) / l) #Two Augmented Treatments(Different Blocks) (averaged over environments)
    SE.testcheck <- sqrt(MSE * (1 + (1 / r) + (1 / c) + (1 / (r * c))) / l) #A Test Treatment and a Control Treatment (averaged over environments)

    SECD <- data.frame(`Std. Error of Diff.` =  c(SE.check, SE.test1,
                                                  SE.test2, SE.testcheck),
                       row.names = S, check.names = FALSE)
    SECD$CD <- t0 * SECD$`Std. Error of Diff.`
    colnames(SECD) <- c("Std. Error of Diff.",
                        paste("CD (", alpha * 100, "%)", sep = ""))

    if (method.comp == "tukey") {
      q0 <- qtukey(p = 1 - alpha, nmeans = nlevels(treatment),
                   df = augmented3.env.aov$df.residual)

      SECD$THSD <- c((q0 * SECD[1:3,]$`Std. Error of Diff.`)/sqrt(2), 0)
      hm <- 4/(1 + (1 / r) + (1 / c) + (1 / (r * c)))
      SECD[4,]$THSD <- q0 * sqrt(MSE/hm)
      colnames(SECD) <- c("Std. Error of Diff.",
                          paste("CD (", alpha * 100, "%)", sep = ""),
                          paste("Tukey HSD (", alpha * 100, "%)", sep = ""))
    }

    rm(augmented.env.aov, augmented2.env.aov,
       augmented3.env.aov, augmented3.env.anova, refgrid)

    # Truncate negative adjusted means
    if (any(Means$`Adjusted Means` < 0)){
      negadjmeans <- which(Means$`Adjusted Means` < 0)
      negadjmeanst <- as.character(Means$Treatment[negadjmeans])

      negmsg <- paste('Negative adjusted means were generated for the following treatment(s)',
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
