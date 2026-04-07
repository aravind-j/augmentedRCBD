# For forbidden combos
# Checks and tests partition treatment. Making checks random but tests fixed breaks
# the nesting/partition logic and creates a non-identifiable treatment variance decomposition.

# trouBBlme4SolveR

# data_df$test <- as.factor(data_df$test)
# data_df$check <- as.factor(data_df$check)


#' Title
#'
#' @param block
#' @param treatment
#' @param env
#' @param y
#' @param checks
#' @param env.random
#' @param check.random
#' @param test.random
#' @param scenario
#' @param drop.nonsig.interaction
#' @param scenario.violation.threshold
#' @param df_method
#' @param console
#'
#' @returns
#'
#' @importFrom lme4 lmerControl isSingular fixef ranef VarCorr
#' @importFrom lmerTest lmer ranova
#' @importFrom stats aggregate AIC BIC as.formula model.frame model.matrix update
#' @importFrom emmeans emmeans
#' @importFrom dplyr %>% bind_rows group_by n summarize
#' @importFrom utils tail
#' @export
#'
#' @examples
augmentedRCBD.mix <- function(block, treatment, env = NULL,
                              y, checks = NULL,
                              env.random = TRUE,
                              check.random = TRUE,
                              test.random = TRUE,
                              # interaction = TRUE,
                              drop.nonsig.interaction = TRUE,
                              scenario = c("I", "II"),
                              scenario.violation.threshold = 0.1,
                              df_method = c("kenward-roger", "satterthwaite"),
                              console = TRUE) {

  # Checks ----

  df_method <- match.arg(df_method)
  scenario <- match.arg(scenario)

  if (check.random && !test.random) {
    stop("Forbidden combination: check.random = TRUE & test.random = FALSE")
  }

  if (is.null(env) && drop.nonsig.interaction) {
    warning("interaction ignored because env = NULL")
    drop.nonsig.interaction <- FALSE
  }

  # block
  if (!is.factor(block)) {
    stop('"block" should be of class "factor".')
  }
  # treatment
  if (!is.factor(treatment)) {
    stop('"treatment" should be of class "factor".')
  }
  # environment
  if (!is.null(env) && !is.factor(env)) {
    stop('"env" should be of class "factor".')
  }
  # y
  if (!(is.vector(y, mode = "integer") |
        is.vector(y, mode = "numeric"))) {
    stop('"y" should be a vector of class "numeric" or "integer".')
  }
  # Size equality of factors
  if (is.null(env)) {
    if (length(unique(lengths(list(y, treatment, block)))) != 1) {
      stop('"block", "treatment" and "y" are of unequal lengths.')
    }
  } else {
    if (!(length(unique(lengths(list(y, treatment, block, env)))) == 1)) {
      stop('"block", "treatment", "env", and "y" are of unequal lengths.')
    }
  }
  # Check for missing values
  if (TRUE %in% is.na(y)) {
    warning('"y" has missing value(s).')
  }

  # Prepare factors ----

  if (!is.null(env)) {
    block2 <- interaction(env, block, drop = TRUE, sep = "_")
  }

  tests <- levels(treatment)[!(levels(treatment) %in% checks)]

  test <- ifelse(treatment %in% checks, 0, 1)
  check <- ifelse(treatment %in% checks,
                  treatment, "test")

  # Fix treatment order so that checks are in the beginning ----
  # treatment_org <- treatment
  treatment <- factor(treatment,
                      levels = c(checks, setdiff(levels(treatment),
                                                 checks)))
  if (!is.null(env)) {
    data_df <- data.frame(y, block2, treatment, env, test, check)
  } else {
    data_df <- data.frame(y, block, treatment, test, check)
  }

  # Force scenarion to be NULL when there is no env
  if (is.null(env)) {
    scenario = NULL
  }

  # Automatic scenario validation ----

  if (!is.null(env)) {

    ## Identify tests vs checks
    test_flag <- ifelse(as.character(treatment) %in%
                          as.character(unique(treatment[check == "test"])),
                        1, 0)

    dat_tmp <- data.frame(trt = treatment,
                          env = env,
                          test = test_flag)

    test_env_counts <-
      stats::aggregate(env ~ trt,
                       data = dat_tmp[dat_tmp$test == 1, ],
                       FUN = function(x) length(unique(x)))

    if (scenario == "I") {

      if (all(test_env_counts$env < 2)) {
        stop(
          "Scenario I violation: ",
          "All test treatments appear in only one environment:\n"
        )
      } else if (any(test_env_counts$env < 2)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env < 2])

        if ((length(bad) / length(tests)) > scenario.violation.threshold) {

          stop(sprintf(
            paste0(
              'Scenario I violation: %.0f%% of test treatments appear ',
              'in only one environment, exceeding the ',
              '"scenario.violation.threshold" (%.0f%%).'
            ),
            (length(bad) / length(tests)) * 100,
            scenario.violation.threshold * 100
          ))

        } else {
          warning(
            "Scenario I violation: ",
            "The following ", length(bad),
            " test treatments appear in only one environment:\n\n",
            paste(bad, collapse = ", "), "\n"
          )
        }

      }
    }

    if (scenario == "II") {
      if (any(test_env_counts$env > 1)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env > 1])

        if ((length(bad) / length(tests)) > scenario.violation.threshold) {

          stop(sprintf(
            paste0(
              'Scenario II violation: %.0f%% of test treatments appear ',
              'in multiple environments, exceeding the ',
              '"scenario.violation.threshold" (%.0f%%).'
            ),
            (length(bad) / length(tests)) * 100,
            scenario.violation.threshold * 100
          ))

        } else {
          warning(
            "Scenario II violation: ",
            "The following ", length(bad),
            " test treatments appear in multiple environments:\n",
            paste(bad, collapse = ", "), "\n"
          )
        }
      }
    }
  }

  #  Build formula ----

  menv <- ifelse(is.null(env), FALSE, TRUE)

  frmla_int <-
    build_formula(block = ifelse(is.null(env), "block", "block2"),
                  treatment = "treatment",
                  test = "test", check = "check",
                  env = "env", y = "y",
                  menv = menv,
                  env.random = env.random,
                  check.random = check.random,
                  test.random = test.random,
                  scenario = scenario, interaction = TRUE)

  if (menv == TRUE) {
    frmla_wo_int <-
      build_formula(block = ifelse(is.null(env), "block", "block2"),
                    treatment = "treatment",
                    test = "test", check = "check",
                    env = "env", y = "y",
                    menv = menv,
                    env.random = env.random,
                    check.random = check.random,
                    test.random = test.random,
                    scenario = scenario, interaction = FALSE)
  }

  # Fit the model ----
  mod_final <-
    lmerTest::lmer(frmla_int, data = data_df,
                   REML = FALSE) # Use ML for testing

  if (menv && drop.nonsig.interaction) {
    ## Decision on inclusion of env test-treatment interactions ----
    mod_wo_int <-
      lmerTest::lmer(frmla_wo_int, data = data_df,
                     REML = FALSE) # Use ML for testing

    LRT <- anova(mod_wo_int, mod_final)
    # LRT_p <- tibble::deframe(broom::tidy(LRT)[2, "p.value"])
    LRT_p <- tail(LRT[["Pr(>Chisq)"]], 1)

    if (LRT_p > 0.05) {
      mod_final <- mod_wo_int
    }
  }

  # Use bobyqa if model is still singular
  if (isSingular(mod_final, tol = 1e-4)) {
    mod_final <-
      update(mod_final, control = lmerControl(optimizer = "bobyqa"),
             data = model.frame(mod_final))
  }

  ## Refit with REML ----
  mod_final <- update(mod_final, REML = TRUE, data = model.frame(mod_final))


  # Diagnostics ----

  vcov_df <- data.frame(VarCorr(mod_final))

  vcov_out <- vcov_df$vcov
  names(vcov_out) <- vcov_df$grp
  rm(vcov_df)

  mod_diag <-
    c(list(conv_opt = mod_final@optinfo$conv$opt,
           conv_lme4 = paste(mod_final@optinfo$conv$lme4, collapse = "\n"),
           opt_message = paste(mod_final@optinfo$message, collapse = "\n"),
           opt_warnings = paste(mod_final@optinfo$warnings, collapse = "\n"),
           singular = isSingular(mod_final)),
      as.list(vcov_out))


  AIC_val <- AIC(mod_final)
  BIC_val <- BIC(mod_final)

  mod_diag <- c(mod_diag, AIC = AIC_val, BIC = BIC_val)

  # ANOVA ----

  ## Fixed effects ----

  # if (length(lme4::fixef(mod_final)) > 0) {
  if (any(colnames(model.matrix(mod_final)) != "(Intercept)")) {
    # fixef_anova <-
    #   getS3method("anova", "lmerModLmerTest",
    #               envir = asNamespace("lmerTest"))(mod_final,
    #                                                type = 3,
    #                                                ddf = "Satterthwaite")

    fixef_anova <- anova(mod_final,  type = 3,
                         ddf = "Satterthwaite")


  } else {
    fixef_anova <- NULL
  }

  ## Random effects ----

  ranef_anova <- lmerTest::ranova(mod_final)


  # Adjusted means ----

  check_mean <- if (check.random) {
    "BLUP"
  } else {
    "BLUE"
  }

  test_mean <- if (test.random) {
    "BLUP"
  } else {
    "BLUE"
  }

  test_mean <- if (!is.null(scenario) && scenario == "II") {
    paste0(test_mean, "_within_env")
  } else {
    test_mean
  }

  aug_adj_means <-
    get_treatment_means(mod = mod_final, checks = checks,
                        tests = tests, env = env,
                        check_mean = check_mean,
                        test_mean = test_mean,
                        df_method = df_method)

  # Final output ----

  output <- list(Details = NULL,
                 Model = mod_final,
                 `Model Diagnostics` = mod_diag,
                 `ANOVA, Fixed Effects` = fixef_anova,
                 `LRT, Random Effects` = ranef_anova,
                 `Means` = aug_adj_means)

  # Set Class
  class(output) <- "augmentedRCBD.mix"

  # if (console) {
  #   print.augmentedRCBD.mix(output)
  # }

  return(output)

}


# Function to get adjusted means ----
get_treatment_means <-
  function(mod, checks, tests, env = NULL,
           check_mean = c("BLUE","BLUP"),
           test_mean = c("BLUE","BLUP",
                         "BLUE_within_env",
                         "BLUP_within_env"),
           df_method = c("kenward-roger", "satterthwaite")) {

    df_method <- match.arg(df_method)

    # Setup
    data_df <- mod@frame
    has_env <- "env" %in% names(data_df)

    # Get model effects
    beta <- fixef(mod)
    re_list <- ranef(mod, condVar = TRUE)

    has_fixed_check <- any(grepl("^check", names(beta)))
    has_fixed_treatment <- any(grepl("^treatment", names(beta)))
    has_fixed_env_check <-
      any(grepl("^env[0-9]+:check[0-9]+$", names(beta)))
    has_fixed_treatment_env <-
      any(grepl("^treatment[0-9]+:env[0-9]+$", names(beta)))

    fetch <- ifelse(has_fixed_check, "check", "treatment")
    fetch <- ifelse(has_fixed_env_check, "treatment | env", fetch)

    nstg_list <- NULL
    if (has_fixed_env_check) {
      nstg_list <- list(treatment = c("env", "check"))
    }

    has_random_treatment <- "treatment" %in% names(re_list)
    has_random_treatment_test <- "treatment:test" %in% names(re_list)

    key <- paste(check_mean, test_mean, sep = "_")


    cases <- list(
      "BLUP_BLUP" = function() {
        out <- get_blup(mod = mod, within_env = FALSE,
                        has_random_treatment = has_random_treatment,
                        has_random_treatment_test = has_random_treatment_test)
        out$type <- "BLUP"
        out
      },
      "BLUE_BLUP" = function() {
        blue_out <-
          get_blue(mod = mod, has_env = has_env,
                   within_env = FALSE, fetch = "check",
                   emm_df = df_method, nesting = NULL)
        blue_out <- blue_out[blue_out$treatment %in% checks, ]
        blue_out$type <- "BLUE"
        blup_out <-
          get_blup(mod = mod, within_env = FALSE,
                   has_random_treatment = has_random_treatment,
                   has_random_treatment_test = has_random_treatment_test)
        blup_out$treatment <- gsub(":.*", "", blup_out$treatment)
        blup_out <- blup_out[blup_out$treatment %in% tests, ]
        blup_out$type <- "BLUP"
        dplyr::bind_rows(blue_out, blup_out)
      },
      "BLUE_BLUE" = function() {
        out <- get_blue(mod = mod, has_env = has_env,
                        within_env = FALSE, fetch = fetch,
                        nesting = nstg_list,
                        emm_df = df_method)

        if (has_fixed_env_check) {
          out <- out %>%
            group_by(treatment) %>%
            summarize(
              # arithmetic mean of the BLUEs across environments
              mean = mean(mean),
              # SE = sqrt(mean(SE^2)),  # approximate
              # SE of the average
              SE = sqrt(sum(SE^2) / (n()^2)),
              # Placeholder for DF
              df = NA,
              .groups = "drop"
            )

          out$type <- "BLUE_avg"
        } else {
          out$type <- "BLUE"
        }

        out

      },
      "BLUP_BLUP_within_env" = function() {
        out <- get_blup(mod = mod, within_env = TRUE,
                        has_random_treatment = has_random_treatment,
                        has_random_treatment_test = has_random_treatment_test)
        out

      },
      "BLUE_BLUP_within_env" = function() {
        blue_out <-
          get_blue(mod = mod, has_env = has_env,
                   within_env = FALSE, fetch = "check",
                   emm_df = df_method, nesting = NULL)
        blue_out <- blue_out[blue_out$treatment %in% checks, ]
        blue_out$type <- "BLUE"
        blup_out <-
          get_blup(mod = mod, within_env = TRUE,
                   has_random_treatment = has_random_treatment,
                   has_random_treatment_test = has_random_treatment_test)
        blup_out$treatment <- gsub(":.*", "", blup_out$treatment)
        blup_out <- blup_out[blup_out$treatment %in% tests, ]
        blup_out$type <- "BLUP"
        dplyr::bind_rows(blue_out, blup_out)
      },
      "BLUE_BLUE_within_env" = function() {
        check_out <- get_blue(mod = mod, has_env = has_env,
                              within_env = FALSE, fetch = "check",
                              nesting = nstg_list,
                              emm_df = df_method)
        check_out <- check_out[check_out$treatment %in% checks, ]
        test_out <- get_blue(mod = mod, has_env = has_env,
                             within_env = FALSE, fetch = "treatment",
                             nesting = nstg_list,
                             emm_df = df_method)
        test_out <- test_out[test_out$treatment %in% tests, ]
        dplyr::bind_rows(check_out, test_out)
      }
    )

    if (!key %in% names(cases)) {
      stop("Invalid combination")
    }

    res <- cases[[key]]()

    return(res)

  }

# BLUE using emmeans ----
get_blue <- function(mod = mod, within_env = FALSE, has_env = has_env,
                     fetch, nesting, emm_df) {

  if (within_env && has_env && fetch != "check") {
    emm <- emmeans(mod,
                   as.formula(paste0("~ ", fetch, " | ", "env")),
                   lmer.df = emm_df)

    emm_df_out <- as.data.frame(emm)

    # average across env
    agg <- aggregate(cbind(emmean, SE, df) ~ treatment,
                     data = emm_df_out,
                     FUN = function(x) c(mean = mean(x)))

    out <- data.frame(
      treatment = agg$treatment,
      mean = agg$emmean[, "mean"],
      SE = agg$SE[, "mean"],
      df = agg$df[, "mean"]
    )

  } else {
    emm <- emmeans(mod,
                   as.formula(paste0("~ ", fetch)),
                   lmer.df = emm_df, nesting = nesting)

    out <- as.data.frame(emm)[, c(gsub(" \\| env", "", fetch),
                                  "emmean", "SE", "df")]
    names(out) <- c("treatment", "mean", "SE", "df")

  }

  return(out)
}

get_blup <- function(mod, within_env = FALSE,
                     has_random_treatment,
                     has_random_treatment_test) {

  beta0 <- fixef(mod)["(Intercept)"]

  # extract random effects
  if (has_random_treatment) {
    re <- re_list["treatment"][[1]]
  } else if (has_random_treatment_test) {
    re <- re_list["treatment:test"][[1]]
  } else {
    stop("No treatment random effect in model")
  }

  re_vals <- re[,1]
  names(re_vals) <- rownames(re)

  # conditional variance
  postVar <- attr(re, "postVar")

  # SE extraction
  se_vals <- sapply(1:length(re_vals),
                    function(i) {
                      sqrt(postVar[,,i])
                    })

  names(se_vals) <- names(re_vals)

  # BLUP mean
  mean_vals <- beta0 + re_vals

  out <- data.frame(treatment = names(mean_vals),
                    mean = as.numeric(mean_vals),
                    SE = as.numeric(se_vals),
                    df = NA)

  return(out)
}

# Formula builder ----
build_formula <- function(block = "block2", treatment = "treatment",
                          test = "test", check = "check", env = "env",
                          y = "y",
                          menv = TRUE,
                          env.random = TRUE,
                          check.random = TRUE,
                          test.random = TRUE,
                          interaction = TRUE,
                          scenario = c("I", "II")) {

  scenario <- match.arg(scenario)

  ## Forbidden configuration (Rule 99)  ----
  if (check.random && !test.random) {
    stop("Forbidden: check.random = TRUE & test.random = FALSE")
  }

  ## Scenario II never allows test x env interaction
  if (scenario == "II") interaction <- FALSE

  fixed  <- character()
  random <- character()

  ## Block term (Rule 1) ----

  if (menv) {
    random <- c(random, sprintf("(1|%s:%s)", env, block))
  } else {
    random <- c(random, sprintf("(1|%s)", block))
  }

  ## Environment main effect (Rule 2) ----

  if (menv) {
    if (env.random) {
      random <- c(random, sprintf("(1|%s)", env))
    } else {
      fixed <- c(fixed, env)
    }
  }

  ## Treatment main effects (Rules 3–6)  ----

  trt_case <- NULL

  if (check.random && test.random) {
    trt_case <- "both_random"
    random <- c(random, sprintf("(1|%s)", treatment))
  }

  if (!check.random && test.random) {
    trt_case <- "check_fixed"
    fixed  <- c(fixed, check)
    random <- c(random, sprintf("(1|%s:%s)", treatment, test))
  }

  if (!check.random && !test.random) {
    trt_case <- "both_fixed"
    fixed <- c(fixed, treatment)
  }

  ## Interactions (Rules 7–12)  ----

  if (menv) {

    if (scenario == "I") {

      if (interaction) {

        if (trt_case == "both_random") {
          random <- c(random, sprintf("(1|%s:%s)", env, treatment))
        }

        if (trt_case == "check_fixed") {
          fixed  <- c(fixed, sprintf("%s:%s", env, check))
          random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, test))
        }

        if (trt_case == "both_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, treatment))
        }

      } else {

        if (trt_case == "both_random") {
          random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, check))
        }

        if (trt_case == "check_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, check))
        }

        if (trt_case == "both_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, check))
        }

      }
    }

    if (scenario == "II") {

      if (trt_case == "both_random") {
        random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, check))
      }

      if (trt_case == "check_fixed") {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
      }

      if (trt_case == "both_fixed") {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
      }

    }
  }

  ## Build formula

  rhs <- paste(c(fixed, random), collapse = " + ")
  as.formula(paste(y, "~", rhs))

}


