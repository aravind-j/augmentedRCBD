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

#' Plot Frequency Distribution from \code{augmentedRCBD.menv} Output
#'
#' \code{freqdist.augmentedRCBD.menv} plots frequency distribution from an object
#' of class \code{augmentedRCBD.menv} along with the corresponding normal curve
#' and check means with standard errors (if specified by argument
#' \code{highlight.check}).
#'
#' @param aug An object of class \code{augmentedRCBD.menv}.
#' @param xlab The text for x axis label as a character string.
#' @param highlight.check If \code{TRUE}, the check means and standard errors
#'   are also plotted. Default is \code{TRUE}.
#' @param check.col The colour(s) to be used to highlight check values in the
#'   plot as a character vector. Must be valid colour values in R (named
#'   colours, hexadecimal representation, index of colours [\code{1:8}] in
#'   default R \code{palette()} etc.).
#' @param ... Unused
#'
#' @return The frequency distribution plot as a ggplot2 plot grob.
#'
#' @import ggplot2
#' @importFrom methods is
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom utils getFromNamespace
#' @export
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD.menv}}
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
#' # Frequency distribution plots
#' freq1 <- freqdist(out1, xlab = "Trait 1")
#' class(freq1)
#' plot(freq1)
#
#' # Change check colours
#' colset <- c("red3", "green4", "purple3", "darkorange3")
#' freq1 <- freqdist(out1, xlab = "Trait 1", check.col = colset)
#' plot(freq1)
#
#' # Without checks highlighted
#' freq1 <- freqdist(out1, xlab = "Trait 1", highlight.check = FALSE)
#' plot(freq1)
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
#' # Frequency distribution plots
#' freq2 <- freqdist(out2, xlab = "Trait 1")
#' class(freq2)
#' plot(freq2)
#
#' # Change check colours
#' colset <- c("red3", "green4", "purple3", "darkorange3")
#' freq2 <- freqdist(out2, xlab = "Trait 1", check.col = colset)
#' plot(freq2)
#
#' # Without checks highlighted
#' freq2 <- freqdist(out2, xlab = "Trait 1", highlight.check = FALSE)
#' plot(freq2)
#'
freqdist.augmentedRCBD.menv <- function(aug, xlab, highlight.check = TRUE,
                                        check.col = "red", ...) {

  if (!is(aug, "augmentedRCBD.menv")) {
    stop('"aug" is not of class "augmentedRCBD.menv".')
  }

  # check.col
  if (!all(iscolour(check.col))) {
    stop('"check.col" specifies invalid colour(s).')
  }

  checks <- aug$Details$`Check treatments`
  dat <- aug$Means$`Adjusted Means`

  if (length(check.col) != 1) {
    if (length(check.col) != length(checks)) {
      stop('"checks" and "check.col" are of unequal lengths.')
    }
  }

  NN <- length(dat)
  bw <- binw(dat, "sturges")

  dat <- data.frame(dat)

  G1 <- ggplot(dat, aes(x = dat)) +
    geom_histogram(colour = "black", fill = "grey",
                   binwidth = bw) +
    scale_x_continuous(limits = c( (min(dat$dat, na.rm = TRUE)),
                                   (max(dat$dat, na.rm = TRUE)))) +
    stat_function(geom = "line", fun = function(x, mean, sd, n, bw){
      dnorm(x = x, mean = mean, sd = sd) * n * bw},
      args = list(mean = mean(dat$dat, na.rm = TRUE),
                  sd = sd(dat$dat, na.rm = TRUE),
                  n = NN, bw = bw), colour = "blue") +
    labs(x = xlab, y = "Frequency") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black"),
          plot.margin = unit(c(0, 1, 1, 1), "lines"))

  if (highlight.check) {
    G1 <- G1 +
      geom_vline(xintercept = aug$Means[aug$Means$Treatment %in% checks,
      ]$`Adjusted Means`,
      linewidth = 1, colour = check.col)

    dat2 <- aug$Means[aug$Means$Treatment %in% checks, ]
    dat2$lower <- dat2$`Adjusted Means` - dat2$SE
    dat2$upper <- dat2$`Adjusted Means` + dat2$SE

    G2 <- ggplot(dat2, aes(x = Treatment, y = Means)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), colour = check.col,
                    width  = 0.25) +
      geom_point(colour = check.col) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(limits = c( (min(dat$dat, na.rm = TRUE)),
                                     (max(dat$dat, na.rm = TRUE)))) +
      coord_flip() +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      theme(legend.position = "none") +
      theme(plot.margin = unit(c(0.25, 0.1, 0, 0.25), "cm"),
            axis.text = element_text(colour = "black"))

    G <- rbind(ggplotGrob(G2), ggplotGrob(G1), size = "max")
    G <- resize_heights(G, c(1, 3))

  } else {
    G <- ggplotGrob(G1 + theme(plot.margin = unit(c(1, 1, 1, 1), "lines")))
  }

  return(G)

}
