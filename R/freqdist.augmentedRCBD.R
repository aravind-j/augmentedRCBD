#'Plot Frequency Distribution from \code{augmentedRCBD} Output
#'
#'\code{freqdist.augmentedRCBD} plots frequncy distribution from an object of
#'class \code{augmentedRCBD} along with the corresponding normal curve and check
#'means with standard errors (if specified by argument \code{highlight.check}).
#'
#'@inheritParams describe.augmentedRCBD
#'@param xlab The text for x axis label as a character string.
#'@param highlight.check If \code{TRUE}, the check means and standard errors are
#'  also plotted. Default is \code{TRUE}.
#'@param check.col The colour(s) to be used to highlight check values in the
#'  plot as a character vector.
#'
#'@return The frequency distribution plot as a ggplot2 plot grob.
#'
#'@import ggplot2
#'
#'@export
#'
#'@seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#'@examples
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
#' # Results for variable y1
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' # Results for variable y2
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y2, method.comp = "lsd",
#'                      alpha = 0.05, group = TRUE, console = TRUE)
#'
#' # Frequency distribution plots
#' freq1 <- freqdist.augmentedRCBD(out1, xlab = "Trait 1")
#' class(freq1)
#' plot(freq1)
#' freq2 <- freqdist.augmentedRCBD(out2, xlab = "Trait 2")
#' plot(freq2)
#'
#' # Change check colours
#' colset <- c("red3", "green4", "purple3", "darkorange3")
#' freq1 <- freqdist.augmentedRCBD(out1, xlab = "Trait 1", check.col = colset)
#' plot(freq1)
#' freq2 <- freqdist.augmentedRCBD(out2, xlab = "Trait 2", check.col = colset)
#' plot(freq2)
#'
#' # Without checks highlighted
#' freq1 <- freqdist.augmentedRCBD(out1, xlab = "Trait 1",
#'                                 highlight.check = FALSE)
#' plot(freq1)
#' freq2 <- freqdist.augmentedRCBD(out2, xlab = "Trait 2",
#'                                 highlight.check = FALSE)
#' plot(freq2)
freqdist.augmentedRCBD <- function(aug, xlab, highlight.check = TRUE,
                                   check.col = "red") {

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class augmentedRCBD')
  }

  checks <- aug$Details$`Check treatments`
  dat <- aug$Means$`Adjusted Means`

  NN <- length(dat)
  bw <- binw(dat, "sturges")

  dat <- data.frame(dat)

  G1 <- ggplot(dat, aes(x = dat)) +
    geom_histogram(colour = "black", fill = "grey",
                   binwidth = bw) +
    scale_x_continuous(limits = c((min(dat$dat, na.rm = TRUE)),
                                  (max(dat$dat, na.rm = TRUE)))) +
    stat_function(geom = "line", fun = function(x, mean, sd, n, bw){
      dnorm(x = x, mean = mean, sd = sd) * n * bw },
      args = list(mean = mean(dat$dat, na.rm = TRUE),
                  sd = sd(dat$dat, na.rm = TRUE),
                  n = NN, bw = bw), colour = "blue") +
    labs(x = xlab, y = "Frequency") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black"))

  if(highlight.check) {
    G1 <- G1 +
      geom_vline(xintercept = aug$Means[aug$Means$Treatment %in% checks,]$`Adjusted Means`,
                 size = 1, colour = check.col)

    dat2 <- aug$Means[aug$Means$Treatment %in% checks,]
    dat2$lower <- dat2$`Adjusted Means` - dat2$SE
    dat2$upper <- dat2$`Adjusted Means` + dat2$SE

    G2 <- ggplot(dat2, aes(x = Treatment, y = Means)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), colour = check.col, width  = 0.25) +
      geom_point(colour = check.col) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(limits = c((min(dat$dat, na.rm = TRUE)),
                                    (max(dat$dat, na.rm = TRUE)))) +
      # geom_hline(yintercept = aug$Means[aug$Means$Treatment %in% checks,]$`Adjusted Means`,
      #            colour = check.col) +
      coord_flip() +
      theme_bw() +
      # theme(axis.text.x = element_blank(),
      #       axis.ticks.x = element_blank()) +
      theme(legend.position="none") +
      theme(plot.margin=unit(c(0.25,0.1,0,0.25),"cm"),
            axis.text = element_text(colour = "black"))

    G <- rbind(ggplotGrob(G2)[-c(1,7),], ggplotGrob(G1)[-1,], size = "last")
    G <- resize_heights(G, c(1,3))

  } else {
    G <- ggplotGrob(G1)
  }

return(G)

}


binw <- function(x, method = c("fd", "scott", "sturges")) {
  method <- match.arg(method)

  if (method == "fd") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)), min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)), min.n = 1, right = TRUE)[1]
  }
  if (method == "scott") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)), min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)), min.n = 1, right = TRUE)[1]
  }
  if (method == "sturges") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)), min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)), min.n = 1, right = TRUE)[1]
  }
  return(bw)
}


resize_heights <- function(g, heights = rep(1, length(idpanels))){
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  g$heights <- grid:::unit.list(g$heights)
  hunits <- lapply(heights, unit, "null")
  class(hunits) <- class(g$heights[idpanels])
  g$heights[idpanels] <- hunits
  g
}