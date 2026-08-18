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

#' Construct an Augmented Randomised Complete Block Design Layout
#'
#' Generates an augmented block design layout (Federer, 1956; Federer, 1961) in
#' which test treatments are divided into blocks of a specified size. A set of
#' check treatments can be randomly positioned among the test treatments, while
#' another set of check treatments can be placed at fixed positions at the end
#' of each block.
#'
#' By default, test treatments retain their supplied order. When
#' \code{randomize.test = TRUE}, the test treatments are randomized before being
#' divided into blocks. The relative order of test treatments within each block
#' is otherwise preserved; only the positions of the \code{random.checks} are
#' randomized.
#'
#' @param block.size Integer specifying the number of test treatments assigned
#'   to each block. Check treatments are added in addition to this number.
#' @param test.treatments A vector containing the test treatment identifiers.
#' @param check.treatments A vector containing all available check treatment
#'   identifiers.
#' @param randomize.test Logical indicating whether the test treatments should
#'   be randomized before being divided into blocks. Defaults to \code{FALSE}.
#' @param random.checks A vector containing the check treatments to be randomly
#'   positioned within each block. Defaults to all \code{check.treatments}.
#' @param fixed.checks A vector containing check treatments to be placed at the
#'   end of each block. Defaults to \code{NULL}.
#'
#' @return A data frame with three columns:
#'   \describe{
#'     \item{Block}{The block number.}
#'     \item{Plot}{The plot position within the block.}
#'     \item{Treatment}{The treatment assigned to the plot.}
#'   }
#'
#' @details The number of blocks is determined by dividing the number of test
#'   treatments by \code{block.size} and rounding up. The final block may
#'   therefore contain fewer test treatments than the specified
#'   \code{block.size}.
#'
#'   Within each block, the test treatments occupy all positions not assigned to
#'   \code{random.checks}. The order of the test treatments is preserved. The
#'   positions of the random checks are selected randomly from the combined set
#'   of test-treatment and random-check positions. The \code{fixed.checks}, when
#'   supplied, are appended to the end of the block in their supplied order.
#'
#'   The elements of \code{random.checks} and \code{fixed.checks} must be
#'   distinct, and all must be present in \code{check.treatments}.
#'
#' @export
#'
#' @references
#'
#' \insertRef{federer_augmented_1956}{augmentedRCBD}
#'
#' \insertRef{federer_augmented_1956-1}{augmentedRCBD}
#'
#' \insertRef{federer_augmented_1961}{augmentedRCBD}
#'
#' @examples
#' checks <- paste0("C", 1:6)
#' tests <- paste0("T", sprintf("%03d", 1:100))
#'
#' design <-
#'   construct.augmentedRCBD(block.size = 20,
#'                           test.treatments = tests,
#'                           check.treatments = checks,
#'                           random.checks = checks[1:4],
#'                           fixed.checks = checks[5:6])
#'
#' head(design)
#'
construct.augmentedRCBD <- function(block.size, test.treatments,
                                    check.treatments,
                                    randomize.test = FALSE,
                                    random.checks = check.treatments,
                                    fixed.checks = NULL) {

  # Checks

  if (length(intersect(random.checks, fixed.checks)) > 0) {
    stop('"random.checks" and "fixed.checks" must be disjoint.')
  }

  if (!all(random.checks %in% check.treatments)) {
    stop('All "random.checks" must be present in "check.treatments".')
  }

  if (!all(fixed.checks %in% check.treatments)) {
    stop('All "fixed.checks" must be present in "check.treatments".')
  }

  if (anyDuplicated(random.checks)) {
    stop('"random.checks" must contain unique treatments.')
  }

  if (anyDuplicated(fixed.checks)) {
    stop('"fixed.checks" must contain unique treatments.')
  }

  if (length(unique(c(random.checks, fixed.checks))) >
      length(check.treatments)) {
    stop('The number of unique checks exceeds the number of "check.treatments".')
  }

  num.blocks <- ceiling(length(test.treatments) / block.size)

  # Randomize test treatments
  if (randomize.test == TRUE) {
    test.treatments <- sample(test.treatments)
  }

  # Split test treatments into blocks
  new_by_block <-
    split(test.treatments,
          ceiling(seq_along(test.treatments) / block.size))

  random.checks.size <- length(random.checks)
  fixed.checks.size <- length(fixed.checks)

  # Generate design
  design <-
    lapply(seq_len(num.blocks), function(b) {

      n.test <- length(new_by_block[[b]])

      # Random positions for the random checks
      check_pos <- sample(seq_len(n.test + random.checks.size),
                          random.checks.size)

      # Blank positions
      block_treatments <- character(n.test + random.checks.size)

      # Positions not occupied by checks
      new_pos <- setdiff(seq_len(n.test + random.checks.size), check_pos)

      # Start with initial order (Randomized/Nor-randomized)
      block_treatments[new_pos] <- new_by_block[[b]]

      # Random checks at random positions
      block_treatments[check_pos] <- random.checks

      # Insert checks at the selected positions
      block_treatments <-
        c(block_treatments, fixed.checks)

      data.frame(Block = b,
                 Plot = seq_len(n.test + random.checks.size + fixed.checks.size),
                 Treatment = block_treatments)
    })

  design <- do.call(rbind, design)
  rownames(design) <- NULL

  design

}
