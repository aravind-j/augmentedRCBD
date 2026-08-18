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


#' Generic functions for \code{augmentedRCBD} family
#'
#' @param aug An object of class \code{augmentedRCBD}, \code{augmentedRCBD.menv}
#'   or \code{augmentedRCBD.mix}.
#'
#' @param ...  Unused.
#'
#' @keywords internal
#'
#' @name generics-augmented

#' @rdname generics-augmented
#' @export
describe <- function(aug, ...) {
 UseMethod("describe")
}

#' @rdname generics-augmented
#' @export
freqdist <- function(aug, ...) {
 UseMethod("freqdist")
}

#' @rdname generics-augmented
#' @export
gva <- function(aug, ...) {
 UseMethod("gva")
}

#' @rdname generics-augmented
#' @export
report <- function(aug, ...) {
 UseMethod("report")
}

# Default error handlers for S3 generics ----

#' @keywords internal
#' @export
describe.default <- function(aug, ...) {
  stop('"aug" is not of class "augmentedRCBD", "augmentedRCBD.menv" or ',
       '"augmentedRCBD.mix".')
}

#' @keywords internal
#' @export
freqdist.default <- function(aug, ...) {
  stop('"aug" is not of class "augmentedRCBD", "augmentedRCBD.menv" or ',
       '"augmentedRCBD.mix".')
}

#' @keywords internal
#' @export
gva.default <- function(aug, ...) {
  stop('"aug" is not of class "augmentedRCBD", "augmentedRCBD.menv" or ',
       '"augmentedRCBD.mix".')
}

#' @keywords internal
#' @export
report.default <- function(aug, ...) {
  stop('"aug" is not of class "augmentedRCBD", "augmentedRCBD.menv", ',
       '"augmentedRCBD.mix", "augmentedRCBD.bulk",  "augmentedRCBD.bulk" ',
       'or "augmentedRCBD.bulk".')
}
