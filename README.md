
<!-- 
<img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" width="20%" />
-->

## `augmentedRCBD`: Analysis of Augmented Randomised Complete Block Designs <img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">

###### Version : [0.1.5](https://aravind-j.github.io/augmentedRCBD/articles/Data_Analysis_with_augmentedRCBD.html#install); Copyright (C) 2015-2021: [ICAR-NBPGR](http://www.nbpgr.ernet.in/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J.<sup>1</sup>, Mukesh Sankar, S.<sup>2</sup>, Wankhede, D. P.<sup>3</sup>, and Kaur, V.<sup>4</sup>*

1.  Division of Germplasm Conservation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.
2.  Division of Genetics, ICAR-Indian Agricultural Research Institute,
    New Delhi.
3.  Division of Genomic Resources, ICAR-National Bureau of Plant Genetic
    Resources, New Delhi.
4.  Division of Germplasm Evaluation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/augmentedRCBD)](https://cran.r-project.org/package=augmentedRCBD)
[![Dependencies](https://tinyverse.netlify.com/badge/augmentedRCBD)](https://cran.r-project.org/package=augmentedRCBD)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/augmentedRCBD?color=green)](https://CRAN.R-project.org/package=augmentedRCBD)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.4.9000-orange.svg)](https://github.com/aravind-j/augmentedRCBD)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/augmentedRCBD.svg)](https://github.com/aravind-j/augmentedRCBD)
[![R-CMD-check](https://github.com/aravind-j/augmentedRCBD/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/augmentedRCBD/actions)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--06--11-yellowgreen.svg)](https://github.com/aravind-j/augmentedRCBD)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1310011.svg)](https://doi.org/10.5281/zenodo.1310011)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/aravind-j.github.io/augmentedRCBD.svg)](https://aravind-j.github.io/augmentedRCBD/)
[![.](https://pro-pulsar-193905.appspot.com/UA-116716530-1/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/augmentedRCBD) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/augmentedRCBD/total.svg)] -->
<!-- [![Rdoc](https://www.rdocumentation.org/badges/version/augmentedRCBD)](https://www.rdocumentation.org/packages/augmentedRCBD) -->

------------------------------------------------------------------------

## Description

Functions for analysis of data generated from experiments in augmented
randomised complete block design according to Federer, W.T. (1961)
[doi:10.2307/2527837](https://doi.org/10.2307/2527837). Computes
analysis of variance, adjusted means, descriptive statistics, genetic
variability statistics etc. Further includes data visualization and
report generation functions.

## Installation

The package can be installed from CRAN as follows:

``` r
# Install from CRAN
install.packages('augmentedRCBD', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
# Install development version from Github
if (!require('devtools')) install.packages('devtools')
devtools::install_github("aravind-j/augmentedRCBD")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = 'augmentedRCBD')
```

The vignette for the latest version is also available
[online](https://aravind-j.github.io/augmentedRCBD/articles/Data_Analysis_with_augmentedRCBD.html).

## What’s new

To know whats new in this version type:

``` r
news(package='augmentedRCBD')
```

## Links

[CRAN page](https://cran.r-project.org/package=augmentedRCBD)

[Github page](https://github.com/aravind-j/augmentedRCBD)

[Documentation website](https://aravind-j.github.io/augmentedRCBD/)

[Zenodo DOI](https://doi.org/10.5281/zenodo.1310011)

## CRAN checks

<table class="table table-striped table-hover" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
Flavour
</th>
<th style="text-align:left;">
CRAN check
</th>
</tr>
</thead>
<tbody>
<tr grouplength="6">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Linux](https://shields.io/badge/Linux--9cf?logo=Linux&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-debian-clang
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-debian-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-clang/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-debian-gcc
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-debian-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-debian-gcc/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-fedora-clang
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-fedora-clang](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-clang/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-linux-x86\_64-fedora-gcc
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-linux-x86\_64-fedora-gcc](https://cranchecks.info/badges/flavor/r-devel-linux-x86_64-fedora-gcc/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-patched-linux-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-patched-linux-x86\_64](https://cranchecks.info/badges/flavor/r-patched-linux-x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-linux-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-linux-x86\_64](https://cranchecks.info/badges/flavor/r-release-linux-x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr grouplength="1">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Solaris](https://shields.io/badge/Solaris--9cf?logo=Oracle&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-patched-solaris-x86
</td>
<td style="text-align:left;">
[![CRAN check -
r-patched-solaris-x86](https://cranchecks.info/badges/flavor/r-patched-solaris-x86/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr grouplength="3">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![Windows](https://shields.io/badge/Windows--9cf?logo=Windows&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-devel-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-devel-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-devel-windows-ix86+x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-release-windows-ix86+x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-oldrel-windows-ix86+x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-oldrel-windows-ix86+x86\_64](https://cranchecks.info/badges/flavor/r-oldrel-windows-ix86+x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr grouplength="2">
<td colspan="2" style="border-bottom: 1px solid;">
<strong>[![MacOS](https://shields.io/badge/MacOS--9cf?logo=Apple&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-release-macos-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-release-macos-x86\_64](https://cranchecks.info/badges/flavor/r-release-macos-x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
r-oldrel-macos-x86\_64
</td>
<td style="text-align:left;">
[![CRAN check -
r-oldrel-macos-x86\_64](https://cranchecks.info/badges/flavor/r-oldrel-macos-x86_64/augmentedRCBD)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)
</td>
</tr>
</tbody>
</table>

## Citing `augmentedRCBD`

To cite the methods in the package use:

``` r
citation("augmentedRCBD")
```


    To cite the R package 'augmentedRCBD' in publications use:

      Aravind, J., Mukesh Sankar, S., Wankhede, D. P., and Kaur, V. (2021).  augmentedRCBD: Analysis of Augmented Randomised
      Complete Block Designs. R package version 0.1.5,
      https://aravind-j.github.io/augmentedRCBD/https://cran.r-project.org/package=augmentedRCBD.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs},
        author = {J. Aravind and S. {Mukesh Sankar} and Dhammaprakash Pandhari Wankhede and Vikender Kaur},
        year = {2021},
        note = {R package version 0.1.5},
        note = {https://aravind-j.github.io/augmentedRCBD/},
        note = {https://cran.r-project.org/package=augmentedRCBD},
      }

    This free and open-source software implements academic research by the authors and co-workers. If you use it, please support
    the project by citing the package.
