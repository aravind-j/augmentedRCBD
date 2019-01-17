
<!-- 
<img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" width="20%" />
-->

## `augmentedRCBD`: Analysis of Augmented Randomised Complete Block Designs <img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">

###### Version : [0.1.0.9000](https://aravind-j.github.io/augmentedRCBD/); Copyright (C) 2015-2018: [ICAR-NBPGR](http://www.nbpgr.ernet.in/); License: [GPL-2|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J.<sup>1</sup>, Mukesh Sankar, S.<sup>2</sup>, Wankhede, D. P.<sup>3</sup>, and Kaur, V.<sup>4</sup>*

1.  Division of Germplasm Conservation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.
2.  Division of Genetics, ICAR-Indian Agricultural Research Institute,
    New Delhi.
3.  Division of Genomic Resources, ICAR-National Bureau of Plant Genetic
    Resources, New Delhi.
4.  Division of Germplasm Evaluation, ICAR-National Bureau of Plant
    Genetic Resources, New Delhi.

-----

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/augmentedRCBD)](https://cran.r-project.org/package=augmentedRCBD)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/augmentedRCBD?color=green)](https://CRAN.R-project.org/package=augmentedRCBD)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/augmentedRCBD) -->
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.0.9000-orange.svg)](https://github.com/aravind-j/augmentedRCBD)
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/augmentedRCBD/total.svg)] -->
[![Project Status:
WIP](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--01--17-yellowgreen.svg)](/commits/master)
[![Rdoc](http://www.rdocumentation.org/badges/version/augmentedRCBD)](http://www.rdocumentation.org/packages/augmentedRCBD)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1310011.svg)](https://doi.org/10.5281/zenodo.1310011)
[![Analytics](https://pro-pulsar-193905.appspot.com/UA-116716530-1/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)

-----

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

## Citing `augmentedRCBD`

To cite the methods in the package use:

``` r
citation("augmentedRCBD")
```

``` 

To cite the R package 'augmentedRCBD' in publications use:

  Aravind, J., Mukesh Sankar, S., Wankhede, D. P., and Kaur, V.
  (2019).  augmentedRCBD: Analysis of Augmented Randomised
  Complete Block Designs. R package version 0.1.0.9000,
  https://aravind-j.github.io/augmentedRCBD/https://cran.r-project.org/package=augmentedRCBD.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs},
    author = {J. Aravind and S. {Mukesh Sankar} and Dhammaprakash Pandhari Wankhede and Vikender Kaur},
    year = {2019},
    note = {R package version 0.1.0.9000},
    note = {https://aravind-j.github.io/augmentedRCBD/},
    note = {https://cran.r-project.org/package=augmentedRCBD},
  }

This free and open-source software implements academic research by
the authors and co-workers. If you use it, please support the
project by citing the package.
```
