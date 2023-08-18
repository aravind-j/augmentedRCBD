
<script type="application/ld+json">
      {
  "@context": "https://schema.org",
  "@graph": [
    {
      "type": "SoftwareSourceCode",
      "author": [
        {
          "id": "https://orcid.org/0000-0002-4791-442X",
          "type": "Person",
          "email": [
            "j.aravind@icar.gov.in",
            "aravindj@nbpgr.ernet.in"
          ],
          "familyName": "Aravind",
          "givenName": "J."
        },
        {
          "type": "Person",
          "email": "mukeshsankar@gmail.com",
          "familyName": "Mukesh Sankar",
          "givenName": "S."
        },
        {
          "id": "https://orcid.org/0000-0001-6384-8664",
          "type": "Person",
          "email": "d.wankhede@icar.gov.in",
          "familyName": "Wankhede",
          "givenName": [
            "Dhammaprakash",
            "Pandhari"
          ]
        },
        {
          "type": "Person",
          "email": "vikender.kaur@icar.gov.in",
          "familyName": "Kaur",
          "givenName": "Vikender"
        }
      ],
      "codeRepository": "https://github.com/aravind-j/augmentedRCBD",
      "copyrightHolder": {
        "type": "Organization",
        "name": "ICAR-NBGPR"
      },
      "description": "Functions for analysis of data generated from experiments in augmented randomised complete block design according to Federer, W.T. (1961) <doi:10.2307/2527837>. Computes analysis of variance, adjusted means, descriptive statistics, genetic variability statistics etc. Further includes data visualization and report generation functions.",
      "license": "https://spdx.org/licenses/GPL-2.0",
      "name": "augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs",
      "programmingLanguage": {
        "type": "ComputerLanguage",
        "name": "R",
        "url": "https://r-project.org"
      },
      "provider": {
        "id": "https://cran.r-project.org",
        "type": "Organization",
        "name": "Comprehensive R Archive Network (CRAN)",
        "url": "https://cran.r-project.org"
      },
      "runtimePlatform": "R Under development (unstable) (2023-04-28 r84338 ucrt)",
      "version": "0.1.6"
    },
    {
      "type": "SoftwareSourceCode",
      "author": [
        {
          "id": "https://orcid.org/0000-0002-4791-442X",
          "type": "Person",
          "email": [
            "j.aravind@icar.gov.in",
            "aravindj@nbpgr.ernet.in"
          ],
          "familyName": "Aravind",
          "givenName": "J."
        },
        {
          "type": "Person",
          "email": "mukeshsankar@gmail.com",
          "familyName": "Mukesh Sankar",
          "givenName": "S."
        },
        {
          "type": "Person",
          "email": "d.wankhede@icar.gov.in",
          "familyName": "Wankhede",
          "givenName": [
            "Dhammaprakash",
            "Pandhari"
          ]
        },
        {
          "type": "Person",
          "email": "vikender.kaur@icar.gov.in",
          "familyName": "Kaur",
          "givenName": "Vikender"
        }
      ],
      "description": [
        "R package version 0.1.6",
        "https://aravind-j.github.io/augmentedRCBD/",
        "https://cran.r-project.org/package=augmentedRCBD"
      ],
      "name": "augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs"
    }
  ]
}
    </script>
<!-- 
<img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" width="20%" />
-->

## `augmentedRCBD`: Analysis of Augmented Randomised Complete Block Designs <img src="https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/inst/extdata/augmentedRCBD.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">

###### Version : [0.1.7](https://aravind-j.github.io/augmentedRCBD/articles/Data_Analysis_with_augmentedRCBD.html#install); Copyright (C) 2015-2023: [ICAR-NBPGR](http://www.nbpgr.ernet.in/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

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
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/augmentedRCBD)](https://cran.r-project.org/package=augmentedRCBD)
[![Dependencies](https://tinyverse.netlify.com/badge/augmentedRCBD)](https://cran.r-project.org/package=augmentedRCBD)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/augmentedRCBD?color=green)](https://CRAN.R-project.org/package=augmentedRCBD)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.6.9000-orange.svg)](https://github.com/aravind-j/augmentedRCBD)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/augmentedRCBD.svg)](https://github.com/aravind-j/augmentedRCBD)
[![R-CMD-check](https://github.com/aravind-j/augmentedRCBD/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/augmentedRCBD/actions)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--08--18-yellowgreen.svg)](https://github.com/aravind-j/augmentedRCBD)
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

[![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|                                   |                                                                                                                                                                                                                        |
|:----------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| r-devel-linux-x86_64-debian-clang | [![CRAN check - r-devel-linux-x86_64-debian-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-clang/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html) |
| r-devel-linux-x86_64-debian-gcc   | [![CRAN check - r-devel-linux-x86_64-debian-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-gcc/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)     |
| r-devel-linux-x86_64-fedora-clang | [![CRAN check - r-devel-linux-x86_64-fedora-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-clang/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html) |
| r-devel-linux-x86_64-fedora-gcc   | [![CRAN check - r-devel-linux-x86_64-fedora-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-gcc/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)     |
| r-patched-linux-x86_64            | [![CRAN check - r-patched-linux-x86_64](https://badges.cranchecks.info/flavor/r-patched-linux-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)                       |
| r-release-linux-x86_64            | [![CRAN check - r-release-linux-x86_64](https://badges.cranchecks.info/flavor/r-release-linux-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)                       |

[![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|                          |                                                                                                                                                                                                      |
|:-------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| r-devel-windows-x86_64   | [![CRAN check - r-devel-windows-x86_64](https://badges.cranchecks.info/flavor/r-devel-windows-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)     |
| r-release-windows-x86_64 | [![CRAN check - r-release-windows-x86_64](https://badges.cranchecks.info/flavor/r-release-windows-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html) |
| r-oldrel-windows-x86_64  | [![CRAN check - r-oldrel-windows-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-windows-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)   |

[![MacOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=apple&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|                        |                                                                                                                                                                                                  |
|:-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| r-release-macos-x86_64 | [![CRAN check - r-release-macos-x86_64](https://badges.cranchecks.info/flavor/r-release-macos-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html) |
| r-oldrel-macos-x86_64  | [![CRAN check - r-oldrel-macos-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-macos-x86_64/augmentedRCBD.svg)](https://cran.r-project.org/web/checks/check_results_augmentedRCBD.html)   |

## Citing `augmentedRCBD`

To cite the methods in the package use:

``` r
citation("augmentedRCBD")
```

    To cite the R package 'augmentedRCBD' in publications use:

      Aravind, J., Mukesh Sankar, S., Wankhede, D. P., and Kaur, V. ().  augmentedRCBD: Analysis of Augmented Randomised Complete Block
      Designs. R package version 0.1.7, https://aravind-j.github.io/augmentedRCBD/https://cran.r-project.org/package=augmentedRCBD.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs},
        author = {J. Aravind and S. {Mukesh Sankar} and Dhammaprakash Pandhari Wankhede and Vikender Kaur},
        note = {R package version 0.1.7 https://aravind-j.github.io/augmentedRCBD/ https://cran.r-project.org/package=augmentedRCBD},
      }

    This free and open-source software implements academic research by the authors and co-workers. If you use it, please support the project
    by citing the package.
