# Version 0.1.7 - First submission

* Fixed the issue while reading CITATION file.

### Test environments
* local Windows 10 Pro 21H2, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
* local Ubuntu 20.04, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
* win-builder, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
* macOS builder, R-release (R 4.3.0).

### R CMD check results
* There were no ERRORs, WARNINGs other than the following.

* Tests show the following invalid doi/url which is for an article hosted in JSTOR. The doi is working on multiple browsers and networks on my end. Kindly advise.
>   Found the following (possibly) invalid URLs:
  >     URL: https://doi.org/10.2307/2527837
>       From: README.md
>       Status: 403
>       Message: Forbidden
>   
  >   Found the following (possibly) invalid DOIs:
  >     DOI: 10.2307/2527837
>       From: DESCRIPTION
>       Status: Forbidden
>       Message: 403
