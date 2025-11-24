# Version 0.1.6 - Third submission

- Fixed the issue while reading CITATION file.

### Test environments

- local Windows 10 Pro 21H2, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- local Ubuntu 20.04, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- win-builder, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
- macOS builder, R-release (R 4.3.0).

### R CMD check results

- There were no ERRORs, WARNINGs.

# Version 0.1.6 - Second submission

- Fixed the wrong date field.

### Test environments

- local Windows 10 Pro 21H2, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- local Ubuntu 20.04, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- win-builder, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
- macOS builder, R-release (R 4.3.0).

### R CMD check results

- There were no ERRORs, WARNINGs.

# Version 0.1.6 - First submission

### Test environments

- local Windows 10 Pro 21H2, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- local Ubuntu 20.04, R-release (R 4.3.0) & R-devel (R 4.4.0
  Pre-release).
- win-builder, R-release (R 4.3.0) & R-devel (R 4.4.0 Pre-release).
- macOS builder, R-release (R 4.3.0).

### R CMD check results

- There were no ERRORs, WARNINGs.

# Version 0.1.5 - First submission

- Fixed `'LazyData' is specified without a 'data' directory` NOTE (as
  there is no `/data` folder).
- Tests show the following invalid doi/url which is for an article
  hosted in JSTOR. The doi is working on multiple browsers and networks
  on my end. Kindly advise. \> Found the following (possibly) invalid
  URLs: \> URL: <https://doi.org/10.2307/2527837> \> From: README.md \>
  Status: 403 \> Message: Forbidden \>  
  \> Found the following (possibly) invalid DOIs: \> DOI:
  10.2307/2527837 \> From: DESCRIPTION \> Status: Forbidden \> Message:
  403

### Test environments

- local Windows 10 Pro v19042.1052, R-release (R 4.1.0) & R-devel (R
  4.2.0 Pre-release).
- local Ubuntu 16.04, R-release (R 4.1.0) & R-devel (R 4.2.0
  Pre-release).
- win-builder, R-release (R 4.1.0) & R-devel (R 4.2.0 Pre-release).
- rhub:solaris-x86-patched - i386-pc-solaris2.10 (32-bit), R-release (R
  4.1.0).
- rhub:solaris-x86-patched-ods - i386-pc-solaris2.10 (32-bit), R-release
  (R 4.1.0).
- rhub:macos-highsierra-release-cran - x86_64-apple-darwin17.0 (64-bit),
  R-release (R 4.1.0).

# Version 0.1.4 - First submission

- Fixed `cairo_pdf` unconditional use.
- Fixed issue with vignette in Solaris build.
- Tests show the following invalid doi/url which is for an article
  hosted in JSTOR. The doi is working on multiple browsers and networks
  on my end. Kindly advise. \> Found the following (possibly) invalid
  URLs: \> URL: <https://doi.org/10.2307/2527837> \> From: README.md \>
  Status: 403 \> Message: Forbidden \>  
  \> Found the following (possibly) invalid DOIs: \> DOI:
  10.2307/2527837 \> From: DESCRIPTION \> Status: Forbidden \> Message:
  403

### Test environments

- local Windows 10 Home v1803, R-release (R 4.0.4) & R-devel (R 4.1.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 4.0.4) & R-devel (R 4.1.0
  Pre-release).
- win-builder, R-release (R 4.0.4) & R-devel (R 4.1.0 Pre-release).
- rhub:solaris-x86-patched - i386-pc-solaris2.10 (32-bit), R-release (R
  4.0.3).
- rhub:solaris-x86-patched-ods - i386-pc-solaris2.10 (32-bit), R-release
  (R 4.0.3).
- rhub:macos-highsierra-release-cran - x86_64-apple-darwin17.0 (64-bit),
  R-release (R 4.0.3).

# Version 0.1.3 - Second submission

- Reverted to using system certificates instead of RCurl ones for
  fetching and displaying version history as suggested by Prof. Brian
  Ripley (<ripley@stats.ox.ac.uk>).

### Test environments

- local Windows 10 Home v1803, R-release (R 4.0.2) & R-devel (R 4.1.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 4.0.2) & R-devel (R 4.1.0
  Pre-release).
- win-builder, R-release (R 4.0.2) & R-devel (R 4.1.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.

- Regarding (possibly) invalid URLs/ DOIs found: URL:
  <https://doi.org/10.2307/2527837> From: README.md Status: 403 Message:
  Forbidden

  ``` R
  DOI: 10.2307/2527837
  From: DESCRIPTION
  Status: Forbidden
  Message: 403
  ```

  - These seem to be false positives due to some sort of redirection as
    they are opening in browser. Kindly advise.

# Version 0.1.3 - First submission

- Reverted to using system certificates instead of RCurl ones for
  fetching and displaying version history as suggested by Prof. Brian
  Ripley (<ripley@stats.ox.ac.uk>).

### Test environments

- local Windows 10 Home v1803, R-release (R 4.0.2) & R-devel (R 4.1.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 4.0.2) & R-devel (R 4.1.0
  Pre-release).
- win-builder, R-release (R 4.0.2) & R-devel (R 4.1.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.

- Regarding (possibly) invalid URLs/ DOIs found: URL:
  <https://doi.org/10.2307/2527837> From: README.md Status: 403 Message:
  Forbidden

  ``` R
  DOI: 10.2307/2527837
  From: DESCRIPTION
  Status: Forbidden
  Message: 403
  ```

  - These seem to be false positives due to some sort of redirection as
    they are opening in browser. Kindly advise.

# Version 0.1.2 - Second submission

- Fixed compatability issues with updated implementation of `unit`
  function/class in `grid` package.

### Test environments

- local Windows 10 Home v1803, R-release (R 3.6.3) & R-devel (R 4.0.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.6.3) & R-devel (R 4.0.0
  Pre-release).
- win-builder, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).

### R CMD check results

- There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.2 - First submission

- Fixed compatability issues with updated implementation of `unit`
  function/class in `grid` package.

### Test environments

- local Windows 10 Home v1803, R-release (R 3.6.3) & R-devel (R 4.0.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.6.3) & R-devel (R 4.0.0
  Pre-release).
- win-builder, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).

### R CMD check results

- There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.1 - Second submission

- Changed `emmeans::cld` to
  [`multcomp::cld`](https://rdrr.io/pkg/multcomp/man/cld.html) to
  account for the changes in `emmeans` 1.3.5.

### Test environments

- local Windows 10 Home v1803, R-release (R 3.6.1) & R-devel (R 3.7.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.6.1) & R-devel (R 3.7.0
  Pre-release).
- win-builder, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- On NOTE specifying that the DOI (10.2307/2527837) and corresponding
  URL (<https://doi.org/10.2307/2527837>) in `Readme.md` and DESCRIPTION
  as being invalid. The link is accessible, so this NOTE is false and
  can be safely ignored.

# Version 0.1.1 - First submission

- Changed `emmeans::cld` to
  [`multcomp::cld`](https://rdrr.io/pkg/multcomp/man/cld.html) to
  account for the changes in `emmeans` 1.3.5.

### Test environments

- local Windows 10 Home v1803, R-release (R 3.6.1) & R-devel (R 3.7.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.6.1) & R-devel (R 3.7.0
  Pre-release).
- win-builder, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- On NOTE specifying that the DOI (10.2307/2527837) and corresponding
  URL (<https://doi.org/10.2307/2527837>) in `Readme.md` and DESCRIPTION
  as being invalid. The link is accessible, so this NOTE is false and
  can be safely ignored.

# Version 0.1.0 - Third submission

- wrap in examples converted to .

### Test environments

- local Windows 10 Home v1803, R-release (R 3.5.1) & R-devel (R 3.6.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.5.1) & R-devel (R 3.6.0
  Pre-release).
- win-builder, R-release (R 3.5.1) & R-devel (R 3.6.0 Pre-release).

# Version 0.1.0 - Second submission

- Added reference to augmented randomised complete block design in the
  ‘Description’ field of the DESCRIPTION file.
- Fixed functions that were writing to user’s home filespace by writing
  to [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
- As examples take more than 5 seconds per Rd file, the wrap is
  retained.

### Test environments

- local Windows 10 Home v1803, R-release (R 3.5.0) & R-devel (R 3.6.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.5.0) & R-devel (R 3.6.0
  Pre-release).
- win-builder, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).

### R CMD check results

- There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.0 - First submission

### Test environments

- local Windows 10 Home v1803, R-release (R 3.5.0) & R-devel (R 3.6.0
  Pre-release).
- local Ubuntu 16.04, R-release (R 3.5.0) & R-devel (R 3.6.0
  Pre-release).
- win-builder, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).

### R CMD check results

- There were no ERRORs, NOTES or WARNINGs.
