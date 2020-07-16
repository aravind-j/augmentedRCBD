# Version 0.1.3 - First submission

* Reverted to using system certificates instead of RCurl ones for fetching and displaying version history as suggested by Prof. Brian Ripley (ripley@stats.ox.ac.uk).

### Test environments
* local Windows 10 Home v1803, R-release (R 4.0.1) & R-devel (R 4.1.0 Pre-release).
* local Ubuntu 16.04, R-release (R 4.0.1) & R-devel (R 4.1.0 Pre-release).
* win-builder, R-release (R 4.0.0) & R-devel (R 4.1.0 Pre-release).

### R CMD check results
* There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.2 - Second submission

* Fixed compatability issues with updated implementation of `unit` function/class in `grid` package.

### Test environments
* local Windows 10 Home v1803, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).
* win-builder, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).

### R CMD check results
* There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.2 - First submission

* Fixed compatability issues with updated implementation of `unit` function/class in `grid` package.

### Test environments
* local Windows 10 Home v1803, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).
* win-builder, R-release (R 3.6.3) & R-devel (R 4.0.0 Pre-release).

### R CMD check results
* There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.1 - Second submission

* Changed `emmeans::cld` to `multcomp::cld` to account for the changes in `emmeans` 1.3.5.

### Test environments
* local Windows 10 Home v1803, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).
* win-builder, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).

### R CMD check results
* There were no ERRORs or WARNINGs.
* On NOTE specifying that the DOI (10.2307/2527837) and corresponding URL (https://doi.org/10.2307/2527837) in `Readme.md` and DESCRIPTION as being invalid. The link is accessible, so this NOTE is false and can be safely ignored.

# Version 0.1.1 - First submission

* Changed `emmeans::cld` to `multcomp::cld` to account for the changes in `emmeans` 1.3.5.

### Test environments
* local Windows 10 Home v1803, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).
* win-builder, R-release (R 3.6.1) & R-devel (R 3.7.0 Pre-release).

### R CMD check results
* There were no ERRORs or WARNINGs.
* On NOTE specifying that the DOI (10.2307/2527837) and corresponding URL (https://doi.org/10.2307/2527837) in `Readme.md` and DESCRIPTION as being invalid. The link is accessible, so this NOTE is false and can be safely ignored.

# Version 0.1.0 - Third submission

* \dontrun{} wrap in examples converted to \donttest{}. 

### Test environments
* local Windows 10 Home v1803, R-release (R 3.5.1) & R-devel (R 3.6.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.5.1) & R-devel (R 3.6.0 Pre-release).
* win-builder, R-release (R 3.5.1) & R-devel (R 3.6.0 Pre-release).

# Version 0.1.0 - Second submission

* Added reference to augmented randomised complete block design in the 'Description' field of the DESCRIPTION file.
* Fixed functions that were writing to user's home filespace by writing to `tempdir()`.
* As examples take more than 5 seconds per Rd file, the \dontrun{} wrap is retained.

### Test environments
* local Windows 10 Home v1803, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).
* win-builder, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).

### R CMD check results
* There were no ERRORs, NOTES or WARNINGs.

# Version 0.1.0 - First submission

### Test environments
* local Windows 10 Home v1803, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).
* local Ubuntu 16.04, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).
* win-builder, R-release (R 3.5.0) & R-devel (R 3.6.0 Pre-release).

### R CMD check results
* There were no ERRORs, NOTES or WARNINGs.

