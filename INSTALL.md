# Installation Guide

## Installation in jamovi

You can install this module after installing jamovi version >= 2.7.2 from here: https://www.jamovi.org/download.html

Then you can install the submodules directly inside the jamovi, using the library.

Submodules are:

- ClinicoPathDescriptives
- jsurvival
- meddecide
- jjstatsplot

<img src="man/figures/jamovi-library.png" align="center" width = 75% />

## Installation via sideload in jamovi

### Step 1:

**Download and install jamovi.**

### Step 2:

**Download the relevant `.jmo` file for your operating system from the [releases page](https://github.com/sbalci/ClinicoPathJamoviModule/releases/).**

### Step 3:

**Install the module using side-load as shown below:**

<img src="man/figures/jamovi-sideload.gif" align="center" width = 75% />

## Installation in R

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("sbalci/ClinicoPathJamoviModule")
```
