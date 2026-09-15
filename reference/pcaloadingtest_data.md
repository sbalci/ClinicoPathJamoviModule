# Example Dataset for PCA Loading Significance Test

A simulated dataset designed to demonstrate permutation-based
significance testing for PCA loadings using the permV method (Linting et
al., 2011). The dataset contains variables with known correlation
structure to illustrate how the loading significance test identifies
meaningful vs. noise variables.

## Usage

``` r
pcaloadingtest_data
```

## Format

A data frame with 100 rows and 10 variables:

- var1:

  Numeric variable, primary loading on PC1 (mean=50, sd=10)

- var2:

  Numeric variable, high correlation with var1 (r=0.8), loads on PC1

- var3:

  Numeric variable, moderate correlation with var1 (r=0.7), loads on PC1

- var4:

  Numeric variable, primary loading on PC2 (mean=100, sd=15)

- var5:

  Numeric variable, high correlation with var4 (r=0.75), loads on PC2

- var6:

  Numeric variable, primary loading on PC3 (mean=75, sd=12)

- var7:

  Numeric variable, moderate correlation with var6 (r=0.6), loads on PC3

- noise:

  Noise variable with no structure, should show no significant loadings

- Body Mass Index:

  Variable with spaces to test escapeVariableNames (mean=25, sd=3)

- Blood Pressure:

  Variable with spaces to test escapeVariableNames (mean=120, sd=15)

## Source

Simulated data with known correlation structure

## Details

This dataset is specifically designed to test the PCA loading
significance functionality:

**Correlation Structure:**

- **PC1 cluster:** var1, var2, var3 are highly intercorrelated (r \>
  0.8)

- **PC2 cluster:** var4, var5 are highly correlated (r = 0.91)

- **PC3 cluster:** var6, var7 are moderately correlated (r = 0.65)

- **Noise:** The 'noise' variable is independent and should not show
  significant loadings on any component

**Special Features:**

- Contains ~5% missing values in var1 to test NA handling

- Includes variables with spaces in names ("Body Mass Index", "Blood
  Pressure") to test the escapeVariableNames functionality

**Expected Results:** When analyzed with default settings (center=TRUE,
scale=TRUE, ncomp=3):

- var1, var2, var3 should have significant loadings on PC1

- var4, var5 should have significant loadings on PC2

- var6, var7 should have significant loadings on PC3

- 'noise' should have no significant loadings (after FDR adjustment)

## References

Linting M, van Os BJ, Meulman JJ. (2011). Statistical Significance of
the Contribution of Variables to the PCA solution: An Alternative
Permutation Strategy. Psychometrika, 76(3):440-460.

## Examples

``` r
# Load the data
data("pcaloadingtest_data")

# Check structure
str(pcaloadingtest_data)
#> 'data.frame':    100 obs. of  10 variables:
#>  $ var1           : num  63.7 44.4 53.6 NA 54 ...
#>  $ var2           : num  54.6 38.6 39.9 50.6 41.2 ...
#>  $ var3           : num  36.6 32.4 42.2 47.7 32.3 ...
#>  $ var4           : num  99.9 111.4 100.6 111 97.8 ...
#>  $ var5           : num  81.6 79.2 75.7 83.5 70.5 ...
#>  $ var6           : num  87.3 86 75 76.6 66.4 ...
#>  $ var7           : num  50.4 55 52.9 52.7 34.5 ...
#>  $ noise          : num  0.295 0.393 -1.001 -0.326 -1.008 ...
#>  $ Body Mass Index: num  27.1 27.2 25.7 24.4 20.9 ...
#>  $ Blood Pressure : num  134 116 121 113 153 ...
#>  - attr(*, "description")= chr "Example dataset for PCA loading significance testing using permV method"
#>  - attr(*, "source")= chr "Simulated data with known correlation structure"
#>  - attr(*, "notes")= chr "Variables var1-var3 should load together on PC1. Variables var4-var5 should load on PC2. Variables var6-var7 sh"| __truncated__

# View correlation matrix
cor(pcaloadingtest_data[, 1:8], use = "pairwise.complete.obs")
#>               var1        var2         var3       var4        var5         var6
#> var1   1.000000000  0.95020732  0.851054152 0.11218874 0.137776196 -0.007683643
#> var2   0.950207318  1.00000000  0.828017833 0.07260073 0.078250940 -0.100870534
#> var3   0.851054152  0.82801783  1.000000000 0.04530166 0.084392778  0.012891594
#> var4   0.112188744  0.07260073  0.045301656 1.00000000 0.907096796  0.069407871
#> var5   0.137776196  0.07825094  0.084392778 0.90709680 1.000000000  0.122736586
#> var6  -0.007683643 -0.10087053  0.012891594 0.06940787 0.122736586  1.000000000
#> var7  -0.057338740 -0.07968496 -0.002818418 0.02015944 0.060901493  0.653103753
#> noise -0.006115688  0.02721466  0.094440035 0.00128191 0.001661286  0.009247655
#>               var7        noise
#> var1  -0.057338740 -0.006115688
#> var2  -0.079684959  0.027214657
#> var3  -0.002818418  0.094440035
#> var4   0.020159445  0.001281910
#> var5   0.060901493  0.001661286
#> var6   0.653103753  0.009247655
#> var7   1.000000000  0.046518779
#> noise  0.046518779  1.000000000

if (FALSE) { # \dontrun{
# Run PCA loading significance test
pcaloadingtest(
  data = pcaloadingtest_data,
  vars = c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "noise"),
  ncomp = 3,
  nperm = 1000,
  center = TRUE,
  scale = TRUE,
  conflevel = 0.95,
  adjustmethod = "BH"
)
} # }
```
