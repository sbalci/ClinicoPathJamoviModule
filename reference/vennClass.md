# Venn Diagram

Generates a Venn Diagram and an Upset diagram from selected categorical
variables. This function converts specified variables to logical values
based on a chosen "true" level. Two visual outputs are produced: a Venn
diagram (via ggvenn) and an Upset plot (via UpSetR or ComplexUpset).
Additionally, a summary table of "true" counts for each variable is
provided.

ComplexUpset features include advanced styling, statistical annotations,
custom sorting, and enhanced theming options for publication-ready
figures.

## Value

The function produces a Venn diagram and an Upset diagram.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`vennBase` -\> `vennClass`

## Methods

### Public methods

- [`vennClass$asSource()`](#method-vennClass-asSource)

- [`vennClass$clone()`](#method-vennClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `vennBase$initialize()`

------------------------------------------------------------------------

### `vennClass$asSource()`

Generate R source code for venn diagram analysis

#### Usage

    vennClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `vennClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    vennClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic 2-variable Venn diagram
data("mtcars")
mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))

# Create Venn diagram showing overlap between V-shaped engines and Manual transmission
venn(data = mtcars, var1 = "vs", var1true = "V-shaped",
     var2 = "am", var2true = "Manual")

# Example 2: 3-variable Venn diagram with penguins data
library(palmerpenguins)
data("penguins")
penguins$large_bill <- factor(ifelse(penguins$bill_length_mm > 45, "Large", "Small"))
penguins$heavy_weight <- factor(ifelse(penguins$body_mass_g > 4000, "Heavy", "Light"))
penguins$adelie_species <- factor(ifelse(penguins$species == "Adelie", "Adelie", "Other"))

venn(data = penguins,
     var1 = "large_bill", var1true = "Large",
     var2 = "heavy_weight", var2true = "Heavy",
     var3 = "adelie_species", var3true = "Adelie")

# Example 3: Variable names with spaces and numbers (requires careful handling)
# jamovi GUI automatically handles most problematic names
# When calling directly in R, variable names with spaces/numbers need backticks:
# venn(data = mydata, var1 = "`Rater 1`", var1true = "Positive",
#      var2 = "`Score 2A`", var2true = "High")

# Note: Names like "Rater 1", "Score 2A", "Item 3B" may cause parsing issues
# at the jamovi interface level. Solutions:
# 1. Use jamovi GUI for variable selection (recommended)
# 2. Rename variables to avoid spaces + numbers: "Rater1", "Score2A", "Item3B"
# 3. In R console, use backticks: `Rater 1` or quote properly

# Example 4: Clinical biomarker analysis
data("biomarkers")  # Hypothetical clinical dataset
venn(data = biomarkers,
     var1 = "ER_positive", var1true = "Positive",
     var2 = "PR_positive", var2true = "Positive",
     var3 = "HER2_amplified", var3true = "Amplified",
     show_ggVennDiagram = TRUE,
     regionLabels = "both",
     clinicalSummary = TRUE)

# Example 5: Medical/Clinical comorbidity analysis
# Create sample clinical data
clinical_data <- data.frame(
  patient_id = 1:100,
  diabetes = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
  hypertension = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.4, 0.6)),
  obesity = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.25, 0.75))
)

# Analyze comorbidity patterns
venn(data = clinical_data,
     var1 = "diabetes", var1true = "Yes",
     var2 = "hypertension", var2true = "Yes",
     var3 = "obesity", var3true = "Yes")

# Example 4: Using ComplexUpset for advanced features
venn(data = clinical_data,
     var1 = "diabetes", var1true = "Yes",
     var2 = "hypertension", var2true = "Yes",
     var3 = "obesity", var3true = "Yes",
     show_complexUpset = TRUE,
     sortBy = "freq",
     minSize = 5,
     showAnnotations = TRUE)

# Example 5: Advanced customization using ggVennDiagram
venn(data = clinical_data,
     var1 = "diabetes", var1true = "Yes",
     var2 = "hypertension", var2true = "Yes",
     var3 = "obesity", var3true = "Yes",
     show_ggVennDiagram = TRUE,
     regionLabels = "both",
     colorPalette = "Set1",
     labelSize = 3.5,
     setNameSize = 4.5)

# Example 6: 5-variable Venn diagram using ggVennDiagram
# Add more clinical variables
clinical_data$smoking <- sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8))
clinical_data$family_history <- sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.35, 0.65))

venn(data = clinical_data,
     var1 = "diabetes", var1true = "Yes",
     var2 = "hypertension", var2true = "Yes",
     var3 = "obesity", var3true = "Yes",
     var4 = "smoking", var4true = "Yes",
     var5 = "family_history", var5true = "Yes",
     show_ggVennDiagram = TRUE,
     regionLabels = "percent",
     colorPalette = "viridis")
} # }
```
