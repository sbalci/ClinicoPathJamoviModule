# Positive Predictive Value Calculator

This module calculates the Positive Predictive Value (PPV) and False
Discovery Rate (FDR) for research findings based on the framework
described by Ioannidis (2005). It helps researchers understand the
probability that their claimed findings are actually true given various
study characteristics.

## Value

An `R6` class generator object for the `ppvClass` backend; used
internally by the jamovi analysis wrapper and not called directly.

## Details

The calculation is based on Bayes' theorem and considers:

- Prior probability of true relationships (percentage of a priori true
  hypotheses)

- Type I error rate (alpha level)

- Statistical power (1 - beta)

- Proportion of p-hacked or biased studies

PPV = (Power x R + u x beta x R) / (R + alpha - beta x R + u - u x
alpha + u x beta x R) where R is the pre-study odds of true
relationships (percTrue/(100-percTrue)) and u is the bias factor
(percHack/100)

## References

Ioannidis, J. P. (2005). Why most published research findings are false.
PLoS medicine, 2(8), e124.

Adapted from https://github.com/raviselker/ppv

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`ppvBase` -\> `ppvClass`

## Methods

### Public methods

- [`ppvClass$readDataset()`](#method-ppvClass-readDataset)

- [`ppvClass$asSource()`](#method-ppvClass-asSource)

- [`ppvClass$clone()`](#method-ppvClass-clone)

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
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `ppvBase$initialize()`

------------------------------------------------------------------------

### `ppvClass$readDataset()`

Read dataset for PPV analysis

#### Usage

    ppvClass$readDataset()

#### Returns

NULL as this analysis doesn't require data

------------------------------------------------------------------------

### `ppvClass$asSource()`

Generate R source code for PPV analysis

#### Usage

    ppvClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `ppvClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ppvClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
