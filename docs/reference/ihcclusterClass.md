# IHC Clustering Analysis Backend

Backend implementation for IHC clustering analysis. Clusters cases based
on immunohistochemistry (IHC) staining patterns using various clustering
algorithms optimized for mixed categorical and continuous data.

## Details

This function supports multiple clustering approaches:

- PAM (k-medoids) - partitioning around medoids

- Hierarchical clustering - with multiple linkage methods (Ward,
  complete, average, single)

- MCA/PCA + k-means - dimension reduction approach

**Distance Metrics:**

- **Gower distance** (default) - handles mixed data types (categorical +
  continuous)

- **Jaccard distance** - optimized for binary IHC data (Sterlacci et al.
  2019)

**Linkage Methods (hierarchical clustering):**

- **Ward** (default) - minimizes within-cluster variance, produces
  balanced clusters

- **Complete** - furthest neighbor, produces compact spherical clusters
  (Sterlacci et al. 2019)

- **Average** - mean distance between clusters

- **Single** - nearest neighbor (may produce chains)

## Features

- Automatic optimal k selection using silhouette analysis

- Multiple distance metrics (Gower, Jaccard)

- Multiple linkage methods for hierarchical clustering

- Multiple testing correction (Bonferroni, FDR, Holm) for marker
  associations

- Comprehensive visualization suite (heatmaps, dendrograms, PCA plots)

- Consensus clustering for stability assessment

- Clinical correlation analysis

- Optimal antibody panel identification

- Variable weighting support

- Missing data handling (complete cases or pairwise distances)

## New in v2.0 (Sterlacci 2019 Features)

- Jaccard distance for binary IHC marker data

- Complete linkage hierarchical clustering

- Bonferroni correction for multiple testing of marker associations

## Author

ClinicoPath Development Team

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`ihcclusterBase` -\> `ihcclusterClass`

## Methods

### Public methods

- [`ihcclusterClass$clone()`](#method-ihcclusterClass-clone)

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
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
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
- `ihcclusterBase$initialize()`

------------------------------------------------------------------------

### `ihcclusterClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ihcclusterClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
