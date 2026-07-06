# Normalize PCA loadings for visualization

Converts PCA rotation matrices to correlation-style loadings that are
comparable across different `scale` settings. Values are clipped within
the (-1, 1) range to avoid small numerical drifts outside the unit
interval.

## Usage

``` r
pcaloadingheatmap_normalized_loadings(pca, pca_data, scaled)
```

## Arguments

- pca:

  A `prcomp` object.

- pca_data:

  Numeric matrix used to fit the PCA.

- scaled:

  Logical; whether the PCA was run with `scale.=TRUE`.
