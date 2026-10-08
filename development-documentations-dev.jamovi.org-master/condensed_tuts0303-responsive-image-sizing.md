<!-- CONDENSED SUMMARY, not a verbatim copy. Source: https://dev.jamovi.org/tutorial/tuts0303-responsive-image-sizing (fetched 2026-10-08 via a summarising fetch tool).
     Replace with the verbatim upstream file (src/content/docs/... in jamovi/dev.jamovi.org) when convenient. -->

# Responsive Image Sizing (condensed)

Requires jamovi 2.7.16. Size = stretchable part (M) + fixed part (B; axis labels, margins).

```r
self$results$plot$setSize2(widthM, heightM, widthB, heightB)
# faceted: 250 px per row, 100 px fixed
self$results$plot$setSize2(500, 250 * nRows, 100, 100)
```

Backward compatible: `if (!is.null(image[["setSize2"]])) image$setSize2(...) else image$setSize(w, h)`.
Gotchas: argument order; scale factor uses M only; fixed space on both axes; compute M from data.
