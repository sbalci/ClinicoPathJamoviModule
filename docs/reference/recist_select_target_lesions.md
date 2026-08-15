# Select the target lesions RECIST v1.1 would follow

RECIST v1.1 limits the target lesions to at most five in total and two
per organ, chosen as the largest that are reproducibly measurable.
Everything else measurable at baseline is followed as NON-target
disease. Applying no limit and summing every lesion overstates tumour
burden; picking the wrong ones understates it. Both mistakes were live
in this module before this function existed.

## Usage

``` r
recist_select_target_lesions(lesion_data, ctx)
```

## Arguments

- lesion_data:

  Lesion-level frame from the caller's data preparation. Must carry
  patientID, lesionID, lesionType, diameter, isBaseline and (optionally)
  location and targetSelection.

- ctx:

  Context from
  [`recist_context()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/recist_context.md).

## Value

`lesion_data` with a logical `targetSelected` column. Baseline target
lesions that were not selected are reclassified to `"NonTarget"` at
every visit, which is what RECIST does with them.

## Details

Selection happens ONCE, at baseline, and the chosen lesion IDs are then
followed at every later visit – a lesion cannot become a target lesion
halfway through.

**Size is not the whole criterion.** RECIST also requires the lesion to
be reproducibly measurable, which is a radiologist's judgement no
algorithm can make. Supply `targetSelection` in the data (via the
caller's override variable) to record the reader's own choice; when
present it is used verbatim and only checked against the limits.
