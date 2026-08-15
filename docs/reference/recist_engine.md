# RECIST v1.1 Response Assessment Engine

The response-assessment rules of RECIST v1.1, extracted from
`waterfallrecist` so that more than one analysis can apply the identical
criteria rather than each carrying its own copy. Duplicated response
logic has already caused real divergence in this module: `waterfall`
once held three copies of its categoriser that had drifted apart, one of
which silently mapped every unevaluable patient to NA under a
non-English locale.

## Details

These are plain functions, not R6 methods. Everything they need from the
analysis arrives in a context list built by
[`recist_context()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/recist_context.md):
the four user options they read, plus an optional `notify` callback so
they can raise notices without knowing how the calling analysis renders
them.

## Criteria implemented

Target lesion summation; partial response referenced to the BASELINE
sum; progressive disease referenced to the NADIR (smallest sum on study)
with the additional \>=5 mm absolute-increase requirement; new-lesion
detection (any new lesion is progression); non-target assessment; the
RECIST overall-response table; confirmation of CR/PR at a minimum
interval; and best overall response truncated at progression.

## Known departure from RECIST v1.1

Non-target progression falls back to a lesion-count heuristic when the
caller supplies no radiologist assessment. RECIST defines it as
*unequivocal progression*, a qualitative judgement no count can
establish. Callers should expose an override variable and pass it
through the context.

## References

Eisenhauer EA, Therasse P, Bogaerts J, et al. New response evaluation
criteria in solid tumours: revised RECIST guideline (version 1.1). Eur J
Cancer. 2009;45(2):228-247.
