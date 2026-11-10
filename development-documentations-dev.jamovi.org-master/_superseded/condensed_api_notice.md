<!-- CONDENSED SUMMARY, not a verbatim copy. Source: https://dev.jamovi.org/api/notice (fetched 2026-10-08 via a summarising fetch tool).
     Replace with the verbatim upstream file (src/content/docs/... in jamovi/dev.jamovi.org) when convenient. -->

# Notice results element (condensed)

```yaml
results:
    - name: dataNote
      type: Notice
```
```r
self$results$dataNote$setContent("Note: 5 cases were excluded.")
self$results$dataNote$setType(jmvcore::NoticeType$INFO)
# dynamic
n <- jmvcore::Notice$new(options=self$options, name='violationNotice', type=jmvcore::NoticeType$STRONG_WARNING)
n$setContent("Assumption of normality was violated.")
self$results$insert(1, n)
```
Types: INFO (blue), WARNING (yellow), STRONG_WARNING (orange), ERROR (red). No minimum version stated.
NOTE: our empirical test (jmvtools 28.3.1) found `type: Notice` rejected by the compiler schema; see vignettes/jamovi_notices_guide.md.
