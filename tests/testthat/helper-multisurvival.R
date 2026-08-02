# The generated R wrapper requires all Level options as formal arguments, even
# though allowNone makes them optional in jamovi. Keep tests focused on analysis
# behaviour by always passing unused mappings explicitly as NULL.
.run_multisurvival <- function(...,
                               outcomeLevel = NULL,
                               dod = NULL,
                               dooc = NULL,
                               awd = NULL,
                               awod = NULL) {
  multisurvival(
    ...,
    outcomeLevel = outcomeLevel,
    dod = dod,
    dooc = dooc,
    awd = awd,
    awod = awod
  )
}
