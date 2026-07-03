#' Multivariable-survival interaction helpers
#'
#' Pure, harness-free helpers used by `multisurvivalClass` to build Cox
#' interaction terms and to summarise effect modification. Kept out of the R6
#' class so they can be unit-tested directly (see
#' tests/testthat/test-multisurvival-interactions.R). All depend only on
#' `survival`, `stats`, and `.escapeVariableNames()` (R/utils.R).

# Map interaction terms from jamovi display labels to real column names.
# `interactions`: list of character vectors (self$options$interactions).
# `all_labels`  : named list (names = real columns, values = display labels).
.mapInteractionTerms <- function(interactions, all_labels) {
  if (is.null(interactions) || length(interactions) == 0)
    return(list())
  lapply(interactions, function(term) {
    vapply(term, function(component) {
      real <- names(all_labels)[match(component, all_labels)]
      if (length(real) == 0 || is.na(real)) component else real
    }, character(1), USE.NAMES = FALSE)
  })
}

# Escaped, colon-joined interaction terms for the Cox model formula RHS.
.interactionTermsForFormula <- function(real_terms) {
  if (length(real_terms) == 0) return(character(0))
  vapply(real_terms, function(components) {
    paste(.escapeVariableNames(components), collapse = ":")
  }, character(1), USE.NAMES = FALSE)
}

# Raw colon-joined interaction terms for finalfit `explanatory` vectors
# (main effects are passed to finalfit unescaped, so match that).
.interactionTermsForFinalfit <- function(real_terms) {
  if (length(real_terms) == 0) return(character(0))
  vapply(real_terms, paste, character(1), collapse = ":", USE.NAMES = FALSE)
}

# Describe a single interaction term: focal (first), moderator (second),
# whether it is 2-way, and whether the moderator is categorical.
.interactionModeratorInfo <- function(real_term, data) {
  twoway <- length(real_term) == 2
  focal <- real_term[1]
  moderator <- if (twoway) real_term[2] else NA_character_
  cat_mod <- twoway && !is.na(moderator) &&
    (is.factor(data[[moderator]]) || is.character(data[[moderator]]))
  list(focal = focal, moderator = moderator,
       twoway = twoway, categorical_moderator = cat_mod)
}
