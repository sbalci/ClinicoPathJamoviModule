#' RECIST v1.1 Response Assessment Engine
#'
#' The response-assessment rules of RECIST v1.1, extracted from
#' \code{waterfallrecist} so that more than one analysis can apply the identical
#' criteria rather than each carrying its own copy. Duplicated response logic has
#' already caused real divergence in this module: \code{waterfall} once held three
#' copies of its categoriser that had drifted apart, one of which silently mapped
#' every unevaluable patient to NA under a non-English locale.
#'
#' These are plain functions, not R6 methods. Everything they need from the
#' analysis arrives in a context list built by \code{recist_context()}: the four
#' user options they read, plus an optional \code{notify} callback so they can
#' raise notices without knowing how the calling analysis renders them.
#'
#' @section Criteria implemented:
#' Target lesion summation; partial response referenced to the BASELINE sum;
#' progressive disease referenced to the NADIR (smallest sum on study) with the
#' additional >=5 mm absolute-increase requirement; new-lesion detection (any new
#' lesion is progression); non-target assessment; the RECIST overall-response
#' table; confirmation of CR/PR at a minimum interval; and best overall response
#' truncated at progression.
#'
#' @section Known departure from RECIST v1.1:
#' Non-target progression falls back to a lesion-count heuristic when the caller
#' supplies no radiologist assessment. RECIST defines it as \emph{unequivocal
#' progression}, a qualitative judgement no count can establish. Callers should
#' expose an override variable and pass it through the context.
#'
#' @section Callers, and why this file can look orphaned:
#' In this umbrella package every primitive here is live: \code{waterfallrecist}
#' calls all of them and \code{recist} calls \code{recist_context()} and
#' \code{recist_select_target_lesions()}. The generated \pkg{OncoPath} submodule,
#' however, currently ships this file WITHOUT either of those two analyses, so a
#' reader of that package alone sees no callers. That is staging, not debris: the
#' engine is here ahead of a forthcoming lesion-level RECIST v1.1 analysis, and
#' \code{jamovi/0000.yaml} already advertises it. Do not prune it as dead code
#' from the submodule.
#'
#' @references
#' Eisenhauer EA, Therasse P, Bogaerts J, et al. New response evaluation criteria
#' in solid tumours: revised RECIST guideline (version 1.1).
#' Eur J Cancer. 2009;45(2):228-247.
#'
#' @name recist_engine
#' @keywords internal
NULL


# RECIST v1.1 thresholds. Single source of truth: the analyses read these rather
# than declaring their own, so a threshold cannot drift between callers.
.RECIST <- list(
    CR                  = -100,  # complete response: disappearance of all lesions
    PR                  = -30,   # partial response: >=30% decrease vs baseline
    PD                  = 20,    # progression: >=20% increase vs nadir...
    PD_ABS_MM           = 5,     # ...and >=5 mm absolute increase
    MIN_TARGET_NONLYMPH = 10,    # minimum measurable target lesion (mm)
    MIN_TARGET_LYMPH    = 15     # minimum measurable target lymph node, short axis (mm)
)


#' Build the context the engine functions read
#'
#' @param baselineTimepoint Visit time treated as baseline.
#' @param confirmationInterval Minimum gap (weeks) before a CR/PR can be confirmed.
#' @param maxTargetLesions,maxLesionsPerOrgan RECIST target-selection limits.
#' @param nonTargetResponseVar Name of the column holding the radiologist's
#'   non-target assessment, or NULL to fall back to the lesion-count heuristic.
#' @param notify Optional \code{function(type, title, content)} used to raise
#'   notices. When NULL the engine stays silent, which is what a non-interactive
#'   caller wants.
#' @keywords internal
recist_context <- function(baselineTimepoint = 0,
                           confirmationInterval = 4,
                           maxTargetLesions = 5,
                           maxLesionsPerOrgan = 2,
                           nonTargetResponseVar = NULL,
                           targetSelectionVar = NULL,
                           notify = NULL) {
    list(
        baselineTimepoint    = baselineTimepoint,
        confirmationInterval = confirmationInterval,
        maxTargetLesions     = maxTargetLesions,
        maxLesionsPerOrgan   = maxLesionsPerOrgan,
        nonTargetResponseVar = nonTargetResponseVar,
        targetSelectionVar   = targetSelectionVar,
        notify               = notify
    )
}


`%||%` <- function(a, b) if (is.null(a)) b else a

# Raise a notice if the caller supplied a way to. Keeps the engine independent of
# how any particular analysis renders messages.
recist_notify <- function(ctx, type, title, content) {
    if (!is.null(ctx$notify) && is.function(ctx$notify)) {
        ctx$notify(type, title, content)
    }
    invisible(NULL)
}


recist_normalise_nontarget <- function(x) {
key <- gsub("[^a-z]", "", tolower(trimws(as.character(x))))
out <- rep(NA_character_, length(key))
# Order matters: check the compound label before the bare "pd",
# otherwise "Non-CR/Non-PD" would never be reached.
out[key %in% c("noncrnonpd", "nonprnonpd", "nonpd", "stable", "sd",
               "persist", "present", "persists")] <- "Non-CR/Non-PD"
out[key %in% c("cr", "completeresponse")] <- "CR"
out[key %in% c("pd", "progressivedisease", "progression",
               "unequivocalprogression")] <- "PD"
out[key %in% c("ne", "notevaluable", "na", "notassessed")] <- "NE"
out

}

recist_nontarget_override <- function(lesion_data, ctx) {
ntOpt <- ctx$nonTargetResponseVar
if (is.null(ntOpt) || length(ntOpt) == 0 ||
    !"nonTargetResponse" %in% names(lesion_data)) {
    return(NULL)
}

raw <- lesion_data$nonTargetResponse
have <- !is.na(raw) & nzchar(trimws(raw))
if (!any(have)) return(NULL)

status <- recist_normalise_nontarget(raw)

unknown <- unique(raw[have & is.na(status)])
if (length(unknown) > 0) {
    recist_notify(ctx, 
        "WARNING", "Unrecognised Non-Target Assessment",
        sprintf(paste0("These Non-Target Response values were not recognised and ",
                       "were ignored, so the computed value was used for those ",
                       "visits instead: %s. Accepted values are CR, Non-CR/Non-PD, ",
                       "PD and NE."),
                paste(utils::head(unknown, 10), collapse = ", "))
    )
}

keep <- !is.na(status)
if (!any(keep)) return(NULL)

out <- data.frame(
    patientID = as.character(lesion_data$patientID[keep]),
    visitTime = lesion_data$visitTime[keep],
    nontarget_status = status[keep],
    stringsAsFactors = FALSE
)
# One assessment per patient-visit; the reader records it once, but it
# may be repeated on every lesion row for that visit.
out <- out[!duplicated(out[, c("patientID", "visitTime")]), , drop = FALSE]

recist_notify(ctx, 
    "INFO", "Non-Target Assessment Supplied",
    sprintf(paste0("The radiologist's non-target assessment was applied to %d ",
                   "patient-visit(s), replacing the lesion-count heuristic. This ",
                   "is the RECIST v1.1 route: non-target progression is a ",
                   "qualitative judgement of unequivocal progression."),
            nrow(out))
)

out

}

#' Select the target lesions RECIST v1.1 would follow
#'
#' RECIST v1.1 limits the target lesions to at most five in total and two per
#' organ, chosen as the largest that are reproducibly measurable. Everything else
#' measurable at baseline is followed as NON-target disease. Applying no limit and
#' summing every lesion overstates tumour burden; picking the wrong ones understates
#' it. Both mistakes were live in this module before this function existed.
#'
#' Selection happens ONCE, at baseline, and the chosen lesion IDs are then followed
#' at every later visit -- a lesion cannot become a target lesion halfway through.
#'
#' \strong{Size is not the whole criterion.} RECIST also requires the lesion to be
#' reproducibly measurable, which is a radiologist's judgement no algorithm can make.
#' Supply \code{targetSelection} in the data (via the caller's override variable) to
#' record the reader's own choice; when present it is used verbatim and only checked
#' against the limits.
#'
#' @param lesion_data Lesion-level frame from the caller's data preparation. Must
#'   carry patientID, lesionID, lesionType, diameter, isBaseline and (optionally)
#'   location and targetSelection.
#' @param ctx Context from \code{recist_context()}.
#' @return \code{lesion_data} with a logical \code{targetSelected} column. Baseline
#'   target lesions that were not selected are reclassified to \code{"NonTarget"} at
#'   every visit, which is what RECIST does with them.
#' @keywords internal
recist_select_target_lesions <- function(lesion_data, ctx) {
    if (is.null(lesion_data) || !is.data.frame(lesion_data) || nrow(lesion_data) == 0) {
        return(lesion_data)
    }
    if (!all(c("patientID", "lesionID", "lesionType", "isBaseline") %in% names(lesion_data))) {
        return(lesion_data)
    }

    max_total <- if (is.null(ctx$maxTargetLesions)) 5 else ctx$maxTargetLesions
    max_organ <- if (is.null(ctx$maxLesionsPerOrgan)) 2 else ctx$maxLesionsPerOrgan
    has_organ <- "location" %in% names(lesion_data)

    # An explicit reader selection wins outright.
    user_choice <- NULL
    if ("targetSelection" %in% names(lesion_data)) {
        key <- gsub("[^a-z0-9]", "", tolower(trimws(as.character(lesion_data$targetSelection))))
        picked <- key %in% c("1", "true", "t", "yes", "y", "target", "selected")
        if (any(picked)) {
            user_choice <- unique(lesion_data$lesionID[picked])
        }
    }

    baseline_targets <- lesion_data[lesion_data$isBaseline %in% TRUE &
                                        lesion_data$lesionType == "Target", , drop = FALSE]
    if (nrow(baseline_targets) == 0) {
        lesion_data$targetSelected <- lesion_data$lesionType == "Target"
        return(lesion_data)
    }

    selected_ids <- character(0)
    dropped <- list()

    for (pt in unique(baseline_targets$patientID)) {
        pt_rows <- baseline_targets[baseline_targets$patientID == pt, , drop = FALSE]

        if (!is.null(user_choice)) {
            keep <- pt_rows$lesionID[pt_rows$lesionID %in% user_choice]
            selected_ids <- c(selected_ids, keep)
            next
        }

        # Largest first; ties broken by lesion ID so the choice is reproducible.
        ord <- order(-pt_rows$diameter,
                     as.character(pt_rows$lesionID),
                     na.last = TRUE)
        pt_rows <- pt_rows[ord, , drop = FALSE]

        keep <- character(0)
        per_organ <- list()
        for (i in seq_len(nrow(pt_rows))) {
            if (length(keep) >= max_total) break
            if (is.na(pt_rows$diameter[i])) next
            organ <- if (has_organ) as.character(pt_rows$location[i]) else "__all__"
            used <- per_organ[[organ]] %||% 0
            if (used >= max_organ) next
            keep <- c(keep, pt_rows$lesionID[i])
            per_organ[[organ]] <- used + 1
        }
        selected_ids <- c(selected_ids, keep)
        excluded <- setdiff(pt_rows$lesionID, keep)
        if (length(excluded) > 0) dropped[[pt]] <- excluded
    }

    lesion_data$targetSelected <- lesion_data$lesionID %in% selected_ids

    # Baseline target lesions that were not selected are followed as non-target
    # disease, exactly as RECIST specifies -- not silently discarded.
    demote <- lesion_data$lesionType == "Target" & !lesion_data$targetSelected
    lesion_data$lesionType[demote] <- "NonTarget"

    if (!is.null(user_choice)) {
        recist_notify(ctx, "INFO", "Target Lesions Chosen by the Reader",
            sprintf(paste0("%d lesion(s) were used as target lesions because they were marked ",
                           "in the data. Automatic selection by size was not applied."),
                    length(unique(selected_ids))))
    } else if (length(dropped) > 0) {
        n_pt <- length(dropped)
        n_les <- sum(vapply(dropped, length, integer(1)))
        recist_notify(ctx, "WARNING", "Target Lesions Selected Automatically",
            sprintf(paste0("RECIST v1.1 follows at most %d target lesions in total and %d per ",
                           "organ. The largest were selected for %d patient(s); %d lesion(s) ",
                           "were moved to non-target disease and are no longer in the sum of ",
                           "diameters (e.g. %s). Size alone does not establish that a lesion is ",
                           "reproducibly measurable -- supply your own target selection to ",
                           "override this."),
                    max_total, max_organ, n_pt, n_les,
                    paste(utils::head(unlist(dropped), 6), collapse = ", ")))
    }

    lesion_data
}


recist_validate_target_selection <- function(lesion_data, ctx) {
# Filter to baseline target lesions only
baseline_targets <- lesion_data[lesion_data$isBaseline == TRUE &
    lesion_data$lesionType == "Target", ]

violations <- character(0)
warnings_list <- list()

# Check each patient
patients <- unique(baseline_targets$patientID)

for (pt in patients) {
    pt_targets <- baseline_targets[baseline_targets$patientID == pt, ]

    # Check 1: Max 5 target lesions per patient
    n_targets <- nrow(pt_targets)
    if (n_targets > ctx$maxTargetLesions) {
        violation_msg <- paste0(
            "Patient ", pt, " has ", n_targets, " target lesions ",
            "(exceeds RECIST v1.1 limit of ", ctx$maxTargetLesions, ")"
        )
        violations <- c(violations, violation_msg)
    }

    # Check 2: Max 2 target lesions per organ
    if ("location" %in% colnames(pt_targets)) {
        location_counts <- table(pt_targets$location)
        over_limit <- location_counts > ctx$maxLesionsPerOrgan

        if (any(over_limit)) {
            for (loc in names(location_counts[over_limit])) {
                violation_msg <- paste0(
                    "Patient ", pt, " has ", location_counts[loc],
                    " target lesions in ", loc,
                    " (exceeds RECIST v1.1 limit of ", ctx$maxLesionsPerOrgan, " per organ)"
                )
                violations <- c(violations, violation_msg)
            }
        }
    }

    # Check 3: Minimum size requirements (10mm non-lymph, 15mm lymph)
    # NOTE: This is a simplified check - in practice, lymph node detection would require
    # additional metadata. Here we just check for 10mm minimum.
    small_lesions <- pt_targets[!is.na(pt_targets$diameter) &
        pt_targets$diameter < .RECIST$MIN_TARGET_NONLYMPH, ]

    if (nrow(small_lesions) > 0) {
        for (i in seq_len(nrow(small_lesions))) {
            violation_msg <- paste0(
                "Patient ", pt, " lesion ", small_lesions$lesionID[i],
                " has diameter ", round(small_lesions$diameter[i], 1), "mm ",
                "(below RECIST v1.1 minimum of ", .RECIST$MIN_TARGET_NONLYMPH, "mm)"
            )
            violations <- c(violations, violation_msg)
        }
    }
}

# Post violations as STRONG_WARNING
if (length(violations) > 0) {
    recist_notify(ctx, "STRONG_WARNING", "RECIST v1.1 Compliance Violations", paste0("RECIST v1.1 COMPLIANCE VIOLATIONS: ", paste(violations, collapse = " \u{2022} "), " \u{2022} Results may not be suitable for regulatory submissions."))
}

return(list(
    valid = length(violations) == 0,
    violations = violations,
    target_lesions = baseline_targets$lesionID
))

}

recist_target_sums <- function(lesion_data, ctx) {
# Shape of an empty result, shared by every early exit so callers
# always receive the same columns.
target_sums_empty <- function() {
    data.frame(
        patientID = character(0),
        visitTime = numeric(0),
        visitNumber = integer(0),
        nTargetLesions = integer(0),
        nBaselineLesions = integer(0),
        baseline_sum = numeric(0),
        current_sum = numeric(0),
        absolute_change = numeric(0),
        percent_change = numeric(0),
        nadir_sum = numeric(0),
        absolute_change_from_nadir = numeric(0),
        percent_change_from_nadir = numeric(0),
        is_baseline_visit = logical(0),
        evaluable = logical(0),
        target_response = character(0),
        stringsAsFactors = FALSE
    )
}

# Filter to target lesions only
target_lesions <- lesion_data[lesion_data$lesionType == "Target", ]

if (nrow(target_lesions) == 0) {
    recist_notify(ctx, 
        "ERROR", "No Target Lesions",
        paste0("No lesion is typed \"Target\". RECIST response is assessed from ",
               "the sum of target lesion diameters, so nothing can be computed. ",
               "Check the Lesion Type variable: values must read Target or ",
               "Non-Target.")
    )
    return(target_sums_empty())
}

# Calculate baseline sums per patient
baseline_rows <- target_lesions[target_lesions$isBaseline == TRUE, ]

# aggregate() on a zero-row frame raises "no rows to aggregate",
# which surfaced as a raw R error rather than a notice.
if (nrow(baseline_rows) == 0) {
    recist_notify(ctx, 
        "ERROR", "No Baseline Target Measurements",
        paste0("No target lesion was measured at the baseline timepoint (",
               ctx$baselineTimepoint, "). Percent change is defined ",
               "relative to the baseline sum of diameters, so no response can ",
               "be assessed. Check that the baseline visit time matches the ",
               "'Baseline Timepoint' setting.")
    )
    return(target_sums_empty())
}

baseline_sums <- stats::aggregate(
    diameter ~ patientID,
    data = baseline_rows,
    FUN = function(x) sum(x, na.rm = TRUE)
)
names(baseline_sums)[2] <- "baseline_sum"

# How many target lesions each patient had measured AT BASELINE.
# The per-visit sum is only comparable with the baseline sum when
# the same lesions are measured again; see nTargetLesions below.
baseline_counts <- stats::aggregate(
    diameter ~ patientID, data = baseline_rows, FUN = length
)
names(baseline_counts)[2] <- "nBaselineLesions"

# Calculate sums for each visit
visit_sums <- stats::aggregate(
    diameter ~ patientID + visitTime,
    data = target_lesions,
    FUN = function(x) sum(x, na.rm = TRUE)
)
names(visit_sums)[3] <- "current_sum"

# Count lesions per visit. The formula interface drops rows with a
# missing diameter, so this counts lesions actually MEASURED.
lesion_counts <- stats::aggregate(
    diameter ~ patientID + visitTime,
    data = target_lesions,
    FUN = length
)
names(lesion_counts)[3] <- "nTargetLesions"

# Patients with no baseline row would be dropped by the inner join
# below without a trace, silently shrinking the ORR/DCR denominator.
missing_baseline <- setdiff(unique(target_lesions$patientID),
                            unique(baseline_sums$patientID))
if (length(missing_baseline) > 0) {
    recist_notify(ctx, 
        "WARNING", "Patients Without a Baseline Excluded",
        sprintf(paste0("%d patient(s) have target lesions but no measurement at ",
                       "the baseline timepoint and are excluded from every table ",
                       "and from the response-rate denominators: %s."),
                length(missing_baseline),
                paste(utils::head(as.character(missing_baseline), 10), collapse = ", "))
    )
}

# Merge baseline sums with visit sums
target_sums <- merge(visit_sums, baseline_sums, by = "patientID")
target_sums <- merge(target_sums, lesion_counts, by = c("patientID", "visitTime"))
target_sums <- merge(target_sums, baseline_counts, by = "patientID")

# Order by patient and visit before any running calculation.
target_sums <- target_sums[order(target_sums$patientID, target_sums$visitTime), ]

# Change from BASELINE. RECIST v1.1 references PR to the baseline
# sum, and this is also the quantity plotted in the waterfall.
target_sums$absolute_change <- target_sums$current_sum - target_sums$baseline_sum
target_sums$percent_change <- ifelse(
    target_sums$baseline_sum > 0,
    (target_sums$current_sum - target_sums$baseline_sum) / target_sums$baseline_sum * 100,
    NA
)

# Change from NADIR -- the smallest sum recorded on study up to and
# including this visit (baseline counts, since it is one of the
# visits). RECIST v1.1 defines PD as ">=20% increase in the sum of
# diameters, taking as reference the SMALLEST SUM ON STUDY", plus an
# absolute increase of >=5mm. Referencing PD to baseline instead
# (as this function previously did) means a patient who shrinks and
# then regrows is never called progressive while their sum remains
# below baseline -- exactly the case where progression matters most.
target_sums$nadir_sum <- stats::ave(
    target_sums$current_sum,
    target_sums$patientID,
    FUN = cummin
)
target_sums$absolute_change_from_nadir <-
    target_sums$current_sum - target_sums$nadir_sum
target_sums$percent_change_from_nadir <- ifelse(
    target_sums$nadir_sum > 0,
    (target_sums$current_sum - target_sums$nadir_sum) / target_sums$nadir_sum * 100,
    NA
)

# The baseline scan is the REFERENCE, not an assessment. Scoring it
# produced a manufactured "SD" for every patient at visit 0, which
# then won the best-overall-response hierarchy over a genuine PD --
# so a cohort of pure progressors reported disease control of 100%.
target_sums$is_baseline_visit <-
    target_sums$visitTime == ctx$baselineTimepoint

# A visit is only comparable with baseline if every target lesion
# measured at baseline was measured again. aggregate() drops rows
# with a missing diameter and the sum uses na.rm = TRUE, so an
# unmeasured lesion would otherwise shrink the sum and fabricate a
# partial response.
target_sums$evaluable <-
    target_sums$nTargetLesions >= target_sums$nBaselineLesions

# Determine target lesion response per RECIST v1.1
target_sums$target_response <- "SD" # Default: Stable Disease

# Partial Response: >=30% decrease from BASELINE.
# Assigned BEFORE CR: a complete response has a percent change of
# -100, which also satisfies the PR mask, so assigning PR second
# overwrote every CR and made the CR row structurally zero.
target_sums$target_response[!is.na(target_sums$percent_change) &
    target_sums$percent_change <= .RECIST$PR] <- "PR"

# Complete Response: all target lesions disappeared. Compared with a
# tolerance rather than exact float equality against 0.
target_sums$target_response[
    !is.na(target_sums$current_sum) &
        target_sums$current_sum < 1e-8] <- "CR"

# Progressive Disease: >=20% increase from NADIR *and* >=5mm absolute
# increase from nadir. Assessed last so it overrides PR/SD, as a visit
# meeting the PD criteria is progressive regardless of how far the sum
# still sits below baseline.
is_pd <- !is.na(target_sums$percent_change_from_nadir) &
    target_sums$percent_change_from_nadir >= .RECIST$PD &
    target_sums$absolute_change_from_nadir >= .RECIST$PD_ABS_MM
target_sums$target_response[is_pd] <- "PD"

# Label the reference and the unusable visits explicitly instead of
# letting them masquerade as stable disease.
target_sums$target_response[target_sums$is_baseline_visit] <- "Baseline"
target_sums$target_response[!target_sums$evaluable &
                                !target_sums$is_baseline_visit] <- "NE"

n_ne <- sum(!target_sums$evaluable & !target_sums$is_baseline_visit)
if (n_ne > 0) {
    recist_notify(ctx, 
        "WARNING", "Incomplete Target Lesion Measurements",
        sprintf(paste0("%d visit(s) measured fewer target lesions than were ",
                       "recorded at baseline. The sum of diameters is not ",
                       "comparable with the baseline sum at those visits, so they ",
                       "are reported as NE (not evaluable) rather than being ",
                       "scored. Summing only the lesions that were measured would ",
                       "understate the tumour burden and can fabricate a partial ",
                       "response."), n_ne)
    )
}

# Add visit number (sequential visits per patient)
target_sums$visitNumber <- stats::ave(
    target_sums$visitTime,
    target_sums$patientID,
    FUN = seq_along
)

# Reorder columns
target_sums <- target_sums[, c(
    "patientID", "visitTime", "visitNumber", "nTargetLesions",
    "nBaselineLesions", "baseline_sum", "current_sum",
    "absolute_change", "percent_change", "nadir_sum",
    "absolute_change_from_nadir", "percent_change_from_nadir",
    "is_baseline_visit", "evaluable", "target_response"
)]

return(target_sums)

}

recist_detect_new_lesions <- function(lesion_data) {
# Filter to new lesions
new_lesions <- lesion_data[lesion_data$lesionType == "New" |
    (lesion_data$isNewLesion == 1 & !lesion_data$isBaseline), ]

if (nrow(new_lesions) == 0) {
    return(data.frame(
        patientID = character(0),
        first_new_lesion_visit = numeric(0),
        new_lesion_location = character(0),
        new_lesion_ID = character(0),
        stringsAsFactors = FALSE
    ))
}

# For each patient, find first visit with new lesion
new_lesion_summary <- stats::aggregate(
    visitTime ~ patientID,
    data = new_lesions,
    FUN = min
)
names(new_lesion_summary)[2] <- "first_new_lesion_visit"

# Add lesion details
first_new <- merge(new_lesion_summary, new_lesions,
    by.x = c("patientID", "first_new_lesion_visit"),
    by.y = c("patientID", "visitTime")
)

# Keep first lesion per patient (if multiple new lesions at same visit)
first_new <- first_new[!duplicated(first_new$patientID), ]

first_new <- first_new[, c(
    "patientID", "first_new_lesion_visit",
    "location", "lesionID"
)]
names(first_new)[3:4] <- c("new_lesion_location", "new_lesion_ID")

return(first_new)

}

recist_assess_nontarget <- function(lesion_data, ctx) {
# Filter to non-target lesions
nontarget_lesions <- lesion_data[lesion_data$lesionType == "NonTarget", ]

# The radiologist's own assessment, if supplied, is authoritative and is
# applied even when no lesion is typed Non-Target (a reader can record
# unequivocal non-target progression without the lesions being itemised).
override <- recist_nontarget_override(lesion_data, ctx)

if (nrow(nontarget_lesions) == 0) {
    if (!is.null(override) && nrow(override) > 0) {
        return(override)
    }
    # No non-target lesions - return empty data frame
    # In RECIST, absence of non-target lesions = non-applicable (treated as Non-CR/Non-PD)
    return(data.frame(
        patientID = character(0),
        visitTime = numeric(0),
        nontarget_status = character(0),
        stringsAsFactors = FALSE
    ))
}

# Count non-target lesions per patient-visit
nontarget_counts <- stats::aggregate(
    lesionID ~ patientID + visitTime,
    data = nontarget_lesions,
    FUN = length
)
names(nontarget_counts)[3] <- "n_nontarget"

# Get baseline counts. A non-target lesion need not exist at baseline -- it may
# appear later -- and aggregate() raises "no rows to aggregate" on an empty
# frame. Patients with no baseline non-target disease count as zero, which is
# the right reference: every lesion present later is an increase on it.
baseline_rows <- nontarget_lesions[nontarget_lesions$isBaseline, , drop = FALSE]
if (nrow(baseline_rows) > 0) {
    baseline_counts <- stats::aggregate(lesionID ~ patientID, data = baseline_rows, FUN = length)
    names(baseline_counts)[2] <- "baseline_n_nontarget"
} else {
    baseline_counts <- data.frame(
        patientID = unique(nontarget_lesions$patientID),
        baseline_n_nontarget = 0L,
        stringsAsFactors = FALSE
    )
}

# Merge. all.x keeps patients whose non-target disease is entirely post-baseline;
# an inner join would drop them silently.
nontarget_assessment <- merge(nontarget_counts, baseline_counts,
                              by = "patientID", all.x = TRUE)
nontarget_assessment$baseline_n_nontarget[
    is.na(nontarget_assessment$baseline_n_nontarget)] <- 0L

# Determine status
# CR: All non-target lesions disappeared (count = 0)
# Non-CR/Non-PD: Some lesions persist but no clear progression
# PD: Unequivocal progression (e.g., increase in number - simplified here)

nontarget_assessment$nontarget_status <- "Non-CR/Non-PD" # Default

# CR: All disappeared
nontarget_assessment$nontarget_status[nontarget_assessment$n_nontarget == 0] <- "CR"

# PD: lesion-count heuristic (>=2 more non-target lesions than at
# baseline). This is NOT the RECIST criterion, which is the reporting
# radiologist's judgement of "unequivocal progression" and cannot be
# derived from a count. Say so plainly whenever the heuristic is what
# is actually driving the result.
increase <- nontarget_assessment$n_nontarget - nontarget_assessment$baseline_n_nontarget
nontarget_assessment$nontarget_status[increase >= 2] <- "PD"

if (is.null(override) || nrow(override) == 0) {
    recist_notify(ctx, 
        "WARNING", "Non-Target Progression Is Estimated, Not Assessed",
        paste0("No radiologist non-target assessment was supplied, so non-target ",
               "progression was inferred from the LESION COUNT: a visit with two ",
               "or more non-target lesions above baseline is called progressive. ",
               "RECIST v1.1 instead requires a qualitative judgement of ",
               "unequivocal progression, which a count cannot establish - this ",
               "heuristic can both miss real progression and over-call it. ",
               "Assign the 'Non-Target Response' variable to record the reader's ",
               "own CR / Non-CR/Non-PD / PD assessment, which overrides this.")
    )
}

nontarget_assessment <- nontarget_assessment[, c("patientID", "visitTime", "nontarget_status")]

# The radiologist's judgement replaces the computed value wherever it
# was recorded. RECIST defines non-target PD as "unequivocal
# progression", which no count can establish, so the supplied value
# wins outright rather than being blended with the heuristic.
if (!is.null(override) && nrow(override) > 0) {
    key  <- paste(nontarget_assessment$patientID, nontarget_assessment$visitTime)
    okey <- paste(override$patientID, override$visitTime)
    hit  <- match(key, okey)
    nontarget_assessment$nontarget_status[!is.na(hit)] <-
        override$nontarget_status[hit[!is.na(hit)]]

    # Visits the reader assessed but that have no non-target lesion row
    extra <- override[!(okey %in% key), , drop = FALSE]
    if (nrow(extra) > 0) {
        nontarget_assessment <- rbind(nontarget_assessment, extra)
    }
}

return(nontarget_assessment)

}

recist_overall_response <- function(target_sums, new_lesions, nontarget_assessment, ctx) {
# The baseline scan is the reference point, not an assessment: it must
# never enter the response stream or it competes in the best-overall-
# response hierarchy as a free "SD".
baseline_only <- character(0)
if ("is_baseline_visit" %in% names(target_sums)) {
    all_pts <- unique(target_sums$patientID)
    post <- target_sums[!target_sums$is_baseline_visit, , drop = FALSE]
    # A patient with no post-baseline scan is NOT EVALUABLE. Dropping
    # them here would quietly remove them from the response-rate
    # denominator instead.
    baseline_only <- setdiff(all_pts, unique(post$patientID))
    target_sums <- post
}

if (nrow(target_sums) == 0) {
    return(data.frame(
        patientID = character(0), visitTime = numeric(0),
        visitNumber = integer(0), target_response = character(0),
        new_lesion_present = logical(0), nontarget_status = character(0),
        overall_response_unconfirmed = character(0),
        stringsAsFactors = FALSE
    ))
}

# Start with target sums as base
responses <- target_sums[, c("patientID", "visitTime", "visitNumber", "target_response")]

# Add new lesion status
if (nrow(new_lesions) > 0) {
    responses$new_lesion_present <- FALSE
    for (i in seq_len(nrow(responses))) {
        pt <- responses$patientID[i]
        vt <- responses$visitTime[i]

        # Check if new lesion appeared at or before this visit
        pt_new <- new_lesions[new_lesions$patientID == pt, ]
        if (nrow(pt_new) > 0 && pt_new$first_new_lesion_visit[1] <= vt) {
            responses$new_lesion_present[i] <- TRUE
        }
    }
} else {
    responses$new_lesion_present <- FALSE
}

# Add non-target status
if (nrow(nontarget_assessment) > 0) {
    responses <- merge(responses, nontarget_assessment,
        by = c("patientID", "visitTime"),
        all.x = TRUE
    )
    # No non-target row for this visit means the patient has no
    # non-target disease -- NOT that it is present and non-CR. RECIST
    # only downgrades a target CR to overall PR when non-target
    # lesions actually exist.
    responses$nontarget_status[is.na(responses$nontarget_status)] <- "None"
} else {
    responses$nontarget_status <- "None"
}

# Apply RECIST v1.1 OVERALL RESPONSE TABLE
responses$overall_response_unconfirmed <- "SD" # Default

# PRIORITY 1: ANY new lesion → PD
responses$overall_response_unconfirmed[responses$new_lesion_present] <- "PD"

# PRIORITY 2: Non-target PD → PD
responses$overall_response_unconfirmed[responses$nontarget_status == "PD"] <- "PD"

# PRIORITY 3: Target PD → PD
responses$overall_response_unconfirmed[responses$target_response == "PD"] <- "PD"

# Now handle non-PD cases
is_not_pd <- responses$overall_response_unconfirmed != "PD"

# PR: Target CR with non-target disease still present, OR Target PR.
# Assigned before CR so the CR branch below can override it.
is_pr <- is_not_pd &
    ((responses$target_response == "CR" &
        !(responses$nontarget_status %in% c("CR", "None"))) |
        (responses$target_response == "PR")) &
    !responses$new_lesion_present
responses$overall_response_unconfirmed[is_pr] <- "PR"

# CR: Target CR, no new lesions, and either non-target CR or no
# non-target disease at all.
is_cr <- is_not_pd &
    responses$target_response == "CR" &
    responses$nontarget_status %in% c("CR", "None") &
    !responses$new_lesion_present
responses$overall_response_unconfirmed[is_cr] <- "CR"

# SD: Everything else (target SD with non-PD non-target, no new lesions)
# Already set as default

# A visit whose target sum could not be compared with baseline is not
# evaluable. A new lesion still makes it PD -- that judgement needs no
# measurement -- but otherwise it must not be scored as SD.
not_evaluable <- responses$target_response == "NE" &
    !responses$new_lesion_present &
    responses$nontarget_status != "PD"
responses$overall_response_unconfirmed[not_evaluable] <- "NE"

# Re-attach patients whose only scan was the baseline, as NE, so they
# stay in the cohort and in the response-rate denominator.
if (length(baseline_only) > 0) {
    responses <- rbind(responses, data.frame(
        patientID = baseline_only,
        visitTime = NA_real_,
        visitNumber = NA_integer_,
        target_response = "NE",
        new_lesion_present = FALSE,
        nontarget_status = "Non-CR/Non-PD",
        overall_response_unconfirmed = "NE",
        stringsAsFactors = FALSE
    )[, names(responses), drop = FALSE])

    recist_notify(ctx, 
        "WARNING", "Patients Not Response-Evaluable",
        sprintf(paste0("%d patient(s) have a baseline scan but no post-baseline ",
                       "assessment and are reported as NE (not evaluable): %s. ",
                       "They are not counted as stable disease."),
                length(baseline_only),
                paste(utils::head(as.character(baseline_only), 10), collapse = ", "))
    )
}

# Sort by patient and visit
responses <- responses[order(responses$patientID, responses$visitTime), ]

return(responses)

}

recist_confirm_responses <- function(visit_responses, ctx) {
# `df$col <- FALSE` on a zero-row frame raises "replacement has 1 row,
# data has 0". Reached whenever no target lesion survives validation.
if (is.null(visit_responses) || nrow(visit_responses) == 0) {
    visit_responses$response_confirmed <- logical(0)
    return(visit_responses)
}

# Add confirmation status column
visit_responses$response_confirmed <- FALSE

# SD and PD do not require confirmation
visit_responses$response_confirmed[visit_responses$overall_response_unconfirmed %in% c("SD", "PD")] <- TRUE

# For CR and PR, check confirmation
patients <- unique(visit_responses$patientID)

for (pt in patients) {
    pt_data <- visit_responses[visit_responses$patientID == pt, ]
    pt_data <- pt_data[order(pt_data$visitTime), ]

    for (i in seq_len(nrow(pt_data))) {
        current_response <- pt_data$overall_response_unconfirmed[i]

        # Only check CR and PR
        if (current_response %in% c("CR", "PR")) {
            current_time <- pt_data$visitTime[i]

            # Check if there's a subsequent visit with same response >= confirmation_interval later
            later_visits <- pt_data[pt_data$visitTime > current_time, ]

            # Confirmation cannot reach across a progression: once the
            # patient has progressed, a later matching assessment belongs
            # to a different disease course and cannot confirm this one.
            pd_times <- pt_data$visitTime[
                pt_data$overall_response_unconfirmed == "PD" &
                    pt_data$visitTime > current_time]
            if (length(pd_times) > 0) {
                later_visits <- later_visits[
                    later_visits$visitTime < min(pd_times), , drop = FALSE]
            }

            if (nrow(later_visits) > 0) {
                # Find visits with sufficient time gap
                confirmed_visits <- later_visits[
                    (later_visits$visitTime - current_time) >= ctx$confirmationInterval &
                        later_visits$overall_response_unconfirmed == current_response,
                ]

                if (nrow(confirmed_visits) > 0) {
                    # Response is confirmed
                    row_idx <- which(visit_responses$patientID == pt &
                        visit_responses$visitTime == current_time)
                    visit_responses$response_confirmed[row_idx] <- TRUE
                }
            }
        }
    }
}

return(visit_responses)

}

recist_best_overall_response <- function(confirmed_responses) {
if (is.null(confirmed_responses) || nrow(confirmed_responses) == 0) {
    return(data.frame(
        patientID = character(0), bestOverallResponse = character(0),
        borConfirmed = character(0), borFirstVisit = character(0),
        timeToResponse = numeric(0), durationOfResponse = numeric(0),
        progressionOccurred = character(0), progressionVisit = character(0),
        stringsAsFactors = FALSE))
}
patients <- unique(confirmed_responses$patientID)

bor_results <- data.frame(
    patientID = patients,
    bestOverallResponse = character(length(patients)),
    borConfirmed = character(length(patients)),
    borFirstVisit = character(length(patients)),
    timeToResponse = numeric(length(patients)),
    durationOfResponse = numeric(length(patients)),
    progressionOccurred = character(length(patients)),
    progressionVisit = character(length(patients)),
    stringsAsFactors = FALSE
)

for (i in seq_along(patients)) {
    pt <- patients[i]
    pt_data <- confirmed_responses[confirmed_responses$patientID == pt, ]
    pt_data <- pt_data[order(pt_data$visitTime), ]

    # Best overall response is the best assessment recorded from the
    # start of treatment UNTIL PROGRESSION. Anything after the first
    # PD belongs to a later line of therapy and must not contribute,
    # otherwise a post-progression measurement can supply the BOR and
    # produce a negative duration of response.
    first_pd_time <- suppressWarnings(
        min(pt_data$visitTime[pt_data$overall_response_unconfirmed == "PD"]))
    if (is.finite(first_pd_time)) {
        pt_data <- pt_data[pt_data$visitTime <= first_pd_time, , drop = FALSE]
    }

    # Filter to confirmed responses only for BOR determination
    confirmed_only <- pt_data[pt_data$response_confirmed, ]

    # Determine BOR using hierarchy: CR > PR > SD > PD
    # Only CONFIRMED CR/PR count for BOR
    has_confirmed_cr <- any(confirmed_only$overall_response_unconfirmed == "CR")
    has_confirmed_pr <- any(confirmed_only$overall_response_unconfirmed == "PR")
    has_sd <- any(confirmed_only$overall_response_unconfirmed == "SD")
    has_pd <- any(confirmed_only$overall_response_unconfirmed == "PD")

    if (has_confirmed_cr) {
        bor_results$bestOverallResponse[i] <- "CR"
        bor_results$borConfirmed[i] <- "Yes"
        first_cr_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
        bor_results$borFirstVisit[i] <- paste0("Visit ", first_cr_visit)
        bor_results$timeToResponse[i] <- first_cr_visit - min(pt_data$visitTime)
    } else if (has_confirmed_pr) {
        bor_results$bestOverallResponse[i] <- "PR"
        bor_results$borConfirmed[i] <- "Yes"
        first_pr_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
        bor_results$borFirstVisit[i] <- paste0("Visit ", first_pr_visit)
        bor_results$timeToResponse[i] <- first_pr_visit - min(pt_data$visitTime)
    } else if (has_sd) {
        bor_results$bestOverallResponse[i] <- "SD"
        bor_results$borConfirmed[i] <- "Yes"
        first_sd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "SD"])
        bor_results$borFirstVisit[i] <- paste0("Visit ", first_sd_visit)
        bor_results$timeToResponse[i] <- NA
    } else if (has_pd) {
        bor_results$bestOverallResponse[i] <- "PD"
        bor_results$borConfirmed[i] <- "Yes"
        first_pd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PD"])
        bor_results$borFirstVisit[i] <- paste0("Visit ", first_pd_visit)
        bor_results$timeToResponse[i] <- NA
    } else {
        # No confirmed response
        bor_results$bestOverallResponse[i] <- "Not Evaluable"
        bor_results$borConfirmed[i] <- "No"
        bor_results$borFirstVisit[i] <- "N/A"
        bor_results$timeToResponse[i] <- NA
    }

    # Check for progression
    if (has_pd) {
        bor_results$progressionOccurred[i] <- "Yes"
        first_pd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PD"])
        bor_results$progressionVisit[i] <- paste0("Visit ", first_pd_visit)

        # Duration of response (if had CR/PR before PD)
        if (has_confirmed_cr || has_confirmed_pr) {
            response_visit <- if (has_confirmed_cr) {
                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
            } else {
                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
            }
            bor_results$durationOfResponse[i] <- first_pd_visit - response_visit
        } else {
            bor_results$durationOfResponse[i] <- NA
        }
    } else {
        bor_results$progressionOccurred[i] <- "No"
        bor_results$progressionVisit[i] <- "N/A"

        # Duration = last visit - first response (censored)
        if (has_confirmed_cr || has_confirmed_pr) {
            response_visit <- if (has_confirmed_cr) {
                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
            } else {
                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
            }
            bor_results$durationOfResponse[i] <- max(pt_data$visitTime) - response_visit
        } else {
            bor_results$durationOfResponse[i] <- NA
        }
    }
}

return(bor_results)

}
