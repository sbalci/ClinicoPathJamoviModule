# R6 Class Implementation ====
waterfallrecistClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "waterfallrecistClass",
        inherit = waterfallrecistBase,
        private = list(
            # RECIST v1.1 Constants ====
            RECIST_CR_THRESHOLD = -100, # Complete response: disappearance of all lesions
            RECIST_PR_THRESHOLD = -30, # Partial response: >=30% decrease
            RECIST_PD_THRESHOLD = 20, # Progressive disease: >=20% increase (+ 5mm absolute)
            RECIST_PD_ABSOLUTE_MM = 5, # Absolute increase required for PD (in addition to 20%)
            MIN_TARGET_DIAMETER_NONLYMPH = 10, # Minimum 10mm for non-lymph node targets
            MIN_TARGET_DIAMETER_LYMPH = 15, # Minimum 15mm short axis for lymph nodes

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),
            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "WARNING: ",
                        WARNING        = "WARNING: ",
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },

            # Show the required data shape. This analysis needs lesion-level data,
            # a layout most users will not have met, and four variables assigned
            # before anything at all appears -- previously it opened to a blank
            # screen with no guidance.
            .renderInstructions = function() {
                configured <- !is.null(self$options$patientID) &&
                    !is.null(self$options$lesionID) &&
                    !is.null(self$options$visitTime) &&
                    !is.null(self$options$diameter)

                if (configured) {
                    self$results$instructions$setContent("")
                    self$results$instructions$setVisible(FALSE)
                    return()
                }

                html <- paste0(
                    "<div style='padding:12px; background-color:#f8fafc; ",
                    "border-left:4px solid #0369a1; border-radius:4px;'>",
                    "<h3 style='margin-top:0; color:#0369a1;'>",
                    .("RECIST v1.1 response analysis from lesion-level data"), "</h3>",

                    "<p>", .("This analysis needs ONE ROW PER LESION PER VISIT. Assign these four variables to begin:"), "</p>",
                    "<ul>",
                    "<li><b>", .("Patient ID"), "</b></li>",
                    "<li><b>", .("Lesion ID"), "</b> &ndash; ",
                    .("identifies each lesion so it can be followed across visits"), "</li>",
                    "<li><b>", .("Visit Time"), "</b> &ndash; ",
                    .("the baseline visit must be present (default: time = 0)"), "</li>",
                    "<li><b>", .("Diameter"), "</b> &ndash; ",
                    .("longest diameter in mm; short axis for lymph nodes"), "</li>",
                    "</ul>",

                    "<p>", .("Optional, and each unlocks a RECIST rule:"), "</p>",
                    "<ul>",
                    "<li><b>", .("Lesion Type"), "</b> (Target / Non-Target / New) &ndash; ",
                    .("enables non-target assessment; without it every lesion is treated as a target lesion"), "</li>",
                    "<li><b>", .("New Lesion Indicator"), "</b> (Yes/No, 1/0 or TRUE/FALSE) &ndash; ",
                    .("any new lesion is automatic progression"), "</li>",
                    "<li><b>", .("Location"), "</b> &ndash; ",
                    .("organ, used to apply the limit of two target lesions per organ"), "</li>",
                    "</ul>",

                    "<p><b>", .("Example layout"), "</b></p>",
                    "<pre style='background:#ffffff; padding:8px; border:1px solid #cbd5e1;'>",
                    "PatientID  LesionID  VisitTime  LesionType  Location  Diameter  IsNew\n",
                    "PT1        L1        0          Target      Liver     60        No\n",
                    "PT1        L2        0          Target      Lung      40        No\n",
                    "PT1        L1        8          Target      Liver     36        No\n",
                    "PT1        L2        8          Target      Lung      24        No\n",
                    "PT1        N1        8          Non-Target  Bone       9        Yes",
                    "</pre>",

                    "<p style='color:#7c2d12;'><b>", .("Please note:"), "</b> ",
                    .("this implementation has not been checked against a reference RECIST tool or a regulatory dataset. Treat it as a research tool and confirm response assignments against the source imaging."),
                    "</p>",

                    "<p>",
                    .("If you have only one tumour burden value per patient (a percent change, or a single measurement per visit), use the patient-level Treatment Response analysis instead."),
                    "</p>",
                    "</div>"
                )

                self$results$instructions$setContent(html)
                self$results$instructions$setVisible(TRUE)
            },

            # Initialization ====
            .init = function() {
                # Render from .init as well as .run: .init runs before jmvcore
                # prepares the data, so the guidance still appears when no
                # variables have been assigned yet (which is exactly when it is
                # needed, and when data preparation cannot succeed).
                private$.renderInstructions()

                # Initialize output elements
                if (self$options$showLesionTable) {
                    private$.initLesionTable()
                }

                if (self$options$showTargetSumTable) {
                    private$.initTargetSumTable()
                }

                if (self$options$showBestResponseTable) {
                    private$.initBestResponseTable()
                }

                if (self$options$showRecistComplianceReport) {
                    private$.initComplianceReport()
                }

                if (self$options$showWaterfallPlot) {
                    private$.initWaterfallPlot()
                }

                if (self$options$showSpiderPlot) {
                    private$.initSpiderPlot()
                }
            },

            # Main Execution ====
            .run = function() {
                # Show the required data shape until the analysis is configured.
                # Without this the analysis opened to a completely blank screen,
                # which is unhelpful for a lesion-level format most users have not
                # met before and which needs four variables assigned.
                private$.renderInstructions()

                # Reset notice collection and post the methodology notice
                private$.noticeList <- list()
                private$.addNotice("INFO", "RECIST v1.1 Methodology", paste0("RECIST v1.1 protocol per Eisenhauer et al. (2009) Eur J Cancer 45:228-247. Target lesion limits: <=", self$options$maxTargetLesions, " total, <=", self$options$maxLesionsPerOrgan, " per organ. CR/PR confirmation: >=", self$options$confirmationInterval, " weeks. Any new lesion = PD."))

                # Input validation
                validation_result <- private$.validateInput()
                if (!validation_result$valid) {
                    return() # Errors already posted as notices
                }

                # Step 1 - Validate and prepare lesion-level data
                lesion_data <- private$.prepareLesionData()

                if (is.null(lesion_data) || nrow(lesion_data) == 0) {
                    private$.addNotice("ERROR", "No Valid Lesion Data", paste0("No valid lesion data after removing rows with missing Patient ID, Lesion ID, or Visit Time. Ensure baseline visit (time=", self$options$baselineTimepoint, ") exists and data is in lesion-level format."))
                    return()
                }

                # Step 1b - Apply the RECIST target-lesion limits. Must run before
                # any sum is taken: it decides which lesions the sum is over.
                lesion_data <- private$.selectTargetLesions(lesion_data)

                # Step 2 - Validate target lesion selection (max 5, max 2 per organ)
                target_validation <- private$.validateTargetLesionSelection(lesion_data)

                # Step 3 - Calculate target lesion sums by visit
                target_sums <- private$.calculateTargetLesionSums(lesion_data)

                # Step 4 - Detect new lesions (any new lesion = PD)
                new_lesions <- private$.detectNewLesions(lesion_data)

                # Step 5 - Assess non-target lesion progression
                nontarget_assessment <- private$.assessNonTargetProgression(lesion_data)

                # Step 6 - Determine overall response per visit
                visit_responses <- private$.determineOverallResponse(target_sums, new_lesions, nontarget_assessment)

                # Step 7 - Apply confirmation rules (CR/PR must be confirmed >=4 weeks)
                confirmed_responses <- private$.confirmResponses(visit_responses)

                # Step 8 - Calculate Best Overall Response (BOR)
                best_responses <- private$.calculateBestOverallResponse(confirmed_responses)

                # Step 9 - Populate output tables
                if (self$options$showLesionTable) {
                    private$.populateLesionTable(lesion_data)
                }

                if (self$options$showTargetSumTable) {
                    private$.populateTargetSumTable(target_sums, confirmed_responses)
                }

                if (self$options$showBestResponseTable) {
                    private$.populateBestResponseTable(best_responses)
                    private$.populateRecistSummary(best_responses)
                }

                if (self$options$showRecistComplianceReport) {
                    private$.populateComplianceReport(target_validation, new_lesions, confirmed_responses)
                }

                # Step 10 - Generate plots
                if (self$options$showWaterfallPlot) {
                    private$.prepareWaterfallPlot(best_responses, target_sums)
                }

                if (self$options$showSpiderPlot) {
                    private$.prepareSpiderPlot(target_sums, best_responses)
                }

                # Step 11 - Analysis completion notice
                n_patients <- length(unique(best_responses$patientID))
                if (n_patients == 0) {
                    private$.addNotice("ERROR", "No Evaluable Patients",
                        "No patient could be assessed for response. See the messages above.")
                    return()
                }
                n_cr <- sum(best_responses$bestOverallResponse == "CR")
                n_pr <- sum(best_responses$bestOverallResponse == "PR")
                orr_pct <- round((n_cr + n_pr) / n_patients * 100, 1)

                private$.addNotice("INFO", "Analysis Complete", paste0("RECIST v1.1 analysis completed for ", n_patients, " patients. ORR: ", orr_pct, "% (CR=", n_cr, ", PR=", n_pr, "). Confirmation interval: >=", self$options$confirmationInterval, " weeks."))

                # Step 12 - Add BOR to dataset if requested
                # TODO: Implement data augmentation
                # if (self$options$addBestResponseToData)
                #     private$.addBestResponseColumn(best_responses)
            },

            # Input Validation ====
            .validateInput = function() {
                # Check for required variables
                if (is.null(self$options$patientID) ||
                    length(self$options$patientID) == 0) {
                    private$.addNotice("ERROR", "Patient ID Required", "Patient ID variable is required. Select a patient identifier variable and re-run.")
                    return(list(valid = FALSE, message = ""))
                }

                if (is.null(self$options$lesionID) ||
                    length(self$options$lesionID) == 0) {
                    private$.addNotice("ERROR", "Lesion ID Required", "Lesion ID variable is required. Select a lesion identifier variable and re-run.")
                    return(list(valid = FALSE, message = ""))
                }

                if (is.null(self$options$visitTime) ||
                    length(self$options$visitTime) == 0) {
                    private$.addNotice("ERROR", "Visit Time Required", "Visit Time variable is required. Select a time variable (baseline=0 recommended) and re-run.")
                    return(list(valid = FALSE, message = ""))
                }

                if (is.null(self$options$diameter) ||
                    length(self$options$diameter) == 0) {
                    private$.addNotice("ERROR", "Diameter Required", "Lesion Diameter variable is required. Select a numeric diameter variable (in mm) and re-run.")
                    return(list(valid = FALSE, message = ""))
                }

                # Get data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice("ERROR", "No Data", "No data available. Load a dataset in lesion-level format (one row per lesion per visit).")
                    return(list(valid = FALSE, message = ""))
                }

                # Extract variables
                data_df <- self$data

                # Verify visitTime variable exists
                visitTimeVar <- private$.resolveVar(self$options$visitTime)
                if (!visitTimeVar %in% colnames(data_df)) {
                    private$.addNotice("ERROR", "Visit Time Variable Not Found", paste0('Visit Time variable "', self$options$visitTime, '" not found in dataset. Verify variable name and re-run.'))
                    return(list(valid = FALSE, message = ""))
                }

                # Verify baseline timepoint exists
                baseline_present <- any(data_df[[visitTimeVar]] == self$options$baselineTimepoint, na.rm = TRUE)
                if (!baseline_present) {
                    private$.addNotice("ERROR", "Baseline Timepoint Not Found", paste0("Baseline timepoint (", self$options$baselineTimepoint, ") not found in Visit Time. Ensure baseline measurements exist or adjust Baseline Timepoint Value."))
                    return(list(valid = FALSE, message = ""))
                }

                # Check diameter variable exists
                diameterVar <- private$.resolveVar(self$options$diameter)
                if (!diameterVar %in% colnames(data_df)) {
                    private$.addNotice("ERROR", "Diameter Variable Not Found", paste0('Diameter variable "', self$options$diameter, '" not found in dataset. Verify variable name and re-run.'))
                    return(list(valid = FALSE, message = ""))
                }

                # Check diameter values (must be non-negative)
                diameter_values <- data_df[[diameterVar]]
                if (any(diameter_values < 0, na.rm = TRUE)) {
                    private$.addNotice("ERROR", "Invalid Diameter Values", "Diameter values must be non-negative (>=0 mm). Correct negative values and re-run.")
                    return(list(valid = FALSE, message = ""))
                }

                # Check if lesionType is provided and has valid values
                if (!is.null(self$options$lesionType) && length(self$options$lesionType) > 0) {
                    lesionTypeVar <- private$.resolveVar(self$options$lesionType)
                    if (lesionTypeVar %in% colnames(data_df)) {
                        lesion_types <- unique(as.character(data_df[[lesionTypeVar]]))
                        lesion_types <- lesion_types[!is.na(lesion_types)]

                        valid_types <- c("Target", "NonTarget", "New", "target", "nontarget", "new")
                        invalid_types <- lesion_types[!lesion_types %in% valid_types]

                        if (length(invalid_types) > 0) {
                            private$.addNotice("WARNING", "Invalid Lesion Types", paste0("Lesion Type contains invalid values: ", paste(invalid_types, collapse = ", "), ". Valid values: Target, NonTarget, New. Invalid entries treated as Target."))
                        }
                    }
                }

                return(list(valid = TRUE, message = ""))
            },

            # RECIST v1.1 Core Methods ====

            .prepareLesionData = function() {
                # Extract data
                data_df <- self$data

                # Get variable names (encoded)
                patientIDVar <- private$.resolveVar(self$options$patientID)
                lesionIDVar <- private$.resolveVar(self$options$lesionID)
                visitTimeVar <- private$.resolveVar(self$options$visitTime)
                diameterVar <- private$.resolveVar(self$options$diameter)

                # Build lesion data frame
                lesion_data <- data.frame(
                    patientID = as.character(data_df[[patientIDVar]]),
                    lesionID = as.character(data_df[[lesionIDVar]]),
                    visitTime = as.numeric(data_df[[visitTimeVar]]),
                    diameter = as.numeric(data_df[[diameterVar]]),
                    stringsAsFactors = FALSE
                )

                # Add optional variables if provided
                if (!is.null(self$options$lesionType) && length(self$options$lesionType) > 0) {
                    lesionTypeVar <- private$.resolveVar(self$options$lesionType)
                    if (lesionTypeVar %in% colnames(data_df)) {
                        raw_type <- as.character(data_df[[lesionTypeVar]])
                        # Normalise by stripping case, whitespace and separators, so
                        # "Non-Target", "non target" and "NONTARGET" all resolve.
                        # Previously only the three exact strings "target",
                        # "nontarget" and "new" matched after tolower(), so the
                        # hyphenated spelling used in most datasets fell through
                        # unchanged and every non-target lesion was silently ignored
                        # by .assessNonTargetProgression (which filters "NonTarget").
                        key <- gsub("[^a-z]", "", tolower(trimws(raw_type)))
                        lesion_data$lesionType <- raw_type
                        lesion_data$lesionType[key == "target"] <- "Target"
                        lesion_data$lesionType[key == "nontarget"] <- "NonTarget"
                        lesion_data$lesionType[key == "new"] <- "New"

                        unknown <- unique(raw_type[!(key %in% c("target", "nontarget", "new")) &
                                                       !is.na(raw_type) & nzchar(raw_type)])
                        if (length(unknown) > 0) {
                            private$.addNotice(
                                "WARNING", "Unrecognised Lesion Type",
                                sprintf(paste0("These Lesion Type values were not recognised and ",
                                               "their lesions are excluded from both the target ",
                                               "sum and the non-target assessment: %s. Use Target, ",
                                               "Non-Target or New."),
                                        paste(utils::head(unknown, 10), collapse = ", "))
                            )
                        }
                    } else {
                        lesion_data$lesionType <- "Target" # Default to Target if not specified
                    }
                } else {
                    lesion_data$lesionType <- "Target" # Default to Target if not specified
                }

                if (!is.null(self$options$location) && length(self$options$location) > 0) {
                    locationVar <- private$.resolveVar(self$options$location)
                    if (locationVar %in% colnames(data_df)) {
                        lesion_data$location <- as.character(data_df[[locationVar]])
                    } else {
                        lesion_data$location <- "Unknown"
                    }
                } else {
                    lesion_data$location <- "Unknown"
                }

                if (!is.null(self$options$isNewLesion) && length(self$options$isNewLesion) > 0) {
                    isNewLesionVar <- private$.resolveVar(self$options$isNewLesion)
                    if (isNewLesionVar %in% colnames(data_df)) {
                        # jmvcore::toNumeric() on a factor of "Yes"/"No" returns the
                        # LABELS, not numbers, so `== 1` was never TRUE and a new
                        # lesion coded that way was never detected -- silently losing
                        # an automatic PD. Accept the codings people actually use.
                        raw_new <- data_df[[isNewLesionVar]]
                        if (is.logical(raw_new)) {
                            flag <- raw_new
                        } else {
                            k <- gsub("[^a-z0-9]", "", tolower(trimws(as.character(raw_new))))
                            flag <- k %in% c("1", "true", "t", "yes", "y", "new")
                            unknown <- unique(as.character(raw_new)[
                                !(k %in% c("0", "1", "true", "false", "t", "f",
                                           "yes", "no", "y", "n", "new", "")) &
                                    !is.na(raw_new)])
                            if (length(unknown) > 0) {
                                private$.addNotice(
                                    "WARNING", "Unrecognised New-Lesion Values",
                                    sprintf(paste0("These New Lesion Indicator values were not ",
                                                   "recognised and are treated as NOT new: %s. ",
                                                   "Use 1/0, TRUE/FALSE or Yes/No. Any new lesion ",
                                                   "is an automatic Progressive Disease, so an ",
                                                   "unrecognised value can hide a progression."),
                                            paste(utils::head(unknown, 10), collapse = ", "))
                                )
                            }
                        }
                        flag[is.na(flag)] <- FALSE
                        lesion_data$isNewLesion <- as.numeric(flag)
                        # Mark new lesions
                        lesion_data$lesionType[lesion_data$isNewLesion == 1] <- "New"
                    } else {
                        lesion_data$isNewLesion <- 0
                    }
                } else {
                    lesion_data$isNewLesion <- 0
                }

                # Optional per-visit non-target assessment supplied by the reporting
                # radiologist. Carried through as-is; normalised and applied in
                # .assessNonTargetProgression.
                ntOpt <- private$.optionOrNull("nonTargetResponseVar")
                if (!is.null(ntOpt) && length(ntOpt) > 0) {
                    ntVar <- private$.resolveVar(ntOpt)
                    if (ntVar %in% colnames(data_df)) {
                        lesion_data$nonTargetResponse <- as.character(data_df[[ntVar]])
                    } else {
                        lesion_data$nonTargetResponse <- NA_character_
                    }
                } else {
                    lesion_data$nonTargetResponse <- NA_character_
                }

                # Optional per-lesion target selection recorded by the reader.
                tsOpt <- private$.optionOrNull("targetSelectionVar")
                if (!is.null(tsOpt) && length(tsOpt) > 0) {
                    tsVar <- private$.resolveVar(tsOpt)
                    if (tsVar %in% colnames(data_df)) {
                        lesion_data$targetSelection <- as.character(data_df[[tsVar]])
                    } else {
                        lesion_data$targetSelection <- NA_character_
                    }
                } else {
                    lesion_data$targetSelection <- NA_character_
                }

                # Remove rows with missing key values
                lesion_data <- lesion_data[!is.na(lesion_data$patientID) &
                    !is.na(lesion_data$lesionID) &
                    !is.na(lesion_data$visitTime), ]

                # Identify baseline measurements
                lesion_data$isBaseline <- lesion_data$visitTime == self$options$baselineTimepoint

                # Sort by patient, lesion, and time
                lesion_data <- lesion_data[order(
                    lesion_data$patientID,
                    lesion_data$lesionID,
                    lesion_data$visitTime
                ), ]

                # Add row ID for reference
                lesion_data$rowID <- seq_len(nrow(lesion_data))

                return(lesion_data)
            },
            .selectTargetLesions = function(lesion_data) {
                recist_select_target_lesions(lesion_data, private$.recistContext())
            },

            .validateTargetLesionSelection = function(lesion_data) {
                recist_validate_target_selection(lesion_data, private$.recistContext())
            },
            .calculateTargetLesionSums = function(lesion_data) {
                recist_target_sums(lesion_data, private$.recistContext())
            },
            .detectNewLesions = function(lesion_data) {
                recist_detect_new_lesions(lesion_data)
            },
            .assessNonTargetProgression = function(lesion_data) {
                recist_assess_nontarget(lesion_data, private$.recistContext())
            },

            # Map free-text non-target assessments onto the RECIST categories.
            # Case, spacing and punctuation are ignored, so "Non-CR/Non-PD",
            # "non cr non pd" and "NonCRNonPD" are one assessment. Anything
            # unrecognised returns NA so the caller can report it and fall back.
            # Pure function of its input: kept separate so it is testable without a
            # configured analysis.
            .normaliseNonTargetStatus = function(x) {
                recist_normalise_nontarget(x)
            },

            # Assemble what the shared RECIST engine needs from this analysis:
            # the options it reads and a way to raise notices. The engine itself
            # knows nothing about R6, jmvcore or how notices are rendered.
            .recistContext = function() {
                recist_context(
                    baselineTimepoint    = self$options$baselineTimepoint,
                    confirmationInterval = self$options$confirmationInterval,
                    maxTargetLesions     = self$options$maxTargetLesions,
                    maxLesionsPerOrgan   = self$options$maxLesionsPerOrgan,
                    nonTargetResponseVar = private$.optionOrNull("nonTargetResponseVar"),
                    targetSelectionVar   = private$.optionOrNull("targetSelectionVar"),
                    notify = function(type, title, content)
                        private$.addNotice(type, title, content)
                )
            },

            # Reading an option that the compiled .h.R does not yet carry raises an
            # error rather than returning NULL, so every access to a newly added
            # option goes through this until the header is regenerated.
            .optionOrNull = function(name) {
                tryCatch(self$options[[name]], error = function(e) NULL)
            },

            # Normalise and collapse the optional radiologist non-target assessment to
            # one row per patient per visit. Returns NULL when the variable is unused.
            .nonTargetOverride = function(lesion_data) {
                recist_nontarget_override(lesion_data, private$.recistContext())
            },
            .determineOverallResponse = function(target_sums, new_lesions, nontarget_assessment) {
                recist_overall_response(target_sums, new_lesions, nontarget_assessment,
                                        private$.recistContext())
            },
            .confirmResponses = function(visit_responses) {
                recist_confirm_responses(visit_responses, private$.recistContext())
            },
            .calculateBestOverallResponse = function(confirmed_responses) {
                recist_best_overall_response(confirmed_responses)
            },

            # Output Population ====

            .initLesionTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },
            .populateLesionTable = function(lesion_data) {
                table <- self$results$lesionTable

                # Calculate baseline diameters for percent change
                baseline_diameters <- lesion_data[lesion_data$isBaseline, c("patientID", "lesionID", "diameter")]
                names(baseline_diameters)[3] <- "baseline_diameter"

                # Merge baseline with all data
                lesion_data_full <- merge(lesion_data, baseline_diameters,
                    by = c("patientID", "lesionID"),
                    all.x = TRUE
                )

                # Calculate changes
                lesion_data_full$changeFromBaseline <- lesion_data_full$diameter - lesion_data_full$baseline_diameter
                lesion_data_full$percentChange <- ifelse(
                    lesion_data_full$baseline_diameter > 0,
                    (lesion_data_full$diameter - lesion_data_full$baseline_diameter) / lesion_data_full$baseline_diameter * 100,
                    NA
                )

                # Add visit numbers
                lesion_data_full <- lesion_data_full[order(
                    lesion_data_full$patientID,
                    lesion_data_full$lesionID,
                    lesion_data_full$visitTime
                ), ]
                lesion_data_full$visitNumber <- ave(
                    lesion_data_full$visitTime,
                    paste(lesion_data_full$patientID, lesion_data_full$lesionID),
                    FUN = seq_along
                )

                # Populate table
                for (i in seq_len(nrow(lesion_data_full))) {
                    row <- list(
                        patientID = lesion_data_full$patientID[i],
                        lesionID = lesion_data_full$lesionID[i],
                        visitTime = lesion_data_full$visitTime[i],
                        visitNumber = lesion_data_full$visitNumber[i],
                        lesionType = lesion_data_full$lesionType[i],
                        location = lesion_data_full$location[i],
                        diameter = lesion_data_full$diameter[i],
                        changeFromBaseline = lesion_data_full$changeFromBaseline[i],
                        percentChange = lesion_data_full$percentChange[i]
                    )
                    table$addRow(rowKey = i, values = row)
                }
            },
            .initTargetSumTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },
            .populateTargetSumTable = function(target_sums, confirmed_responses) {
                table <- self$results$targetSumTable

                # Merge confirmation status
                target_sums_full <- merge(
                    target_sums,
                    confirmed_responses[, c("patientID", "visitTime", "response_confirmed")],
                    by = c("patientID", "visitTime"),
                    all.x = TRUE
                )

                # Populate table
                for (i in seq_len(nrow(target_sums_full))) {
                    row <- list(
                        patientID = target_sums_full$patientID[i],
                        visitTime = target_sums_full$visitTime[i],
                        visitNumber = target_sums_full$visitNumber[i],
                        nTargetLesions = target_sums_full$nTargetLesions[i],
                        baselineSum = target_sums_full$baseline_sum[i],
                        currentSum = target_sums_full$current_sum[i],
                        absoluteChange = target_sums_full$absolute_change[i],
                        percentChange = target_sums_full$percent_change[i],
                        nadirSum = target_sums_full$nadir_sum[i],
                        percentChangeFromNadir = target_sums_full$percent_change_from_nadir[i],
                        targetResponse = target_sums_full$target_response[i],
                        responseConfirmed = ifelse(target_sums_full$response_confirmed[i], "Yes", "No")
                    )
                    table$addRow(rowKey = i, values = row)
                }
            },
            .initBestResponseTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },
            .populateBestResponseTable = function(best_responses) {
                table <- self$results$bestResponseTable

                for (i in seq_len(nrow(best_responses))) {
                    row <- best_responses[i, ]
                    table$addRow(rowKey = i, values = as.list(row))
                }
            },
            .populateRecistSummary = function(best_responses) {
                # Calculate ORR (Objective Response Rate: CR + PR)
                n_total <- nrow(best_responses)
                n_cr <- sum(best_responses$bestOverallResponse == "CR")
                n_pr <- sum(best_responses$bestOverallResponse == "PR")
                n_orr <- n_cr + n_pr
                orr_rate <- if (n_total > 0) n_orr / n_total * 100 else 0

                # 95% CI for binomial proportion (Wilson score interval)
                orr_ci <- if (n_total > 0) {
                    prop_test <- prop.test(n_orr, n_total, correct = FALSE)
                    paste0(
                        round(prop_test$conf.int[1] * 100, 1), "% - ",
                        round(prop_test$conf.int[2] * 100, 1), "%"
                    )
                } else {
                    "N/A"
                }

                # Populate ORR table
                orr_table <- self$results$recistSummary$orrConfirmed
                orr_table$addRow(rowKey = 1, values = list(
                    metric = "ORR (CR + PR)",
                    value = paste0(n_orr, "/", n_total, " (", round(orr_rate, 1), "%)"),
                    ci = orr_ci,
                    interpretation = if (orr_rate >= 20) "Promising activity" else "Limited activity"
                ))

                # Calculate DCR (Disease Control Rate: CR + PR + SD)
                n_sd <- sum(best_responses$bestOverallResponse == "SD")
                n_dcr <- n_cr + n_pr + n_sd
                dcr_rate <- if (n_total > 0) n_dcr / n_total * 100 else 0

                dcr_ci <- if (n_total > 0) {
                    prop_test <- prop.test(n_dcr, n_total, correct = FALSE)
                    paste0(
                        round(prop_test$conf.int[1] * 100, 1), "% - ",
                        round(prop_test$conf.int[2] * 100, 1), "%"
                    )
                } else {
                    "N/A"
                }

                # Populate DCR table
                dcr_table <- self$results$recistSummary$dcrConfirmed
                dcr_table$addRow(rowKey = 1, values = list(
                    metric = "DCR (CR + PR + SD)",
                    value = paste0(n_dcr, "/", n_total, " (", round(dcr_rate, 1), "%)"),
                    ci = dcr_ci,
                    interpretation = if (dcr_rate >= 50) "Good disease control" else "Poor disease control"
                ))

                # Response distribution
                dist_table <- self$results$recistSummary$responseDistribution
                response_categories <- c("CR", "PR", "SD", "PD", "Not Evaluable")

                for (i in seq_along(response_categories)) {
                    cat <- response_categories[i]
                    n_cat <- sum(best_responses$bestOverallResponse == cat)
                    pct_cat <- if (n_total > 0) n_cat / n_total * 100 else 0

                    dist_table$addRow(rowKey = i, values = list(
                        category = cat,
                        confirmed = n_cat,
                        confirmedPercent = pct_cat,
                        unconfirmed = 0, # Placeholder
                        unconfirmedPercent = 0
                    ))
                }
            },
            .initComplianceReport = function() {
                # HTML output is defined in .r.yaml
            },
            .populateComplianceReport = function(target_validation, new_lesions, confirmed_responses) {
                html_content <- "<div style='font-family: Arial, sans-serif;'>"

                # Section 1: Target Lesion Selection Compliance
                html_content <- paste0(
                    html_content,
                    "<h3>1. Target Lesion Selection Compliance</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Maximum ", self$options$maxTargetLesions,
                    " target lesions per patient</li>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Maximum ", self$options$maxLesionsPerOrgan,
                    " target lesions per organ</li>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Minimum diameter >=10mm (non-lymph nodes) or >=15mm (lymph nodes)</li>"
                )

                if (target_validation$valid) {
                    html_content <- paste0(
                        html_content,
                        "<li style='color: green;'><strong>STATUS:  COMPLIANT</strong> - All patients meet RECIST v1.1 target lesion criteria</li>"
                    )
                } else {
                    html_content <- paste0(
                        html_content,
                        "<li style='color: red;'><strong>STATUS:  NON-COMPLIANT</strong> - ",
                        length(target_validation$violations), " violation(s) detected:</li>",
                        "<ul style='color: red;'>"
                    )
                    for (violation in target_validation$violations) {
                        html_content <- paste0(html_content, "<li>", htmltools::htmlEscape(violation), "</li>")
                    }
                    html_content <- paste0(html_content, "</ul>")
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 2: New Lesion Detection
                html_content <- paste0(
                    html_content,
                    "<h3>2. New Lesion Detection</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> ANY new lesion automatically indicates Progressive Disease (PD)</li>"
                )

                if (nrow(new_lesions) == 0) {
                    html_content <- paste0(
                        html_content,
                        "<li style='color: green;'><strong>STATUS:</strong> No new lesions detected</li>"
                    )
                } else {
                    html_content <- paste0(
                        html_content,
                        "<li style='color: orange;'><strong>STATUS:</strong> ", nrow(new_lesions),
                        " patient(s) with new lesions detected (automatically classified as PD):</li>",
                        "<ul>"
                    )
                    for (i in seq_len(nrow(new_lesions))) {
                        html_content <- paste0(
                            html_content,
                            "<li>Patient ", htmltools::htmlEscape(new_lesions$patientID[i]),
                            " - New lesion at visit ", new_lesions$first_new_lesion_visit[i],
                            " (Location: ", htmltools::htmlEscape(new_lesions$new_lesion_location[i]), ")</li>"
                        )
                    }
                    html_content <- paste0(html_content, "</ul>")
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 3: Response Confirmation Summary
                html_content <- paste0(
                    html_content,
                    "<h3>3. Response Confirmation Summary</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> CR and PR must be confirmed by repeat assessment >=",
                    self$options$confirmationInterval, " weeks after initial documentation</li>"
                )

                cr_pr_responses <- confirmed_responses[
                    confirmed_responses$overall_response_unconfirmed %in% c("CR", "PR"),
                ]

                if (nrow(cr_pr_responses) > 0) {
                    n_confirmed <- sum(cr_pr_responses$response_confirmed)
                    n_unconfirmed <- sum(!cr_pr_responses$response_confirmed)

                    html_content <- paste0(
                        html_content,
                        "<li><strong>Confirmed CR/PR:</strong> ", n_confirmed, " assessment(s)</li>",
                        "<li><strong>Unconfirmed CR/PR:</strong> ", n_unconfirmed,
                        " assessment(s) (awaiting confirmation or lost)</li>"
                    )
                } else {
                    html_content <- paste0(
                        html_content,
                        "<li>No CR or PR responses documented</li>"
                    )
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 4: Overall Compliance Summary
                html_content <- paste0(
                    html_content,
                    "<h3>4. Overall RECIST v1.1 Compliance Summary</h3>",
                    "<p style='padding: 10px; background-color: #f0f0f0; border-left: 4px solid ",
                    ifelse(target_validation$valid, "green", "orange"), ";'>",
                    "<strong>Compliance Status:</strong> ",
                    ifelse(target_validation$valid,
                        "This analysis meets RECIST v1.1 protocol requirements and may be suitable for regulatory submissions.",
                        "This analysis contains protocol deviations. Review violations above and consider data quality implications."
                    ),
                    "</p>",
                    "</div>"
                )

                self$results$complianceReport$setContent(html_content)
            },

            # Plot Generation ====

            .initWaterfallPlot = function() {
                image <- self$results$waterfallPlot
                image$setSize(800, 600)
            },
            .prepareWaterfallPlot = function(best_responses, target_sums) {
                image <- self$results$waterfallPlot

                # Score post-baseline assessments only. Including the baseline row
                # (whose percent change is 0 by construction) means a patient whose
                # tumour only ever grew would plot a 0% bar instead of their smallest
                # recorded increase.
                plot_source <- target_sums
                if ("is_baseline_visit" %in% names(plot_source)) {
                    plot_source <- plot_source[!plot_source$is_baseline_visit, , drop = FALSE]
                }
                plot_source <- plot_source[!is.na(plot_source$percent_change), , drop = FALSE]

                # aggregate() on a zero-row frame raises "no rows to aggregate".
                if (nrow(plot_source) == 0 || nrow(best_responses) == 0) {
                    image$setState(NULL)
                    return()
                }

                # Calculate best percent change for each patient (nadir)
                nadir_data <- aggregate(
                    percent_change ~ patientID,
                    data = plot_source,
                    FUN = function(x) x[which.min(x)[1]] # Nadir: most negative (best) % change
                )
                names(nadir_data)[2] <- "best_change"

                # Merge with best response
                plot_data <- merge(nadir_data, best_responses, by = "patientID")

                # Sort by best change (most negative to most positive)
                plot_data <- plot_data[order(plot_data$best_change), ]
                plot_data$patient_order <- seq_len(nrow(plot_data))

                # Store for rendering
                state <- list(
                    plot_data = as.data.frame(plot_data),
                    color_scheme = self$options$colorScheme,
                    pr_threshold = private$RECIST_PR_THRESHOLD,
                    pd_threshold = private$RECIST_PD_THRESHOLD
                )

                image$setState(state)
            },
            .waterfallPlot = function(image, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                plot_data <- image$state$plot_data
                color_scheme <- image$state$color_scheme
                pr_threshold <- image$state$pr_threshold
                pd_threshold <- image$state$pd_threshold

                # Define colors
                if (color_scheme == "recist") {
                    colors <- c("CR" = "#00A087", "PR" = "#4DBBD5", "SD" = "#F39B7F", "PD" = "#E64B35", "Not Evaluable" = "#8491B4")
                } else if (color_scheme == "colorblind") {
                    colors <- c("CR" = "#009E73", "PR" = "#0072B2", "SD" = "#F0E442", "PD" = "#D55E00", "Not Evaluable" = "#999999")
                } else { # jamovi
                    colors <- c("CR" = "#3498DB", "PR" = "#2ECC71", "SD" = "#F39C12", "PD" = "#E74C3C", "Not Evaluable" = "#95A5A6")
                }

                # Create plot
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = patient_order, y = best_change, fill = bestOverallResponse)) +
                    ggplot2::geom_col(width = 0.8) +
                    ggplot2::geom_hline(yintercept = pr_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = pd_threshold, linetype = "dashed", color = "darkred", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                    ggplot2::scale_fill_manual(values = colors, name = "Best Overall Response") +
                    ggplot2::labs(
                        title = "RECIST v1.1 Waterfall Plot",
                        subtitle = "Best Confirmed Percent Change from Baseline",
                        x = "Patient (sorted by response)",
                        y = "Best % Change in Target Lesion Sum"
                    ) +
                    ggplot2::theme_minimal(base_size = 12) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    ) +
                    ggplot2::annotate("text",
                        x = nrow(plot_data) * 0.05, y = pr_threshold - 5,
                        label = "PR (-30%)", hjust = 0, color = "darkgreen", size = 3
                    ) +
                    ggplot2::annotate("text",
                        x = nrow(plot_data) * 0.05, y = pd_threshold + 5,
                        label = "PD (+20%)", hjust = 0, color = "darkred", size = 3
                    )

                print(plot)
                return(TRUE)
            },
            .initSpiderPlot = function() {
                image <- self$results$spiderPlot
                image$setSize(800, 600)
            },
            .prepareSpiderPlot = function(target_sums, best_responses) {
                image <- self$results$spiderPlot

                # Merge best response for coloring
                plot_data <- merge(target_sums, best_responses[, c("patientID", "bestOverallResponse")],
                    by = "patientID"
                )

                # Store for rendering
                state <- list(
                    plot_data = as.data.frame(plot_data),
                    color_scheme = self$options$colorScheme,
                    pr_threshold = private$RECIST_PR_THRESHOLD,
                    pd_threshold = private$RECIST_PD_THRESHOLD
                )

                image$setState(state)
            },
            .spiderPlot = function(image, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                plot_data <- image$state$plot_data
                color_scheme <- image$state$color_scheme
                pr_threshold <- image$state$pr_threshold
                pd_threshold <- image$state$pd_threshold

                # Define colors
                if (color_scheme == "recist") {
                    colors <- c("CR" = "#00A087", "PR" = "#4DBBD5", "SD" = "#F39B7F", "PD" = "#E64B35", "Not Evaluable" = "#8491B4")
                } else if (color_scheme == "colorblind") {
                    colors <- c("CR" = "#009E73", "PR" = "#0072B2", "SD" = "#F0E442", "PD" = "#D55E00", "Not Evaluable" = "#999999")
                } else { # jamovi
                    colors <- c("CR" = "#3498DB", "PR" = "#2ECC71", "SD" = "#F39C12", "PD" = "#E74C3C", "Not Evaluable" = "#95A5A6")
                }

                # Create plot
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
                    x = visitTime, y = percent_change,
                    group = patientID, color = bestOverallResponse
                )) +
                    ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = pr_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = pd_threshold, linetype = "dashed", color = "darkred", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                    ggplot2::scale_color_manual(values = colors, name = "Best Overall Response") +
                    ggplot2::labs(
                        title = "RECIST v1.1 Spider Plot",
                        subtitle = "Target Lesion Sum Trajectories Over Time",
                        x = "Time from Baseline",
                        y = "% Change in Target Lesion Sum"
                    ) +
                    ggplot2::theme_minimal(base_size = 12) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        panel.grid.minor = ggplot2::element_blank()
                    ) +
                    ggplot2::annotate("text",
                        x = max(plot_data$visitTime) * 0.95, y = pr_threshold - 5,
                        label = "PR (-30%)", hjust = 1, color = "darkgreen", size = 3
                    ) +
                    ggplot2::annotate("text",
                        x = max(plot_data$visitTime) * 0.95, y = pd_threshold + 5,
                        label = "PD (+20%)", hjust = 1, color = "darkred", size = 3
                    )

                print(plot)
                return(TRUE)
            },

            # Utility Methods ====

            .addBestResponseColumn = function(best_responses) {
                # TODO: Add BOR as new variable to original dataset
                # NOTE: Merge best_responses back to patient-level
                # New column: "RECIST_BestOverallResponse"
            },
            .resolveVar = function(name) {
                # self$data uses ORIGINAL (raw) variable names in both the R wrapper
                # and the jamovi GUI, so the raw option value is the correct data[[]]
                # key. Fall back to a base64-encoded name only for legacy runtimes
                # that may hand back encoded columns. (jmvcore::toB64() applied
                # unconditionally never matches raw column names, so every lookup
                # returned NULL and validation always reported "Variable Not Found".)
                if (is.null(name) || length(name) == 0 || identical(name, "")) {
                    return(name)
                }
                nm <- names(self$data)
                if (name %in% nm) {
                    return(name)
                }
                b64 <- jmvcore::toB64(name)
                if (b64 %in% nm) {
                    return(b64)
                }
                return(name)
            },
            .escapeVar = function(x) {
                # Escape variable names with spaces/special characters for safe R usage
                if (is.null(x) || length(x) == 0 || x == "") {
                    return(x)
                }
                safe_name <- make.names(x)
                safe_name <- gsub("[^A-Za-z0-9_]+", "_", safe_name)
                return(safe_name)
            }
        )
    )
}
