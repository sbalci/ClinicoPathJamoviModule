#' Generate Test Data for Swimmer Plot Testing
#'
#' @param n_patients Number of patients to generate in the dataset
#' @param seed Random seed for reproducibility
#' @param missing_prop Proportion of data that should be missing (0-1)
#' @param date_format Whether to return dates as Date objects or raw numeric values
#' @param start_date Base start date for the study (if date_format = "date")
#' @param staggered_entry Whether to simulate patients entering the study at different times
#'
#' @return A data frame containing simulated patient timeline data
#' @export
#'
#' @examples
#' # Generate data for 20 patients with 10% missing data
#' test_data <- generate_swimmerplot_data(n_patients = 20, missing_prop = 0.1)
#'
#' # Generate data with dates instead of numeric values
#' test_data_dates <- generate_swimmerplot_data(n_patients = 15, date_format = "date")
#'
#' # Generate data with staggered entry times
#' test_data_staggered <- generate_swimmerplot_data(n_patients = 25, staggered_entry = TRUE)
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

generate_swimmerplot_data <- function(
    n_patients = 30,
    seed = 123,
    missing_prop = 0.15,
    date_format = c("numeric", "date"),
    start_date = as.Date("2020-01-01"),
    staggered_entry = TRUE
) {
  # Set seed for reproducibility
  set.seed(seed)

  # Process input arguments
  date_format <- match.arg(date_format)

  # Create patient IDs with a standard format (e.g., PT001, PT002, etc.)
  patient_ids <- sprintf("PT%03d", 1:n_patients)

  # Define possible response types with clinical relevance
  response_types <- c("CR", "PR", "SD", "PD", "NE")
  response_probs <- c(0.15, 0.25, 0.30, 0.25, 0.05)  # Realistic distribution

  # Define patient types to simulate different clinical scenarios
  patient_types <- c(
    "standard",             # Standard treatment course
    "early_responder",      # Quick response to treatment
    "late_responder",       # Delayed response to treatment
    "non_responder",        # No response to treatment
    "early_progression",    # Early progression
    "treatment_gap",        # Had a gap in treatment
    "lost_followup",        # Lost to follow-up
    "death",                # Died during follow-up
    "ongoing"               # Still on treatment
  )

  # Assign patient types with a realistic distribution
  patient_type_probs <- c(0.25, 0.10, 0.10, 0.15, 0.10, 0.10, 0.05, 0.10, 0.05)
  assigned_types <- sample(patient_types, n_patients, replace = TRUE, prob = patient_type_probs)

  # Initialize the data frame
  data <- data.frame(
    PatientID = patient_ids,
    PatientType = assigned_types,  # For reference, can be removed in final dataset
    stringsAsFactors = FALSE
  )

  # Generate start times - allow for staggered entry
  if (date_format == "numeric") {
    if (staggered_entry) {
      # Generate start times between 0 and 12 (months/units)
      # With more early enrollees (skewed distribution)
      data$StartTime <- round(rbeta(n_patients, 1, 3) * 12, 1)
    } else {
      # Traditional approach - all patients start at time 0
      data$StartTime <- 0
    }
  } else {
    # For date format, create realistic enrollment dates over a 6-month period
    if (staggered_entry) {
      # Realistic enrollment curve - faster at beginning, slower later
      enrollment_days <- round(rbeta(n_patients, 1, 3) * 180)
    } else {
      # More uniform enrollment across the period
      enrollment_days <- sample(0:180, n_patients, replace = TRUE)
    }
    data$StartDate <- start_date + enrollment_days
  }

  # Generate durations based on patient type
  generate_duration <- function(type) {
    base_duration <- switch(type,
                            "standard" = runif(1, 10, 24),
                            "early_responder" = runif(1, 6, 12),
                            "late_responder" = runif(1, 18, 36),
                            "non_responder" = runif(1, 2, 8),
                            "early_progression" = runif(1, 1, 4),
                            "treatment_gap" = runif(1, 8, 20),
                            "lost_followup" = runif(1, 3, 12),
                            "death" = runif(1, 2, 18),
                            "ongoing" = runif(1, 24, 48),
                            runif(1, 6, 24)  # Default
    )

    # Add some random variation
    return(round(base_duration * runif(1, 0.9, 1.1)))
  }

  # Generate durations and end times
  durations <- sapply(data$PatientType, generate_duration)

  if (date_format == "numeric") {
    # For numeric format, add duration to the start time
    data$EndTime <- data$StartTime + durations
  } else {
    # For date format, add duration in days
    data$EndDate <- data$StartDate + (durations * 30)  # Convert to approximate days
  }

  # Generate response data based on patient type
  generate_response <- function(type) {
    response_by_type <- switch(type,
                               "standard" = sample(c("PR", "SD"), 1),
                               "early_responder" = "CR",
                               "late_responder" = sample(c("CR", "PR"), 1),
                               "non_responder" = sample(c("SD", "PD"), 1),
                               "early_progression" = "PD",
                               "treatment_gap" = sample(c("PR", "SD"), 1),
                               "lost_followup" = sample(c("NE", "SD"), 1),
                               "death" = sample(c("PD", "NE"), 1, prob = c(0.8, 0.2)),
                               "ongoing" = sample(c("CR", "PR", "SD"), 1, prob = c(0.3, 0.4, 0.3)),
                               sample(response_types, 1, prob = response_probs)  # Default
    )
    return(response_by_type)
  }

  data$BestResponse <- sapply(data$PatientType, generate_response)

  # Generate milestone times
  generate_milestone_times <- function(start_time, duration, type, format) {
    # Define milestone timing ranges as proportions of total duration
    surgery_range <- c(-0.1, 0.1)  # Can occur before or after enrollment
    treatment_start_range <- c(0.05, 0.15)  # Near the beginning
    response_range <- c(0.3, 0.5)  # Around the middle
    progression_range <- c(0.7, 0.9)  # Later part
    death_range <- c(0.9, 1.1)  # At or after the end time

    # Adjust ranges based on patient type
    if (type == "early_responder") {
      response_range <- c(0.1, 0.3)
    } else if (type == "late_responder") {
      response_range <- c(0.5, 0.7)
    } else if (type == "non_responder" || type == "early_progression") {
      progression_range <- c(0.3, 0.5)
    } else if (type == "treatment_gap") {
      # For treatment gap, the surgery might be delayed
      surgery_range <- c(0.1, 0.2)
    }

    # Generate times - anchored to the individual patient's start time
    surgery_time <- start_time + duration * runif(1, surgery_range[1], surgery_range[2])
    treatment_time <- start_time + duration * runif(1, treatment_start_range[1], treatment_start_range[2])
    response_time <- start_time + duration * runif(1, response_range[1], response_range[2])

    # Progression and death depend on patient type
    if (type %in% c("early_responder", "ongoing") && runif(1) < 0.7) {
      # Many early responders won't progress
      progression_time <- NA
      death_time <- NA
    } else if (type == "death") {
      progression_time <- start_time + duration * runif(1, progression_range[1], progression_range[2])
      death_time <- start_time + duration * runif(1, 0.95, 1.05)  # Death near the end time
    } else if (type == "lost_followup") {
      # Lost to follow-up may or may not have progression
      progression_time <- if (runif(1) < 0.5) start_time + duration * runif(1, 0.3, 0.6) else NA
      death_time <- NA  # No death recorded
    } else {
      progression_time <- start_time + duration * runif(1, progression_range[1], progression_range[2])
      # Only some patients die during the observation period
      death_time <- if (runif(1) < 0.3) start_time + duration * runif(1, death_range[1], death_range[2]) else NA
    }

    # Handle surgery potentially occurring before study start (for realistic scenarios)
    if (format == "numeric" && surgery_time < 0) {
      # Keep negative values for numeric format - surgery before enrollment
      surgery_time <- max(surgery_time, -6)  # Limit to 6 months before
    }

    # Round times if they're numeric
    if (format == "numeric") {
      surgery_time <- round(surgery_time, 1)
      treatment_time <- round(treatment_time, 1)
      response_time <- round(response_time, 1)
      progression_time <- if (!is.na(progression_time)) round(progression_time, 1) else NA
      death_time <- if (!is.na(death_time)) round(death_time, 1) else NA
    } else {
      # Dates are already handled above
    }

    return(list(
      Surgery = surgery_time,
      TreatmentStart = treatment_time,
      ResponseAssessment = response_time,
      Progression = progression_time,
      Death = death_time
    ))
  }

  # Generate milestone times for each patient
  milestone_data <- lapply(1:n_patients, function(i) {
    if (date_format == "numeric") {
      start <- data$StartTime[i]
      duration <- data$EndTime[i] - data$StartTime[i]  # Duration is relative to start
    } else {
      # Convert dates to numeric for calculation, then back to dates
      start <- 0
      duration <- as.numeric(data$EndDate[i] - data$StartDate[i])
    }

    milestones <- generate_milestone_times(start, duration, data$PatientType[i], date_format)

    if (date_format == "date") {
      # Convert numeric days back to actual dates
      # Allow for some events to happen before study start (especially surgery)
      surgery_days <- milestones$Surgery
      surgery_date <- data$StartDate[i] + surgery_days

      milestones$Surgery <- surgery_date
      milestones$TreatmentStart <- data$StartDate[i] + milestones$TreatmentStart
      milestones$ResponseAssessment <- data$StartDate[i] + milestones$ResponseAssessment
      milestones$Progression <- if (!is.na(milestones$Progression)) data$StartDate[i] + milestones$Progression else NA
      milestones$Death <- if (!is.na(milestones$Death)) data$StartDate[i] + milestones$Death else NA
    }

    return(milestones)
  })

  # Add milestone data to the main data frame
  data$Surgery <- sapply(milestone_data, function(x) x$Surgery)
  data$TreatmentStart <- sapply(milestone_data, function(x) x$TreatmentStart)
  data$ResponseAssessment <- sapply(milestone_data, function(x) x$ResponseAssessment)
  data$Progression <- sapply(milestone_data, function(x) x$Progression)
  data$Death <- sapply(milestone_data, function(x) x$Death)

  # Introduce missing data
  if (missing_prop > 0) {
    n_cells <- nrow(data) * (ncol(data) - 2)  # Exclude PatientID and PatientType from missing data
    n_missing <- round(n_cells * missing_prop)

    # Determine which cells to set as missing
    cols_for_missing <- 3:ncol(data)
    missing_indices <- sample(n_cells, n_missing)

    for (idx in missing_indices) {
      row_idx <- (idx - 1) %/% length(cols_for_missing) + 1
      col_idx <- (idx - 1) %% length(cols_for_missing) + cols_for_missing[1]

      # Don't make StartTime/StartDate or EndTime/EndDate missing
      if ((date_format == "numeric" && !colnames(data)[col_idx] %in% c("StartTime", "EndTime")) ||
          (date_format == "date" && !colnames(data)[col_idx] %in% c("StartDate", "EndDate"))) {
        data[row_idx, col_idx] <- NA
      }
    }
  }

  # Add additional variables that can be used for sorting or analysis
  data$Risk <- sample(c("High", "Medium", "Low"), n_patients, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  data$Age <- round(rnorm(n_patients, mean = 62, sd = 12))
  data$ECOG <- sample(0:3, n_patients, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))

  # Add a numeric variable for response duration
  if (date_format == "numeric") {
    data$ResponseDuration <- ifelse(!is.na(data$ResponseAssessment) & !is.na(data$Progression),
                                    data$Progression - data$ResponseAssessment,
                                    NA)
  } else {
    data$ResponseDuration <- ifelse(!is.na(data$ResponseAssessment) & !is.na(data$Progression),
                                    as.numeric(data$Progression - data$ResponseAssessment),
                                    NA)
  }

  # Calculate total follow-up duration (for each patient)
  if (date_format == "numeric") {
    # For numeric, actual observation time
    data$FollowUpDuration <- data$EndTime - data$StartTime
  } else {
    # For dates, convert to days
    data$FollowUpDuration <- as.numeric(data$EndDate - data$StartDate)
  }

  # Drop PatientType column as it was just for generation purposes
  data$PatientType <- NULL

  return(data)
}
