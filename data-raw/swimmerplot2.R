# Create long-format dataset for swimmerplot2
swimmer_data <- data.frame(
    # Patient identifier
    patient_id = c(
        # PT001 timeline
        "PT001", "PT001", "PT001", "PT001",
        # PT002 timeline
        "PT002", "PT002", "PT002",
        # PT003 timeline
        "PT003", "PT003", "PT003", "PT003",
        # PT004 timeline
        "PT004", "PT004",
        # PT005 timeline
        "PT005", "PT005", "PT005",
        # PT006 timeline
        "PT006", "PT006",
        # PT007 timeline
        "PT007", "PT007", "PT007",
        # PT008 timeline
        "PT008", "PT008", "PT008",
        # PT009 timeline
        "PT009", "PT009",
        # PT010 timeline
        "PT010", "PT010", "PT010"
    ),

    # Event timing
    start_time = c(
        # PT001 events
        0, 3, 8, 12,
        # PT002 events
        0, 6, 10,
        # PT003 events
        0, 5, 9, 14,
        # PT004 events
        0, 7,
        # PT005 events
        0, 3, 10,
        # PT006 events
        0, 9,
        # PT007 events
        0, 3, 6,
        # PT008 events
        0, 4, 11,
        # PT009 events
        0, 9,
        # PT010 events
        0, 3, 12
    ),

    # Event types - these become markers
    event_type = c(
        # PT001 events
        "Treatment Start", "Dose Reduction", "Treatment Restart", "Follow-up",
        # PT002 events
        "Treatment Start", "Dose Reduction", "Follow-up",
        # PT003 events
        "Treatment Start", "Adverse Event", "Dose Reduction", "Follow-up",
        # PT004 events
        "Treatment Start", "Disease Progression",
        # PT005 events
        "Treatment Start", "Adverse Event", "Follow-up",
        # PT006 events
        "Treatment Start", "Disease Progression",
        # PT007 events
        "Treatment Start", "Dose Reduction", "Follow-up",
        # PT008 events
        "Treatment Start", "Adverse Event", "Follow-up",
        # PT009 events
        "Treatment Start", "Disease Progression",
        # PT010 events
        "Treatment Start", "Treatment Restart", "Follow-up"
    ),

    # End time for each lane
    end_time = c(
        # PT001 lanes
        3, 8, 12, 14,
        # PT002 lanes
        6, 10, 12,
        # PT003 lanes
        5, 9, 14, 16,
        # PT004 lanes
        7, 9,
        # PT005 lanes
        3, 10, 12,
        # PT006 lanes
        9, 11,
        # PT007 lanes
        3, 6, 9,
        # PT008 lanes
        4, 11, 14,
        # PT009 lanes
        9, 10,
        # PT010 lanes
        3, 12, 15
    ),

    # Response status for patient during this period
    response_status = c(
        # PT001 status
        "PR", "CR", "CR", "CR",
        # PT002 status
        "SD", "PR", "PR",
        # PT003 status
        "SD", "PR", "CR", "CR",
        # PT004 status
        "PD", "PD",
        # PT005 status
        "PR", "PR", "CR",
        # PT006 status
        "SD", "PD",
        # PT007 status
        "SD", "PR", "PR",
        # PT008 status
        "PR", "PR", "CR",
        # PT009 status
        "SD", "PD",
        # PT010 status
        "PR", "CR", "CR"
    ),

    # Whether patient is still on study at the final timepoint
    on_study = c(
        # PT001 status
        FALSE, FALSE, FALSE, TRUE,
        # PT002 status
        FALSE, FALSE, TRUE,
        # PT003 status
        FALSE, FALSE, FALSE, TRUE,
        # PT004 status
        FALSE, FALSE,
        # PT005 status
        FALSE, FALSE, TRUE,
        # PT006 status
        FALSE, FALSE,
        # PT007 status
        FALSE, FALSE, TRUE,
        # PT008 status
        FALSE, FALSE, TRUE,
        # PT009 status
        FALSE, FALSE,
        # PT010 status
        FALSE, FALSE, TRUE
    ),

    # Marker flag - whether this row should also create a marker
    is_marker = c(
        # PT001 markers (all events get markers)
        TRUE, TRUE, TRUE, TRUE,
        # PT002 markers
        TRUE, TRUE, TRUE,
        # PT003 markers
        TRUE, TRUE, TRUE, TRUE,
        # PT004 markers
        TRUE, TRUE,
        # PT005 markers
        TRUE, TRUE, TRUE,
        # PT006 markers
        TRUE, TRUE,
        # PT007 markers
        TRUE, TRUE, TRUE,
        # PT008 markers
        TRUE, TRUE, TRUE,
        # PT009 markers
        TRUE, TRUE,
        # PT010 markers
        TRUE, TRUE, TRUE
    )
)

write.csv(swimmer_data, "./data/swimmer_data.csv", row.names = FALSE)
