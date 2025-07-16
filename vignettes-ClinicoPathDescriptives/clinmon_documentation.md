# Clinical Hemodynamic Monitoring Documentation

This document provides a comprehensive overview of the Clinical Hemodynamic Monitoring module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Clinical Hemodynamic Monitoring module is designed to calculate various hemodynamic indices from continuous physiological monitoring data. It utilizes the `clintools` R package to derive metrics such as estimated cardiac output (COest), optimal cerebral perfusion pressure (CPPopt), pressure reactivity index (PRx), and various flow indices (Mx, Dx, Sx, CVRi, PI, RI, PWA) from inputs like arterial blood pressure (ABP), intracranial pressure (ICP), middle cerebral artery velocity (MCAv), cerebral perfusion pressure (CPP), and heart rate (HR).

The module's features can be broadly categorized as follows:

*   **Hemodynamic Index Calculation:** Computes a wide array of indices relevant to neurocritical care and cardiovascular monitoring.
*   **Flexible Input:** Supports various physiological variables as input, allowing for comprehensive analysis based on available data.
*   **Customizable Processing Parameters:** Users can define parameters such as sampling frequency, block size, epoch size, and output level to tailor the analysis to specific data characteristics.
*   **Output Granularity:** Provides options for different levels of output (period, epoch, block, CPPopt) and detailed or summarized results.
*   **Efficiency Options:** Includes a fast processing option for quicker computation on large datasets.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Variables**              |                                |                                        |                                     |                                      |
| Time Variable                    | `time_var`                     | Time Variable                          | (Input)                             | `.run`                               |
| Arterial Blood Pressure          | `abp`                          | ABP                                    | (Input)                             | `.run`                               |
| MCA Blood Velocity               | `mcav`                         | MCAv                                   | (Input)                             | `.run`                               |
| Intracranial Pressure            | `icp`                          | ICP                                    | (Input)                             | `.run`                               |
| Cerebral Perfusion Pressure      | `cpp`                          | CPP                                    | (Input)                             | `.run`                               |
| Heart Rate                       | `hr`                           | HR                                     | (Input)                             | `.run`                               |
| **Processing Parameters**        |                                |                                        |                                     |                                      |
| Frequency                        | `freq`                         | Frequency (Hz)                         | (Parameter)                         | `.run`                               |
| Block Size                       | `blocksize`                    | Block Size (seconds)                   | (Parameter)                         | `.run`                               |
| Epoch Size                       | `epochsize`                    | Epoch Size (blocks)                    | (Parameter)                         | `.run`                               |
| Output Level                     | `output_level`                 | Output Level                           | (Parameter)                         | `.run`                               |
| Overlapping Calculation          | `overlapping`                  | Overlapping Calculation                | (Parameter)                         | `.run`                               |
| Block Minimum                    | `blockmin`                     | Block Minimum (%)                      | (Parameter)                         | `.run`                               |
| Epoch Minimum                    | `epochmin`                     | Epoch Minimum (%)                      | (Parameter)                         | `.run`                               |
| Fast Processing                  | `fast_processing`              | Fast Processing                        | (Parameter)                         | `.run`                               |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Summary Statistics          | `show_summary`                 | Show Summary Statistics                | `summary_stats`                     | `.generateSummary`                   |
| Show Detailed Results            | `show_detailed`                | Show Detailed Results                  | `detailed_results`                  | `.generateDetailedResults`           |
| Instructions                     | (N/A)                          | Instructions                           | `instructions`                      | `.init`, `.run`                      |
