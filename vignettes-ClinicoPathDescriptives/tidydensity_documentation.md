# Statistical Distribution Generator Analysis Documentation

This document provides a comprehensive overview of the Statistical Distribution Generator module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Statistical Distribution Generator module, powered by the `TidyDensity` package, allows users to generate random data from various statistical distributions. It provides comprehensive visualization and analysis capabilities, including density plots, quantile plots, probability plots, and Q-Q plots. This tool is ideal for simulation studies, power analysis, distribution comparison, and statistical education in clinical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Distribution Generation**      |                                |                                        |                                     |                                      |
| Distribution Type                | `distribution_type`            | Distribution Type                      | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info`, `interpretation` | `.run`, `.generate_distribution_data`, `.generate_parameter_info`, `.generate_interpretation_guide` |
| Number of Observations           | `n_observations`               | Number of Observations                 | `main_plot`, `distribution_statistics`, `summary_table` | `.run`, `.generate_distribution_data` |
| Number of Simulations            | `n_simulations`                | Number of Simulations                  | `main_plot`, `distribution_statistics`, `summary_table` | `.run`, `.generate_distribution_data` |
| **Distribution Parameters**      |                                |                                        |                                     |                                      |
| Normal Mean (μ)                  | `normal_mean`                  | Mean (μ)                               | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Normal Standard Deviation (σ)    | `normal_sd`                    | Standard Deviation (σ)                 | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Beta Shape 1 (α)                 | `beta_shape1`                  | Shape 1 (α)                            | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Beta Shape 2 (β)                 | `beta_shape2`                  | Shape 2 (β)                            | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Gamma Shape (k)                  | `gamma_shape`                  | Shape (k)                              | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Gamma Scale (θ)                  | `gamma_scale`                  | Scale (θ)                              | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Exponential Rate (λ)             | `exponential_rate`             | Rate (λ)                               | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Uniform Minimum                  | `uniform_min`                  | Minimum                                | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Uniform Maximum                  | `uniform_max`                  | Maximum                                | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Chi-Square Degrees of Freedom    | `chisq_df`                     | Degrees of Freedom                     | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Student's t Degrees of Freedom   | `t_df`                         | Degrees of Freedom                     | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| F Distribution Numerator DF      | `f_df1`                        | Numerator DF                           | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| F Distribution Denominator DF    | `f_df2`                        | Denominator DF                         | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Binomial Number of Trials (n)    | `binomial_size`                | Number of Trials (n)                   | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Binomial Success Probability (p) | `binomial_prob`                | Success Probability (p)                | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Poisson Rate (λ)                 | `poisson_lambda`               | Rate (λ)                               | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Weibull Shape (k)                | `weibull_shape`                | Shape (k)                              | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Weibull Scale (λ)                | `weibull_scale`                | Scale (λ)                              | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Log-Normal Mean of Log           | `lognormal_meanlog`            | Mean of Log                            | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Log-Normal SD of Log             | `lognormal_sdlog`              | SD of Log                              | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Logistic Location                | `logistic_location`            | Location                               | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Logistic Scale                   | `logistic_scale`               | Scale                                  | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Cauchy Location                  | `cauchy_location`              | Location                               | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| Cauchy Scale                     | `cauchy_scale`                 | Scale                                  | `main_plot`, `distribution_statistics`, `summary_table`, `parameter_info` | `.generate_distribution_data`, `.generate_parameter_info` |
| **Visualization**                |                                |                                        |                                     |                                      |
| Plot Type                        | `plot_type`                    | Plot Type                              | `main_plot`                         | `.plot`                              |
| Plot Enhancements                | `plot_enhancements`            | Plot Enhancements                      | `main_plot`                         | `.plot`                              |
| Use Economist Theme              | `economist_theme`              | Use Economist Theme (for Economist plots)| `main_plot`                         | Not implemented in `.b.R`            |
| Use Economist Colors             | `economist_colors`             | Use Economist Colors (for Economist plots)| `main_plot`                         | Not implemented in `.b.R`            |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Show Distribution Statistics     | `show_statistics`              | Show Distribution Statistics           | `distribution_statistics`           | `.generate_distribution_statistics`  |
| Show Summary Table               | `show_summary_table`           | Show Summary Table                     | `summary_table`                     | `.generate_summary_table`            |
| Show Parameter Information       | `show_parameter_info`          | Show Parameter Information             | `parameter_info`                    | `.generate_parameter_info`           |
| Show Usage Guide                 | `show_interpretation`          | Show Usage Guide                       | `interpretation`                    | `.generate_interpretation_guide`     |
