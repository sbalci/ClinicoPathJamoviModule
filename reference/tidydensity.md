# Statistical Distribution Generator

Statistical distribution generator and analyzer using TidyDensity
package. Generate random data from various statistical distributions
with comprehensive visualization and analysis capabilities. Create
density plots, quantile plots, probability plots, and Q-Q plots. Perfect
for simulation studies, power analysis, distribution comparison, and
statistical education in clinical research.

## Usage

``` r
tidydensity(
  data,
  distribution_type = "normal",
  n_observations = 100,
  n_simulations = 1,
  normal_mean = 0,
  normal_sd = 1,
  beta_shape1 = 2,
  beta_shape2 = 5,
  gamma_shape = 2,
  gamma_scale = 1,
  exponential_rate = 1,
  uniform_min = 0,
  uniform_max = 1,
  chisq_df = 3,
  t_df = 5,
  f_df1 = 5,
  f_df2 = 10,
  binomial_size = 10,
  binomial_prob = 0.5,
  poisson_lambda = 3,
  weibull_shape = 2,
  weibull_scale = 1,
  lognormal_meanlog = 0,
  lognormal_sdlog = 1,
  logistic_location = 0,
  logistic_scale = 1,
  cauchy_location = 0,
  cauchy_scale = 1,
  plot_type = "density",
  plot_enhancements = FALSE,
  show_statistics = TRUE,
  show_summary_table = TRUE,
  show_parameter_info = TRUE,
  show_interpretation = TRUE,
  economist_theme = TRUE,
  economist_colors = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- distribution_type:

  Type of statistical distribution to generate. Each distribution has
  specific parameters that can be configured.

- n_observations:

  Number of random observations to generate from the selected
  distribution.

- n_simulations:

  Number of simulation runs. Multiple simulations allow comparison of
  sampling variability.

- normal_mean:

  Mean parameter for Normal distribution.

- normal_sd:

  Standard deviation parameter for Normal distribution.

- beta_shape1:

  First shape parameter for Beta distribution.

- beta_shape2:

  Second shape parameter for Beta distribution.

- gamma_shape:

  Shape parameter for Gamma distribution.

- gamma_scale:

  Scale parameter for Gamma distribution.

- exponential_rate:

  Rate parameter for Exponential distribution.

- uniform_min:

  Minimum value for Uniform distribution.

- uniform_max:

  Maximum value for Uniform distribution.

- chisq_df:

  Degrees of freedom for Chi-Square distribution.

- t_df:

  Degrees of freedom for Student's t distribution.

- f_df1:

  Numerator degrees of freedom for F distribution.

- f_df2:

  Denominator degrees of freedom for F distribution.

- binomial_size:

  Number of trials for Binomial distribution.

- binomial_prob:

  Probability of success for Binomial distribution.

- poisson_lambda:

  Rate parameter for Poisson distribution.

- weibull_shape:

  Shape parameter for Weibull distribution.

- weibull_scale:

  Scale parameter for Weibull distribution.

- lognormal_meanlog:

  Mean of the logarithm for Log-Normal distribution.

- lognormal_sdlog:

  Standard deviation of the logarithm for Log-Normal distribution.

- logistic_location:

  Location parameter for Logistic distribution.

- logistic_scale:

  Scale parameter for Logistic distribution.

- cauchy_location:

  Location parameter for Cauchy distribution.

- cauchy_scale:

  Scale parameter for Cauchy distribution.

- plot_type:

  Type of visualization to generate for the distribution data.

- plot_enhancements:

  Add enhanced plot features like points, rug plots, and smooth lines.

- show_statistics:

  Display comprehensive statistics for the generated distribution.

- show_summary_table:

  Display summary table of generated data with key statistics.

- show_parameter_info:

  Display information about distribution parameters and their effects.

- show_interpretation:

  Display guidance on statistical distributions and their applications
  in clinical research.

- economist_theme:

  Apply The Economist's visual theme when using Economist-style plots.

- economist_colors:

  Apply The Economist's signature color scheme for distribution
  elements.

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                    |     |     |     |     | a html   |
| `results$main_plot`               |     |     |     |     | an image |
| `results$distribution_statistics` |     |     |     |     | a html   |
| `results$summary_table`           |     |     |     |     | a html   |
| `results$parameter_info`          |     |     |     |     | a html   |
| `results$interpretation`          |     |     |     |     | a html   |
