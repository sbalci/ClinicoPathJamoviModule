set.seed(123)
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(123)

#------------------------------------------------------------------------------
# Function to simulate no‐gold‐standard test data
#------------------------------------------------------------------------------
simulate_nogold_data <- function( n           = 200,
                                  prevalence  = 0.3,
                                  sens        = c(0.80, 0.75, 0.85),
                                  spec        = c(0.90, 0.85, 0.80) ){

    if( length(sens) != length(spec) )
        stop("`sens` and `spec` must have the same length")

    K       <- length(sens)
    disease <- rbinom(n, 1, prevalence)

    # build data.frame with latent status (for evaluation only)
    df <- data.frame(
        caseID  = seq_len(n),
        disease = factor(disease, levels = 0:1,
                         labels = c("healthy","disease"))
    )

    # simulate each test
    for(i in seq_len(K)) {
        pi <- sens[i]   # sensitivity
        qi <- spec[i]   # specificity

        # P(test = positive) =
        #   if disease: Bernoulli(pi)
        #   else:        Bernoulli(1 - qi)
        positive <- ifelse(disease == 1,
                           rbinom(n, 1, pi),
                           rbinom(n, 1, 1-qi))

        df[[ paste0("test", i) ]] <-
            factor(positive,
                   levels = 0:1,
                   labels = c("neg","pos"))
    }

    df
}

#------------------------------------------------------------------------------
# Generate a toy data set and peek at the first few rows
#------------------------------------------------------------------------------
df <- simulate_nogold_data(
    n          = 200,
    prevalence = 0.25,
    sens       = c(0.85, 0.80, 0.75, 0.90),
    spec       = c(0.90, 0.88, 0.85, 0.92)
)

head(df)

readr::write_csv(
    df,
    file = "./data/nogold_standard.csv",
    na = ""
)
