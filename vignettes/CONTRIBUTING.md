# Contributing to ClinicoPathJamoviModule

We welcome contributions to the ClinicoPathJamoviModule!

## How to Contribute

There are several ways you can contribute to this project:

*   **Reporting Bugs:** If you find a bug, please open an issue on our [GitHub issue tracker](https://github.com/sbalci/ClinicoPathJamoviModule/issues). Please provide as much detail as possible, including the version of the module you are using, the steps to reproduce the bug, and any error messages you receive.
*   **Suggesting Enhancements:** If you have an idea for a new feature or an improvement to an existing feature, please open an issue on our [GitHub issue tracker](https://github.com/sbalci/ClinicoPathJamoviModule/issues).
*   **Pull Requests:** If you would like to contribute code to the project, please submit a pull request. Before submitting a pull request, please make sure that your code adheres to the project's coding standards and that all tests pass.

## Development Setup

To get started with developing the ClinicoPathJamoviModule, you will need to have R and the `devtools` package installed. You can then clone the repository and install the package in development mode:

```R
# Install devtools if you don't have it already
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

# Clone the repository
# git clone https://github.com/sbalci/ClinicoPathJamoviModule.git

# Install the package in development mode
devtools::load_all()
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). By participating in this project you agree to abide by its terms.
