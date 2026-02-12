# Working on this package

Use the following workflow to contribute to this package.

1. Fork this repository.
1. Clone your fork.
1. Create a new branch with your contribution.
1. Run `devtools::load_all()` to test your changes to the code.
1. Thoroughly update documentation for any changes made. To update documentation and NAMESPACE file `devtools::document()`.
1. Test the installation. To reinstall the package: `pak::local_install()`.
Note that the vignettes both end up slightly larger than `devtools::check()` prefers, so running as `devtools::check(build_args = "--compact-vignettes=both")` is preferred.
1. Conduct tests of the new functionality using `testthat`. 
1. Increment the package version number.
1. Stage and commit your changes with clear commit descriptions, then push to GitHub.
1. Submit a Pull Request on this repo, including a clear description of the alterations.

# Code style/formatting standards

Please minimize imports of additional functions. Where additional functions are necessary, please consider suggesting rather than importing functions. 

Use descriptive variable names in lower_snake_case, and provide descriptive comments to help users understand the new functionality.

Functions are documented with roxygen2. Please fully document any new changes before submitting a pull request.

# Testing requirements

This package uses `testthat` to test functionality against expected behavior.

See [test_procedures.Rmd](https://github.com/ucd-cepb/textNet/blob/main/vignettes/test_procedures.Rmd) for the existing test suite. When submitting a pull request, please demonstrate its functionality by including a testing file in the vignettes folder explaining its usage and incorporating relevant checks.

# Reporting bugs

Please ensure you are using the latest version of `textNet` and check existing [issues](https://github.com/ucd-cepb/textNet/issues) to ensure there is not already an existing bug report for your issue. 

Please use the [bug report template](https://github.com/ucd-cepb/textNet/tree/main/.github/issue_templates/bug_report.md) when reporting a bug.

# Asking questions and getting support

Before asking a question, please search for existing issues that may be relevant, and please ensure your question is not already addressed in the vignette or function documentation. If you still need clarification, please open an issue, with as much context as possible, including the output of `sessionInfo()`.

# Feature request template

Please use the [feature request template](https://github.com/ucd-cepb/textNet/tree/main/.github/issue_templates/feature_request.md) to request a feature. Please include a clear, complete description of the requested feature, as well as a step-by-step explanation of the enhancement. Include an explanation of the utility for the general user of the requested feature.