# Find Retracted Papers from DOI Analysis Documentation

This document provides a comprehensive overview of the Find Retracted Papers from DOI module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module checks Digital Object Identifiers (DOIs) against retraction databases to identify retracted publications. It primarily uses the OpenRetractions API to validate publication status and can optionally retrieve PubMed IDs for valid DOIs. The function includes robust error handling, DOI format validation, and rate limiting for API calls, ensuring reliable and accurate results for bibliometric analysis in clinical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Functionality**           |                                |                                        |                                     |                                      |
| DOI Variable                     | `doi`                          | DOI Variable                           | `summary`                           | `.run`, `.validate_doi`, `.check_retractions` |
| Retraction Database              | `database`                     | Retraction Database                    | `summary`                           | `.run`, `.check_retractions`         |
| Add PMID to Data                 | `pmid`                         | Add PMID to Data                       | `summary`, `pmids`                  | `.run`                               |
| PMIDs Output                     | `resids`                       | PMIDs                                  | `pmids`                             | `.run`                               |
