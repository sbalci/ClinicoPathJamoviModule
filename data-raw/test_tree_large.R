#!/usr/bin/env Rscript

# Test tree function with larger dataset
source('R/tree.h.R')
source('R/tree.b.R')
library(jmvcore)

# Load a larger test dataset
load('data/cancer_biomarkers.rda')

# Test with larger dataset
print('Testing with cancer biomarkers dataset...')
print(paste('Data dimensions:', nrow(cancer_biomarkers), 'rows x', ncol(cancer_biomarkers), 'columns'))

tryCatch({
  result <- tree(
    data = cancer_biomarkers,
    vars = c('PSA', 'age', 'tumor_size'),
    facs = c('grade', 'stage'),
    target = 'diagnosis',
    targetLevel = 'cancer',
    train = 'cohort',
    trainLevel = 'discovery',
    spatialCoords = NULL
  )
  
  print('SUCCESS: Tree function executed with larger dataset!')
  print(paste('Result class:', class(result)))
  print(paste('Result components:', paste(names(result), collapse=', ')))
  
  # Check specific results
  if ('modelSummary' %in% names(result)) {
    print('SUCCESS: Model summary found')
  }
  if ('plot' %in% names(result)) {
    print('SUCCESS: Plot found')
  }
  
}, error = function(e) {
  print(paste('FAILED with larger dataset:', e$message))
})

print('Large dataset test completed.')