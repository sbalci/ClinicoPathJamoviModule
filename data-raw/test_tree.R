#!/usr/bin/env Rscript

# Test tree function implementation
source('R/tree.h.R')
source('R/tree.b.R')
library(jmvcore)

# Load test data
load('data/small_sample_tree.rda')

# Test the tree function with all required parameters
print('Testing tree function with complete parameters...')

tryCatch({
  result <- tree(
    data = small_sample_tree,
    vars = c('biomarker_1', 'biomarker_2', 'age'),
    facs = c('treatment', 'stage'),
    target = 'outcome',
    targetLevel = 'Yes',
    train = 'cohort',
    trainLevel = 'train',
    spatialCoords = NULL
  )
  
  print('SUCCESS: Tree function executed successfully!')
  print(paste('Result class:', class(result)))
  print(paste('Result components:', paste(names(result), collapse=', ')))
  
  # Check if we have any output
  if (length(result) > 0) {
    print('SUCCESS: Tree function returned results')
    
    # Try to access some results
    if ('modelSummary' %in% names(result)) {
      print('SUCCESS: Model summary component found')
    }
    if ('plot' %in% names(result)) {
      print('SUCCESS: Plot component found')
    }
  }
  
}, error = function(e) {
  print(paste('FAILED: Tree function failed:', e$message))
})

print('Test completed.')