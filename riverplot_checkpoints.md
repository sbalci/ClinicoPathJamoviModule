# Riverplot Function - Checkpoint Implementation Summary

## Overview
Added `private$.checkpoint()` calls strategically throughout the riverplot function to enable:
1. **Incremental Results**: Users see tables populate gradually in Jamovi
2. **Change Detection**: Aborts computation if user changes settings
3. **Improved Responsiveness**: Better user experience with large datasets

## Checkpoint Locations

### Data Processing Phase
1. **`.process_data()`** - Line 276
   - Before main data format processing (long vs wide)
   - Allows early abort if user changes data format settings

### Flow Summary Generation
2. **`.generate_long_flow_summary()`** - Line 409
   - Before expensive flow calculation for longitudinal data
   - Inside loop at Line 459 (every 100 rows) for large datasets

3. **`.generate_wide_flow_summary()`** 
   - Line 470: Before transition calculations
   - Line 477: Inside stage loop (flush=FALSE for polling only)
   - Line 507: Inside row population loop (every 100 rows)

### Stage Summary Generation  
4. **`.generate_stage_summary()`**
   - Line 518: Before stage summary calculation
   - Line 545: Inside stage variable loop for wide format (flush=FALSE)

### Transition Matrix Generation
5. **`.generate_transition_matrix()`**
   - Line 579: Before expensive probability calculations
   - Line 615: Inside row population loop (every 20 rows)
   - Line 635: Inside stage transition loop for wide format (flush=FALSE)
   - Line 669: Inside second row population loop (every 20 rows)

### Riverplot Object Generation
6. **`.generate_riverplot_object()`** - Line 1050
   - Before CRAN riverplot structure creation

7. **`.create_long_riverplot_structure()`**
   - Line 1114: Before individual transition processing (flush=FALSE)
   - Line 1135: Inside edge creation loop (every 50 edges, flush=FALSE)
   - Line 1170: Inside proportional edge creation (every 30 edges, flush=FALSE)

8. **`.create_wide_riverplot_structure()`**  
   - Line 1252: Inside stage transition loop (flush=FALSE)

## Checkpoint Strategy

### Full Checkpoints (`private$.checkpoint()`)
- Used before major computation blocks
- Flushes current results to Jamovi UI
- Checks for user changes

### Polling-Only Checkpoints (`private$.checkpoint(flush = FALSE)`)
- Used inside tight loops
- Only checks for user changes without UI update
- Reduces overhead while maintaining responsiveness

### Periodic Checkpoints in Loops
- Every 20 rows for transition matrices (smaller, more complex)
- Every 50 edges for riverplot structures
- Every 100 rows for flow tables (larger, simpler)

## Performance Considerations

1. **Minimal Overhead**: Checkpoints use flush=FALSE in inner loops
2. **Adaptive Frequency**: More frequent for complex operations, less for simple ones
3. **Early Abort**: Expensive operations can abort quickly on user changes
4. **Progressive Display**: Tables populate incrementally for better UX

## Benefits

- ✅ Users see immediate feedback as analysis progresses
- ✅ Settings changes abort unnecessary computation
- ✅ Large dataset processing doesn't freeze the UI
- ✅ Maintains computational efficiency
- ✅ Improves perceived performance

## Testing

Run the test script to verify checkpoint behavior:
```r
source("test_riverplot.R")
```

Monitor Jamovi UI to see incremental table population and test abort behavior by changing settings mid-computation.