---
description: Add checkpoint calls to functions before expensive operations in Jamovi modules
allowed-tools: ["Read", "Edit", "MultiEdit", "Grep"]
---

Add `private$.checkpoint()` calls to the specified function before expensive operations. This enables:

1. **Incremental Results**: Pushes computed results to Jamovi so users see tables fill gradually
2. **Change Detection**: Aborts current run if user modifies settings, avoiding wasted computation

**Usage**: `/checkpoint [function_name] [file_path]`

**Best Practices**:
- Place checkpoints immediately before expensive operations (loops, statistical computations, data processing)
- Don't add checkpoints before fast operations that return instantly
- Use `private$.checkpoint(flush = FALSE)` to only poll for changes without re-pushing results

**Example Pattern**:
```r
for (item in items) {
    if (table$getCell(rowKey = item, 'result')$isEmpty) {
        table$setStatus('running')
        private$.checkpoint()  # Add before expensive operation
        
        result <- expensiveComputation(item)
        table$setRow(rowKey = item, result)
        table$setStatus('complete')
    }
}
```

Analyze the function $ARGUMENTS and add checkpoints before any expensive operations like loops, statistical tests, or data processing steps.