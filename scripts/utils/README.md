# Utilities Directory

This directory contains shared utility functions and helper scripts used across the Endo-Review analysis pipeline.

## Utility Modules

### `error_handling.R` - Error Management
Comprehensive error handling and recovery utilities:
- **Safe Execution**: `safe_execute()` wrapper with fallback handling
- **Error Logging**: File-based error logging with timestamps
- **Data Validation**: Data frame structure and content validation
- **Chunked Processing**: Memory-safe batch processing with error recovery
- **File Validation**: Comprehensive file existence and integrity checks
- **Backup Creation**: Automatic backup of important files

### `memory_optimization.R` - Memory Management
Large dataset processing optimization utilities:
- **Memory Monitoring**: `monitor_memory()` for usage tracking
- **Garbage Collection**: Aggressive cleanup with reporting
- **Sparse Matrices**: Automatic conversion for memory efficiency
- **Chunked Processing**: Large dataset handling with memory limits
- **Temporary Files**: Managed temporary storage for intermediate results
- **Data Optimization**: Data frame compression and type optimization

### `plot_utils.R` - Visualization Utilities
Plotting and visualization helper functions:
- **Color Palettes**: Colorblind-friendly color schemes
- **Theme Functions**: Consistent plot styling (`endo_theme()`)
- **Color Access**: `get_endo_colors()` and `get_endo_gradient()` functions
- **Custom Scales**: Specialized color and fill scales for Endo-Review data

## Common Usage Patterns

### Error Handling
```r
# Safe execution with error recovery
result <- safe_execute({
  # Your code here
  risky_operation()
}, context = "Data processing")

if (result$success) {
  # Use result$result
} else {
  # Handle error: result$error
}
```

### Memory Management
```r
# Monitor memory usage
monitor_memory(threshold_gb = 4)

# Process large datasets efficiently
process_large_dataset(
  data = large_dataframe,
  process_function = my_analysis_function,
  chunk_size = 1000
)
```

### Visualization
```r
# Apply consistent theme
ggplot(data, aes(x, y)) +
  geom_point() +
  endo_theme() +
  scale_color_manual(values = get_endo_colors(3))
```

## Integration

Utilities are automatically available when sourced:
```r
source("scripts/config/pipeline_config.R")  # Loads utilities automatically
# OR
source("scripts/utils/error_handling.R")
source("scripts/utils/memory_optimization.R")
source("scripts/utils/plot_utils.R")
```

## Dependencies

- **error_handling.R**: Base R only
- **memory_optimization.R**: Requires `Matrix`, `tidytext`, `readr`
- **plot_utils.R**: Requires `ggplot2`

## Best Practices

1. **Error Handling**: Always wrap risky operations in `safe_execute()`
2. **Memory Management**: Monitor memory usage for large datasets
3. **Visualization**: Use `endo_theme()` and color utilities for consistency
4. **Logging**: Log errors to `results/logs/pipeline_errors.log`
5. **Backup**: Create backups before modifying critical data

## Performance Features

- **Vectorized Operations**: Optimized for speed and memory efficiency
- **Lazy Loading**: Functions load only when needed
- **Fallback Mechanisms**: Graceful degradation when dependencies unavailable
- **Progress Tracking**: Built-in progress monitoring for long operations
- **Resource Cleanup**: Automatic cleanup of temporary files and memory

## Maintenance

When adding new utilities:
1. Follow naming conventions (`function_name()`)
2. Include comprehensive documentation
3. Add error handling where appropriate
4. Test for memory efficiency
5. Update this README

These utilities provide the foundation for robust, efficient, and maintainable analysis pipelines.