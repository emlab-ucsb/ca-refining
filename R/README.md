# R Script Organization Guide

## Structure

The repository's R scripts have been split and organized into logical components to improve maintainability and readability.

### Core Files

- `read_files.R` - Contains compatibility layer for the most commonly used reading functions (legacy)
- `read_files_core.R` - Basic file reading helpers (fread, xlsx, etc.)
- `read_files_refinery.R` - Refinery-specific data reading functions
- `read_files_census.R` - Census and demographic data reading functions
- `read_files_labor.R` - Labor-related data reading functions
- `read_files_misc.R` - Miscellaneous data reading functions

### Long Scripts That Could Be Split Further
Some scripts in the repository are still quite long and could benefit from further organization:

1. `R/figures/health_labor_revised_figs.R` - 13,699 lines (extremely long!)
2. `R/figures/fig1.R` - 3,104 lines (very long)
3. `R/module/post_module_processing.R` - 1,801 lines (long)
4. `R/health/census_pm.R` - 1,581 lines (long)
5. `R/labor/labor_functions_product_px.R` - 1,508 lines (long)

## Usage Notes

- The `targets` workflow will automatically source all R files in the R/ directory and subdirectories
- No changes are needed to `_targets.R`
- The original `read_files.R` now serves as a compatibility layer and contains comments about where functions have moved

## Lint Issues

You may notice lint errors related to:
- Non-standard evaluation in data.table syntax (`[, .(columns)]` or `:=` assignment)
- Use of global variables within data.table expressions
- Missing package namespace prefixes

These can be addressed by:
1. Adding `@importFrom data.table :=` to function documentation
2. Using `data.table::` namespace prefix for functions
3. Adding proper package dependencies in a DESCRIPTION file

## Future Organization

Consider further organizing the very long scripts in the future, particularly:
- `health_labor_revised_figs.R` could be split into health_figures.R and labor_figures.R
- `fig1.R` could be split by figure components
