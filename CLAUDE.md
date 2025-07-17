# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Essential Commands

### Pipeline Management
- `tar_make()` - Run the entire pipeline (main command)
- `tar_manifest(fields=command)` - Debug pipeline structure and check for issues
- `tar_visnetwork()` - Visualize pipeline dependencies and status
- `tar_edit()` - Open the main pipeline file (_targets.R)

### Target Inspection
- `tar_read(target_name)` - View a specific target's data in console
- `tar_load(target_name)` - Load a target into your environment
- `tar_outdated()` - Check which targets need to be rebuilt

### Running the Pipeline
Execute the pipeline using any of these methods:
```r
# Standard execution
tar_make()

# Parallel execution (if needed)
tar_make_clustermq(workers = 2)
tar_make_future(workers = 2)
```

## Architecture Overview

This project analyzes the distributional and climate impacts of California's oil refining transition using a reproducible R `targets` pipeline.

### Core Components

1. **Targets Pipeline**: The entire analysis is orchestrated through `_targets.R`, which defines ~350 targets in a dependency graph
2. **Refining Module**: Core computational engine (`R/module/module.R`) that models refinery operations under different scenarios
3. **Modular Code Organization**: Specialized R functions organized by purpose
4. **Standardized Output Management**: Version-controlled output structure with git tracking rules

### Key Directories

```
R/
├── data-processing/     # Data ingestion and preprocessing
├── figures/            # Plotting and visualization functions
├── health/            # Health impact analysis
├── labor/             # Labor impact analysis
├── module/            # Core refining module and calculations
├── read_files*.R      # File reading functions by category
├── save_functions.R   # Standardized output saving
└── write_files.R      # File writing utilities

data/                  # Input data (not tracked in git)
outputs/               # Versioned results with selective git tracking
_targets/              # Targets cache (not tracked in git)
```

## User Configuration

**CRITICAL**: Before running the pipeline, update the user path in `_targets.R`:

```r
tar_target(name = user, "meas"), # Change to your username
```

And ensure your path is defined in the `list_paths` target. The pipeline supports multiple users with different data paths.

## Data Processing Flow

The pipeline follows this general sequence:

1. **Data Ingestion** (`read_files*.R`) - Load raw data from various sources
2. **Preprocessing** (`data-processing/`) - Clean and standardize data
3. **Core Module** (`module/`) - Run refining scenarios and calculations
4. **Health Analysis** (`health/`) - Calculate health impacts and demographics
5. **Labor Analysis** (`labor/`) - Analyze employment and economic impacts
6. **Visualization** (`figures/`) - Generate plots and figures
7. **Output Saving** - Save results using standardized functions

## Output Management

### Output Structure
The project uses a standardized 3-level output structure:
```
outputs/version/iteration/
├── intermediate/     # Intermediate data files
├── results/         # Final figures and analysis
└── tables/         # Output tables and summaries
```

### Git Tracking Rules
- Files marked with `*` in `output_structure.csv` are tracked in git
- Most intermediate files are NOT tracked (too large)
- Final figures and key summary tables ARE tracked
- Use `update_gitignore.R` to sync git ignore rules

### Saving Outputs
Always use these standardized functions:

```r
# For data files
simple_fwrite_repo(
  data = your_data,
  filename = "filename.csv",
  save_path = save_path,
  file_type = "health|labor|figure|table"
)

# For plots
simple_ggsave_repo(
  plot = your_plot,
  filename = "plot_name",
  save_path = save_path,
  file_type = "figure",
  figure_number = "figure-3"
)
```

## Key Analysis Components

### Refining Module (`R/module/`)
- **module.R**: Main refining calculations and scenario modeling
- **pre_module_prep.R**: Data preparation for refining analysis
- **post_module_processing.R**: Process refining outputs
- **ghg_factor_functions.R**: GHG emission calculations

### Health Analysis (`R/health/`)
- Uses InMAP source-receptor matrix for PM2.5 exposure
- Calculates mortality impacts with age-adjusted VSL
- Demographic analysis by race, income, and DAC status

### Labor Analysis (`R/labor/`)
- Direct, indirect, and induced employment impacts
- Uses IMPLAN economic multipliers
- Demographic breakdowns and compensation analysis

### Key Scenarios
- **Demand scenarios**: BAU (Business as Usual) vs LC1 (Low Carbon)
- **Refining scenarios**: "historic exports", "historic production", "low exports"
- **Years**: 2020-2045 projections

## Common Development Tasks

### Adding New Targets
1. Add target definition to `_targets.R`
2. Implement required functions in appropriate `R/` subdirectory
3. Update output structure if creating new files
4. Run `tar_manifest()` to check for issues

### Modifying Existing Analysis
1. Update relevant function in `R/` directory
2. Check dependencies with `tar_visnetwork()`
3. Run `tar_outdated()` to see what will be rebuilt
4. Run `tar_make()` to update pipeline

### File Management
- Use `verify_file_paths.R` to check output paths are correct
- Use `update_gitignore.R` to sync git tracking rules
- Follow the output structure defined in `structure.md`

## Important Notes

- The pipeline is designed to skip unchanged targets automatically
- Each target's results are cached in `_targets/` directory
- User path configuration is essential for cross-platform compatibility
- The refining module supports both cluster-level and refinery-level GHG calculations
- Health VSL calculations can use age-adjusted or constant VSL approaches
- Labor analysis includes re-employment assumptions and wage compensation factors

## Testing and Validation

- Run `tar_manifest()` before major changes to catch issues early
- Run `tar_make()` to make sure the full pipeline runs successfully (check before doing so).
- Use `tar_visnetwork()` to understand target dependencies
- Check `tar_outdated()` to see impact of changes
- Verify output files are saved in correct locations using `verify_file_paths.R`