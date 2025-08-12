# File Output System Documentation

This document provides comprehensive documentation for understanding and modifying the CA refining analysis pipeline's file output system.

## Overview

The pipeline uses a three-tier output structure with automated git tracking management to organize and version-control analysis results. All outputs are systematically cataloged in `extras/output_files.csv` for validation and path resolution.

### Responsive Tracking System

The system features a dependency-based architecture where the `save_folders` target in `_targets.R` depends on `file_output_structure`, ensuring that any changes to `extras/output_files.csv` automatically trigger `.gitignore` file updates on the next pipeline run. This creates a responsive system where tracking modifications take effect immediately without manual intervention.

## Architecture

### Three-Tier Output Structure

```
outputs/
└── {version}/           # Version identifier (e.g., "ref-count")
    └── {iteration}/     # Iteration parameters (e.g., "cuf=0.6_beta-scenario=main")
        ├── intermediate/      # Intermediate data processing files
        │   ├── health/       # Health analysis intermediate data
        │   └── labor/        # Labor analysis intermediate data
        ├── results/
        │   └── figures/      # All visualization outputs
        │       ├── figure-1/ through figure-5/  # Main manuscript figures
        │       ├── figures-si/                   # Supplementary information figures
        │       └── extra/                        # Additional analysis figures
        │           ├── extra-figure-3/           # Alternative figure-3 versions
        │           ├── extra-figure-4/           # Alternative figure-4 versions
        │           ├── extra-figure-5/           # Alternative figure-5 versions
        │           ├── health-figures/           # Health-specific plots
        │           ├── health-labor-figures/     # Combined health-labor plots
        │           ├── labor-figures/            # Labor-specific plots
        │           ├── pulse-figs/               # PM2.5 pulse simulation figures
        │           └── state-levels-fig/         # State-level analysis plots
        └── tables/           # Analysis result tables
            ├── health/       # Health impact tables
            ├── labor/        # Labor impact tables
            ├── health-and-labor/  # Combined impact tables
            └── other/        # Miscellaneous tables
```

### File Categorization System

The system uses `file_type` parameters to automatically determine save locations:

- **`"figure"`**: Visual outputs (PNG + PDF format)
- **`"health"`**: Health analysis intermediate data
- **`"labor"`**: Labor analysis intermediate data  
- **`"table"`**: Final analysis result tables
- **`"fig-csv"`**: CSV data files associated with figures

## Path Resolution System

### Core Function: `get_structured_path()`

Located in `R/save_functions.R`, this function maps file types to appropriate directories:

```r
get_structured_path(save_path, file_type, file_name, figure_number, extra_subfolder)
```

**Parameters:**
- `save_path`: Base path (`outputs/version/iteration`)
- `file_type`: Category of file (`"figure"`, `"health"`, `"labor"`, `"table"`)
- `file_name`: Name of file being saved
- `figure_number`: For figures, specifies subdirectory (`"figure-1"`, `"figures-si"`, etc.)
- `extra_subfolder`: For extra figures, specifies subcategory

**Resolution Logic:**

1. **Figure Files** (`file_type = "figure"`):
   - Main figures: `figure_number` ∈ {`"figure-1"`, `"figure-2"`, `"figure-3"`, `"figure-4"`, `"figure-5"`} → `results/figures/{figure_number}/`
   - Supplementary: `figure_number = "figures-si"` → `results/figures/figures-si/`
   - Extra figures: `extra_subfolder` specified → `results/figures/extra/{extra_subfolder}/`
   - Default extra: → `results/figures/extra/`

2. **Health Files** (`file_type = "health"`):
   - → `intermediate/health/`

3. **Labor Files** (`file_type = "labor"`):
   - → `intermediate/labor/`

4. **Table Files** (`file_type = "table"`):
   - Health + Labor content → `tables/health-and-labor/`
   - Health content (keywords: `health`, `mortality`, `pm25`) → `tables/health/`
   - Labor content (keywords: `labor`, `jobs`, `employment`) → `tables/labor/`
   - Other → `tables/other/`

## Git Tracking Management

### Tracking Decision Logic

Files are marked for git tracking based on their importance and size:

**Tracked Files (`tracked = YES`):**
- All main figure files (figure-1 through figure-5, figures-si)
- Key summary tables and health/labor outputs
- Figure input CSV files used for analysis replication
- Important intermediate files identified for version control

**Not Tracked Files (`tracked = NO`):**
- Extra figure variants and exploratory plots
- Large intermediate health/labor processing files
- Pulse simulation outputs (large number of individual files)
- Detailed county-level and demographic breakdowns

### Automatic `.gitignore` Generation

The `create_save_folders_repo()` function automatically creates `.gitignore` files based on directory tracking patterns:

1. **ALL_TRACKED**: Directories where all files are tracked
   - Creates comment-only `.gitignore`: `"# All files in this directory are tracked in git"`

2. **NONE_TRACKED**: Directories where no files are tracked
   - Creates ignore-all `.gitignore`: `"*"`

3. **MIXED**: Directories with selective tracking
   - Creates selective `.gitignore` with exceptions for tracked files:
   ```
   # Only specific files are tracked in this directory
   *
   !.gitignore
   !.gitkeep
   !{tracked_file_1}
   !{tracked_file_2}
   ...
   ```


## File Saving Functions

### Primary Functions

#### `simple_fwrite_repo()`
Saves data frames with automatic path resolution and directory creation.

**Parameters:**
```r
simple_fwrite_repo(
    data,                    # Data frame to save
    folder_path,             # Legacy parameter (use NULL with new system)
    filename,                # File name
    save_path = NULL,        # Base save path (outputs/version/iteration) 
    file_type = NULL,        # File category for path resolution
    figure_number = NULL,    # Figure directory specification
    extra_subfolder = NULL   # Extra subfolder specification
)
```

#### `simple_ggsave_repo()`
Saves ggplot objects in both PNG and PDF formats.

**Parameters:**
```r
simple_ggsave_repo(
    plot,                    # ggplot object
    folder_path,             # Legacy parameter (use NULL with new system)
    filename,                # File name without extension
    width = 10,              # Plot width in inches
    height = 8,              # Plot height in inches  
    dpi = 300,               # Resolution for PNG
    save_path = NULL,        # Base save path
    file_type = NULL,        # File category ("figure" for plots)
    figure_number = NULL,    # Figure directory
    extra_subfolder = NULL   # Extra subfolder
)
```

### Advanced Functions

#### `validate_and_save_file()`
Uses `extras/output_files.csv` to validate file locations and apply correct tracking.

#### `should_be_tracked()`
Checks `extras/output_files.csv` to determine if a file should be tracked in git.

## Configuration Management

### `extras/output_files.csv` Structure

The central configuration file contains three columns:

```csv
file_name,relative_path,tracked
state_npv_fig_2020_ppx_bartik.png,results/figures/figure-3,YES
pulse_119.png,results/figures/extra/pulse-figs,NO
health_dac_legend.pdf,results/figures/figure-4,YES
...
```

**Columns:**
- `file_name`: Exact filename as saved (no special markers needed)
- `relative_path`: Directory path relative to `outputs/version/iteration/`
- `tracked`: Whether file should be tracked in git (`YES`/`NO`)

### Integration with Targets Pipeline

The `_targets.R` file references the configuration:

```r
# Output structure validation and dependency tracking
tar_target(
  name = file_output_structure,
  command = "extras/output_files.csv",
  format = "file"
)

# Responsive folder creation that depends on CSV changes
tar_target(
  name = save_folders,
  command = create_save_folders_repo(save_path, iteration, file_output_structure)
)
```

## Modification Instructions

### Adding New Output Files

1. **Add save operation in `_targets.R`:**
   ```r
   tar_target(
     name = save_my_new_output,
     command = simple_fwrite_repo(
       data = my_data,
       folder_path = NULL,
       filename = "my_output.csv",
       save_path = save_path,
       file_type = "health"  # or appropriate type
     ),
     format = "file"
   )
   ```

2. **Add entry to `extras/output_files.csv`:**
   ```csv
   my_output.csv,intermediate/health,YES
   ```

3. **Verify path resolution:** Check that `file_type` maps to intended directory via `get_structured_path()`

### Changing File Tracking Status

**To track a previously untracked file:**
1. Change `tracked` column in `extras/output_files.csv` from `NO` to `YES`
2. Run `tar_make()` - the dependency system will automatically update `.gitignore` files
3. Verify changes: Check that the file appears in the appropriate directory's `.gitignore` exceptions

**To untrack a previously tracked file:**
1. Change `tracked` column from `YES` to `NO` in `extras/output_files.csv`
2. Run `tar_make()` - `.gitignore` files will be updated automatically
3. Verify changes: Confirm the file is no longer in `.gitignore` exceptions

### Modifying Save Locations

**To change where a file is saved:**

1. **Update `extras/output_files.csv`:**
   ```csv
   # From:
   my_file.csv,tables/health,YES
   # To:
   my_file.csv,tables/labor,YES
   ```

2. **If using automatic path resolution, change `file_type` in `_targets.R`:**
   ```r
   # In _targets.R, change:
   file_type = "health"  # saves to intermediate/health/
   # To:
   file_type = "table"   # saves to tables/ with content-based subdirectory
   ```

3. **Verification steps:**
   - Run `tar_make()` to trigger directory updates
   - Check that `.gitignore` files reflect the new location
   - Verify the file appears in the expected directory

4. **For custom paths, modify `get_structured_path()` logic in `R/save_functions.R`**

### Changing Output Filenames

**To rename an output file:**

1. **Update the save operation in `_targets.R`:**
   ```r
   # Change filename parameter:
   filename = "old_name.csv"  # From this
   filename = "new_name.csv"  # To this
   ```

2. **Update `extras/output_files.csv`:**
   ```csv
   # Change file_name column:
   old_name.csv,results/figures/figure-3,YES  # From this
   new_name.csv,results/figures/figure-3,YES  # To this
   ```

3. **Verification:**
   - Run `tar_make()` to generate the new filename
   - Confirm old filename is no longer generated
   - Check `.gitignore` files reference the new name

### Adding New Directory Categories

1. **Extend `get_structured_path()` function:**
   ```r
   } else if (file_type == "my_new_type") {
       return(file.path(base_path, "my_new_directory"))
   }
   ```

2. **Update directory creation in `create_save_folders_repo()`**

3. **Add files to `extras/output_files.csv` with new relative paths**

## Troubleshooting

### Common Issues

**File not found errors:**
- Verify `extras/output_files.csv` contains entry for the file
- Check file_name exactly matches (no asterisk in CSV)
- Ensure `relative_path` exists and is correct

**Incorrect file locations:**  
- Verify `file_type` parameter in save function
- Check `get_structured_path()` logic for the file type
- Confirm `extras/output_files.csv` has correct `relative_path`

**Git tracking problems:**
- Run `tar_make()` to regenerate `.gitignore` files via dependency system
- Check `tracked` column in `extras/output_files.csv` matches intent
- Verify changes to CSV trigger the `save_folders` target rebuild
- Confirm `.gitignore` files contain correct file exceptions

### Validation Commands

```r
# Verify dependency system works
tar_make()  # Should rebuild save_folders if CSV changed

# Check if output directories are created correctly
tar_outdated()  # See which targets need rebuilding

# Verify file path resolution
get_structured_path(save_path, "health", "test.csv")

# Check tracking status  
should_be_tracked("my_file.csv")

# Validate CSV changes take effect
tar_visnetwork()  # Visualize target dependencies
```

## Quick Reference

### Common Modification Workflows

1. **Add new output file:**
   - Add save target to `_targets.R`
   - Add entry to `extras/output_files.csv`
   - Run `tar_make()`

2. **Change tracking status:**
   - Edit `tracked` column in CSV
   - Run `tar_make()`
   - Verify `.gitignore` updates

3. **Move file to different directory:**
   - Update `relative_path` in CSV
   - Optionally update `file_type` in `_targets.R`
   - Run `tar_make()` and verify location

4. **Rename output file:**
   - Change `filename` in `_targets.R`
   - Update `file_name` in CSV
   - Run `tar_make()`

### Troubleshooting Checklist

- [ ] Is the file listed in `extras/output_files.csv`?
- [ ] Does the `relative_path` match the intended location?
- [ ] Has `tar_make()` been run after CSV changes?
- [ ] Do `.gitignore` files reflect current tracking settings?
- [ ] Is the `file_type` parameter correct in save functions?

This comprehensive system ensures reproducible, well-organized, and appropriately version-controlled research outputs while maintaining flexibility for future modifications and extensions.