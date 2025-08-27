# Data Setup Guide

This project automatically finds your data folder when you run the pipeline for the first time. No manual configuration is needed in most cases.

## Automatic Setup (Recommended)

Just run:
```r
tar_make()
```

The system will automatically:
1. Search for the data folder in common locations
2. Use it if found
3. Ask you to specify a location if not found
4. Save the configuration for future runs

## Supported Data Locations

The system automatically searches for data in these locations:

### Local Data Folder
- `data/` (in project directory)
- `ca-refining-data/`
- `refining-data/`
- `project-data/`

### Google Drive (All Operating Systems)
- Shared folder: `*/calepa-cn/refining-paper/data`
- Standard Google Drive locations for your OS

### Manual Configuration

If you need to specify a custom location:

1. **Interactive**: When prompted, enter your data folder path
2. **Script**: Create a `data_config.R` file:
   ```r
   data_path <- "/path/to/your/data/folder"
   ```

## Reconfiguration

To change your data path:

### Option 1: Edit data_config.R directly
```r
# Edit data_config.R and change the path:
data_path <- "/new/path/to/data"

# Next time you run tar_make(), you'll see:
tar_make()
# > Data path changed:
# >   Previous: /old/path
# >   Current:  /new/path
# > 
# > Data path changed during tar_make() - cannot prompt for rebuild.
# > To force rebuild: Stop tar_make(), run tar_destroy(), then tar_make() again.
```

If you want the interactive prompt, run the path detection separately first:
```r
source("setup_data_paths.R")  # This will prompt interactively
# > Do you want to force a full rebuild? (y/n):
```

### Option 2: Use reconfiguration function
```r
source("setup_data_paths.R")
reconfigure_data_path()
```

### Option 3: Delete config and restart
Simply delete `data_config.R` and run `tar_make()` again for fresh auto-detection.

## For Public Users

If you're not part of the core team:
1. Download or obtain access to the input data
2. Place it in a `data/` folder in the project directory
3. Run `tar_make()` - it will be found automatically

## Path Changes and Rebuilds

When you change data paths, the system will:

1. **Detect the change** automatically when you run `tar_make()`
2. **Show you** the old vs new paths
3. **Provide guidance** on how to force a rebuild if needed

### Interactive vs Pipeline Context

- **When running `source("setup_data_paths.R")` directly**: You'll get an interactive prompt asking if you want to force a full rebuild
- **When running `tar_make()`**: The system detects it's in pipeline context and provides instructions instead of prompting (since prompts can interrupt long-running pipelines)

### Rebuild Options

- **Trust targets**: Let the pipeline only update targets if actual data files have changed
- **Force rebuild**: Run `tar_destroy()` then `tar_make()` to rebuild everything from scratch

This approach prevents accidentally interrupting long-running pipelines while still giving you control over rebuilds.

## Troubleshooting

- **"Data folder not found"**: The system couldn't locate your data. You'll be prompted to specify the path manually.
- **Permission errors**: Make sure you have read access to the data folder.
- **Path changes**: The system will detect and offer rebuild options automatically.
- **Force rebuild anytime**: Run `tar_destroy()` then `tar_make()` to rebuild everything.

The system works across Windows, Mac, and Linux without any code changes needed.