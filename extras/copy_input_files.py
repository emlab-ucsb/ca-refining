import os
import shutil
from pathlib import Path

# Base paths
MAIN_PATH = "/Users/meas/Library/CloudStorage/GoogleDrive-mmeng@ucsb.edu/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn"
REPO_PATH = "/Users/meas/ucsbit/ca-refining"
DATA_PATH = os.path.join(REPO_PATH, "data")

# Input files from the targets pipeline
INPUT_FILES = [
    # Raw data files
    "data-staged-for-deletion/stocks-flows/raw/Study 1 - Preliminary Fuel Volumes BAU & LC1.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/Distillates 10-10.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/5-20 Jet Fuel Demand.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/California Transportion Fuel Consumption - Summary 2020-06-01 GDS_rename.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/Finished_Products_Movements.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/altair_refinery_capacity.xlsx",
    "data-staged-for-deletion/stocks-flows/raw/hydrogen_facilities_list.xlsx",
    "data-staged-for-deletion/health/raw/ces3results.xlsx",
    "data-staged-for-deletion/health/raw/SB535DACresultsdatadictionary_F_2022/SB535DACresultsdatadictionary_F_2022.xlsx",
    "data-staged-for-deletion/Census/ca-median-house-income.csv",
    "data-staged-for-deletion/Census/ca-median-house-income-county.csv",
    "data-staged-for-deletion/Census/nhgis_2020/nhgis0024_csv/nhgis0024_ds249_20205_tract.csv",
    "data-staged-for-deletion/Census/nhgis_2020/nhgis0024_csv/nhgis0024_ds254_20215_tract.csv",
    "data-staged-for-deletion/Census/nhgis0039_csv/nhgis0039_ds258_2020_tract.csv",
    "data-staged-for-deletion/Census/nhgis_2020/nhgis0029_csv/nhgis0029_csv/nhgis0029_ds254_20215_tract.csv",
    
    # Processed data files
    "data-staged-for-deletion/stocks-flows/processed/refinery_loc_cap_manual.csv",
    "data-staged-for-deletion/stocks-flows/processed/CARB_RE_fuels_CA_imports_figure10_053120.xlsx",
    "data-staged-for-deletion/stocks-flows/processed/renewable_refinery_capacity.xlsx",
    "data-staged-for-deletion/stocks-flows/processed/finished_product_movements_weekly_cec.csv",
    "data-staged-for-deletion/stocks-flows/processed/fuel_watch_data.csv",
    "data-staged-for-deletion/stocks-flows/processed/refinery_lat_long_revised.csv",
    "data-staged-for-deletion/stocks-flows/processed/oil_price_projections_revised.xlsx",
    "data-staged-for-deletion/health/processed/ref_emission_factor.csv",
    "data-staged-for-deletion/health/processed/refinery_emission_factor.csv",
    "data-staged-for-deletion/health/processed/age_based_VSL_2019.csv",
    "data-staged-for-deletion/health/processed/ct_inc_45_2020.csv",
    "data-staged-for-deletion/health/processed/ces3_data.csv",
    "data-staged-for-deletion/benmap/processed/growth_per_cap.csv",
    "data-staged-for-deletion/labor/raw/ca_regions.csv",
    "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/20240623-census_regions-Detail Economic Indicators.csv",
    "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/Emp_FTE and W&S_EC_546 Industry Scheme.xlsx",
    "data-staged-for-deletion/labor/implan/20241010-census_regions_2019-Detail Economic Indicators.csv",
    
    # Output files from previous runs
    "outputs-staged-for-deletion/stocks-flows/refinery_ghg_emissions.csv",
    "outputs-staged-for-deletion/refining-2023/health/refining_health_income_2023.csv",
    "model-development/scenario-plot-staged-for-deletion/refinery-outputs/site_refining_outputs_2019.csv",
    "model-development/scenario-plot-staged-for-deletion/refinery-outputs/county_refining_outputs_2019.csv",
    "model-development/scenario-plot-staged-for-deletion/refinery-outputs/refining_emissions_state_2019_revised.csv",
]

# Shapefiles and their components
SHAPEFILE_DIRS = [
    "data-staged-for-deletion/GIS/raw/ct-cartographic-boundaries/cb_2019_06_tract_500k",
    "data-staged-for-deletion/GIS/raw/ct-cartographic-boundaries/nhgis0030_shapefile_tl2020_us_tract_2020",
    "data-staged-for-deletion/GIS/raw/CA_counties_noislands",
    "data-staged-for-deletion/GIS/raw/Petroleum_Refineries_US_EIA",
]

# Directory with multiple CSV files
CSV_DIRS = [
    "data-staged-for-deletion/stocks-flows/processed/ghg_mrr",
    "data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining",
]

def copy_file(src_path, dst_path):
    """Copy a single file, creating directories as needed."""
    try:
        # Create destination directory if it doesn't exist
        os.makedirs(os.path.dirname(dst_path), exist_ok=True)
        
        # Copy the file
        shutil.copy2(src_path, dst_path)
        print(f"✓ Copied: {os.path.basename(src_path)}")
        return True
    except Exception as e:
        print(f"✗ Failed to copy {src_path}: {e}")
        return False

def copy_directory(src_path, dst_path):
    """Copy an entire directory."""
    try:
        if os.path.exists(dst_path):
            shutil.rmtree(dst_path)
        shutil.copytree(src_path, dst_path)
        print(f"✓ Copied directory: {os.path.basename(src_path)}")
        return True
    except Exception as e:
        print(f"✗ Failed to copy directory {src_path}: {e}")
        return False

def main():
    """Main function to copy all input files."""
    print("Starting to copy input files to repository data folder...")
    print(f"Source: {MAIN_PATH}")
    print(f"Destination: {DATA_PATH}")
    print("-" * 60)
    
    # Create data directory if it doesn't exist
    os.makedirs(DATA_PATH, exist_ok=True)
    
    success_count = 0
    total_count = 0
    
    # Copy individual files
    print("\nCopying individual files:")
    for file_path in INPUT_FILES:
        src_path = os.path.join(MAIN_PATH, file_path)
        dst_path = os.path.join(DATA_PATH, file_path)
        
        if os.path.exists(src_path):
            if copy_file(src_path, dst_path):
                success_count += 1
        else:
            print(f"✗ Source file not found: {file_path}")
        total_count += 1
    
    # Copy shapefile directories
    print("\nCopying shapefile directories:")
    for dir_path in SHAPEFILE_DIRS:
        src_path = os.path.join(MAIN_PATH, dir_path)
        dst_path = os.path.join(DATA_PATH, dir_path)
        
        if os.path.exists(src_path):
            if copy_directory(src_path, dst_path):
                success_count += 1
        else:
            print(f"✗ Source directory not found: {dir_path}")
        total_count += 1
    
    # Copy CSV directories
    print("\nCopying CSV directories:")
    for dir_path in CSV_DIRS:
        src_path = os.path.join(MAIN_PATH, dir_path)
        dst_path = os.path.join(DATA_PATH, dir_path)
        
        if os.path.exists(src_path):
            if copy_directory(src_path, dst_path):
                success_count += 1
        else:
            print(f"✗ Source directory not found: {dir_path}")
        total_count += 1
    
    # Summary
    print("-" * 60)
    print(f"Copy complete: {success_count}/{total_count} items copied successfully")
    
    # Create a README file
    readme_path = os.path.join(DATA_PATH, "README.md")
    with open(readme_path, 'w') as f:
        f.write("# Data Directory\n\n")
        f.write("This directory contains copies of all input files used in the targets pipeline.\n")
        f.write("Files were copied from the main project directory to enable reproducible analysis.\n\n")
        f.write("## Structure\n\n")
        f.write("The directory structure mirrors the original project structure:\n")
        f.write("- `data-staged-for-deletion/`: Raw and processed data files\n")
        f.write("- `outputs-staged-for-deletion/`: Output files from previous analysis runs\n")
        f.write("- `model-development/`: Model development outputs\n\n")
        f.write(f"Last updated: {os.popen('date').read().strip()}\n")
    
    print(f"✓ Created README.md at {readme_path}")

if __name__ == "__main__":
    main()
