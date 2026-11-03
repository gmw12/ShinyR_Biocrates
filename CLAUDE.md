# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Shiny (R) app that processes concentration exports from Biocrates WebIDQ: the **MxP Quant 500 (Q500)** kit, the **Bile Acids** kit, or the **MxP Quant 1000 (Q1000)** kit in one of three panels (Full Panel 1231 analytes, Lipids 906, Small Molecules 325). It takes a config workbook and a WebIDQ `.txt` export. It then imputes values below LOD, computes QC accuracy and CV, normalizes across plates, filters analytes, and writes a multi-sheet Excel report.

## Running

There is no build or lint step. Run the app from the repo root (all `source()` calls use relative paths):

```r
shiny::runApp(".")        # or open ShinyR_Biocrates.Rproj in RStudio and click "Run App"
```

Tests (need the sample data in `data/`, about 1.5 minutes):

```sh
Rscript tests/run_all.R                                   # all sample exports + synthetic Q1000 small molecule / 2 plate exports
Rscript tests/pipeline_check.R <config.xlsx> <export.txt> <tag> [material]   # one run
```

`tests/pipeline_check.R` runs the `*_bg` pipeline functions headless on a scratch database and checks every analyte value (data, LODs, imputation, QC, normalization, Excel) against the raw export **by analyte name**. `tests/make_synthetic_q1000.R` builds the synthetic exports from the full panel export. Run the tests after any change to the pipeline.

The required packages are listed in `Shiny_Libraries.R`. They include shiny, shinyjs, shinydashboard, shinyFiles, shinyalert, tidyverse, data.table, readxl, openxlsx, RSQLite, DT, callr, gplots, rgl and others.

Sample inputs are in `data/<kit>/` (gitignored): `data/Q500/` (`Q500_Config_020325.xlsx` + `Q500-single plate.txt` / `Q500-multiple plate.txt`), `data/Bile_Acids/` (`Bile_Acids_Config_020325.xlsx` + `new BA-simple plate.txt` / `BA-multiple plates.txt`) and `data/Q1000/` (one config per panel; exports `Q1000-Full panel.txt` and `Q1000 lipid.txt`, there is no small-molecule export). The multi-plate exports contain tissue samples (pmol/mg, non-zero `Tissue factor`).

## Architecture

### Entry and wiring
- `ui.R` builds a shinydashboard with these tabs: Welcome, Load, QC, Samples (with sub-tabs for Data/Norm/Filtered/SPQC/plots/PCA/Excel), and Admin.
- `server.R` sources `Shiny_Startup.R` at load time. `set_user()` picks the `volumes` roots for the file chooser by matching `Sys.info()["nodename"]`. Unknown hosts fall back to the "public" config with `/data`.
- Inside `shinyServer`, `Shiny_Source.R` sources every other `Shiny_*.R` module. Each button has an `observeEvent` in `server.R` that calls the pipeline functions in order. **`server.R` is the best place to see the processing order.**
- `*_erase.R` files and `scratch.R` are dead or scratch code. Nothing sources them.

### State model: global `params` + SQLite
- `params` is a **one-row global data.frame** of settings and paths (`database_path`, `data_path`, `error_path`, `plot_path`, `plate_number`, `plates`, `materials`, `material_select`, `norm_select`, `data_source`, …). It is modified with `<<-`. List-valued fields such as plates and materials are stored as comma-joined strings.
- Every intermediate dataset is a table in a per-project SQLite DB at `database/<file_prefix>.db`. Use `read_table_try` / `write_table_try` in `Shiny_File.R`; both `stop()` after their retries fail. `params` is also saved there as the `params` table.
- SQLite allows at most 2000 columns per table. The raw export (`data_raw`) and `data_status` can be wider (Q1000 with Status columns), so they are `.rds` files next to the DB (`database/<file_prefix>_<table>.rds`, included in the archive zip). Use `write_wide_table` / `read_wide_table` / `wide_table_fields`.
- Output files go to `<config file dir>/<file_prefix>/` with `Error/`, `Plots/`, `Backup/` and `archive/` subfolders. **`create_dir()` deletes an existing directory before recreating it.**

### The `foo()` / `foo_bg()` pattern
Almost every step is split in two:
- `foo(session, input, output, params)` runs in the Shiny process. It copies the `input$...` values it needs into `params`, then launches `callr::r_bg(foo_bg, args = list(params, ...), stderr = <error_path>/error_*.txt)`, calls `$wait()`, and echoes the log with `print_stderr()`. Check for failure with `bg_error(bg)` (error message or NULL): callr's exit status is 0 even when the function stops, the error only surfaces in `get_result()`. The load steps return TRUE/FALSE and `server.R` stops the chain at the first failure.
- `foo_bg(params, ...)` runs in a **fresh R process**. It must `source('Shiny_File.R')` (and any other helpers it needs) itself and cannot see globals or `input`. It communicates only through SQLite tables or its return value. If it changes `params`, the caller has to assign the result back, either `params <<- bg$get_result()` or by re-reading the `params` table.

When adding a step, follow this pattern and pass all UI inputs in as arguments.

### Processing pipeline (table names in SQLite)
1. **Load config** (`load_config_file` → `load_config_bg` → `config_kit`): the workbook sheets `Analytes` (Name, Class, Sub_platform, PCA_Class) and `QC` (expected QC1–3 levels) become DB tables. A `New Name` column (Q1000 small-molecule config) is renamed to `Name`. Names must be non-empty and unique, also after `make.names`. `data_source` is a label from the analyte count (20 `BileAcid`, 630 `Q500`, 1231/906/325 `Q1000 Full Panel`/`Lipids`/`Small Molecules`). The number of leading report columns comes from `report_info_cols()` (columns up to `R_colnames`), not from `data_source`.
2. **Load data** (`load_data_bg`): `fread(skip=1)` reads the WebIDQ export into `data_raw`. The step flags "infinity" values and cleans `Material` and special characters.
3. `remove_status_cols` → `data_status`. `remove_indicators` finds every config analyte **by name** (Q1000 exports list analytes in a different order than the config), keeps the sample info columns before the first analyte, puts the analytes in config order and drops everything else (metabolism indicators), giving `data_no_indicators`. A config analyte missing from the export stops the load (wrong config for the panel). Reference samples are found by `Sample.description` with `is_ref_sample()` (`Shiny_File.R`); "Golden West" matches any spelling (e.g. `GoldenWest-LotB`).
4. `separate_data`: rows whose first column starts with a digit are samples (`data_start`). Everything else is the LOD/LLOQ/ULOQ header block (`data_info`). This step also fills in `params$plates` and `materials`, writes `plate_info`, and checks that the data analyte order matches the config.
   - **Plate groups**: `params$plates` holds the study-sample barcodes, `"FIA | LC"` for Q500 and `"A | B | C"` for the Q1000 full panel. Kit rows (standards, and for Q1000 also the kit QCs) are on single barcodes of a group; `plate_group()` maps any `Plate.bar.code` to its group. Study rows on part of a group get the re-injection warning.
5. `report_template` → `Report_template`: the per-analyte table with a per-plate LOD column `LOD_<plate>, uM` ("LOD (from OP)", falling back to "LOD (calc.)"; the barcodes of a plate group are combined per analyte), plus Lowest/Highest CS. The `R_colnames` column maps analytes to R-mangled column names.
6. **Replace LOD** (`replace_lod`) → `data_impute`: zeros or "< LOD" values become the plate LOD, or a random 0.1–1 × LOD (seed 1234) unless `fixed_lod` is set.
7. **QC calc** (`qc_calc`) → `QC_Report`: mean, accuracy against expected value, and CV for QC1/QC2/QC3 on each plate group. Expected values are matched to analytes by name. In the Q1000 full panel each QC family (MxQP, MxQN, LxQ) is on its own barcode and only has values for that barcode's analytes; the rows are pooled per plate group.
8. **Process material** (`process_data`, run once per material and accumulated in `params$material_list`):
   - `<material>`: the samples, optionally with replacement SPQC rows added.
   - `normalize_data`, only when more than one plate and `norm_select != "None"`: per-plate factors from the reference (SPQC or another sample description) give `<norm>_Norm_<material>`, `<norm>_Norm_Factor_<material>` and `Norm_Mean_<material>`.
   - `SPQC_Report_<material>` and `SPQC_Summary_Report_<material>` (CV counts and means by Class/Sub_platform).
   - `spqc_missing_filter` drops analytes with a high SPQC CV or too many values below LOD, giving `Filtered_<material>` and `<norm>_Filtered_Norm_<material>`.
9. **Excel** (`final_excel_bg`): builds sheets from the checkboxes. Sheet names are truncated to 31 chars. It also copies all `*.R` files into `archive/`.
10. **Archive** (`zip_data_save`): zips `database/` together with a saved `params` object. Loading that zip (`load_archive_file`) restores a session.

Analyte data is found **positionally** after `remove_indicators`. The last `nrow(Report_template)` (or `nrow(Analytes)`) columns of a data table are the analytes in config order, with names `make.names(Analytes$Name)` (= `Report_template$R_colnames`), and the columns before them are sample metadata. Keep that layout when you add or remove columns, and check the names (`identical(tail(colnames(df), n), make.names(Name))`) where it matters. Filtered tables only drop analyte columns.

### Other modules
- `Shiny_Tables.R`: DT tables, built in background processes and rendered into `output$...`.
- `Shiny_Plots.R`: QC/SPQC bar and box plots and the norm-factor line plot, saved as PNGs to `params$plot_path` and then displayed. Also contains the PCA.
- `Shiny_UI_Update.R`: `ui_render_*` / `update_widgets` refresh outputs after each step. `ui_render_startup` rebuilds the UI from whatever tables already exist.
- `Shiny_Hide.R`: shows normalization options only when there is more than one plate.
- `Shiny_Misc_Functions.R`: `simple_plate_name` (removes the shared prefix and suffix from plate barcodes) and `clean_dataframe`.

## Conventions
- Each function logs `cat(file = stderr(), "Function x...")` on entry and `"...end"` on exit. Keep this up, because the stderr log is the main debugging tool. Errors from background steps end up in `<data_path>/Error/*.txt`.
- Column names in the DB are R-mangled (`make.names`-style dots). `fix_header()` restores the real analyte names for Excel output.
