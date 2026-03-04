# patient-safety-data-request-template

Template for extracting data from the national patient safety systems in England.

## Setup

1. Install renv: `install.packages("renv")`
2. Restore packages: `renv::init()` and select "Restore the project from the lockfile".

## Usage

1. Set search parameters in `params.qmd`.
2. Run `run_data_pipeline()` in the Console window.
3. Output will be generated and uploaded to SharePoint

## File Structure

````
patient-safety-data-request-template/
├── params.qmd
└── R/
    ├── orchestrator.R
    ├── setup.R
    ├── config/
    │   ├── column_selection_lookups.R
    │   ├── connections.R
    │   ├── neopaeds.R
    │   └── styles.R
    ├── output/
    │   ├── formatter.R
    │   └── microsoft365R.R
    ├── processors/
    │   ├── lfpse.R
    │   ├── nrls.R
    │   └── steis.R
    └── utils/
        ├── date_utils.R
        ├── expand_categorical_filters.R
        ├── functions.R
        ├── logging_utils.R
        ├── neopaed_utils.R
        ├── sampling_utils.R
        └── text_filtering.R
````

- `params.qmd` — Set search parameters here
- `R/config/` — Database connections, column lookups, styles
- `R/processors/` — Database-specific extraction logic
- `R/utils/` — Shared helper functions
- `R/output/` — Excel formatting and SharePoint upload

## Data flow

Each processor creates objects in sequence:

1. `x_parsed` - Raw data with renamed columns
2. `x_filtered_categorical` - After date/categorical filters
3. `x_filtered_text` - After text filters
4. `x_labelled` - Coded values replaced with labels
5. `x_sampled` - After sampling strategy applied
6. `x_for_release_*` - Final outputs for Excel

For more information about the process that is followed for each database. Please check the [Wiki](https://github.com/nhs-england-patient-safety/patient-safety-data-request-template/wiki).
