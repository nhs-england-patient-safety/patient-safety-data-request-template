# patient-safety-data-request-template

Template for extracting data from the national patient safety systems in England.

## Instructions:

-   Set the search parameters in the params file.

-   Source the params file.This will source each relevant script automatically.

## Scripts:

-   `params.R` - In this file, you set up the parameters for the search, including specifying which databases should be searched and any categorical and text filters.

-   `connections.R` - This file is sourced by the params file. It connects to the the LFPSE and NRLS databases if required.

-   `functions.R` - This file is sourced by the params file. It contains functions used by the other scripts.

-   `column_selection_lookups.R` - This file is sourced by the params file. It creates a list with the required columns for each of the three databases.

-   `flow.R` - this file is sourced by the params file. It sources the script for first database that needs to be run for this search (The order is `nrls.R`, `lfpse.R`, `steis.R` )

-   `nrls.R` - this file searches the NRLS database according to the parameters specified in the params file. It sources the next file in the process (`lfpse.R`, `steis.R` or `formatter.R` depending on which databases are required)

-   `lfpse.R` - this file searches the LFPSE database according to the parameters specified in the params file. It sources the next file in the process (`steis.R` or `formatter.R` depending on which databases are required)

-   `steis.R`- this file searches the StEIS csv specified by the csv name in the params file using the parameters specified in the params file. It then sources `formatter.R`

-   `formatter.R` - this file creates an excel output file with a search strategy tab containing the information in the params file. It loops through the databases searched, adding a tab with summary tables, and a tab with incident level data (if required) for each database. It sources `microsoft365.R`

-   `microsoft365.R` - this file uploads the excel file created by `formatter.R` to sharepoint.

-   `styles.R` - this file is where the styles used by openxlsx are created.

-   `neopaeds.R` - stores text strings to use for neopaeds search.

## Process in each database file

Each file follows a very similar process. They will all create these objects in the following order (where x is the name of the database.

1.  `x_parsed` - SQL table object with some renamed columns

2.  `x_filtered_categorical` - `x_parsed` object with date and categorical filters applied, only relevant columns selected, collected into R memory and additional required columns generated (e.g. month and year)

3.  `x_filtered_text` - `x_filtered_categorical` object with text filters applied

4.  `x_labelled` - `x_filtered_text` object with the categorical variables labelled - e.g. in the form "medications", "devices" rather than 3 or 4.

5.  `x_age_classified` - `x_labelled` object with columns added indicating whether the incident is neonate or paediatric or neither. (note- lfpse has an extra step before this, where age validation is carried out)

6.  `x_neopaed` - `x_age_classified` object with a filter applied for the desired neopaed strategy- neonate , paediatric or none.

7.  `x_sampled` - `x_neopaed` object with sampling strategy applied (or no sampling if no sampling strategy is required)

8.  `x_for_release_sampled_pt_level` - `x_sampled` object with columns renamed to more human readable column names. The patient level data is required for data tabs.

9.  `x_for_release_unsampled_pt_level` - `x_labelled` object with columns renamed to more human readable column names. This patient level is required for data tabs.

10. `x_for_release_sampled_incident_level` - `x_sampled` object with columns renamed to more human readable column names. For LFPSE, this is converted from patient level to incident level data, by removing patient level columns and calling `distinct()`. This incident level data is required for summary tabs.

11. `x_for_release_unsampled_incident_level` - `x_labelled` object with columns renamed to more human readable column names. For LFPSE, this is converted from patient level to incident level data, by removing patient level columns and calling `distinct()`. This incident level data is required for summary tabs.

Note- for NRLS and StEIS, the patient level is identical to the incident level (apart from the month, month-year and year columns)

Note- where sampling has not been carried out, the sampled data is identical to the unsampled data.
