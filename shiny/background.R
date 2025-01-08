db_names <- unique(data[[1]]$cdm_name) 


db_list <- paste0("- ", db_names, collapse = "\n")


background_md <- glue::glue("
# Characterisation

This Shiny app presents the results of analyses conducted on the following databases:

{db_list}

The analyses include:

- A snapshot of the CDM.

- Population characteristics.

- Counts of missing data points.

- Numbers of records and subjects for each concept.

- Distribution of records and records per person in the OMOP tables.

- Counts of person-days and records in observation within the OMOP tables.

- Distribution of observation durations (in days) and days to the next observation for subjects under observation.

- Distribution of records per person for each ordinal observation period.


![](hdruk_logo.svg){width='100px'}
")
writeLines(background_md, "background.md")