# Characterisation

This Shiny app presents the results of analyses conducted on the following databases:

-   IDRIL_1 (Lancashire) - secondary care

-   Barts Health- secondary care

-   UCLH from 2019 - secondary care

-   CPRD Aurum - primary care

-   DataLoch_Covid19 (University of Edinburgh) - secondary care

-   LTHT (Leeds) - secondary care

-   GOSH - secondary care

The analyses include:

-   A `snapshot` of the CDM containing general info like person count and vocabulary version.

-   A `characterisation of the population` for each age group analysed: 0-19, 20-39, 40-59, 60-79, 80+.

-   Characterisation of the `clinical tables`, stratified by age, sex and year:

    -   `Missing data`: counts of missing data points.

    -   `Record count`: counts of records in observation within the OMOP tables.

    -   `Clinical records`: distribution of records per person in the OMOP tables.

-   Characterisation of the `observation period` table, stratified by age, sex:

    -   `In observation`: counts of person-days and records in observation for each year in the study period.

    -   `Observation periods`: distribution of observation durations (in days) and days to the next observation for each ordinal observation period.

The focus of the analysis is the study period 01/01/2012 - present.

::: {style="display: flex; justify-content: center; align-items: center; height: 50vh;"}
<img src="hdruk_logo.svg" style="max-width: 200px; height: auto;"/>
:::
