# shiny is prepared to work with this resultList, please do not change them
resultList <- list(
  "summarise_omop_snapshot" = c(1L),
  "summarise_characteristics" = c(2L),
  "summarise_missing_data" = c(3L),
  "summarise_all_concept_counts" = c(4L),
  "summarise_clinical_records" = c(5L),
  "summarise_record_count" = c(6L),
  "summarise_in_observation" = c(7L),
  "summarise_observation_period" = c(8L)
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)
