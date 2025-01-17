# shiny is prepared to work with this resultList, please do not change them
resultList <- list(
  "summarise_omop_snapshot" = c(1L),
  "summarise_characteristics" = c(2L),
  "summarise_missing_data" = c(3L),
  "summarise_concept_id_counts" = c(4L),
  "summarise_clinical_records" = c(5L),
  "summarise_record_count" = c(6L),
  "summarise_in_observation" = c(7L),
  "summarise_observation_period" = c(8L)
)

source(file.path(getwd(), "functions.R"))

data_path <- file.path(getwd(), "data")
csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

result <- purrr::map(csv_files, \(x){
  d <- omopgenerics::importSummarisedResult(x)
  attr(d, "settings") <- attr(d, "settings")|>dplyr::mutate(result_type = dplyr::if_else(.data$result_type == "summarise_all_concept_counts", "summarise_concept_id_counts", .data$result_type))
  d
}) |> 
  dplyr::bind_rows() |>
  omopgenerics::newSummarisedResult()

# result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)
