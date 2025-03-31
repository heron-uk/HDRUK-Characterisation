# shiny is prepared to work with this resultList, please do not change them
resultList <- list(
  "summarise_omop_snapshot" ,
  "summarise_concept_id_counts"
)

source(file.path(getwd(), "functions.R"))

data_path <- file.path(getwd(), "data")
csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

result <- purrr::map(csv_files, \(x){
  d <- omopgenerics::importSummarisedResult(x)
  attr(d, "settings") <- attr(d, "settings") |>
    dplyr::mutate(result_type = dplyr::if_else(.data$result_type == "summarise_all_concept_counts", "summarise_concept_id_counts", .data$result_type))
  d |>
    omopgenerics::filterSettings(.data$result_type %in% c("summarise_omop_snapshot", "summarise_concept_id_counts"))
}) |> 
  omopgenerics::bind()

resultList <- resultList |>
  purrr::map(\(x) {
    omopgenerics::settings(result) |>
      dplyr::filter(.data$result_type %in% .env$x) |>
      dplyr::pull(.data$result_id) }) |>
  rlang::set_names(resultList)
data <- prepareResult(result, resultList)

filterValues <- defaultFilterValues(result, resultList)

data$summarise_concept_id_counts <- data$summarise_concept_id_counts |>
  omopgenerics::tidy() |>
  dplyr::select(!c("study_period_end", "study_period_start")) |>
  dplyr::filter(!is.na(.data$count_records))

filterValues$summarise_concept_id_counts_variable_name <- NULL

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)
