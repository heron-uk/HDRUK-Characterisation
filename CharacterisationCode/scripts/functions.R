characterisePersonTable <- function(cdm) {
  person <- dplyr::ungroup(cdm$person)
  cols <- colnames(person)
  result <- list()
  
  # number individuals
  subjects <- person |> dplyr::tally() |> dplyr::pull() |> as.numeric()

  result <- dplyr::tibble(
    column = c(
      "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth",
      "race_concept_id", "ethnicity_concept_id", "location_id", "care_site_id",
      "gender_source_concept_id", "race_source_concept_id", 
      "ethnicity_source_concept_id"
    ),
    join = c(
      "concept", NA, NA, NA, "concept", "concept", "location", "care_site",
      "concept", "concept", "concept"
    ),
    variable_name = .data$column |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_sentence()
  ) |>
    purrr::pmap(\(column, join, variable_name) {
      if (column %in% cols) {
        counts <- person |>
          dplyr::group_by(dplyr::across(dplyr::all_of(column))) |>
          dplyr::tally(name = "count")
        if (!is.na(join)) {
          joinBy <- switch(join, 
                           "concept" = "concept_id",
                           "location" = "location_id",
                           "care_site" = "care_site_id")
          interestCol <- switch (join,
                                 "concept" = "concept_name",
                                 "location" = "location_source_value",
                                 "care_site" = "care_site_name")
          sel <- c(rlang::set_names(joinBy, column), interestCol)
          counts <- counts |>
            dplyr::left_join(
              cdm[[join]] |>
                dplyr::select(dplyr::all_of(sel)),
              by = column
            )
        }
        counts |>
          dplyr::collect() |>
          dplyr::rename("variable_level" = dplyr::all_of(column)) |>
          dplyr::arrange(.data$variable_level) |>
          dplyr::mutate(variable_name = .env$variable_name, 
                        variable_level = sprintf("%i", round(.data$variable_level)))
      } else {
        cli::cli_inform(c("!" = "Column: {column} not found in person table."))
        NULL
      }
    }) |>
    purrr::compact() |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      percentage = 100 * as.numeric(.data$count) / .env$subjects,
      count = as.integer(.data$count)
    )
  
  # build result
  additionalCols <- c("concept_name", "location_source_value", "care_site_name")
  additionalCols <- additionalCols[additionalCols %in% colnames(result)]
  result <- result |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(additionalCols),
      \(x) dplyr::coalesce(x, "missing")
    )) |>
    dplyr::mutate(
      omop_table = "person",
      cdm_name = omopgenerics::cdmName(cdm),
      package_name = "HERON-UK Chracterisation",
      package_version = "0.0.1",
      result_type = "summarise_person_table"
    ) |>
    omopgenerics::transformToSummarisedResult(
      group = "omop_table",
      strata = character(),
      additional = additionalCols,
      estimates = c("count", "percentage"),
      settings = c("package_name", "package_version", "result_type")
    )
  
  return(result)
}
summariseImdRecords <- function(cdm) {
  x <- cdm$observation |>
    dplyr::filter(.data$observation_source_concept_id == 35812882) |>
    dplyr::collect() |>
    dplyr::ungroup()
  persons <- cdm$person |> 
    dplyr::select("person_id") |>
    dplyr::collect()
  
  result <- list()
  
  # total records
  result$total <- dplyr::tibble(
    variable_name = "Number IMD records",
    variable_level = NA_character_,
    count = x |> dplyr::tally() |> dplyr::pull()
  )
  
  # records per person
  result$records_per_person <- x |>
    dplyr::group_by(.data$person_id) |>
    dplyr::tally(name = "counts") |>
    dplyr::right_join(persons, by = "person_id") |>
    dplyr::mutate(counts = dplyr::coalesce(as.integer(.data$counts), 0L)) |>
    dplyr::summarise(
      mean = mean(.data$counts), 
      sd = sd(.data$counts),
      min = min(.data$counts),
      q25 = quantile(.data$counts, 0.25),
      median = median(.data$counts),
      q75 = quantile(.data$counts, 0.75),
      max = max(.data$counts),
      count = sum(as.integer(.data$counts >= 1))
    ) |>
    dplyr::mutate(
      variable_name = "Number IMD records per person",
      variable_level = NA_character_
    )
  
  if (TRUE) { #nrow(x) > 0) {
    # values
    result$values <- x |>
      dplyr::group_by(.data$value_as_number) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "IMD value", 
        variable_level = as.character(.data$value_as_number)
      ) |>
      dplyr::select(!"value_as_number")
    
    # most recent value
    result$recent_value <- persons |>
      dplyr::left_join(
        x |>
          dplyr::group_by(.data$person_id) |>
          dplyr::filter(
            .data$observation_date == min(.data$observation_date, na.rm = TRUE)
          ) |>
          dplyr::summarise(value_as_number = dplyr::first(.data$value_as_number)),
        by = "person_id"
      ) |>
      dplyr::group_by(.data$value_as_number) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "Most recent IMD value", 
        variable_level = as.character(.data$value_as_number)
      ) |>
      dplyr::select(!"value_as_number")
    
    # records per year
    result$record_per_year <- x |>
      dplyr::mutate(year = as.integer(clock::get_year(.data$observation_date))) |>
      dplyr::group_by(.data$year) |>
      dplyr::tally(name = "count") |>
      dplyr::arrange(.data$year) |>
      dplyr::mutate(
        variable_name = "Year of IMD recording", 
        variable_level = as.character(.data$year)
      ) |>
      dplyr::select(!"year")
  }

  # format result
  result <- result |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      package_name = "HERON-UK Chracterisation",
      package_version = "0.0.1",
      result_type = "summarise_imd"
    ) |>
    omopgenerics::transformToSummarisedResult(
      group = character(),
      strata = character(),
      additional = character(),
      settings = c("package_name", "package_version", "result_type"),
      estimates = c("count", "mean", "sd", "min", "q25", "median", "q75", "max")
    ) |>
    dplyr::filter(!is.na(.data$estimate_value))
  
  return(result)
}
summariseEthnicityRecords <- function(cdm) {
  x <- cdm$observation |>
    dplyr::filter(.data$observation_source_concept_id %in% c(
      700362, 700363, 700364, 700365, 700366, 700367, 700368, 700369, 700385, 
      700386, 700387, 700388, 700389, 700390, 700391
    )) |>
    dplyr::collect() |>
    dplyr::ungroup()
  persons <- cdm$person |> 
    dplyr::select("person_id") |>
    dplyr::collect()
  
  result <- list()
  
  # total records
  result$total <- dplyr::tibble(
    variable_name = "Number Ethnicity records",
    variable_level = NA_character_,
    count = x |> dplyr::tally() |> dplyr::pull()
  )
  
  # records per person
  result$records_per_person <- x |>
    dplyr::group_by(.data$person_id) |>
    dplyr::tally(name = "counts") |>
    dplyr::right_join(persons, by = "person_id") |>
    dplyr::mutate(counts = dplyr::coalesce(as.integer(.data$counts), 0L)) |>
    dplyr::summarise(
      mean = mean(.data$counts), 
      sd = sd(.data$counts),
      min = min(.data$counts),
      q25 = quantile(.data$counts, 0.25),
      median = median(.data$counts),
      q75 = quantile(.data$counts, 0.75),
      max = max(.data$counts),
      count = sum(as.integer(.data$counts >= 1))
    ) |>
    dplyr::mutate(
      variable_name = "Number Ethnicity records per person",
      variable_level = NA_character_
    )
  
  if (TRUE) { #nrow(x) > 0) {
    # values
    result$values <- x |>
      dplyr::group_by(.data$observation_source_concept_id) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "Ethnicity record", 
        variable_level = dplyr::case_when(
          .data$observation_source_concept_id == 700362 ~ "Asian or Asian British - Indian",
          .data$observation_source_concept_id == 700363 ~ "Asian or Asian British - Pakistani",
          .data$observation_source_concept_id == 700364 ~ "Asian or Asian British - Bangladeshi",
          .data$observation_source_concept_id == 700365 ~ "Asian or Asian British - Any other Asian background",
          .data$observation_source_concept_id == 700366 ~ "Black or Black British - Caribbean",
          .data$observation_source_concept_id == 700367 ~ "Black or Black British - African",
          .data$observation_source_concept_id == 700368 ~ "Black or Black British - Any other Black background",
          .data$observation_source_concept_id == 700369 ~ "Other Ethnic Groups - Chinese",
          .data$observation_source_concept_id == 700385 ~ "White - British",
          .data$observation_source_concept_id == 700386 ~ "White - Irish",
          .data$observation_source_concept_id == 700387 ~ "White - Any other White background",
          .data$observation_source_concept_id == 700388 ~ "Mixed - White and Black Caribbean",
          .data$observation_source_concept_id == 700389 ~ "Mixed - White and Black African",
          .data$observation_source_concept_id == 700390 ~ "Mixed - White and Asian",
          .data$observation_source_concept_id == 700391 ~ "Mixed - Any other mixed background"
        )
      ) |>
      dplyr::select(!"observation_source_concept_id")
    
    # most recent value
    result$recent_value <- persons |>
      dplyr::left_join(
        x |>
          dplyr::group_by(.data$person_id, .data$observation_source_concept_id) |>
          dplyr::summarise(
            n = dplyr::n(), 
            recent_date = max(.data$observation_date, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::group_by(.data$person_id) |>
          dplyr::filter(.data$n == max(.data$n, na.rm = TRUE)) |>
          dplyr::filter(.data$recent_date == max(.data$recent_date, na.rm = TRUE)) |>
          dplyr::summarise(observation_source_concept_id = dplyr::first(.data$observation_source_concept_id)),
        by = "person_id"
      ) |>
      dplyr::group_by(.data$observation_source_concept_id) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "Most common/recent Ethnicity record", 
        variable_level = dplyr::case_when(
          .data$observation_source_concept_id == 700362 ~ "Asian or Asian British - Indian",
          .data$observation_source_concept_id == 700363 ~ "Asian or Asian British - Pakistani",
          .data$observation_source_concept_id == 700364 ~ "Asian or Asian British - Bangladeshi",
          .data$observation_source_concept_id == 700365 ~ "Asian or Asian British - Any other Asian background",
          .data$observation_source_concept_id == 700366 ~ "Black or Black British - Caribbean",
          .data$observation_source_concept_id == 700367 ~ "Black or Black British - African",
          .data$observation_source_concept_id == 700368 ~ "Black or Black British - Any other Black background",
          .data$observation_source_concept_id == 700369 ~ "Other Ethnic Groups - Chinese",
          .data$observation_source_concept_id == 700385 ~ "White - British",
          .data$observation_source_concept_id == 700386 ~ "White - Irish",
          .data$observation_source_concept_id == 700387 ~ "White - Any other White background",
          .data$observation_source_concept_id == 700388 ~ "Mixed - White and Black Caribbean",
          .data$observation_source_concept_id == 700389 ~ "Mixed - White and Black African",
          .data$observation_source_concept_id == 700390 ~ "Mixed - White and Asian",
          .data$observation_source_concept_id == 700391 ~ "Mixed - Any other mixed background",
          .default = "missing ethnicity record"
        )
      ) |>
      dplyr::select(!"observation_source_concept_id")
    
    # records per year
    result$record_per_year <- x |>
      dplyr::mutate(year = as.integer(clock::get_year(.data$observation_date))) |>
      dplyr::group_by(.data$year) |>
      dplyr::tally(name = "count") |>
      dplyr::arrange(.data$year) |>
      dplyr::mutate(
        variable_name = "Year of Ethnicity recording", 
        variable_level = as.character(.data$year)
      ) |>
      dplyr::select(!"year")
  }
  
  # format result
  result <- result |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      package_name = "HERON-UK Chracterisation",
      package_version = "0.0.1",
      result_type = "summarise_ethnicity"
    ) |>
    omopgenerics::transformToSummarisedResult(
      group = character(),
      strata = character(),
      additional = character(),
      settings = c("package_name", "package_version", "result_type"),
      estimates = c("count", "mean", "sd", "min", "q25", "median", "q75", "max")
    ) |>
    dplyr::filter(!is.na(.data$estimate_value))
  
  return(result)
}
