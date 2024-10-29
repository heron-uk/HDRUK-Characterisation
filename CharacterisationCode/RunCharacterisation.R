# Start
log_file <- paste0(here::here("Results"),"/characterisation_log.txt")

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = log_file, append = TRUE)
}

log_message("Start time recorded.")

tableName <- "drug_exposure"
sex <- TRUE # FALSE
ageGroup <- list("0 to 50"= c(0,50), "51 to 100"=c(51,100)) # NULL
ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup, ageGroupName = "")[[1]]

# Snapshot
log_message("Getting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 

# Population Characteristics
log_message("Getting population characteristics")
result_populationCharacteristics <- CohortConstructor::demographicsCohort(cdm, "population", ageRange = ageGroup, sex = "Both" ) |>
  PatientProfiles::addDemographicsQuery(sex = TRUE, age = FALSE, ageGroup = ageGroup) |>
  CohortCharacteristics::summariseCharacteristics(strata = list("sex", "age_group") )

# Summarize clinical records
log_message("Summarising clinical records")
result_clinicalRecords<- OmopSketch::summariseClinicalRecords(cdm, 
                                                  omopTableName = tableName, 
                                                  sex = sex, 
                                                  ageGroup = ageGroup) 

# Summarize record counts
log_message("Summarising record counts")
result_recordCounts <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                   sex = sex, 
                                                   ageGroup = ageGroup) 



# Summarize in observation records
log_message("Summarising in observation records and person-days")
result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                           output = c("records","person-days"), 
                                                           sex = sex,
                                                           ageGroup = ageGroup) 




# Summarize observation period
log_message("Summarising observation period")
result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                                                                   sex = sex, 
                                                                   ageGroup = ageGroup
)

# Combine results and export
result <- omopgenerics::bind(snapshot, result_populationCharacteristics, result_clinicalRecords, result_recordCounts, result_inObservation, result_observationPeriod)
omopgenerics::exportSummarisedResult(result, minCellCount = 5, path = here::here("Results"), fileName = paste0(
  "result_", db_name, ".csv"))


# Zip the results
log_message("Zipping results")
files_to_zip <- list.files(here::here("Results"))
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here::here("Results"), "/results_", db_name, ".zip"
)),
files = files_to_zip,
root = here::here("Results"))

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Close connection
cdm_disconnect(cdm)
log_message("Database connection closed.")