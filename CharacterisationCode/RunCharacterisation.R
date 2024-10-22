
snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 
snapshot |>
  OmopSketch::tableOmopSnapshot() 
snapshot |>
  write.csv(paste0(here::here("Results"), 
    "snapshot_", db_name, ".csv"
  ))

snapshot |> OmopViewer::exportStaticApp(directory = "~/OnboardingCharacterisation/Shiny", 
                                        title = paste0("snapshot_", db_name), 
                                        open = FALSE)

'
result <- OmopSketch::summarisePopulationCharacteristics(cdm,
                                                         sex = TRUE,
                                                         ageGroup = list(c(0,19), c(20,39), c(40, 59), c(60, 79), c(80, Inf))
)
result |> 
  OmopSketch::tablePopulationCharacteristics()
'

start_time <- Sys.time()
result_cr <- OmopSketch::summariseClinicalRecords(cdm, 
                                                  omopTableName = tableName, 
                                                  sex = sex, 
                                                  ageGroup = ageGroup) 
end_time <- Sys.time() 
print(end_time - start_time) 

result_cr |>
  write.csv(paste0(here::here("Results"), 
                   "summarised_clinical_records", db_name, ".csv"))



result_cr |>
  OmopSketch::tableClinicalRecords()

result_records <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                   sex = sex, 
                                                   ageGroup = ageGroup
) 
result_records |>
  OmopSketch::plotRecordCount()

result_records |>
  write.csv(paste0(here::here("Results"), 
                   "summarised_record_counts_", db_name, ".csv"))
  
result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                           output = c("records","person-days"), 
                                                           sex = sex,
                                                           ageGroup = ageGroup
) 



result_inObservation |> 
  dplyr::filter(variable_name == 'Number records in observation')|>
  OmopSketch::plotInObservation()

result_inObservation |>
  write.csv(paste0(here::here("Results"),
                   "summarised_in_observation_", db_name, ".csv"))


result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                                                                   sex = sex, 
                                                                   ageGroup = ageGroup
)

result_observationPeriod |>
  OmopSketch::tableObservationPeriod()

result_observationPeriod |>
  write.csv(paste0(here::here("Results"), 
                 "summarised_observation_period_", db_name, ".csv"))








