tableName <- "drug_exposure"
sex <- TRUE # FALSE
ageGroup <- list(c(0,19), c(20,39), c(40, 59), c(60, 79), c(80, Inf)) # NULL


snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 


'
result <- OmopSketch::summarisePopulationCharacteristics(cdm,
                                                         sex = TRUE,
                                                         ageGroup = list(c(0,19), c(20,39), c(40, 59), c(60, 79), c(80, Inf))
)
result |> 
  OmopSketch::tablePopulationCharacteristics()
'


result_clinicalRecords<- OmopSketch::summariseClinicalRecords(cdm, 
                                                  omopTableName = tableName, 
                                                  sex = sex, 
                                                  ageGroup = ageGroup) 




result_recordCounts <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                   sex = sex, 
                                                   ageGroup = ageGroup
) 



result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                           output = c("records","person-days"), 
                                                           sex = sex,
                                                           ageGroup = ageGroup
) 




result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                                                                   sex = sex, 
                                                                   ageGroup = ageGroup
)



omopgenerics::exportSummarisedResult(snapshot, results_clinicalRecords, result_recordCounts, result_inObservation, result_observationPeriod, minCellCount = 5, path = here::here("Results"), fileName = "results.csv")
