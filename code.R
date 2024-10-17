host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                 dbname = "cdm_gold_202407",
                 host = host,
                 port = port,
                 user = username,
                 password = password)


cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "public_100k", writeSchema = "results", .softValidation = TRUE, writePrefix = "cc_")





snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 
snapshot |>
  OmopSketch::tableOmopSnapshot() 

result <- OmopSketch::summarisePopulationCharacteristics(cdm,
                                   sex = TRUE,
                                   ageGroup = list(c(0,19), c(20,39), c(40, 59), c(60, 79), c(80, Inf))
                                   )
result |> 
  OmopSketch::tablePopulationCharacteristics()

start_time <- Sys.time()
result_cr <- OmopSketch::summariseClinicalRecords(cdm, 
                         omopTableName = c("condition_occurrence", "drug_exposure"), 
                         sex = TRUE, 
                         ageGroup = list("0 to 19" = c(0,19), "20 to 39" = c(20,39), "40 to 59" = c(40, 59), "60 to 79" = c(60, 79), "80 to Inf" = c(80, Inf))
                         ) 
end_time <- Sys.time() 
print(end_time - start_time) # 2.450601 hours
result_cr |>
  OmopSketch::tableClinicalRecords()

result_records <- OmopSketch::summariseRecordCount(cdm,  "condition_occurrence",
                                       sex = TRUE, 
                                       ageGroup = list( c(0,19), c(20,39),  c(40, 59),  c(60, 79),  c(80, Inf))
                                       ) 
result_records |>
  OmopSketch::plotRecordCount(colour = "sex", facet = age_group~.)


result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                       output = c("records","person-days"), 
                       sex = TRUE, 
                       ageGroup = list("0 to 19" = c(0,19), "20 to 39" = c(20,39), "40 to 59" = c(40, 59), "60 to 79" = c(60, 79), "80 to Inf" = c(80, Inf))
                       ) 



result_inObservation |> 
 filter(variable_name == 'Number records in observation')|>
  plotInObservation(colour = "sex", facet = age_group~.)
 


result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                           sex = TRUE, 
                           ageGroup = list("0 to 19" = c(0,19), "20 to 39" = c(20,39), "40 to 59" = c(40, 59), "60 to 79" = c(60, 79), "80 to Inf" = c(80, Inf))
                           )
result_observationPeriod |>
  OmopSketch::tableObservationPeriod()











