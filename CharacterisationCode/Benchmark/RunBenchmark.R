timings <- list()
iterations <- 1 # number of times to run the same query


for(i in 1:iterations){
  
  print(paste0("iteration ", i, " of ", iterations))
  
  # 1) count
  tictoc::tic()
  cdm$condition_occurrence |>
    dplyr::tally() |>
    dplyr::pull("n")
  t<- tictoc::toc()
  timings[[paste0("1_", i)]]<- tibble::tibble(task="Count condition occurrence rows",
                                      time_taken_secs= as.numeric(t$toc-t$tic)) %>%
    
    dplyr::mutate(iteration = i )
  
  # 2) grouped count
  tictoc::tic()
  cdm$condition_occurrence %>%
    dplyr::group_by(person_id) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::tally() %>%
    dplyr::pull("n")
  t<- tictoc::toc()
  timings[[paste0("2_", i)]]<-tibble::tibble(task="Count individuals in condition occurrence table",
                                     time_taken_secs= as.numeric(t$toc-t$tic)) %>%
    dplyr::mutate(iteration = i)
  
  # 3) join
  tictoc::tic()
  cdm$person %>%
    dplyr::left_join(cdm$condition_occurrence %>%
                       dplyr::group_by(person_id) %>%
                  dplyr::tally() %>%
                  dplyr::ungroup()) %>%
    dplyr::filter(is.na(n)) %>%
    dplyr::tally() %>%
    dplyr::pull("n")
  t<- tictoc::toc()
  timings[[paste0("3_", i)]] <- tibble::tibble(task="Count individuals in person but not in condition occurrence table",
                                       time_taken_secs= as.numeric(t$toc-t$tic)) %>%
    dplyr::mutate(iteration = i)
  
  # 4) compute to write schema
  tictoc::tic()
  cdm$person_ws<- cdm$person %>%
    dplyr::compute(name = "person_ws",
            temporary = FALSE)
  cdm$person_ws |>
    dplyr::tally() |>
    dplyr::pull("n")
    
  t<- tictoc::toc()
  timings[[paste0("4_", i)]] <- tibble::tibble(task="Compute person table to write schema",
                                      time_taken_secs= as.numeric(t$toc-t$tic))%>%
    dplyr::mutate(iteration = i)
  
  # 5) join with a table in the write_schema
  tictoc::tic()
  cdm$person_ws %>%
    dplyr::left_join(cdm$condition_occurrence %>%
                       dplyr::group_by(person_id) %>%
                       dplyr::tally() %>%
                       dplyr::ungroup()) %>%
    dplyr::filter(is.na(n)) %>%
    dplyr::tally() %>%
    dplyr::pull("n")
  t<- tictoc::toc()
  timings[[paste0("5_", i)]] <- tibble::tibble(task="Count individuals in person (in write schema) but not in condition occurrence table",
                                       time_taken_secs= as.numeric(t$toc-t$tic))%>%
    dplyr::mutate(iteration = i)
  
  
  # incprev denominator
  tictoc::tic()
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator",
                                      ageGroup = list(c(18,150),
                                                      c(18,64),
                                                      c(65,150)),
                                      cohortDateRange = as.Date(c("2013-01-01",
                                                                  "2022-12-31")),
                                      sex = c("Both", "Male", "Female"),
                                      daysPriorObservation = c(365))
  
  
  t<- tictoc::toc()
  timings[[paste0("6_", i)]] <- tibble::tibble(task="Create IncidencePrevalence cohorts",
                                       time_taken_secs= as.numeric(t$toc-t$tic))%>%
    dplyr::mutate(iteration = i)
  

  
  # drugutilisation cohort
  tictoc::tic()
  druglist <- CodelistGenerator::getDrugIngredientCodes(cdm, c("acetaminophen", "metformin"))
  tictoc::toc()
  timings[[paste0("7_", i)]] <- tibble::tibble(task="Get ingredient codes with CodelistGenerator",
                                               time_taken_secs= as.numeric(t$toc-t$tic))%>%
    dplyr::mutate(iteration = i)
  
  tictoc::tic()
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "drug_cohorts",
    conceptSet = druglist,
    priorObservation = 365
  )
 
  t<- tictoc::toc()
  timings[[paste0("8_", i)]] <- tibble::tibble(task="Create DrugUtilisation cohorts",
                                       time_taken_secs= as.numeric(t$toc-t$tic))%>%
    dplyr::mutate(iteration = i)
  
  #DrugUtilisation::benchmarkDrugUtilisation(cdm, indicationCohortName = "drug_cohorts", drugExposureName = "drug_exposure" )
  res<-IncidencePrevalence::benchmarkIncidencePrevalence(cdm, analysisType = "all")
  
  timings[[paste0("9_", i)]] <- tibble::tibble(task=unlist(res$group_level),
                                               time_taken_secs= unlist(as.numeric(res$estimate_value)))%>%
    dplyr::mutate(iteration = i)
  
  bm_connector<-benchmarkCDMConnector(cdm)
  
  timings[[paste0("10_", i)]] <- tibble::tibble(task=unlist(bm_connector$task),
                                               time_taken_secs= unlist(as.numeric(bm_connector$time_taken_secs)))%>%
    dplyr::mutate(iteration = i)
 tictoc::tic()
 codes_sin <- CodelistGenerator::getCandidateCodes(cdm, c("sinusitis"))$concept_id
 codes_ph <- CodelistGenerator::getCandidateCodes(cdm, c( "pharyngitis"))$concept_id
 codes_bro <- CodelistGenerator::getCandidateCodes(cdm, c( "bronchitis"))$concept_id
 tictoc::toc()
 timings[[paste0("11_", i)]] <- tibble::tibble(task="Get conditions codes with CodelistGenerator",
                                              time_taken_secs= as.numeric(t$toc-t$tic))%>%
   dplyr::mutate(iteration = i) 
 
 codes <- omopgenerics::newCodelist(list("sinusitis"=codes_sin, "bronchitis"=codes_bro, "pharyngitis"=codes_ph))
 
 tictoc::tic()
 cdm$conditions_cohort <- CohortConstructor::conceptCohort(cdm, conceptSet = codes, name = "conditions_cohort")
 tictoc::toc()
 timings[[paste0("12_", i)]] <- tibble::tibble(task="Create condtions cohorts with CohortConstructor",
                                              time_taken_secs= as.numeric(t$toc-t$tic))%>%
   dplyr::mutate(iteration = i)
 
 
}
# export
timings<- dplyr::bind_rows(timings) %>%
  dplyr::mutate(cdm_name = cdmName(cdm),
         person_count = cdm$person %>%
           dplyr::tally() %>%
           dplyr::pull("n")) %>%
  dplyr::mutate(time_taken_mins= round(time_taken_secs / 60,1))

write.csv(timings,
          here::here("Results",
               paste0(db_name,"_timings_",
                      Sys.Date(),
                      ".csv")), 
          row.names = FALSE)