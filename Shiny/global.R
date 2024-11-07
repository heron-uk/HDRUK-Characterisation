
library(shiny)



data_file<-list.files(here::here("data"), recursive = TRUE,
                    full.names = TRUE)

data_file<-data_file[stringr::str_detect(data_file, ".csv")]
results_file<-data_file[stringr::str_detect(data_file, "characterisation")]
results <- list()
for(i in seq_along(results_file)){
  results[[i]]<-omopgenerics::importSummarisedResult(path=results_file[[i]])
 
}
data <- dplyr::bind_rows(results) |>OmopViewer::correctSettings()

