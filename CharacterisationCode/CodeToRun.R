
renv::restore()
dbName <- "..."

con <- DBI::dbConnect("...")

cdmSchema <- "..."
writeSchema <- "..."

prefix <- "..."


cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema, 
                                writeSchema = c(schema = writeSchema,
                                                prefix = prefix),
                                cdmName = dbName)

minCellCount = 5


# set Addendum = FALSE if you want to run the all characterisation, 
# set Addendum = TRUE if you want to run only the addendum  
Addendum <- ...


if (!Addendum){
  
  source("RunCharacterisation.R")
  
} else {
  
  source("Addendum.R")
  
}
