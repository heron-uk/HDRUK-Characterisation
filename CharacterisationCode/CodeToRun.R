
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

characterisation <- TRUE
conceptCounts <- TRUE

source("RunCharacterisation.R")
