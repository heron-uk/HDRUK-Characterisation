host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")

db_name <- "cprd100k"

con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                      dbname = "cdm_gold_202407",
                      host = host,
                      port = port,
                      user = username,
                      password = password)

cdmSchema <- "public_100k"
writeSchema <- "results"

prefix <- "cc_"


cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema, 
                                writeSchema = c(schema = writeSchema,
                                                prefix = prefix))

tableName <- "drug_exposure"
sex <- TRUE # FALSE
ageGroup <- list(c(0,19), c(20,39), c(40, 59), c(60, 79), c(80, Inf)) # NULL


source("RunCharacterisation.R")
