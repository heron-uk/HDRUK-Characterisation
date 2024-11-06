library(DBI)
library(duckdb)
library(CDMConnector)
library(dplyr)
library(dbplyr)

db <- dbConnect(duckdb(), dbdir = eunomia_dir())
cdm <- cdmFromCon(con = db, cdmSchema = "main",writeSchema = "main")


db_name <-"eunomia"
source("Characterisation/RunCharacterisation.R")