library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(janitor)

pull_from_sql <- function(schema, sql_table) {
  
tryCatch({
  con <-
    dbConnect(odbc(), Driver = "SQL Server", server = "MLCSU-BI-SQL-SU")
  df <- con %>%
    tbl(
      in_schema(
        paste0("[FD_UserDB].[Central_Midlands_csu_UserDB].[", schema, "]"),
        sql_table
      )
    ) %>%
    collect()
}, finally = {
  dbDisconnect(con)
  con <- NULL
  rm(con)
})
  
}

# foo <- pull_from_sql("CCG_OIS", "Record_Of_Lung_Cancer_Stage_At_Decision_To_Treat1")
