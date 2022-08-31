## ---------------------------
##
## Script name: 2025_RF_users_database
##
## Project: GPE_2025_RF
##
## Purpose of script: Generate country level indicators database
##
## Author: Andrei Wong Espejo
##
## Date Created: 2022-08-31
##
## Email: awongespejo@worldbank.org
##
## ---------------------------
##
## Notes: Needs input of indicators_db-Vx file from 2025_RF_indicators.R
##
## ---------------------------

## Program Set-up ------------

 options(scipen = 100, digits = 4) # Prefer non-scientific notation

## Load required packages ----

 if (!require("pacman")) {
   install.packages("pacman")
 }
 pacman::p_load(here, dplyr, janitor, tidyverse)

## Runs the following --------


## Import data and create output directory -------------------------------------

 # Create output directory
 output_directory <- c("2025_RF_indicators/Countries_db")
 
 dir.create(output_directory)
 
 files <- list.files(here("2025_RF_indicators")
            , pattern = "[indicators_db]-"
            , full.names = TRUE)
 
 # Assure we are using the correct file
 if (length(files) > 1) {
   message("More than one file found, please select file")
   
   Sys.sleep(2)
   
   choose.files(here("2025_RF_indicators/..."))
   
 } else {
   
   m <- fs::path_file(files)
   message("The file that will be processed is ", paste(m))
 }
 
 # Reading file

 sheets <- openxlsx::getSheetNames(files)
 db <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = files)
 names(db) <- sheets

 
 country <- unique(db[[1]][["country"]])
 
 message("The number of countries in the data set is "
         , paste(length(country)))
 
 message("The countries are "
         , paste(sapply(country, paste), collapse = " "))

 
 indicators_db <- function(country) {
   
   p <- progressr::progressor(along = country)
   
   db <- future_lapply(seq_along(country), function(i) {
     
     # Creating database for each sheet
     indicator <- file_list %>% 
       map_dfr(~openxlsx::readWorkbook(.
                                       , sheet = country[i]
                                       , colNames = TRUE
                                       , skipEmptyRows = TRUE
                                       , check.names = TRUE
                                       , fillMergedCells = FALSE) %>% 
                 mutate(across(.fns = as.character))
               , .id = "file_path")
     
     # Creating indicator id variable
     indicator$id <- fs::path_ext_remove(fs::path_file(indicator$file_path)) 
     
     indicator$id <- gsub("(GPE_indicator-)|(GPE2025_Indicator-)|(GPE2025_indicator-)"
                          , ""
                          , indicator$id
                          , perl = TRUE
     )
     
     indicator <- indicator %>%
       separate(.
                , id 
                , c("ind_id", "ind_year")
                , sep = "-"
                , remove = TRUE
                , extra = "warn") 
     
     indicator <- indicator %>%
       select(!file_path) %>%
       mutate(data_update = format(Sys.Date())) %>%
       dplyr::relocate(c("ind_id", "ind_year"))
     
     # exists("indicator")
     
     db <- Filter(function(x) is(x, "data.frame"), mget(ls()))
     
     openxlsx::write.xlsx(indicator
                          , here("2025_RF_indicators",
                                 paste(country[i],".xlsx", sep = "_"))
                          , sheetName = country[i]
                          , append = TRUE
                          , overwrite = TRUE)
     
     # Signaling progression updates
     p(paste("Processing sheet", country[i], Sys.time(), "\t"))
     
     # Collecting garbage after each iteration
     invisible(gc(verbose = FALSE, reset = TRUE)) 
     
     return(db)
     
   }, future.seed  = NULL #Ignore random numbers warning
   )
   
   # names(db) <- country
   # 
   # openxlsx::write.xlsx(db
   #                      , here("2025_RF_indicators",
   #                             paste("indicators_db-V0.6.xlsx", sep = "_"))
   #                      , sheetName = names(db)
   #                      , colNames = TRUE
   # )
   # rm(db)
   
   # Delete previous file
   unlink(list.files(here("2025_RF_indicators")
                     , pattern = "[indicators_db]-"
                     , full.names = TRUE))
   
   # Set file path list
   filenames_list <- list.files(path = here("2025_RF_indicators")
                                , pattern    = ".xlsx"
                                , all.files  = FALSE
                                , full.names = TRUE
                                , recursive  = FALSE)
   
   # Read all files
   
   All <- lapply(filenames_list, function(filename){
     
     print(paste("Merging", filename, sep = " "))
     
     read.xlsx(filename)
   })
   
   names(All) <- country
   
   openxlsx::write.xlsx(All
                        , here("2025_RF_indicators", "indicators_db-V0.6.xlsx")
                        , sheetName = names(All)
                        , overwrite = TRUE)
 }
 
 indicators_db(country)
 
 
 
 
 
 
 
 
 
 
 
 

