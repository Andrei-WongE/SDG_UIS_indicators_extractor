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
## Notes: Needs input of users_db-Vx file from 2025_RF_indicators.R
##
## ---------------------------

## Program Set-up ------------

 options(scipen = 100, digits = 4) # Prefer non-scientific notation

## Load required packages ----

 if (!require("pacman")) {
   install.packages("pacman")
 }
 pacman::p_load(here, dplyr, janitor, tidyverse, future.apply
                , progressr)

## Runs the following --------


## Import data and create output directory -------------------------------------

# Create output directory
 output_directory <- c("2025_RF_indicators/Countries_db")
 
 dir.create(output_directory)
 
 files <- list.files(here("2025_RF_indicators")
            , pattern = "[users_db]-"
            , full.names = TRUE)
 
# Assure we are using the correct file
 if (length(files) > 1) {
   message("More than one file found, please select file")
   
   Sys.sleep(2)
   
   files <- choose.files(here("2025_RF_indicators/..."))
   
 } else {
   
   m <- fs::path_file(files)
   message("The file that will be processed is ", paste(m))
 }
 
# Reading file

 sheets <- openxlsx::getSheetNames(files)
 db <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = files)
 names(db) <- sheets

# Pre processing diagnostics
 
 country <- as.vector(unique(db[[1]][["country"]]))
 
 message("The number of countries in the data set is "
         , paste(length(country)))
 
 Sys.sleep(1)
 
 message("The countries are "
         , paste(sapply(country, paste), "\t"))

# Parallel Processing set-up
 plan(multisession)
 nbrOfWorkers()
 
# Customization of how progress is reported
 handlers(global = TRUE)
 handlers(handler_progress(
   format   = ":spin [:bar] :percent in :elapsed ETA: :eta",
   width    = 60,
   complete = "+"
 )
 ) 
 
users_db <- function(country) {
   
   p <- progressr::progressor(along = country)
   
     db <- future_lapply(seq_along(country), function(i) {
       
     # Subset A: by country, data_country sheet
       
       db <- db[[1]][db[[1]][["country"]] %in% country[1],] |> 
     # Transpose data_country sheet
               group_by(ind_id, ind_year) |>
               select(!c("iso", "region", "income_group", "pcfc")) |> 
               pivot_wider( id_cols = c(ind_id, ind_year)
                           ,names_from  = ind_year
                           ,values_from = DF[!c(ind_id, ind_year)]
               )
     
       subset_ind <- db[[1]][["id"]] 
     # Subset B: by indicator, data_aggregate and metadata sheet
     
        db <- db[[2]][db[[2]][["id"]] %in% subset_ind,]
        db <- db[[3]][db[[3]][["id"]] %in% subset_ind,]
      
      # Saving database by country
      
        openxlsx::write.xlsx(db
                             , here("2025_RF_indicators/Countries_db",
                                    paste0( i
                                          # , sheet_names[i]
                                          ,".xlsx"))
                             , sheetName = names(db)
                             , append = TRUE
                             , overwrite = TRUE)
      
     # Signaling progression updates
       p(paste("Processing sheet", country[i], Sys.time(), "\t"))
     
     # Collecting garbage after each iteration
       invisible(gc(verbose = FALSE, reset = TRUE)) 
       rm(db)
     
   }, future.seed  = NULL #Ignore random numbers warning
   )
 }
 
 users_db(country)
 
 
