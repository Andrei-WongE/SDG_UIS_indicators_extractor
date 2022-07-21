## ---------------------------
##
## Script name: 2025_RF_indicators_aggregate
##
## Project: GPE_2025_RF
##
## Purpose of script: Aggregate all RF indicators
##
## Author: Andrei Wong Espejo
##
## Date Created: 2022-07-20
##
## Email: awongespejo@worldbank.org
##
## ---------------------------
##
## Notes: Aggregation of all indicators as main data base for PowerBI.
##   
##
## ---------------------------

## Program Set-up ------------

  options(scipen = 100, digits = 4) # Prefer non-scientific notation

## Load required packages ----

  if (!require("pacman")) {
    install.packages("pacman")
  }
  pacman::p_load(here, dplyr, tidyverse, openxlsx, fs, future.apply,
                 progressr)

## Runs the following --------

  
## Import and clean data -------------------------------------------------------
  
  directory_excels <- here("2025_RF_indicators")
  # Checking content
  fs::dir_tree(directory_excels)
  
  sheet_names <- c("data_country", "data_aggregate", "metadata")
  # sheet_names <- c(1, 2, 3)
    
  # Checking sheet names! As of 2022/07/21
  
    # 1
    # 2
    # 3.i
    # 3.ii
    # 4.i
    # 4.ii.a 
    # 4.ii.b: No data
    # 5.i
    # 5.ii.a
    # 5.ii.b: No data
    # 5.ii.c
    # 6
    # 7.i
    # 7.ii: No data  
    # 8.i
    # 8.ii.a
    # 8.ii.b: No data
    # 8.ii.c
    # 8.iii.a
    # 8.iii.b: No data
    # 8.iii.c: data_leg -> data_country
      # Listing workbooks

      rm(data.files)
  
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_8iiic"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_8iiic", x))
      })

      # Rename woksheets

      lapply(wb, function(x) renameWorksheet(x, "data_leg", "data_country"))

      # Change formula manually!!! [CHECK!]

      # Save

      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
          file = here("2025_RF_indicators", "Indicator_8iiic", data.files[i]),
          overwrite = TRUE
        )
      })

      rm(wb)
      
    # 9: No data
    # 10: No data
    # 11: No data
    # 12.i & 12.ii: data_country_grant -> data_country
      # Listing workbooks
      rm(data.files)
      
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_12i_12ii"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_12i_12ii", x))
      })

      # Rename woksheets

      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_country_grant",
          "data_country"
        )
      })

      # Change formula manually!!! [CHECK!]

      # Save

      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
          file = here(
            "2025_RF_indicators", "Indicator_12i_12ii",
            data.files[i]
          ),
          overwrite = TRUE
        )
      })

      rm(wb)
    
    # 13.i: No data
    # 13.ii: No data
    # 14.i.a: No data
    # 14.i.b: No data
    # 14.ii: No data
    # 15
    # 16.i: No data
    # 16.ii: No data
    # 16.iii: No data
    # 17 (data_policy??? currently not in database)
    # 18
  
  
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
  
# ├ Extract by sheet and merge -------------------------------------------------


indicators_db <- function(sheet_names) {
    
  p <- progressr::progressor(along = sheet_names) 
  
  future_lapply(seq_along(sheet_names), function(i) {
    
  file_list <- directory_excels %>% 
    fs::dir_ls(., recurse = TRUE, type = "file", glob = "*.xlsx") %>%
    # Only keep correct files, include indicator, exclude template
    fs::path_filter(., regexp = "*.template.xlsx$", invert = TRUE) %>%
    fs::path_filter(., regexp = "*.[0-9].xlsx$")
  
  
  indicator <- file_list %>% 
    map_dfr(~readWorkbook(.,
                       sheet = sheet_names[i],
                       colNames = TRUE,
                       skipEmptyRows = TRUE,
                       check.names = TRUE,
                       fillMergedCells = FALSE) %>% 
            mutate(across(.fns = as.character))
   , .id = "file_path")

  # exists(indicator_database_i)
  # is.environment(indicators_db)
  
  # assign(paste0("indicator_database_", i),
  #        indicator_database,
  #        envir = .GlobalEnv)  # saves to environment
  # assign()) does not work when running in parallel!!!
  
  # Exporting each db to specific workbook
  openxlsx::write.xlsx(indicator 
                       , here("2025_RF_indicators", 
                         paste0(sheet_names[i], ".xlsx"))
                       , overwrite = TRUE)
  
  # Signaling progression updates
  p(paste("Processing sheet", i, Sys.time(),"\t"))
  
  # return(paste0("indicator_",sheet_names[i]))
  
  }, future.seed = NULL #Ignore random numbers warning
  )
  
  invisible(gc(verbose = FALSE)) # Collecting garbage after each iteration
  
  } 

 indicators_db(sheet_names)
 

 # ├ 
 
