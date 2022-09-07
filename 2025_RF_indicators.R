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
  pacman::p_load(here, dplyr, tidyverse, DescTools, openxlsx, fs, future.apply,
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
    # 7.ii: MOCK DATA! Manually modified file name.
    # 8.i
    # 8.ii.a
    # 8.ii.b: No data
    # 8.ii.c
    # 8.iii.a
    # 8.iii.b: No data
    # 8.iii.c: data_leg -> data_country
      # Listing workbooks
      
      # rm(data.files)
      # 
      # data.files <- list.files(
      #   path = here("2025_RF_indicators", "Indicator_8iiic"),
      #   pattern = paste0("*.xlsx"),
      #   recursive = TRUE
      # )
      # # Removing element form list, template
      # 
      # pattern <- "template"
      # data.files <- data.files[-grep(pattern, data.files)]
      # 
      # # Loading workbooks
      # 
      # wb <- lapply(data.files, function(x) {
      #   loadWorkbook(here("2025_RF_indicators", "Indicator_8iiic", x))
      # })
      # 
      # # Rename woksheets
      # 
      # lapply(wb, function(x) renameWorksheet(x, "data_leg", "data_country"))
      # 
      # # Save
      # 
      # lapply(seq_along(wb), function(i) {
      #   saveWorkbook(wb[[i]],
      #     file = here("2025_RF_indicators", "Indicator_8iiic", data.files[i]),
      #     overwrite = TRUE
      #   )
      # })
      # 
      # rm(wb)
    # 9: No data
    # 10: No data
    # 11: No data
    # 12.i & 12.ii: data_country_grant -> data_country
      #Listing workbooks
      # rm(data.files)
      # 
      # data.files <- list.files(
      #   path = here("2025_RF_indicators", "Indicator_12i_12ii"),
      #   pattern = paste0("*.xlsx"),
      #   recursive = TRUE
      # )
      # # Removing element form list, template
      # 
      # pattern <- "template"
      # data.files <- data.files[-grep(pattern, data.files)]
      # 
      # # Loading workbooks
      # 
      # wb <- lapply(data.files, function(x) {
      #   loadWorkbook(here("2025_RF_indicators", "Indicator_12i_12ii", x))
      # })
      # 
      # # Rename woksheets
      # 
      # lapply(wb, function(x) {
      #   renameWorksheet(
      #     x,
      #     "data_country_grant",
      #     "data_country"
      #   )
      # })
      # 
      # # Save
      # 
      # lapply(seq_along(wb), function(i) {
      #   saveWorkbook(wb[[i]],
      #     file = here(
      #       "2025_RF_indicators", "Indicator_12i_12ii",
      #       data.files[i]
      #     ),
      #     overwrite = TRUE
      #   )
      # })
      # 
      # rm(wb)
    # 13.i: No data
    # 13.ii: No data
    # 14.i.a: MOCK DATA! Manually modified file name
    # 14.i.b: data_country_grant -> data_country
      #Listing workbooks
      # rm(data.files)
      # 
      # data.files <- list.files(
      #   path = here("2025_RF_indicators", "Indicator_14ib"),
      #   pattern = paste0("*.xlsx"),
      #   recursive = TRUE
      # )
      # # Removing element form list, template
      # 
      # pattern <- "template"
      # data.files <- data.files[-grep(pattern, data.files)]
      # 
      # # Loading workbooks
      # 
      # wb <- lapply(data.files, function(x) {
      #   loadWorkbook(here("2025_RF_indicators", "Indicator_14ib", x))
      # })
      # 
      # # Rename woksheets
      # 
      # lapply(wb, function(x) {
      #   renameWorksheet(
      #     x,
      #     "data_country_grant",
      #     "data_country"
      #   )
      # })
      # 
      # # Save
      # 
      # lapply(seq_along(wb), function(i) {
      #   saveWorkbook(wb[[i]],
      #                file = here(
      #                  "2025_RF_indicators", "Indicator_14ib",
      #                  data.files[i]
      #                ),
      #                overwrite = TRUE
      #   )
      # })
      # 
      # rm(wb)  
      
    # 14.ii: No data
    # 15: Accumulated data! Only upload last year data!
    # 16.i: No data
    # 16.ii: No data
    # 16.iii: data_country_grant -> data_country
      #Listing workbooks
      # rm(data.files)
      # 
      # data.files <- list.files(
      #   path = here("2025_RF_indicators", "Indicator_16iii"),
      #   pattern = paste0("*.xlsx"),
      #   recursive = TRUE
      # )
      # # Removing element form list, template
      # 
      # pattern <- "template"
      # data.files <- data.files[-grep(pattern, data.files)]
      # 
      # # Loading workbooks
      # 
      # wb <- lapply(data.files, function(x) {
      #   loadWorkbook(here("2025_RF_indicators", "Indicator_16iii", x))
      # })
      # 
      # # Rename woksheets
      # 
      # lapply(wb, function(x) {
      #   renameWorksheet(
      #     x,
      #     "data_country_grant",
      #     "data_country"
      #   )
      # })
      # 
      # # Save
      # 
      # lapply(seq_along(wb), function(i) {
      #   saveWorkbook(wb[[i]],
      #     file = here(
      #       "2025_RF_indicators", "Indicator_16iii",
      #       data.files[i]
      #     ),
      #     overwrite = TRUE
      #   )
      # })
      # 
      # rm(wb)
    # 17 (data_policy??? currently not in database)
    # 18 (data_donor -> data_country)
      # #Listing workbooks
      # suppressWarnings(rm(data.files))
      # 
      # data.files <- list.files(
      #   path = here("2025_RF_indicators", "Indicator_18"),
      #   pattern = paste0("*.xlsx"),
      #   recursive = TRUE
      # )
      # # Removing element form list, template
      # 
      # pattern <- "template"
      # data.files <- data.files[-grep(pattern, data.files)]
      # 
      # # Loading workbooks
      # 
      # wb <- lapply(data.files, function(x) {
      #   loadWorkbook(here("2025_RF_indicators", "Indicator_18", x)) 
      # })
      #   
      # 
      # # Rename woksheets
      # 
      # lapply(wb, function(x) {
      #   renameWorksheet(
      #     x,
      #     "data_donor",
      #     "data_country"
      #   )
      # })
      # 
      # # Save
      # lapply(seq_along(wb), function(i) {
      #   saveWorkbook(wb[[i]],
      #     file = here(
      #       "2025_RF_indicators", "Indicator_18",
      #       data.files[i]
      #     ),
      #     overwrite = TRUE
      #   )
      # })
      # 
      # rm(wb)
  
## Parallel Processing set-up --------------------------------------------------

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
  
## Extract by sheet and merge -------------------------------------------------

indicators_db <- function(sheet_names) {
    
  p <- progressr::progressor(along = sheet_names)
  # Delete previous file
  unlink(list.files(here("2025_RF_indicators")
                         , pattern = "[indicators_db]-"
                         , full.names = TRUE))
  
  # Generating list of paths  
  file_list <- directory_excels %>% 
    fs::dir_ls(., recurse = TRUE, type = "file", glob = "*.xlsx") %>%
    # Only keep correct files, include indicator, exclude template
    fs::path_filter(., regexp = "*.template.xlsx$", invert = TRUE) %>%
    fs::path_filter(., regexp = "*.[0-9].xlsx$") %>%
    fs::path_filter(., regexp = "(PCFC).*.xlsx$", invert = TRUE)
    
  
  db <- future_lapply(seq_along(sheet_names), function(i) {
  
  # Creating database for each sheet
  indicator <- file_list %>% 
    map_dfr(~openxlsx::readWorkbook(.
                          , sheet = sheet_names[i]
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
                         , remove = FALSE
                         , extra = "warn") 
  
  indicator <- indicator %>%
              select(!file_path) %>%
              mutate(data_update = format(Sys.Date())) %>%
              dplyr::relocate(c("id", "ind_id", "ind_year"))
  
  # Cleaning database 
  values_delete <- c("Technical%", "Notes%") # Thanks DescTools!
  
  if (sheet_names[i] == "data_aggregate") {
  
    indicator <- indicator[!(indicator$indicator %like any% values_delete), ]

  }
  
  if (sheet_names[i] == "metadata") {
    
    indicator <- indicator[!(indicator$var_name %like any% values_delete), ]
    
  }

  # exists("indicator")
  
  # Collecting databases in a list
  list_db <- list(indicator)
  
  # Combining elements of list such as to maintain column headers 
  n_r <- seq_len(max(sapply(list_db, nrow)))
  db <- do.call(cbind, lapply(list_db, function(x) x[n_r, , drop = FALSE]))
  
  # Signaling progression updates
  p(paste("Processing sheet", sheet_names[i], Sys.time(), "\t"))
  
  # Collecting garbage after each iteration
  invisible(gc(verbose = FALSE, reset = TRUE)) 
  
  return(db)
  
  }, future.seed  = NULL #Ignore random numbers warning
  )
  
  names(db) <- sheet_names

  openxlsx::write.xlsx(db
                       , here("2025_RF_indicators",
                              paste("indicators_db-V0.75.xlsx", sep = "_"))
                       , sheetName = names(db)
                       , colNames = TRUE
  )
  # Listing indicators in the database
  ind_final <- sort(as.vector(unique(db[[1]][["ind_id"]])))
  message(paste("The processed indicators are:","\n") 
          , paste(sapply(ind_final, paste), "\n"))
  
  rm(db)
  }

  indicators_db(sheet_names)

 # ├ Cleaning and setting-up data ----------------------------------------------
   
   # # Create a blank workbook
   # wb <- openxlsx::createWorkbook()
   # 
   # # Add sheets to the workbook
   # openxlsx::addWorksheet(wb, paste(sheet_names[i]))
   # 
   # # Exporting each db to specific workbook
   # openxlsx::writeData(wb
   #                      , sheet = sheet_names[i]
   #                      , x = indicator)
   # # Export the file
   # openxlsx::saveWorkbook(wb
   #                        , here("2025_RF_indicators",
   #                       "indicators_db.xlsx")
   #                        , overwrite = TRUE)
   # Map(function(data, name){ 
   #   
   #   openxlsx::addWorksheet(wb, name)
   #   openxlsx::writeData(wb, name, data)
   #   
   # }, list, names(list))
 
