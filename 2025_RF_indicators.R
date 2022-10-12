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
                 progressr, utils, janitor)

## Runs the following --------

  
## Import and clean data -------------------------------------------------------
  
  directory_excels <- here("2025_RF_indicators")
  # Checking content
  fs::dir_tree(directory_excels)
  
  sheet_names <- c("data_country", "data_aggregate", "metadata")
  # sheet_names <- c(1, 2, 3)
    
  # Checking sheet names! Data as of 2022/09/19
  clean_sheet_names <- utils::askYesNo("Do you want to clean sheet name? (y/n)? "
                                      , default = FALSE
                                      , prompts = getOption("askYesNo"
                                                            , gettext(
                                                              c("Yes"
                                                               ,"No"
                                                               ,"Cancel")
                                                                    )
                                                            )
                                       )

    if (clean_sheet_names != FALSE) {
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
    # 7.ii:
    # 8.i
    # 8.ii.a
    # 8.ii.b: No data
    # 8.ii.c
    # 8.iii.a
    # 8.iii.b: No data
    # 8.iii.c: data_leg -> data_country
      # Listing workbooks
      suppressWarnings(rm(data.files))
  
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
      suppressWarnings(rm(data.files))
  
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
    # 14.i.a: data_country_grant -> data_country
      #Listing workbooks
      suppressWarnings(rm(data.files))
      
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_14ia"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_14ia", x))
      })

      # Rename woksheets

      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_country_grant",
          "data_country"
        )
      })

      # Save

      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
                     file = here(
                       "2025_RF_indicators", "Indicator_14ia",
                       data.files[i]
                     ),
                     overwrite = TRUE
        )
      })
      rm(wb)
  
    # 14.i.b: data_country_grant -> data_country
       # Listing workbooks
      suppressWarnings(rm(data.files))
      
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_14ib"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_14ib", x))
      })

      # Rename woksheets

      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_country_grant",
          "data_country"
        )
      })

      # Save

      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
                     file = here(
                       "2025_RF_indicators", "Indicator_14ib",
                       data.files[i]
                     ),
                     overwrite = TRUE
        )
      })

      rm(wb)
      
    # 14.ii: No data
    # 15: Accumulated data! Only upload last year data!
    # 16.i: No data
    # 16.ii: No data
    # 16.iii: data_country_grant -> data_country
       # Listing workbooks
      suppressWarnings(rm(data.files))
      
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_16iii"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_16iii", x))
      })

      # Rename woksheets

      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_country_grant",
          "data_country"
        )
      })

      # Save

      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
          file = here(
            "2025_RF_indicators", "Indicator_16iii",
            data.files[i]
          ),
          overwrite = TRUE
        )
      })

      rm(wb)
    # 17: data_country_unique -> data_country
      #Listing workbooks
      suppressWarnings(rm(data.files))
      
      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_17"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template
      
      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]
      
      # Loading workbooks
      
      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_17", x))
      })
      
      
      # Rename woksheets
      
      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_country_unique",
          "data_country"
        )
      })
      
      # Save
      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
                     file = here(
                       "2025_RF_indicators", "Indicator_17",
                       data.files[i]
                     ),
                     overwrite = TRUE
        )
      })
      
      rm(wb)
      
    # 18: data_donor -> data_country
      #Listing workbooks
      suppressWarnings(rm(data.files))

      data.files <- list.files(
        path = here("2025_RF_indicators", "Indicator_18"),
        pattern = paste0("*.xlsx"),
        recursive = TRUE
      )
      # Removing element form list, template

      pattern <- "template"
      data.files <- data.files[-grep(pattern, data.files)]

      # Loading workbooks

      wb <- lapply(data.files, function(x) {
        loadWorkbook(here("2025_RF_indicators", "Indicator_18", x))
      })


      # Rename woksheets

      lapply(wb, function(x) {
        renameWorksheet(
          x,
          "data_donor",
          "data_country"
        )
      })

      # Save
      lapply(seq_along(wb), function(i) {
        saveWorkbook(wb[[i]],
          file = here(
            "2025_RF_indicators", "Indicator_18",
            data.files[i]
          ),
          overwrite = TRUE
        )
      })

      rm(wb)
  }
  
  # Indicators that need 2b formatted to comma separated thousands, 2 digit
   formatting_integers <- c("ind_14ia_PA1_percentage_1"
                          ,"ind_14ia_PA1_allocated"
                          ,"ind_14ia_PA1_allocated_1"
                          ,"ind_14ia_PA2_percentage_1"
                          ,"ind_14ia_PA2_allocated"
                          ,"ind_14ia_PA2_allocated_1"
                          ,"ind_14ia_PA3_percentage_1"
                          ,"ind_14ia_PA3_allocated"
                          ,"ind_14ia_PA3_allocated_1"
                          ,"ind_14ia_PA4_percentage_1"
                          ,"ind_14ia_PA4_allocated"
                          ,"ind_14ia_PA4_allocated_1"
                          ,"ind_14ia_PA5_percentage_1"
                          ,"ind_14ia_PA5_allocated"
                          ,"ind_14ia_PA5_allocated_1"
                          ,"ind_14ia_PA6_percentage_1"
                          ,"ind_14ia_PA6_allocated"
                          ,"ind_14ia_PA6_allocated_1"
                          ,"ind_14ia_PA7_percentage_1"
                          ,"ind_14ia_PA7_allocated"
                          ,"ind_14ia_PA7_allocated_1"
                          ,"ind_14ia_PA8_percentage_1"
                          ,"ind_14ia_PA8_allocated"
                          ,"ind_14ia_PA8_allocated_1"
                          # ,"ind_14ib_PA1_allocated"
                          # ,"ind_14ib_PA1_allocated_met"
                          # ,"ind_14ib_PA3_allocated"
                          # ,"ind_14ib_PA3_allocated_met"
                          # ,"ind_14ib_PA4_allocated"
                          # ,"ind_14ib_PA4_allocated_met"
                          # ,"ind_14ib_PA5_allocated"
                          # ,"ind_14ib_PA5_allocated_met"
                          # ,"ind_14ib_PA6_allocated"
                          # ,"ind_14ib_PA6_allocated_met"
                          # ,"ind_14ib_PA7_allocated"
                          # ,"ind_14ib_PA7_allocated_met"
                          # ,"ind_14ib_PA8_allocated"
                          # ,"ind_14ib_PA8_allocated_met"
                          ,"pledged_amount_local_currency"
                          ,"pledged_amount_USD"
                          ,"pledge_fulfillment_local_currency"
                          ,"pledge_fulfillment_USD"
                          ,"pledge_fulfillment_percentage"
                          ,"indi_2"
                          ,"indi_2_f"
                          ,"indi_2_pop"
                          ,"indi_2_f_pop"
                          ,"indi_3ia"
                          ,"indi_3ia_f"
                          ,"indi_3ib"
                          ,"indi_3ib_f"
                          ,"indi_3ia_pop"
                          ,"indi_3ia_f_pop"
                          ,"indi_3ib_pop"
                          ,"indi_3ib_f_pop"
                          ,"indi_3iia"
                          ,"indi_3iia_f"
                          ,"indi_3iia_q1"
                          ,"indi_3iia_q2"
                          ,"indi_3iia_q3"
                          ,"indi_3iia_q4"
                          ,"indi_3iia_q5"
                          ,"indi_3iia_rural"
                          ,"indi_3iia_urban"
                          ,"indi_3iia_pop"
                          ,"indi_3iia_f_pop"
                          ,"indi_3iib"
                          ,"indi_3iib_f"
                          ,"indi_3iib_q1"
                          ,"indi_3iib_q2"
                          ,"indi_3iib_q3"
                          ,"indi_3iib_q4"
                          ,"indi_3iib_q5"
                          ,"indi_3iib_rural"
                          ,"indi_3iib_urban"
                          ,"indi_3iib_pop"
                          ,"indi_3iib_f_pop"
                          ,"indi_3iic"
                          ,"indi_3iic_f"
                          ,"indi_3iic_q1"
                          ,"indi_3iic_q2"
                          ,"indi_3iic_q3"
                          ,"indi_3iic_q4"
                          ,"indi_3iic_q5"
                          ,"indi_3iic_rural"
                          ,"indi_3iic_urban"
                          ,"indi_3iic_pop"
                          ,"indi_3iic_f_pop"
                          ,"base_education_share"
                          ,"current_education_share"
                          ,"indi_5i"
                          ,"indi_5i_pop"
                          ,"indi_6aii"
                          ,"indi_6aii_f"
                          ,"indi_6cii"
                          ,"indi_6cii_f"
                          ,"indi_6bii"
                          ,"indi_6bii_f"
                          ,"indi_6ai"
                          ,"indi_6ai_f"
                          ,"indi_6ci"
                          ,"indi_6ci_f"
                          ,"indi_6bi"
                          ,"indi_6bi_f"
                          ,"indi_6_primarypop"
                          ,"indi_6_f_primarypop"
                          ,"indi_6_lsecondarypop"
                          ,"indi_6_f_lsecondarypop"
                          ,"indi_7i_primarypop"
                          ,"indi_7i_f_primarypop"
                          ,"indi_7ia"
                          ,"indi_7ia_f"
                          ,"indi_7ib"
                          ,"indi_7ib_f"
                          ,"indi_7ic"
                          ,"indi_7ic_f"
                          ,"indi_7id"
                          ,"indi_7id_f"
                          )

  # Indicators that need 2b formatted from excel dates to date
   formatting_date <- c("grant_start_date"
                       ,"grant_report_submission_date"
                       ,"grant_closing_date"
                       ,"EOI_approval_date"
                       )

  # Get Date Origin for data conversion
   DateOrigin <- getDateOrigin(here("2025_RF_indicators"
                                   ,"Indicator_1"
                                   ,"CY2020"
                                   ,"GPE2025_indicator-1-CY2020.xlsx"
                                   ))

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
  
## Extract by sheet and merge --------------------------------------------------

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

  # Cleaning database and order variables
  values_delete <- c("Technical%", "Notes%") # Thanks DescTools!

  if (sheet_names[i] == "data_country") {

    indicator <- indicator |> 
                 dplyr::relocate( "entity"
                                , .after = "country"
                                ) |>
                 dplyr::relocate( "data_year"
                                , .before = "data_update"
                                )

  #Formatting dates
  indicator[(formatting_date)] <- lapply(indicator[(formatting_date)]
                                        , openxlsx::convertToDate
                                        , optional = TRUE
                                        , orgin    = DateOrigin
                                        )

  indicator[is.na(formatting_date)] <- ""

  #Formatting integers
  indicator[formatting_integers] <- lapply( indicator[formatting_integers]
                                            , function(x) replace(
                                              format( as.numeric(x)
                                                    , nsmall     = 2
                                                    , big.mark   = ","
                                                    , scientific = FALSE)
                                                                     , is.na(x)
                                                                     , ""
                                                                  )
                                          )

  indicator$indi_1_pop <- replace( format( as.numeric(indicator$indi_1_pop)
                                             , nsmall     = 0
                                             , big.mark   = ","
                                             , scientific = FALSE)
                                     , is.na(indicator$indi_1_pop)
                                     , ""
  )

  indicator$grant_amount <- replace( format( as.numeric(indicator$grant_amount)
                                              , nsmall     = 0
                                              , big.mark   = ","
                                              , scientific = FALSE)
                                    , is.na(indicator$grant_amount)
                                    , ""
                                   )

  #Due to check.names = TRUE, X in front of colnames start with number
  # names(indicator)[names(indicator) == 
  #                 `4i_increase_or_maintained`] <- "X4i_increase_or_maintained"

    }

  if (sheet_names[i] == "data_aggregate") {

    indicator <- indicator[!(indicator$indicator %like any% values_delete), ]

    #Formatting integers
    indicator["value2"] <- indicator["value"]

    indicator$value <- replace(
                          format(round(as.numeric(indicator$value), 2)
                                       , nsmall     = 0
                                       , big.mark   = ","
                                       , scientific = FALSE)
                      , is.na(indicator$value)
                      , ""
                              )

    #Formatting different text values in an integer column
    indicator$value[indicator$value2 == "n.a."] <- "n.a."
    indicator$value[indicator$value2 == "n/a"]  <- "n/a"

    indicator["value2"] <- NULL

  }

  if (sheet_names[i] == "metadata") {

    indicator <- indicator[!(indicator$var_name %like any% values_delete), ] |>
      filter(!(var_name %in% "PCFC")) 

  }

  # exists("indicator")

  # Collecting databases in a list
  list_db <- list(indicator)

  # Combining elements of list such as to maintain column headers 
  n_r <- seq_len(max(sapply(list_db, nrow)))
  db  <- do.call(cbind, lapply(list_db, function(x) x[n_r, , drop = FALSE]))

  # Signaling progression updates
  p(paste("Processing sheet", sheet_names[i], Sys.time(), "\t"))

  # Collecting garbage after each iteration
  invisible(gc(verbose = FALSE, reset = TRUE)) 

  return(db)

  }, future.seed  = NULL #Ignore random numbers warning
  )
 
  # Save as one excel file with named sheets
  names(db) <- sheet_names

  # Creating header
  # for (i in 1:3) {
  #   names(db[[i]][[i]]) <- db[[i]][[i]] %>% 
  #                          slice(1) %>% 
  #                          janitor::make_clean_names()
  # 
  #   db[[i]][[i]] %>% slice(-1)
  # 
  # }

  #Numeric formatting
  options("openxlsx.numFmt" = "0.00") # 2 decimal cases formatting

  #Saving file
  openxlsx::write.xlsx( db
                      , here("2025_RF_indicators",
                              paste("indicators_db-V0.97.xlsx", sep = "_"))
                      , sheetName = names(db)
                      , colNames  = TRUE #To avoid having green flags in excel
                      # , colWidths = "auto"
                      )

  # Listing indicators in the database
  ind_final <- sort(as.vector(unique(db[[1]][["ind_id"]])))

  message( paste("The processed indicators are:", "\n") 
         , paste(sapply(ind_final, paste), "\n"))

  rm(db)
  }

  indicators_db(sheet_names)

