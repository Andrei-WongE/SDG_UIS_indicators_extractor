## -----------------------------------------------------------------------
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
## Date Updated: 2023-09-25
##
## Email: awongespejo@worldbank.org
#### Email: psoneja@worldbank.org
## -------------------------------------------------------------------------
##
## Notes: Needs input of users_db-V[x] file from 2025_RF_indicators.R
##
## ---------------------------

## Program Set-up ------------
 # .rs.restartR() # As to avoid conflicts with 2025_RF_indicators
 options(scipen = 100, digits = 4) # Prefer non-scientific notation

## Load required packages ----

 if (!require("pacman")) {
   install.packages("pacman")
 }
 pacman::p_load(here, dplyr, janitor, tidyverse, future.apply
                , progressr, openxlsx)

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
   
   files <- choose.files(here("2025_RF_indicators/..."))
   
 } else {
   
   m <- fs::path_file(files)
   message("The file that will be processed is ", paste(m))
   
 }
 
# Reading file, loadworkbook mantains formatting

 sheets <- openxlsx::getSheetNames(files)
 db <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = files, detectDates = TRUE)
 names(db) <- sheets

# Creating country/entities pairs
 country_pairs <-  data.frame( stringsAsFactors = FALSE
                             ,country = c( "Pakistan"
                                          ,"Pakistan"
                                          ,"Pakistan"
                                          ,"Pakistan"
                                          ,"Somalia"
                                          ,"Somalia"
                                          ,"Somalia"
                                          ,"Tanzania"
                                          ,"Tanzania"
                                        )
                             ,entity = c( "Balochistan"
                                         ,"Khyber Pakhtoonkhwa"
                                         ,"Punjab"
                                         ,"Sindh"
                                         ,"Federal"
                                         ,"Puntland"
                                         ,"Somaliland"
                                         ,"Mainland"
                                         ,"Zanzibar"
                                        )
                            )

 pair_function <- function(country_name, entity_name) {

   temp <- db[[1]][db[[1]][["country"]] %in% country_name, ]
   temp <- temp[temp$entity %in% c(entity_name, NA), ]
   temp$country <- paste(country_name, entity_name, sep = "-")

   return(temp)
 }
 
 result_list <- mapply( pair_function
                      , country_pairs$country
                      , country_pairs$entity
                      , SIMPLIFY = FALSE
                      , USE.NAMES = FALSE)
 
 result_data_frame <- do.call(rbind, result_list)
 db[["data_country"]] <- rbind(db[["data_country"]], result_data_frame)
 
 rm(result_list, result_data_frame)

# Pre processing diagnostics
 country <- sort(as.vector(unique(na.omit(db[[1]][["country"]])))
                 ,decreasing = FALSE)
 
 message( "The number of countries and country/entities pairs in"
         ," "
         ,"the data set is: "
         ,paste(length(country),"\n"))
 
 Sys.sleep(1)
 
 message("The countries are: "
         , paste(sapply(country, paste), "\n"))
 
 Sys.sleep(2)

 # Create output sub-folders
 sapply(paste0(output_directory, "/", country), dir.create)

# Columns from ind14 to delete 
 clean_ind14 <- c( "ind_14ia_PA1"
                  ,"ind_14ia_PA1_percentage_1"
                  ,"ind_14ia_PA1_allocated"
                  ,"ind_14ia_PA1_allocated_1"
                  ,"ind_14ia_PA2"
                  ,"ind_14ia_PA2_percentage_1"
                  ,"ind_14ia_PA2_allocated"
                  ,"ind_14ia_PA2_allocated_1"
                  ,"ind_14ia_PA3"
                  ,"ind_14ia_PA3_percentage_1"
                  ,"ind_14ia_PA3_allocated"
                  ,"ind_14ia_PA3_allocated_1"
                  ,"ind_14ia_PA4"
                  ,"ind_14ia_PA4_percentage_1"
                  ,"ind_14ia_PA4_allocated"
                  ,"ind_14ia_PA4_allocated_1"
                  ,"ind_14ia_PA5"
                  ,"ind_14ia_PA5_percentage_1"
                  ,"ind_14ia_PA5_allocated"
                  ,"ind_14ia_PA5_allocated_1"
                  ,"ind_14ia_PA6"
                  ,"ind_14ia_PA6_percentage_1"
                  ,"ind_14ia_PA6_allocated"
                  ,"ind_14ia_PA6_allocated_1"
                  ,"ind_14ia_PA7"
                  ,"ind_14ia_PA7_percentage_1"
                  ,"ind_14ia_PA7_allocated"
                  ,"ind_14ia_PA7_allocated_1"
                  ,"ind_14ia_PA8"
                  ,"ind_14ia_PA8_percentage_1"
                  ,"ind_14ia_PA8_allocated"
                  ,"ind_14ia_PA8_allocated_1"
                  ,"ind_14ib_PA1"
                  ,"ind_14ib_PA1_percentage_met"
                  ,"ind_14ib_PA1_allocated"
                  ,"ind_14ib_PA1_allocated_met"
                  ,"ind_14ib_PA2"
                  ,"ind_14ib_PA2_percentage_met"
                  ,"ind_14ib_PA2_allocated"
                  ,"ind_14ib_PA2_allocated_met"
                  ,"ind_14ib_PA3"
                  ,"ind_14ib_PA3_percentage_met"
                  ,"ind_14ib_PA3_allocated"
                  ,"ind_14ib_PA3_allocated_met"
                  ,"ind_14ib_PA4"
                  ,"ind_14ib_PA4_percentage_met"
                  ,"ind_14ib_PA4_allocated"
                  ,"ind_14ib_PA4_allocated_met"
                  ,"ind_14ib_PA5"
                  ,"ind_14ib_PA5_percentage_met"
                  ,"ind_14ib_PA5_allocated"
                  ,"ind_14ib_PA5_allocated_met"
                  ,"ind_14ib_PA6"
                  ,"ind_14ib_PA6_percentage_met"
                  ,"ind_14ib_PA6_allocated"
                  ,"ind_14ib_PA6_allocated_met"
                  ,"ind_14ib_PA7"
                  ,"ind_14ib_PA7_percentage_met"
                  ,"ind_14ib_PA7_allocated"
                  ,"ind_14ib_PA7_allocated_met"
                  ,"ind_14ib_PA8"
                  ,"ind_14ib_PA8_percentage_met"
                  ,"ind_14ib_PA8_allocated"
                  ,"ind_14ib_PA8_allocated_met"
                  
                  ,"grant_report_submission_date"
                  ,"ind_14ib_grant_report_submission_date"
                  ,"data_year_cumulative"
                  )
 
# Parallel Processing set-up
 plan(multisession)
 nbrOfWorkers()
 
# Customization of how progress is reported
 handlers(global = TRUE)
 handlers(
   handler_progress( format = ":spin [:bar] :percent in :elapsed ETA: :eta"
                    , width    = 60
                    , complete = "+"
 )
 )
 
users_db <- function(country) {
   
   p <- progressr::progressor(along = country)
   
      future_lapply(seq_along(country), function(i) {

     # Subset A: by country, data_country sheet
       DF <- db

       DF[[1]] <- db[[1]][db[[1]][["country"]] %in% country[i],] |>
                  janitor::remove_empty(which = c("cols"), quiet = TRUE)

       subset_ind <- DF[[1]][["id"]]

     # Subset B: by indicator, data_aggregate and metadata sheet
       DF[[2]] <- db[[2]][db[[2]][["id"]] %in% subset_ind,]
       DF[[3]] <- db[[3]][db[[3]][["id"]] %in% subset_ind,]

     # Clean data
      #Delete unnecessary columns specific for data_country
        DF[[1]] <- DF[[1]] |> 
           dplyr::select(!any_of(c("iso", "region", "income_group", "pcfc"))) |>
           dplyr::select(!ends_with("_m")) |>
           dplyr::select(!num_range(prefix = "_wq", range = 2:4))
        
      #Delete unnecessary columns of ind_14
        DF[[1]] <- DF[[1]] |> 
           dplyr::select(!any_of(clean_ind14))

      #Delete unnecessary and empty columns, ALL sheets
       vect <- seq(1,3)
       clean_func <- function(x) {
          DF[[x]] <- DF[[x]] |>
           select(!c("id", "data_update")) 
          # |>
          #  remove_empty(which = c("cols"), quiet = TRUE) 
       }

       DF <- lapply(vect, clean_func)

      #Delete duplicates in metadata
       DF[[3]] <- DF[[3]] |>
         select(!c("ind_year")) |>
         dplyr::distinct()
       
       DF[[3]] <- DF[[3]] %>%
         filter(!((var_name %in% c("country", "entity","region", "income_group", "pcfc", "iso")) & (ind_id != "4i")))
         
       
       DF[[3]] <- DF[[3]] %>%
         filter(!((var_name %in% c("income_level") & (ind_id == "15"))))
      
       # Indicators that need to be rounded to 2 decimal places.
         round_indicators <- c("indi_2"
                               ,"indi_2_f"
                               ,"base_education_share"
                               ,"current_education_share"
                               ,"indi_3ia"
                             ,"indi_3ia_f"
                             ,"indi_3ib"
                             ,"indi_3ib_f"
                             ,"indi_3iia"
                             ,"indi_3iia_f"
                             ,"indi_3iia_rural"
                             ,"indi_3iia_urban"
                             ,"indi_3iia_q1"
                             ,"indi_3iia_q2"
                             ,"indi_3iia_q3"
                             ,"indi_3iia_q4"
                             ,"indi_3iia_q5"
                             ,"indi_3iib"
                             ,"indi_3iib_f"
                             ,"indi_3iib_rural"
                             ,"indi_3iib_urban"
                             ,"indi_3iib_q1"
                             ,"indi_3iib_q2"
                             ,"indi_3iib_q3"
                             ,"indi_3iib_q4"
                             ,"indi_3iib_q5"
                             ,"indi_3iic"
                             ,"indi_3iic_f"
                             ,"indi_3iic_rural"
                             ,"indi_3iic_urban"
                             ,"indi_3iic_q1"
                             ,"indi_3iic_q2"
                             ,"indi_3iic_q3"
                             ,"indi_3iic_q4"
                             ,"indi_3iic_q5"
                             ,"indi_5i"
                             ,"indi_6ai"
                             ,"indi_6ai_f"
                             ,"indi_6aii"
                             ,"indi_6aii_f"
                             ,"indi_6bi"
                             ,"indi_6bi_f"
                             ,"indi_6bii"
                             ,"indi_6bii_f"
                             ,"indi_6ci"
                             ,"indi_6ci_f"
                             ,"indi_6cii"
                             ,"indi_6cii_f"
                             ,"indi_7ia"
                             ,"indi_7ia_f"
                             ,"indi_7ib"
                             ,"indi_7ib_f"
                             ,"indi_7ic"
                             ,"indi_7ic_f"
                             ,"indi_7id"
                             ,"indi_7id_f"
       )
       
      round_indicators_country <- intersect(round_indicators,names(DF[[1]]))
      
         DF[[1]][round_indicators_country] <- lapply( DF[[1]][round_indicators_country]
                                                , function(x) replace(
                                                  round( as.numeric(x), 2)
                                                  , is.na(x)
                                                  , ""
                                                )
         )
         
     # Transpose data_country sheet
       # DF[[1]] <- group_by(ind_id, ind_year) |>
       #   select(!c("iso", "region", "income_group", "pcfc")) |>
       #   pivot_longer( id_cols = c(ind_id, ind_year)
       #                ,names_from  = ind_year
       #                ,values_from = DF[!c(ind_id, ind_year)]
       #   )
       n_rows_1 <- nrow(DF[[1]])
       n_rows_2 <- nrow(DF[[2]])
       #Considering sheet +1 due to index sheet inserted later on
       n_cols_2 <- ncol(DF[[1]])
       n_cols_3 <- ncol(DF[[2]])
       n_cols_4 <- ncol(DF[[3]])

     # Create workbook
       wb <- openxlsx::createWorkbook()
       
     # Write data to workbook
       purrr::imap(
         .x = DF,
         .f = function(df, object_name) {
           openxlsx::addWorksheet(wb = wb, sheetName = object_name)
           openxlsx::writeData(wb = wb, sheet = object_name, x = df)
         }
       )
       
       temp <- tempfile(pattern = "c_temp", fileext = ".xlsx")
       openxlsx::saveWorkbook(wb = wb, file = temp)
       
     # Adding index sheet
       wb2 <- openxlsx::loadWorkbook(here("2025_RF_indicators"
                                          ,"index.xlsx"))

     # Write country name in index sheet, in cell (C,7)
       openxlsx::writeData( wb    =  wb2
                          , sheet = "index"
                          , x     = country[i]
                          , xy    = c(3,7)
                          )

     # Insert GPE image
       openxlsx::insertImage( wb2
                            , sheet = "index"
                            , file  = here("2025_RF_indicators"
                                          , "GPE.PNG")
                            , width = 2
                            , height = 0.8
                            , startRow = 1
                            , startCol = 2
                            , units = "in"
                            , dpi = 300
                            )

    # Add databases to index sheet (inefficient code as appending workbook objects not possible ATM)
       
       lapply(names(wb), function(s) {
         dt <- openxlsx::read.xlsx(temp, sheet = s, detectDates = TRUE)
         openxlsx::addWorksheet(wb2 , sheetName = s)
         openxlsx::writeData(wb2, s, dt)
       })
       
       names(wb2) <- c( "index"
                      , "data_country"
                      , "data_aggregate"
                      , "metadata"
                      )

     # Formatting 
       #Set worksheet gridlines to hide
        openxlsx::showGridLines( wb            = wb2
                               , sheet         = "index"
                               , showGridLines = FALSE
                               )

        #Numeric formatting
        #options("openxlsx.numFmt" = "0.00") # 2 decimal cases formatting
  
        # numeric_style_1 <- createStyle(numFmt = "#,##0")
        # addStyle( wb2, 2, style = numeric_style_1
        #                 , rows = 2:n_rows_1
        #                 , cols = 3:16
        #                 , gridExpand = TRUE
        #         )
  
        numeric_style_2 <- createStyle(numFmt = "#,#0.00") 
        addStyle( wb2, 2, style = numeric_style_2
                        , rows  = 2:n_rows_2
                        , cols  = 6:7
                        , gridExpand = TRUE
                )
  
        #Header formatting sheets 2 to 4: black border + bold header
        header_style <- createStyle( halign = "CENTER"
                                    ,valign = "CENTER"
                                    ,border = "TopBottomLeftRight"
                                    ,borderColour = "black"
                                    # ,wrapText = TRUE
                                   )

        for (x in 2:4) {

          end_column <- get(paste0("n_cols_", x))
          addStyle(wb2, x, style = header_style 
                   , rows  = 1
                   , cols  = 1:end_column
                   , gridExpand = TRUE)
        }

     # Saving workbook by country in each sub-folder
       folder <- country[i]

       openxlsx::saveWorkbook( wb = wb2
                             , here("2025_RF_indicators/Countries_db"
                             , folder
                             , paste0(country[i],".xlsx"))
                             , overwrite = TRUE
                             )

    # Signaling progression updates
      p(paste("Processing country", country[i], Sys.time(), "\t"))

    # Collecting garbage after each iteration
      invisible(gc(verbose = FALSE, reset = TRUE)) 

    rm(DF, wb)

   }, future.seed  = NULL #Ignore random numbers warning
   )
 }
 
 users_db(country)
 
