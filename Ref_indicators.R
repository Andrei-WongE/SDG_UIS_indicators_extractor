
## ---------------------------
##
## Script name: SDG_UIS_indicators
##
## Project: GPE
##
## Purpose of script: Automatize extraction of indicators
##
## Author: Andrei Wong Espejo
##
## Date Created: 2022-06-08
##
## Email: awongespejo@worldbank.org
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

## Program Set-up ------------

# ├ Overall settings ----

options(scipen = 100, digits = 4) # Prefer non-scientific notation

# ├ Load required packages ----

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr, tidyverse, tidylog, openxlsx, here, purrr, listviewer, fs)

## Runs the following ----------------------------------------------------------

# Extracts, merges, subsets and saves SDG indicators by GPE aggregation

## Loading databases -----------------------------------------------------------

ind_ref <- readWorkbook(here("SDG_related_indicators", 
                               "ref_indicators_UIS.xlsx"),
                          colNames = TRUE,
                          skipEmptyRows = TRUE,
                          check.names = TRUE,
                          fillMergedCells = FALSE
                        ) |> 
                          select(c(gpe, label, gpe_name))
    
    
ind_db <-  readWorkbook(here("SDG_related_indicators", 
                               "1.gpe_data_national_REPORT.xlsx"),
                          colNames = TRUE,
                          skipEmptyRows = TRUE,
                          check.names = TRUE,
                          fillMergedCells = FALSE
                        ) |> 
                          select(c(indicator_id, country_id, year, value, country_name_en, indicator_label_en))
  
  # pcfc_2020 <- readWorkbook(here("SDG_related_indicators", 
  #                                "df_unesco_gpe_2020.xlsx"),
  #                           colNames = TRUE,
  #                           skipEmptyRows = TRUE,
  #                           check.names = TRUE,
  #                           fillMergedCells = FALSE
  #                        ) 
    
pcfc_2021 <- readWorkbook(here("SDG_related_indicators", 
                                 "df_unesco_gpe_2021.xlsx"),
                            colNames = TRUE,
                            skipEmptyRows = TRUE,
                            check.names = TRUE,
                            fillMergedCells = FALSE
                          )

## Preparing data for function -------------------------------------------------

db <- left_join(ind_db, ind_ref, by = c("indicator_label_en" = "label")) |>
    filter(!(gpe %in% "GPE indicator 1")) |> 
    rename("iso" = "country_id") |> 
    select(!c(indicator_label_en, gpe_name))
    
  # Check list of gpe indicators
  
db |> distinct(gpe) |> 
    count()   # 28 UIS indicators, as indicator 1 was dropped 
  

# NOTE: Using PCFC 2021 for reporting years 2020 and 2021 as per UIS directive.


## Set up the folder structure -------------------------------------------------

folder_structure <- (
  #Main folder
  c("rf_indicators"
    # Sub-folders
    ,"rf_indicators/Indicator_2"   
    ,"rf_indicators/Indicator_3i" 
    ,"rf_indicators/Indicator_3ii"
    ,"rf_indicators/Indicator_6"  
    ,"rf_indicators/Indicator_7i"
    ,"rf_indicators/Indicator_8i"  
  ))

for (j in seq_along(folder_structure)) {
  dir.create(folder_structure[j])
}

directory_excels <- as.list(folder_structure)
directory_excels[1] <- NULL # Deleting main folder

## Bundling UIS indicators into GPE indicators ---------------------------------

db <- db |> mutate(gpe_new = substr(gpe, 1, 15)) 

  # Bundling conditions

    #group_2 <- #All 2
    
    #group_3i <- c("GPE indicator 3ia"
    #              ,"GPE indicator 3ib")
    
    #group_3ii <- c("GPE indicator 3iia"
    #               ,"GPE indicator 3iib"
    #               ,"GPE indicator 3iic")
    
    #group_6 <- #All 6
    
    #group_7 <- #All 7
    
    #group_8 <- #ALL 8

db <- db |>
  mutate(gpe_new = case_when( gpe == "GPE indicator 3ia"  ~ "GPE indicator 3i"
                             ,gpe == "GPE indicator 3iia" ~ "GPE indicator 3ii"
                             ,gpe == "GPE indicator 3iib" ~ "GPE indicator 3ii"
                             ,gpe == "GPE indicator 3iic" ~ "GPE indicator 3ii"
                             ,gpe == "GPE indicator 3ib"  ~ "GPE indicator 3i"
                             ,TRUE ~ gpe_new
  )) |> 
  mutate(gpe_new = recode(gpe_new,  "GPE indicator 8" = "GPE indicator 8i"
                  ,"GPE indicator 7" = "GPE indicator 7i"))
  # Trim
  db$gpe_new <- trimws(db$gpe_new, which = c("left"))
  db$gpe     <- trimws(db$gpe, which = c("left"))
  
  # Snake_case 
  db$gpe_new <- db$gpe_new <- gsub("\\s", "_",  db$gpe_new, perl = TRUE)
  db$gpe     <- db$gpe     <- gsub("\\s", "_",  db$gpe, perl = TRUE)
  

# Generating database for sub-folders
db2 <- db |> 
  select(gpe, gpe_new) |> 
  distinct()
  
  # Deleting GPE_ in order to match folder naming
  db2$gpe_new <- db2$gpe_new <- gsub("[GPE_$]", " ",  db2$gpe_new, perl = TRUE)
  
  # Trim
  db2$gpe_new <- trimws(db2$gpe_new, which = c("left"))
  
  # Sentence_case in order to match folder naming
  db2$gpe_new <- gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", db2$gpe_new ,
                      perl = TRUE)

  # Snake_case in order to match folder naming
  db2$gpe_new <- db2$gpe_new <- gsub("\\s", "_",  db2$gpe_new, perl = TRUE)
  
  bundle <- unique(db2$gpe_new) # 6 GPE indicators, as indicator 1 was dropped 
  listviewer::jsonedit(bundle) # Verifying that we have the right indicators

## Implementing function -------------------------------------------------------

  pcfc <- pcfc_2021
  
  data <- db
  
  reporting_year <- 2020
  
  indicators <- unique(db$gpe)
  listviewer::jsonedit(indicators) # Verifying that we have the right indicators
  
    
  subset_a <- c(  "GPE_indicator_2",
                  "GPE_indicator_3ia",
                  "GPE_indicator_3ib",
                  "GPE_indicator_7ia",
                  "GPE_indicator_7ib",
                  "GPE_indicator_7ic",
                  "GPE_indicator_7id",
                  "GPE_indicator_8i3",
                  "GPE_indicator_8i4",
                  "GPE_indicator_8i5",
                  "GPE_indicator_8i6",
                  "GPE_indicator_8i7",
                  "GPE_indicator_8i8",
                  "GPE_indicator_8i9",
                  "GPE_indicator_8i10",
                  "GPE_indicator_8i11",
                  "GPE_indicator_8i12")

  
lapply(indicators, function(i) {
    
    # Setting-up conditions
    if (reporting_year == 2020 & i %in% subset_a) {
      subset_years <- c(2017:2019)
      # pcfc <- pcfc_2020
      
    } else if (reporting_year == 2020) {
      subset_years <- c(2015:2019)
      # pcfc <- pcfc_2020
      
    } else if (reporting_year == 2021 & i %in% subset_a) {
      subset_years <- c(2018:2020)
      # pcfc <- pcfc_2021
      
    } else if (reporting_year == 2021) {
      subset_years <- c(2016:2020)
      # pcfc <- pcfc_2021
    }
    
    print(paste("Processing indicator", i, subset_years, Sys.time()))
    
    # Subsetting by years and indicator
    DF <- data[data[,3] %in% subset_years,]
    DF <- data[data[,6] %in% i,]
    
    # Adding full country list
    DF <- left_join(pcfc, DF, by = "iso")
    
    # Generating data.frame list
    DF_list <-  DF |> 
      select(!c(country_name_en, code_un)) |>
      group_by(indicator_id) |> 
      pivot_wider(
        names_from = indicator_id,
        values_from = value
      ) |>
      group_map(~.x, .keep = TRUE)
    
    # Dirty way to direct sub-directory flow
    j <- which(db2$gpe == i) 
                
    folder <- db2[j,2]
   
    
    # Exporting each indicator to specific folder and workbook
    openxlsx::write.xlsx(DF_list, here("rf_indicators", folder,
                                       paste0(i, "_CY",
                                              reporting_year,".xlsx"))
                         , overwrite = TRUE)
  }
  )

# ├ Merging files in the same directory ----
# 
# directory <- directory_excels
# directory[1] <- NULL # Deleting folder that does not need to merge
# 
# for (j in directory) {
# 
#   data.files = list.files(here(j), 
#                           pattern = paste0("*","_",reporting_year,".xlsx"))
#   
#   data <- lapply(data.files, function(x) read.xlsx(here(j,x), sheet = 1))
#   
#   for (i in data) {
#     data <- rbind(data[i])
#   }
# }

