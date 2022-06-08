
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
pacman::p_load(dplyr, tidyverse, tidylog, openxlsx, here, purrr)

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
                        ) 
  
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
  mutate(gpe_new = substr(gpe, 1, 15)) |> 
  select(!c(indicator_label_en, gpe_name, gpe))
  
#Check list of gpe indicators

db |> distinct(gpe_new) |> 
  count()   #17 indicators, as indicator 1 was dropped 
  

#NOTE: Using PCFC 2021 for reporting years 2020 and 2021, as per UIS directive.

## Implementing function -------------------------------------------------------
pcfc <- pcfc_2021

data <- db

reporting_year <- 2021

indicators <- unique(db$gpe_new) 
  

subset_a <- c("GPE indicator 2",
                "GPE indicator 3ia",
                "GPE indicator 3ib",
                "GPE indicator 7ia",
                "GPE indicator 7ib",
                "GPE indicator 7ic",
                "GPE indicator 7id",
                "GPE indicator 8i3",
                "GPE indicator 8i4",
                "GPE indicator 8i5",
                "GPE indicator 8i6",
                "GPE indicator 8i7",
                "GPE indicator 8i8",
                "GPE indicator 8i9",
                "GPE indicator 8i10",
                "GPE indicator 8i11",
                "GPE indicator 8i12")

  
  lapply(indicators, function(i) {

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
  
    print(subset_years)
    
  DF <- data[data[,3] %in% subset_years,]
  DF <- data[data[,6] %in% i,]
  DF <- left_join(pcfc, DF, by = "iso")
  
  
  DF_list <-  DF |> 
    select(!c(country_name_en, code_un)) |>
    group_by(indicator_id) |> 
    pivot_wider(
    names_from = indicator_id,
    values_from = value
  ) |>
    group_map(~.x, .keep = TRUE)
  

openxlsx::write.xlsx(DF_list, here("SDG_related_indicators", 
                                   paste0(i, "_UIS_",reporting_year,".xlsx")))
  
  }
)


# DF_list <-  DF |> 
#   select(!c(country_name_en, gpe)) |>
#   group_by(indicator_id) |> 
#   pivot_wider(
#               names_from = indicator_id,
#               values_from = value
#   ) 
# 
# 
# assign(paste0("PCFC_compare_", i),
#        PCFC_compare,
#        envir = .GlobalEnv)  # saves to environment






