# SDG_UIS_indicators_extractor
Automatize extraction of indicators

Updating of code for FY 2023

 Corrections:
 indicator_17: GPE2025_indicator_17-FY2022 to GPE2025_indicator-17-FY2022 OK
 indicator_14ia: data_aggregate, OK
 Country names: Cote d'Ivoire <- Cote D'Ivoire (16iii-FY2022); Burkina Faso <- Burkina faso (17-FY2022) OK
 Cleaned Notes and Technical notes in aggregate_data and metada, OK

 New after 05/09, version 0.75
 8iiic-CY2021 LEG -->Leg var_level
 GPE2025_indicator-7ii-CY2021_MOCKDATA -> GPE2025_indicator-7ii-CY2021


New 08/09, OK
1-CY2020	1	CY2020	pcfc
1-CY2021	1	CY2021	pcfc
16iii-FY2022	16iii	FY2022	pcfc
2-CY2020	2	CY2020	pcfc
2-CY2021	2	CY2021	pcfc
4i-CY2020	4i	CY2020	pcfc
4i-CY2021	4i	CY2021	pcfc

12i_12ii-FY2020, grant_amount, number format was in cientific, converted to normal
12i_12ii-FY2021
12i_12ii-FY2022
14ia-FY2022

New 13/09, ok in version 9 (inclues 75 which was not published)
indi_3iia	aggregate	Out-of-school rate at primary school age
indi_3iia	country	Out-of-school rate at primary school age
indi_3iib	aggregate	Out-of-school rate at primary school age
indi_3iib	country	Out-of-school rate at primary school age
indi_3iic	aggregate	Out-of-school rate at primary school age
indi_3iic	country	Out-of-school rate at primary school age



Add to database:
  1. ~~we will need entity for Somalia, Pakistan, and Tanzania, right? ideally only those three should have that additional column, if thats not possible, its fine to have it for all countries. ~~
  2. ~~lease move column E to be the last column in the right.~~
  3. Should we inlcude indicator 18 here- only aggregate
  4. Milestone     this is great! this makes me think that we could add this column to the indicator files that apply-- SDG4 based, country level objectives, and enabling objectives. Please could I ask either of you to take this on and update all files -- ideally in the two folders we work on (baselines... and 2025_RF), however, this may be time consuming, so you can prioritize for 2025_RF folder. Thank you!
  5. ~~Metadata, delete PCFC~~
  6. ~~Delete empty columns!~~
  
indicator 17: take data from data_country_unique OK
indicator 14ia: take data from data_country_grant OK


Indicator 14ib, only 2 countries OK
indicator_18:  ind_18_total_percentage AND ind_18_total_amount have not var_label in metadata OK
indicator_8iic, has only CY2021, but there are 2 years of data --> Error in file name! OK

Add milestones CHECK


NEW 26/09
indicator_15, FY2022, doesnt have pcfc 2022 column! OK

round data to 2 decimal points CHECK Comment Sissy, IN data_aggregate: value OK
X4i variable!! 


changed to IN data_aggregate AND metadata OK
to ind_14ib_overall from ind_14ia_overall

indicator 7i CY2020/CY2021 IN data_aggregate n.a. from n.a OK
indicator 17 FY2021, income_level IN data_country OK
indicator 3iic, IN data_aggregate data_year_3iic from data_year OK
indicator 3iia, IN data_aggregate data_year_3iia from data_year_3ia OK
indicator 3i, IN data_aggregate ADDED data_year OK (check if not done before)
