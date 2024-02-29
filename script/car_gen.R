# CARACTERISTIQUES GENERALES

# chargement de la table opération
list.files("./data_raw/export_csv_data-20240220142639")

library(readr)
library(tidyverse)
install.packages("lubridate")
library("lubridate")


operation <- read_delim("data_raw/export_csv_data-20240220142639/OPERATION.csv", 
                        delim = ";", escape_double = FALSE, col_types = cols(OPE_DEBIT_MESURE = col_number(), 
                                                                             OPE_DEBIT_STATION_HYDRO = col_number(), 
                                                                             OPE_LPBEV = col_number(), OPE_LMEV = col_number(), 
                                                                             OPE_LR = col_number(), OPE_LTH = col_number(), 
                                                                             OPE_DIP = col_number(), OPE_PENTE_LIGNE_EAU = col_number()), 
                        locale = locale(encoding = "WINDOWS-1252"), 
                        na = c("null",""),
                        trim_ws = TRUE)

str(operation)


#chargement de la table point de pvmt pour récupérer le code station

pvmt <- read_delim("data_raw/export_csv_data-20240220142639/POINT_PRELEVEMENT.csv",
                   na=c("null",""))
str(pvmt)

#croisement pour rapatrier code sandre dans tableau de travail et récupérer date

ope_croise <- operation %>% 
  select(PPR_ID,OPE_ID,OPE_DATE_REALISATION,OPE_LPBEV:OPE_PENTE_LIGNE_EAU) %>% 
  left_join(y=pvmt %>% 
              select(PPR_ID,SME_CD_STATION_MESURE_EAUX_SURFACE)) %>% 
  mutate(annee=dmy(OPE_DATE_REALISATION),
         annee=year(annee))

# par station,selectionner code opération le plus récent
ope_recent <-ope_croise %>% 
  dplyr::group_by(SME_CD_STATION_MESURE_EAUX_SURFACE) %>% 
  filter(last(annee))






