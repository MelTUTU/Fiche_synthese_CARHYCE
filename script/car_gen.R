# CARACTERISTIQUES GENERALES

# chargement de la table opération
list.files("./data_raw/export_csv_data-20240220142639")

library(readr)
library(tidyverse)

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

#croisement pour récupérer code sandre

ope_croise <- operation %>% 
  select(PPR_ID,OPE_ID,OPE_LPBEV:OPE_PENTE_LIGNE_EAU) %>% 
  left_join(y=pvmt %>% 
              select(PPR_ID,PPR_EST_SANDRE))



