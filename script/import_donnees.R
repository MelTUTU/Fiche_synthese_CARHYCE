# PREPARATION DES TABLEAUX DE TRAVAIL

# définition du répertoire de travail
list.files("./data_raw/export_csv_data-20240220142639")

# charger les librairies utiles
library(readr)
library(tidyverse)
# install.packages("lubridate")
library("lubridate")
#install.packages("stringr")
library(stringr)

## chargement des principales tables de travail issues de l'export CARHYCE
# table OPERATION
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

#table STATION DE MESURE
station_mesure <- read_delim("data_raw/export_csv_data-20240220142639/STATION_MESURE.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                             trim_ws = TRUE)
str(station_mesure)

#table POINT DE PRELEVEMENT
pvmt <- read_delim("data_raw/export_csv_data-20240220142639/POINT_PRELEVEMENT.csv",
                   na=c("null",""))
str(pvmt)

#table TRANSECT
transect <- read_delim("data_raw/export_csv_data-20240220142639/TRANSECT.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(TRA_LPB = col_number(), 
                                                                            TRA_L_MOUILLEE = col_number(), TRA_HPB = col_number()), 
                       locale = locale(encoding = "WINDOWS-1252"), 
                       na = "empty", trim_ws = TRUE)
str(transect)



#table POINT
point <- read_delim("data_raw/export_csv_data-20240220142639/POINT.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(point)

point2<-point %>%
  mutate (POI_EN_EAU= gsub(x=POI_EN_EAU,pattern='";"',replacement=''))


##Croisement entre tables

#pour rapatrier dans tableau de travail code sandre,date, département et info station
ope_croise <- operation %>% 
  select(PPR_ID,OPE_ID,OPE_DATE_REALISATION,OPE_LPBEV:OPE_PENTE_LIGNE_EAU) %>% 
  left_join(y=pvmt %>% 
              select(PPR_ID,SME_CD_STATION_MESURE_EAUX_SURFACE)) %>% 
  mutate(annee=dmy(OPE_DATE_REALISATION),
         annee=year(annee)) %>% 
  left_join(y=station_mesure %>% 
              select(SME_CD_STATION_MESURE_EAUX_SURFACE:SME_COORD_Y_STATION_MESURE_EAUX_SURFACE))

## Travail sur les données les plus récentes
# par station,selectionner infos de l'opé la plus recente
ope_recent <-ope_croise %>% 
  dplyr::group_by(SME_CD_STATION_MESURE_EAUX_SURFACE) %>% 
  filter(annee==max(annee))

#renommer colonne (pour le fun!)
# nom<-colnames(ope_recent)
# nouveau_nom<-nom %>% 
#   str_replace_all(pattern = "\\(", replacement="") %>% # pas utile dans ce contexte mais c'est pour avoir la fonction ;)
#   str_to_lower()
# colnames(ope_recent)<-nouveau_nom
