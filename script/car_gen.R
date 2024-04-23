# CARACTERISTIQUES GENERALES

# chargement de la table opération
list.files("./data_raw/export_csv_data-20240220142639")

library(readr)
library(tidyverse)
# install.packages("lubridate")
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


#chargement de la table pour récupérer les codes dpt
station_mesure <- read_delim("data_raw/export_csv_data-20240220142639/STATION_MESURE.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                             trim_ws = TRUE)
str(station_mesure)


#chargement de la table point de pvmt pour récupérer le code departement

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

# croisement pour rapatrier code departement dans table operation
ope_croise2 <-ope_croise %>% 
  left_join(y=station_mesure %>% 
              select(SME_CD_STATION_MESURE_EAUX_SURFACE:SME_COORD_Y_STATION_MESURE_EAUX_SURFACE))


# par station,selectionner infos de l'opé la plus recente
ope_recent <-ope_croise2 %>% 
  dplyr::group_by(SME_CD_STATION_MESURE_EAUX_SURFACE) %>% 
  filter(annee==max(annee))

#renommer colonne (pour le fun!)
# nom<-colnames(ope_recent)
# nouveau_nom<-nom %>% 
#   str_replace_all(pattern = "\\(", replacement="") %>% # pas utile dans ce contexte mais c'est pour avoir la fonction ;)
#   str_to_lower()
# colnames(ope_recent)<-nouveau_nom


#chargement de la table transect
transect <- read_delim("data_raw/export_csv_data-20240220142639/TRANSECT.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(TRA_LPB = col_number(), 
                                                                            TRA_L_MOUILLEE = col_number(), TRA_HPB = col_number()), 
                       locale = locale(encoding = "WINDOWS-1252"), 
                       na = "empty", trim_ws = TRUE)
str(transect)


# Comparaison des lpbev et largeur pb moyennes effectives
transect2<- transect %>% 
  dplyr::mutate(TRA_LPB=ifelse(TRA_LPB<=0,
                                NA,
                                TRA_LPB)) %>% 
  dplyr::group_by(TRA_OPE_ID) %>% 
  summarise(lpb_vraie_ev=mean(TRA_LPB,na.rm=TRUE))
  
transect2<-rename(transect2,OPE_ID=TRA_OPE_ID)

#rappatriement de la colonne dans tableau operation
ope_recent2<-ope_recent %>% 
  left_join(y=transect2)
summary(transect2)

# Calcul du nomdre d'opération par département et par 
nb_ope<-ope_recent2 %>% 
  dplyr::group_by(annee) %>% 
 count() %>% 
  ungroup()

nb_ope_dpt<-ope_recent2 %>% 
  dplyr::group_by(annee,SME_CD_DEPARTEMENT) %>% 
  count() %>% 
  ungroup()

# info à la station
ma_station <-ope_recent2 %>% 
  filter (OPE_ID=="817")


#représenter nombre opération par département
fichier_coord<-station_mesure %>% 
  sf::st_as_sf(coords=c("SME_COORD_X_STATION_MESURE_EAUX_SURFACE","SME_COORD_Y_STATION_MESURE_EAUX_SURFACE"),
               crs = 2154)
  
mapview::mapview(fichier_coord)

#charger le package qui va bien ATTENTION, NE FONCTIONNE PAS
devtools::install_github("maeltheuliere/COGiter")

#carto dpt ATTENTION, NE FONCTIONNE PAS
dpt_occitan<-COGiter::departements_metro_geo %>% 
  filter(DEP%in%c("09","11","12","30","31","32","34","46","48","65","66","81","82")) %>% 
  
mapview::mapview(dpt_occitan)

dpt_occitan_mod<-dpt_occitan %>% 
  left_join(y=nb_ope_dpt,
            by=join_by(DEP==SME_CD_DEPARTEMENT))

nb_ope_tot_geo <-dpt_occitan_mod %>%
  group_by(DEP) %>% 
  summarise(nb_tot=sum(n)) %>%
  ungroup()
mapview::mapview((nb_ope_tot_geo))





