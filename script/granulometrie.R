# GRANULOMETRIE CARHYCE

# charger les librairies
library(readr)
library (tidyverse)

# WOLMAN
#charger la table contenant les mesures
granulo <- read_delim("data_raw/export_csv_data-20240220142639/MESURE_GRANULOMETRIE.csv",
                      delim = ";", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

# POINTS
# charger la table contenant les mesures
POINT <- read_delim("data_raw/export_csv_data-20240220142639/POINT.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) # bizarrerie : dernière colonne avec des ";" ?!
# enlever la dernière colonne chelou 
point <- POINT %>% 
  select(POI_ID:SUB_CODE)

# renommer la colonne de l'identifiant transect
point <- point %>% 
  rename_at("POI_TRA_ID",~"TRA_ID")

# charger la table contenant l'identifiant de l'opération
transect <- read_delim("data_raw/export_csv_data-20240220142639/TRANSECT.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(TRA_LPB = col_number(), 
                                                                            TRA_L_MOUILLEE = col_number(), TRA_HPB = col_number()), 
                       locale = locale(encoding = "WINDOWS-1252"), 
                       na = "empty", trim_ws = TRUE)



# croiser les tables pour récupérer l'identifiant de l'opération via le code du transect
point2 <- point %>% 
  left_join(y=transect)


# Histogramme
histo1<- ggplot(data = point)+
                aes(SUB_CODE)+
                geom_bar(fill = "white", colour = "red")+
                labs(x="code substrat minéral", y="nombre d'occurences", title ="Les substrats minéraux")

#Selectionner une seule opération
mon_ope <- point2 %>% 
  filter (TRA_OPE_ID == "141")

#Histogramme
histo2<-ggplot (data=mon_ope)+
                aes (SUB_CODE)+
                geom_bar()

# pour la prochaine fois : il faudrait compter le nombre d'entités avant de faire le graphique
