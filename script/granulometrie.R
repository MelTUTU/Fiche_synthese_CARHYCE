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

# Histogramme
histo1<- ggplot(data = point)+
                aes(SUB_CODE)+
                geom_bar(fill = "white", colour = "red")+
                labs(x="code substrat minéral", y="nombre d'occurences", title ="Les substrats minéraux")
