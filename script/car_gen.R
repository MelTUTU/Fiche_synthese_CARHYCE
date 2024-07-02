# ANALYSES DE DONNEES

# ATTENTION : faire tourner script import_donnees au préalable

#comparaison des lpbev et largeur pb moyennes effectives
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





