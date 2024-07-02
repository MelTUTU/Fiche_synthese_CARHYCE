# ANALYSE A LA STATION

# ATTENTION : faire tourner script car_gen au préalable

#calculer la largeur à plein bord moyenne, médiane, min, max
lpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lpb=mean(TRA_LPB),median_lpb=median(TRA_LPB),min_lpb=min(TRA_LPB),max_lpb=max(TRA_LPB))

#calculer la largeur mouillée moyenne, médiane, min, max
lm_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lm=mean(TRA_L_MOUILLEE),median_lm=median(TRA_L_MOUILLEE),min_lm=min(TRA_L_MOUILLEE),max_lm=max(TRA_L_MOUILLEE))

#calculer la hauteur plein bord moyenne, médiane, min, max
hpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_hpb=mean(TRA_HPB),median_hpb=median(TRA_HPB),min_hpb=min(TRA_HPB),max_lpb=max(TRA_HPB))

#nombre de transect réalisés lors de l'opération
nb_trans<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise()


# sélectionner une station
#ma_station <-ope_recent2 %>% 
#  filter (OPE_ID=="817")