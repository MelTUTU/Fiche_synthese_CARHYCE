# ANALYSE A LA STATION

# ATTENTION : faire tourner script import_donnees au préalable

#chargement librairie
library(ggplot2)
library(wesanderson)

#dans tableau transect, remplacer tous les -999 par NA pour les largeurs et hauteurs > à optimiser !
Larg_haut_analyse<-transect %>%
  dplyr::mutate (TRA_LPBmod=ifelse(TRA_LPB<0,'NA',TRA_LPB)) %>% 
  dplyr::mutate (TRA_L_MOUILLEEmod=ifelse(TRA_L_MOUILLEE<0,'NA',TRA_L_MOUILLEE)) %>% 
  dplyr::mutate (TRA_HPBmod=ifelse(TRA_HPB<0,'NA',TRA_HPB))


#calculer la largeur à plein bord moyenne, médiane, min, max > ca ca fonctionne pas :(
#lpb_analyse<-Larg_haut_analyse %>% 
#  dplyr::group_by (TRA_OPE_ID) %>%
#  summarise(moy_lpb=mean(TRA_LPBmod,na.rm=TRUE),median_lpb=median(TRA_LPBmod, na.rm=TRUE),min_lpb=min(TRA_LPBmod, na.rm=TRUE),max_lpb=max(TRA_LPBmod,na.rm=TRUE))

#calculer la largeur à plein bord moyenne, médiane, min, max > ca ca fonctionnait
lpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lpb=mean(TRA_LPB),median_lpb=median(TRA_LPB),min_lpb=min(TRA_LPB),max_lpb=max(TRA_LPB))


#faire un boxplot de la largeur à plein bord pour une station choisie >> A FINALISER

graph_lpb <-
  ggplot(data=lpb_analyse, aes(x=TRA_OPE_ID,y=moy_lpb))+
  geom_point(aes(color='coral'))  +
  geom_segment( aes(x=TRA_OPE_ID, xend=TRA_OPE_ID, y=0, yend=moy_lpb,color='deepskyblue'))
graph_lpb


# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_point() + 
  geom_segment( aes(x=x, xend=x, y=0, yend=y))

#calculer la largeur mouillée moyenne, médiane, min, max
lm_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lm=mean(TRA_L_MOUILLEE),median_lm=median(TRA_L_MOUILLEE),min_lm=min(TRA_L_MOUILLEE),max_lm=max(TRA_L_MOUILLEE))

#calculer la hauteur plein bord moyenne, médiane, min, max
hpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_hpb=mean(TRA_HPB),median_hpb=median(TRA_HPB),min_hpb=min(TRA_HPB),max_hpb=max(TRA_HPB))

#nombre de transect réalisés lors de l'opération
nb_trans<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(nb=n())


#combiner les tableaux
statistique_ope<-lpb_analyse %>% 
  left_join(y=lm_analyse) %>% 
      left_join(y=hpb_analyse) %>% 
          left_join(y=nb_trans)


# sélectionner une station
ma_station <-ope_recent2 %>% 
  filter (OPE_ID=="817")
mon_ope<-nb_trans %>% 
  filter (TRA_OPE_ID=="817")


#afficher stat sur une seule opé
mes_stat<-statistique_ope %>% 
  filter(TRA_OPE_ID=="817")


      



