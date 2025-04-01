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


#représenter graphiquement les valeurs "structurantes" de la station
graph_lpb1 <-
  ggplot(data=lpb_analyse, aes(x=moy_lpb,y=TRA_OPE_ID))+
  geom_point(aes(color='coral'))  +
  geom_segment( aes(x=0, xend=moy_lpb, y=TRA_OPE_ID, yend=TRA_OPE_ID,color='deepskyblue'))+
  ggtitle ("Largeur moyenne à plein bords")+
  xlab('mètres')
graph_lpb1
# faudrait pouvoir rajouter les autres points dessus > je tente une autre option


# construction d'un nouveau tableau contenant lpb et lm en ligne
#tableau plus court
tab_court<-transect %>% 
  dplyr::select(TRA_ID,TRA_LPB,TRA_L_MOUILLEE)

tab_transfo<-tab_court %>% 
  pivot_longer(cols=-'TRA_ID',
               names_to="type",
               values_to="valeur")%>% 

  # il faudrait rajouter le TRA_ID
#graphique > marche pas car je n'arrive pas à installer la librairie qu'il faut
#library(streamgraph)
#graph_1<-streamgraph(tab_transfo,key='type',value='valeur',date='TRA_ID',
 #                    width="400px", height="300px")



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


      



