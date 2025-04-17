# ANALYSE A LA STATION

# ATTENTION : faire tourner script import_donnees au préalable

#chargement librairie
library(ggplot2)
library(wesanderson)

#Remplacer par NA, directement dans mon tableau, toutes valeurs qui sont inférieures strictement à 0
transect[transect<0]<-NA

#calculer la largeur à plein bord moyenne, médiane, min, max
lpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lpb=mean(TRA_LPB,na.rm=TRUE),median_lpb=median(TRA_LPB, na.rm=TRUE),min_lpb=min(TRA_LPB, na.rm=TRUE),max_lpb=max(TRA_LPB,na.rm=TRUE))

#calculer la largeur mouillée moyenne, médiane, min, max
lm_analyse <-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lm=mean(TRA_L_MOUILLEE,na.rm=TRUE),median_lm=median(TRA_L_MOUILLEE, na.rm=TRUE),min_lm=min(TRA_L_MOUILLEE, na.rm=TRUE),max_lm=max(TRA_L_MOUILLEE,na.rm=TRUE))

#calculer la hauteur pb moyenne, médiane, min, max
Hpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_Hpb=mean(TRA_HPB,na.rm=TRUE),median_Hpb=median(TRA_HPB, na.rm=TRUE),min_Hpb=min(TRA_HPB, na.rm=TRUE),max_Hpb=max(TRA_HPB,na.rm=TRUE))

# Grouper tout ça dans un tableau
tot_analyse<-dplyr::left_join(lpb_analyse,lm_analyse,by='TRA_OPE_ID')
tot_analyse<-dplyr::left_join(tot_analyse,Hpb_analyse,by='TRA_OPE_ID')



#représenter graphiquement les valeurs "structurantes" de la station : exemple de la largeur à plein bord
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
               values_to="valeur")

# il faudrait rajouter le TRA_ID
#tab_transfo2<-tab_court %>% 
#  left_join(y=transect$TRA_OPE_ID,by="TRA_ID") marche pas !

 
graphlpb2<-ggplot(data=tab_transfo)+
  geom_point(aes(x=valeur,y=TRA_ID,color=type))+
  geom_segment(aes(x=0,xend=valeur,y=TRA_ID,yend=TRA_ID),color='blue')

graphlpb2  



# Change baseline
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


  
  
  
 
  
  
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


      



