# à lancer après car_gen
library(ggplot2)
library(wesanderson) # palette de couleurs sympa

# boxplot des largeur à plein bord en fontion des départements
graphique1<-
  ggplot(data=ope_recent)+
  aes(x=SME_CD_DEPARTEMENT,
      y=OPE_LPBEV)+
  geom_boxplot(outlier.shape = NA)
  
graphique1 <-
  graphique1+
  aes (fill=SME_CD_DEPARTEMENT)+
  
  geom_jitter(fill="grey80",
              width=0.1)+
  theme_grey()+
  labs (title = "Répartition des largeurs à plein bord évaluée par département occitans",
        subtitle = "Lpbev est approchée par la moyenne de trois mesures de largeur à plein bord",
        x = "Département",
        y = "Largeur à plein bord évaluée (en m)")+
  theme(legend.position = "none")

graphique1

ggsave(plot=graphique1,
       filename="output/largeur_pb_ev_dpt.png",
       width = 7,
       height = 5,
       dpi=180)


# Histogramme en vignette par département
graphique2<- ggplot(data=ope_recent)+
  aes(x=OPE_LPBEV)+
  geom_density(fill="green")+
  facet_wrap(~SME_CD_DEPARTEMENT)
graphique2

# Comparaison entre largeur plein bord évaluée et moyenne des largeurs plein bord
graphique3 <-ggplot(data=ope_recent2)+
  aes(x=OPE_LPBEV,y=lpb_vraie_ev,col=SME_CD_DEPARTEMENT)+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE,col="black")+ 
  scale_x_log10()+
  scale_y_log10()+
  theme_grey()+
  labs (title = "Comparaison de la moyenne des largeurs plein bord avec la largeur plein bord évaluée",
        x = "Largeur plein bord évaluée (m)",
        y = "Largeur plein bord moyenne par station (en m)")+
  theme(legend.position = "bottom")




# scale_fill_manual (values = wes_palette("GrandBudapest1",n=13))
# geom_violin()
