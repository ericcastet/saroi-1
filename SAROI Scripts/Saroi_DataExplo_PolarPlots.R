##########################################################################################################################################################################
##         2019/05/09 - Ambre D.N.                                                                                                                                      ##
##                                         Create graph to compare performances across reticle and target positions                                                     ##
##                                                                                                                                                                      ##
# dans la première moitié du fichier, ça crée le plot montrant les reticle positions
# Alors que
# dans la deuxième partie du fichier, ça crée le plot avec les target positions
##########################################################################################################################################################################


###########################################################################################################################################################################
## ---- Packages                                                                                                                                                      - ----
library(ggplot2)

library(tidyverse)

## ---- Set paths                                                                                                                                                     - ----
(wd = getwd())
# setwd(wd)

setwd('./DataCollection_DataFrames')
wd.data = getwd()

setwd(wd)
setwd('./Graphs')
wd.graphs = getwd()

## ---- Load data                                                                                                                                                     - ----
setwd(wd.data)
load("df_Data_AllInfos_Saroi_AllSubjects.RData")

summary(df_all)
names(df_all)

summary(df_all$Target.Diameter)
summary(df_all$Reticle.Selection.Zone.Diameter)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
## - /!\ SP37 == erreur expérimentateur !!! Target.Diameter == 2 ; Reticle.Selection.Zone.Diameter == 3                                                                ##
##                                                                                                                                                                     ##
## - Pour deux sujets, quelques essais avec erreur lorsque la cible est en haut (quelque soit la reticle.position). Je ne sais pas pourquoi mais quand la cible est en ##
##   haut (7,180), les coordonnées dans les colonnes "Desired.Target.X" et "Desired.Target.Y" sont différentes de ce qui est attendu. Pour ce point cible on attend 0  ##
##   et 18 respectivement. Là on a ... (même chose dans fichiers bruts) :                                                                                              ##
##        SUJET       BLOC         RETICLE.POSITION         TARGET.ID      Desired.Target.X     Desired.Target.Y                                                       ##
##         SP36          2                 top-left           B (top)                2.2372              30.9240                                                       ##
##         SP36          7                     left           B (top)               -0.0387              31.4337                                                       ##
##         SP39          7                    right           B (top)                0.5690              24.7622                                                       ##
##                                                                                                                                                                     ##
##                                                                       SHOULD BE:  0.0000              18.0000                                                       ##
##  --> Removed from the graphs but still in the df !                                                                                                                  ##
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

###########################################################################################################################################################################

## Averaged trial duration by subject and RETICLE position (in sec)
## ==== ## ARRANGE THE DATA ## ===================================================================================================================================== ## ----

## ---- ## 1 ## SUMMARISE THE DATA TO GET ONLY 1 VALUE PER RETICLE POSITION (MEAN TRIAL DURATION) FOR EACH SUBJECT                                                   ## ----
h1 = df_all %>% 
  ## Filter trials in which the subject couldn't aim the target
  dplyr::filter(Trial.Skipped==0) %>% 
  
  dplyr::group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number, Target.Diameter, Phase.Exp) %>% 
  ## Get the (unique) trial duration of each trial as well as the polar angle of the reticle and its eccentricity 
  dplyr::summarise(Trial.Dur = unique(Trial.Duration), 
                   Ret.Ang=unique(Reticle.Polar.Angle), 
                   Ret.Ecc=unique(Reticle.Eccentricity)) %>% 
  dplyr::ungroup() %>% 
  
  ## Calculate the reversed duration (or speed) of each trial
  dplyr::mutate(Trial.Dur.reverse = 1/Trial.Dur) %>% 
  ## Take the data of the second experiment (the one with a reticle) only
  dplyr::filter(Experiment.Number==2) %>% # , Phase.Exp==2
  
  dplyr::group_by(Num.Sujet, Reticle.Position, Target.Diameter, Phase.Exp) %>% 
  ## For each subject and condition, get: 
  dplyr::summarise(avg.rev=mean(Trial.Dur.reverse), ## the mean reversed duration (or speed), 
                   avg=mean(Trial.Dur),             ## the "normal" duration (in sec), 
                   Ret.Ang=unique(Ret.Ang),         ## the polar angle of the reticle,
                   Ret.Ecc=unique(Ret.Ecc)) %>%     ## and its eccentricity
  dplyr::ungroup() # %>% 

## Check
h1$Reticle.Position = factor(h1$Reticle.Position)
levels(h1$Reticle.Position)
summary(h1$Ret.Ang)
summary(h1$Ret.Ecc)

## ---- ## 2 ## FOR EACH SUBJECT, RANK THE TRIALS ACCORDING TO PERFORMANCES                                                                                          ## ----
## Order the data by subject, target diameter, and trial duration (in order to run the function "seq_along" in rs3)
h2 = h1[ order (h1$Num.Sujet, h1$Phase.Exp, h1$Target.Diameter, h1$avg) , ]
h3 = h2 %>% 
  dplyr::group_by(Num.Sujet, Phase.Exp) %>% 
  dplyr::mutate(cond_rank = seq_along(avg),         ## Create a column to have the ranked position of each condition for each subject
                max_dur = max(avg, na.rm=T)) %>%    ## Create a column containing the max trial duration of each subject
  dplyr::ungroup() %>% 
  dplyr::mutate(Ratio.Dur.Suj = avg/max_dur)        ## Calculate a ratio of the trial duration relative to each subject (normalisation)

str(h3)

## ---- ## 3 ## ADD INFO ON THE NUMBER OF SKIPPED TRIALS + CONVERT POLAR ANGLES TO CARTESIAN                                                                         ## ----
h4 = df_all %>% 
  dplyr::group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number, Target.Diameter, Phase.Exp) %>% 
  dplyr::summarise(Trial.Sk = unique(Trial.Skipped),                      ## For each trial, Was it skipped or not ?
                   Ret.Ang = unique(Reticle.Polar.Angle), 
                   Ret.Ecc = unique(Reticle.Eccentricity)) %>%
  dplyr::ungroup() %>% 
  
  dplyr::filter(Experiment.Number==2) %>% # , Phase.Exp==2                ## Take the data of experiment 2 only (with the retcile enabled)
  
  dplyr::group_by(Num.Sujet, Reticle.Position, Target.Diameter, Phase.Exp) %>% 
  dplyr::summarise(Nb.Echec = sum(Trial.Sk),                              ## Number of skipped trial by subject and codnition
                   Ret.Ang=unique(Ret.Ang),         
                   Ret.Ecc=unique(Ret.Ecc)) %>% 
  dplyr::ungroup() %>% 
  
  dplyr::mutate(Nb.Echec.Label = ifelse(Nb.Echec==0, "", Nb.Echec)) %>%   ## Create a column containing the labels taht will be used in the following chart (nbr of skipped trials if > 0)
  
  dplyr::left_join(h3, by=c("Num.Sujet", "Reticle.Position", "Target.Diameter", "Phase.Exp", "Ret.Ang", "Ret.Ecc")) %>% 
  
  ## Transform polar angles into cartesian plan
  dplyr::mutate(Ret.Ang.rad=Ret.Ang*pi/180,                               ## Because the value of Reticle angle is in degree, we convert it to radians
                
                pos.x = Ret.Ecc*cos(Ret.Ang.rad), 
                pos.y = Ret.Ecc*sin(Ret.Ang.rad) ) %>% 
  
  dplyr::group_by(Num.Sujet) %>% 
  dplyr::mutate(n.levels=length(unique(Phase.Exp)), 
                Num.Sujet.2 = ifelse(n.levels>1, paste(Num.Sujet, Phase.Exp, sep="_"), Num.Sujet)) %>% 
  dplyr::ungroup()




## ==== ## MAKE THE CHART   ## ===================================================================================================================================== ## ----
# my_gradient = c("#000000", "#00c7fc", "#fc8900")
# my_gradient = c("#fc8900", "#00c7fc", "#000000")
my_gradient = c("#02A601", "#ffff00", "#ff0000")

ggplot(h4,  aes(x=pos.x, y=pos.y) ) + # , fill=Num.Sujet
  geom_hline(yintercept = 0, color="black") +
  geom_vline(xintercept = 0, color="black") +
  geom_point(aes(size=avg, colour=Ratio.Dur.Suj)) + # , show_guide=F
  scale_size(limits=c(5,155), breaks=c(5,20,35,50,65,80,95,110,125,140,155)) +
  facet_wrap(~Num.Sujet.2, ncol=12) + 
  # scale_colour_gradient2(low="#006293", mid="#00aaff", high="#ff0000") +
  scale_colour_gradientn(colors=my_gradient, limits=c(0, 1)) +
  geom_text(aes(label=Nb.Echec.Label, x=pos.x+0.5), colour="#000000", size=2.5) +
  theme_bw() +
  ggtitle(label = "Averaged trial duration by subject and reticle position (in sec)", subtitle = "Numbers indicate the number of skipped (unsuccessful) trials") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#000000"),
        axis.title=element_blank(),  axis.text=element_blank(), axis.ticks=element_blank(),
        # axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.background = element_rect(fill="#585858"),
        plot.background=element_rect(fill='#e9e9e9'), strip.background=element_rect(colour="#e9e9e9", fill="#e9e9e9"),
        axis.line.x=element_line(colour="#000000"), axis.line.y=element_line(colour="#000000"), 
        legend.key=element_blank(), legend.title=element_blank(), legend.position="left", legend.background=element_rect(colour="#e9e9e9", fill="#e9e9e9"),
        plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        aspect.ratio=1) +
  guides(size=guide_legend(order=1))

## ==== ## AND SAVE IT      ## ===================================================================================================================================== ## ----
setwd(wd.graphs)
ggsave("PolarPlot_TrialDur_EachSubj_RetPos.png", width=26, height=14.4, units="cm", dpi=600)

#----------------------------------------------------------------------------------
# FIN de la première partie: par RETICLE position (et non par target position)
#----------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------- ## ----


## Averaged trial duration by subject and TARGET position (in sec)
## ==== ## ARRANGE THE DATA ## ===================================================================================================================================== ## ----
## ---- ## 1 ## SUMMARISE THE DATA TO GET ONLY 1 VALUE PER TARGET POSITION (MEAN TRIAL DURATION) FOR EACH SUBJECT                                                    ## ----
df_all$Desired.Target.X = as.integer(df_all$Desired.Target.X)
df_all$Desired.Target.Y = as.integer(df_all$Desired.Target.Y)
summary(df_all$Desired.Target.X)

t1 = df_all %>% 
  ## Filter trials in which the subject couldn't aim the target
  dplyr::filter(Trial.Skipped==0, Desired.Target.Y<19) %>% 
  
  dplyr::group_by(Num.Sujet, Experiment.Number, Target.ID, Bloc, Trial.Number, Target.Diameter, Phase.Exp) %>% 
  ## Get the (unique) trial duration of each trial as well as the polar angle of the reticle and its eccentricity 
  dplyr::summarise(Trial.Dur = unique(Trial.Duration), 
                   Target.x=unique(Desired.Target.X), 
                   Target.y=unique(Desired.Target.Y)) %>% 
  dplyr::ungroup() %>% 
  
  ## Calculate the reversed duration (or speed) of each trial
  dplyr::mutate(Trial.Dur.reverse = 1/Trial.Dur) %>% 
  ## Take the data of the second experiment (the one with a reticle) only
  dplyr::filter(Experiment.Number==2) %>% # , Phase.Exp==2
  
  dplyr::group_by(Num.Sujet, Target.ID, Target.x, Target.y, Target.Diameter, Phase.Exp) %>% 
  ## For each subject and condition, get: 
  dplyr::summarise(avg.rev=mean(Trial.Dur.reverse), ## the mean reversed duration (or speed), 
                   avg=mean(Trial.Dur) ) %>%        ## the "normal" duration (in sec), 
  dplyr::ungroup() # %>% 

## ---- ## 2 ## FOR EACH SUBJECT, RANK THE TRIALS ACCORDING TO PERFORMANCES                                                                                          ## ----
## Order the data by subject, target diameter, and trial duration (in order to run the function "seq_along" in rs3)
t2 = t1[ order (t1$Num.Sujet, t1$Phase.Exp, t1$Target.Diameter, t1$avg) , ]
t3 = t2 %>% 
  dplyr::group_by(Num.Sujet, Phase.Exp) %>% 
  dplyr::mutate(cond_rank = seq_along(avg),         ## Create a column to have the ranked position of each condition for each subject
                max_dur = max(avg, na.rm=T)) %>%    ## Create a column containing the max trial duration of each subject
  dplyr::ungroup() %>% 
  dplyr::mutate(Ratio.Dur.Suj = avg/max_dur)        ## Calculate a ratio of the trial duration relative to each subject (normalisation)

str(t3)

## ---- ## 3 ## ADD INFO ON THE NUMBER OF SKIPPED TRIALS + CONVERT POLAR ANGLES TO CARTESIAN                                                                         ## ----
t4 = df_all %>% 
  dplyr::group_by(Num.Sujet, Experiment.Number, Target.ID, Bloc, Trial.Number, Target.Diameter, Phase.Exp) %>% 
  dplyr::summarise(Trial.Sk = unique(Trial.Skipped)) %>%                  ## For each trial, Was it skipped or not ?
  dplyr::ungroup() %>% 
  
  dplyr::filter(Experiment.Number==2) %>% # , Phase.Exp==2                ## Take the data of experiment 2 only (with the retcile enabled)
  
  dplyr::group_by(Num.Sujet, Target.ID, Target.Diameter, Phase.Exp) %>% 
  dplyr::summarise(Nb.Echec = sum(Trial.Sk)) %>%                          ## Number of skipped trial by subject and codnition
  dplyr::ungroup() %>% 
  
  dplyr::mutate(Nb.Echec.Label = ifelse(Nb.Echec==0, "", Nb.Echec)) %>%   ## Create a column containing the labels taht will be used in the following chart (nbr of skipped trials if > 0)
  
  dplyr::left_join(t3, by=c("Num.Sujet", "Target.ID", "Target.Diameter", "Phase.Exp")) %>% 
  
  # ## Transform polar angles into cartesian plan
  # dplyr::mutate(Ret.Ang.rad=Ret.Ang*pi/180,                               ## Because the value of Reticle angle is in degree, we convert it to radians
  #               
  #               pos.x = Ret.Ecc*cos(Ret.Ang.rad), 
  #               pos.y = Ret.Ecc*sin(Ret.Ang.rad) ) %>% 
  
  dplyr::group_by(Num.Sujet) %>% 
  dplyr::mutate(n.levels=length(unique(Phase.Exp)), 
                Num.Sujet.2 = ifelse(n.levels>1, paste(Num.Sujet, Phase.Exp, sep="_"), Num.Sujet))



## ==== ## MAKE THE CHART   ## ===================================================================================================================================== ## ----
# my_gradient = c("#000000", "#00c7fc", "#fc8900")
# my_gradient = c("#fc8900", "#00c7fc", "#000000")
my_gradient = c("#02A601", "#FFFF00", "#ff0000")

ggplot(t4,  aes(x=Target.x, y=Target.y) ) + # , fill=Num.Sujet
  geom_hline(yintercept = 0, color="black") +
  geom_vline(xintercept = 0, color="black") +
  geom_point(aes(size=avg, colour=Ratio.Dur.Suj)) + # , show_guide=F
  scale_size(limits=c(5,155), breaks=c(5,20,35,50,65,80,95,110,125,140,155)) +
  facet_wrap(~Num.Sujet.2, ncol=12) + 
  # scale_colour_gradient2(low="#006293", mid="#00aaff", high="#ff0000") +
  scale_colour_gradientn(colors=my_gradient, limits=c(0, 1)) +
  geom_text(aes(label=Nb.Echec.Label, x=Target.x+0.5), colour="#000000", size=2.5) +
  theme_bw() +
  ggtitle(label = "Averaged trial duration by subject and target position (in sec)", subtitle = "Numbers indicate the number of skipped (unsuccessful) trials") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#000000"),
        axis.title=element_blank(),  axis.text=element_blank(), axis.ticks=element_blank(),
        # axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.background = element_rect(fill="#585858"),
        plot.background=element_rect(fill='#e9e9e9'), strip.background=element_rect(colour="#e9e9e9", fill="#e9e9e9"),
        axis.line.x=element_line(colour="#000000"), axis.line.y=element_line(colour="#000000"), 
        legend.key=element_blank(), legend.title=element_blank(), legend.position="left", legend.background=element_rect(colour="#e9e9e9", fill="#e9e9e9"), 
        plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        aspect.ratio=1) +
  guides(size=guide_legend(order=1))

## ==== ## AND SAVE IT      ## ===================================================================================================================================== ## ----
#setwd(wd.graphs)
#ggsave("PolarPlot_TrialDur_EachSubj_TargetPos.png", width=26, height=14.4, units="cm", dpi=600)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------- ## ----


