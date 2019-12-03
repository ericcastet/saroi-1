########################################################################################################################################################################
##         2019/03/07 - Ambre D.N.                                                                                                                                    ##
##                                           Create graph to compare performances across reticle position                                                             ##
##                                                                                                                                                                    ##
########################################################################################################################################################################

## -------------- ## BASICS ## -------------------------------------------------------------------------------------------------------------------------------------- ## ----
(wd = getwd())

setwd('./2.Data_collection')
wd.data = getwd()

# library (tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd(wd.data)
setwd("F:/Projets/5.ContratEric_SAROI/2.Data_collection")
wd.data = getwd()
load("df_Data_AllInfos_Saroi_AllSubjects.RData")

names(df_all)

summary(df_all$Trial.Duration)
summary(df_all$Triggering.Time)
unique(df_all$Num.Sujet)


setwd('F:/Projets/5.ContratEric_SAROI/4.Graphs')
wd.graphs = getwd()
## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----

# l = unlist(names(df_all))
# write.table(l, sep="\t", row.names=FALSE, file="F:/Projets/5.ContratEric_SAROI/colnames.txt", qmethod="double", quote=FALSE, col.names=F)


## We should compare "Trial.Duration" for "Reticle.Position == "center"" vs the mean of the other positions. 
## And for each subject, we want to know if one of the Reticle.Position was easier than the others. 

## -------------- ## CHECKS ## -------------------------------------------------------------------------------------------------------------------------------------- ## ----
df_all$Reticle.Position = as.factor(df_all$Reticle.Position)

dfx = df_all %>% 
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Target.ID) %>% 
  summarise(Trial.Dur = mean(Trial.Duration, na.rm=T)) %>% 
  ungroup()

dfy = df_all %>% 
  # filter(Experiment.Number==2) %>% 
  group_by(Num.Sujet, Experiment.Number) %>% 
  summarise(Nb.Reticle.Pos = length(unique(Reticle.Position)), 
            Nb.Blocs = length(unique(Bloc)))

s26 = df_all %>% 
  filter(Num.Sujet=="S26M")  
unique(s26$Hour)  
## Il y a un bloc pour lequel on a aucune donnée (le fichier data est vide), je ne sais pas pourquoi...
## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----


## -------------- ## Graph 1 ## -- Central reticle vs. Others -- All Subjects ## ------------------------------------------------------------------------------------ ## ----

levels(df_all$Reticle.Position)

tt = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  filter(Experiment.Number==2) %>% 
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>% 
  
  group_by(Num.Sujet, Reticle.cond) %>% 
  summarise(avg.sjt=mean(Trial.Dur)) %>% 
  ungroup() # %>% 
  
uu = tt %>% 
  # filter(Trial.Skipped==0) %>% 

  group_by(Reticle.cond) %>% 
  summarise(std.grp=sd(avg.sjt, na.rm=T), avg.grp=mean(avg.sjt)) %>% 
  ungroup() %>% 
  
  mutate(stdmin=avg.grp-(std.grp/2), stdmax=avg.grp+(std.grp/2)) %>% 
  
  right_join(tt, by="Reticle.cond")


ggplot(uu, aes(x=Reticle.cond, y=avg.grp, colour=Reticle.cond, shape=Reticle.cond, fill=Reticle.cond)) +
  geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
  geom_line(position="dodge", aes(group=Reticle.cond, linetype=Reticle.cond), size=0.9) +
  stat_summary(fun.y=mean, geom="point", size=4) +
  geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Trial duration (sec)") + 
  # facet_grid(.~fix.t) +
  scale_y_continuous(breaks=round(seq(0, 30, by=5),1), expand=c(0,0)) +
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax, colour=Reticle.cond, linetype=Reticle.cond),width=0.15, size=0.9, position=position_dodge(width=0.01)) +

  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
  # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
  coord_cartesian(ylim=c(0, 30))

## -------------- ## Graph 2 ## -- Central reticle vs. Others -- Each Subject ## ------------------------------------------------------------------------------------ ## ----

levels(df_all$Reticle.Position)

vv = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  filter(Experiment.Number==2) %>% 
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>% 
  
  group_by(Num.Sujet, Reticle.cond) %>% 
  summarise(avg=mean(Trial.Dur), std=sd(Trial.Dur)) %>% 
  mutate(stdmin=avg-(std/2), stdmax=avg+(std/2)) %>% 
  ungroup() 


# ggplot(vv, aes(x=Reticle.cond, y=avg, colour=Reticle.cond, shape=Reticle.cond, fill=Reticle.cond)) +
#   geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
#   geom_line(position="dodge", aes(group=Reticle.cond), size=0.9) +
#   stat_summary(fun.y=mean, geom="point", size=4) +
#   # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
#   ylab("Trial duration (sec)") + 
#   facet_grid(.~Num.Sujet) +
#   scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
#   geom_errorbar(aes(ymin=stdmin, ymax=stdmax, colour=Reticle.cond, linetype=Reticle.cond),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
#   
#   theme_bw() +
#   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
#         axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         # text=element_text(family="Times New Roman", size=15),
#         plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
#         axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
#         legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
#         legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
#   # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
#   coord_cartesian(ylim=c(0, 130))

ggplot(vv, aes(x=Reticle.cond, y=avg)) +
  # geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
  geom_line(position="dodge", aes(group=Num.Sujet), size=0.9) +
  # stat_summary(fun.y=mean, geom="point", size=4) +
  # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Trial duration (sec)") + 
  facet_grid(.~Num.Sujet) +
  scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
  # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
  coord_cartesian(ylim=c(0, 130))

## -------------- ## Graph 2b## -- Central reticle vs. Others -- Each Subject ## ------------------------------------------------------------------------------------ ## ----

levels(df_all$Reticle.Position)

vv = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  filter(Experiment.Number==2) %>% 
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>% 
  
  group_by(Num.Sujet, Reticle.cond) %>% 
  summarise(avg=mean(log10(Trial.Dur)), std=sd(Trial.Dur)) %>% 
  mutate(stdmin=avg-(std/2), stdmax=avg+(std/2)) %>% 
  ungroup() 


# ggplot(vv, aes(x=Reticle.cond, y=avg, colour=Reticle.cond, shape=Reticle.cond, fill=Reticle.cond)) +
#   geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
#   geom_line(position="dodge", aes(group=Reticle.cond), size=0.9) +
#   stat_summary(fun.y=mean, geom="point", size=4) +
#   # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
#   ylab("Trial duration (sec)") + 
#   facet_grid(.~Num.Sujet) +
#   scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
#   geom_errorbar(aes(ymin=stdmin, ymax=stdmax, colour=Reticle.cond, linetype=Reticle.cond),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
#   
#   theme_bw() +
#   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
#         axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         # text=element_text(family="Times New Roman", size=15),
#         plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
#         axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
#         legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
#         legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
#   # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
#   coord_cartesian(ylim=c(0, 130))

ggplot(vv, aes(x=Reticle.cond, y=avg)) +
  # geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
  geom_line(position="dodge", aes(group=Num.Sujet), size=0.9) +
  # stat_summary(fun.y=mean, geom="point", size=4) +
  # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Trial duration (sec)") + 
  facet_grid(.~Num.Sujet) +
  scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
  # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
  coord_cartesian(ylim=c(0, 50))

## -------------- ## Graph 3 ## -- Central reticle vs. Others -- Each Subject ## ------------------------------------------------------------------------------------ ## ----

levels(df_all$Reticle.Position)

w1 = df_all %>%
  filter(Trial.Skipped==0, Num.Sujet!="S09F") %>%

  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>%
  summarise(Trial.Dur = unique(Trial.Duration)) %>%
  ungroup() %>%
  filter(Experiment.Number==2) %>%
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>%

  group_by(Num.Sujet, Reticle.cond) %>%
  summarise(Std=sd(Trial.Dur)) %>%        # , std=sd(Trial.Dur)
  ungroup()


ww = df_all %>% 
  filter(Trial.Skipped==0, Num.Sujet!="S09F") %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  filter(Experiment.Number==2) %>% 
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>% 
  
  group_by(Num.Sujet, Reticle.cond) %>% 
  summarise(avg=mean(Trial.Dur)) %>%        # , std=sd(Trial.Dur)
  ungroup() %>% 
  
  spread(Reticle.cond, avg) %>% 
  # rename(Avg.P=periph, Avg.C=center) %>% 
  mutate(Avg.all=(center+periph)/2 ) %>% 
  
  gather(Reticle.cond, Avg, -Num.Sujet, -Avg.all) %>% 
  left_join(w1, by=c("Num.Sujet", "Reticle.cond")) %>% 
  mutate(EffS = Avg - Avg.all, 
         stdmin = EffS - (Std/2), stdmax = EffS + (Std/2))


w2 = ww %>% 
  filter(!Num.Sujet%in%c("S03F", "S24F"))

ggplot(w2, aes(x=Reticle.cond, y=EffS)) +
  # geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
  geom_line(position="dodge", aes(group=Num.Sujet), size=0.9) +
  # stat_summary(fun.y=mean, geom="point", size=4) +
  # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Subject's mean for each condition\n-\nSubject's mean for both conditions (sec)") + 
  facet_grid(.~Num.Sujet) +
  geom_hline(yintercept=0, colour="red") +
  # scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) #+
  # scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
  # coord_cartesian(ylim=c(0, 130))


## -------------- ## Graph 4 ## -- Central reticle vs. Others -- Each Subject ## ------------------------------------------------------------------------------------ ## ----

ggplot(ww, aes(x=Reticle.cond, y=EffS)) +
  # geom_bar(position="dodge", stat="identity", fill="white", aes(linetype=Reticle.cond), size=0.9) +
  geom_line(position="dodge", aes(group=Num.Sujet), size=0.9) +
  stat_summary(fun.y=mean, geom="point", size=4) +
  # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Subject's mean for each condition\n-\nSubject's mean for both conditions (sec)") + 
  facet_grid(.~Num.Sujet) +
  geom_hline(yintercept=0, colour="red") +
  # scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
  # geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) #+
# scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
# coord_cartesian(ylim=c(0, 130))

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----
## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----
## -------------- ## Graph 4b## -- Central reticle vs. Others -- Each Subject ## ----------- ## saved ## ------------------------------------------------------------ ## ----
levels(df_all$Reticle.Position)

w1 = df_all %>%
  filter(Trial.Skipped==0) %>%
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>%
  summarise(Trial.Dur = unique(Trial.Duration)) %>%
  ungroup() %>%
  filter(Experiment.Number==2) %>%
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>%
  
  group_by(Num.Sujet, Reticle.cond) %>%
  summarise(Std=sd(Trial.Dur)) %>%        # , std=sd(Trial.Dur)
  ungroup()


ww = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  filter(Experiment.Number==2) %>% 
  mutate(Reticle.cond = ifelse(Reticle.Position=="center", "center", "periph")) %>% 
  
  group_by(Num.Sujet, Reticle.cond) %>% 
  summarise(avg=mean(Trial.Dur)) %>%        # , std=sd(Trial.Dur)
  ungroup() %>% 
  
  spread(Reticle.cond, avg) %>% 
  # rename(Avg.P=periph, Avg.C=center) %>% 
  mutate(Avg.all=(center+periph)/2 ) %>% 
  
  gather(Reticle.cond, Avg, -Num.Sujet, -Avg.all) %>% 
  left_join(w1, by=c("Num.Sujet", "Reticle.cond")) %>% 
  mutate(EffS = Avg - Avg.all, 
         stdmin = EffS - (Std/2), stdmax = EffS + (Std/2))

## Dans graph ci-dessous, en bleu = pattern conforme aux attentes = patients qui sont plus lents dans condition viseur au centre que dans condition viseur en périphérie (moyenne de toutes les positions du viseur en périph).
## En rouge = pattern inverse aux attentes = patients qui sont plus lents dans condition viseur en périphérie (moyenne de toutes les positions du viseur en périph) que dans condition viseur au centre 

xx = ww %>%
  mutate(colback = ifelse(Reticle.cond=="center" & EffS<0, "reversed", 
                          ifelse(Reticle.cond=="center" & EffS>0, "ok", 
                                 ifelse(Reticle.cond=="periph" & EffS<0, "ok", 
                                        ifelse(Reticle.cond=="periph" & EffS>0, "reversed", "xx")))) )

facefield = df_all %>% 
  dplyr::group_by(Num.Sujet) %>% 
  dplyr::summarise(fixexc = unique(FaceField.ODS.fixation.excentrée),
                   excstab = unique(FaceField.ODS.Stable.across.repetition)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(excstab = ifelse(Num.Sujet=="S01F", 0, excstab),
                ffodg = ifelse(fixexc==1 & excstab==1, "★★", 
                               ifelse(fixexc==1 & excstab%in%c(0, "NA"), "★",
                                      ifelse(fixexc==0, "---", "na"))), 
         EffS = -1) %>%
  dplyr::left_join(xx, by=c("Num.Sujet")) %>% 
  dplyr::select(-EffS.y) %>% 
  dplyr::rename(EffS=EffS.x)

## ★★ = fixation excentrée & position fix° stable à travers les répétitions
##  ★ = fixation excentrée mais position fix° non stable à travers les répétitions
## --- = fixation non excentrée


mypal=c("#6069f3", "#ec7373")

ggplot(xx, aes(x=Reticle.cond, y=EffS, fill=factor(colback)) ) +
  
  scale_fill_manual(values=mypal) +
  scale_colour_manual(values=mypal) +
  
  geom_bar(position="dodge", stat="identity", size=0.9, alpha=.3) +
  # geom_line(position="dodge", aes(group=Num.Sujet), size=0.9) +
  stat_summary(fun.y=mean, geom="point", size=4, aes(colour=factor(colback))) +
  # geom_jitter(aes(y=avg.sjt), alpha=0.8) +
  ylab("Subject's mean for each condition\n-\nSubject's mean for both conditions (sec)") + 
  facet_grid(.~Num.Sujet) +
  geom_hline(yintercept=0, colour="black") +
  geom_text(data=facefield, aes(x=1.5, label=ffodg), colour="black") +
  
  # scale_y_continuous(breaks=round(seq(0, 130, by=10),1), expand=c(0,0)) +
  # geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  # axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line=element_line(colour="#000000"), axis.text.x=element_text(colour="#000000", angle=90, vjust=0.15),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) #+
# scale_colour_manual(values=mypal) + scale_fill_manual(values=mypal) +
# coord_cartesian(ylim=c(0, 130))

setwd(wd.graphs)
ggsave("EffSize_PeriphVSCenter_EachSubj.png", width=33, height=18, units="cm", dpi=600)


## -------------- ## Graph 5 ## -- Compare each reticle pos°  -- Each Subject ## ----------- ## saved ## ------------------------------------------------------------ ## ----
levels(df_all$Reticle.Position)

t1 = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number, Reticle.Position, Bloc, Trial.Number) %>% 
  summarise(Trial.Dur = unique(Trial.Duration)) %>% 
  ungroup() %>% 
  
  mutate(Trial.Dur.log10 = log10(Trial.Dur)) %>% 
  
  filter(Experiment.Number==2) %>% 
  
  group_by(Num.Sujet, Reticle.Position) %>% 
  summarise(avg=mean(Trial.Dur.log10), std=sd(Trial.Dur.log10)) %>% 
  ungroup() %>% 
  
  mutate(stdmin=avg-(std/2), stdmax=avg+(std/2)) # %>% 
  # group_by(Num.Sujet) %>%
  # arrange_('avg', .by_group=T)


t2 = t1[ order (t1$Num.Sujet, t1$avg) , ]

t3 = t2 %>% 
  group_by(Num.Sujet) %>% 
  mutate(x.axis = seq_along(avg)) # %>% 


txt = t3 %>% 
  dplyr::select(Num.Sujet, x.axis, Reticle.Position, avg) %>% 
  dplyr::mutate(avg2 = avg/2) %>% 
  dplyr::select(-avg) %>% 
  dplyr::rename(avg=avg2)

facefield = df_all %>% 
  group_by(Num.Sujet, Reticle.Position) %>% 
  summarise(fixexc = unique(FaceField.ODS.fixation.excentrée), 
            excstab = unique(FaceField.ODS.Stable.across.repetition)) %>% 
  ungroup() %>% 
  mutate(excstab = ifelse(Num.Sujet=="S01F", 0, excstab),
         ffodg = ifelse(fixexc==1 & excstab==1, "★★", 
                        ifelse(fixexc==1 & excstab%in%c(0, "NA"), "★", 
                               ifelse(fixexc==0, "---", "na"))), 
         avg = 2.1, 
         x.axis = 3 ) 

## ★★ = fixation excentrée & position fix° stable à travers les répétitions
##  ★  = fixation excentrée mais position fix° non stable à travers les répétitions
## --- = fixation non excentrée

ggplot(t3, aes(x=x.axis, y=avg, fill=Reticle.Position, colour=Reticle.Position) ) +
  geom_bar(position="dodge", stat="identity", size=0.9, alpha=.3) +
  geom_point(aes(y=avg), size=4) +
  
  ylab("Trial duration (log10)") + 

  facet_wrap(~Num.Sujet) +
  scale_y_continuous(breaks=round(seq(0, 2.2, by=0.2), 1), expand=c(0,0)) +
  
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01), colour="black") +
  geom_text(data=txt, aes(label=Reticle.Position, angle=90)) +
  geom_text(data=facefield, aes(label=ffodg), colour="black") +
  
  # annotate(geom="text", x=1:5, y=0.2, label=c(t3$Reticle.Position)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        axis.line.x=element_line(colour="#FFFFFF"), axis.line.y=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="none", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
  coord_cartesian(ylim=c(0, 2.2))

setwd(wd.graphs)
ggsave("TrialDur_RetPos_EachSubj.png", width=32, height=28, units="cm", dpi=600)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----


## -------------- ## Graph 6 ## -- Mean trial duration by visual acuity       ## ----------- ## xxxxx ## ------------------------------------------------------------ ## ----
levels(df_all$Reticle.Position)

t1 = df_all %>% 
  filter(Trial.Skipped==0) %>% 
  
  group_by(Num.Sujet, Experiment.Number) %>% 
  summarise(Trial.Dur = mean(log10(Trial.Duration)), 
            acOD = unique(Acuité.OD.logMAR), 
            acOS = unique(Acuité.OS.logMAR), 
            
            fixexc = unique(FaceField.ODS.fixation.excentrée),
            excstab = unique(FaceField.ODS.Stable.across.repetition) ) %>% 
  ungroup() %>% 
  mutate(best_ac = ifelse(acOD>acOS, acOD, acOS), 
         
         excstab = ifelse(Num.Sujet=="SP01", 0, excstab),
         ffodg = ifelse(fixexc==1 & excstab==1, "★★", 
                        ifelse(fixexc==1 & excstab%in%c(0, "NA"), "★",
                               ifelse(fixexc==0, "---", "na")))) %>% 
  
  filter(Experiment.Number==2) 


## ★★ = fixation excentrée & position fix° stable à travers les répétitions
##  ★  = fixation excentrée mais position fix° non stable à travers les répétitions
## --- = fixation non excentrée

ggplot(t1, aes(x=best_ac, y=Trial.Dur, fill=ffodg, colour=ffodg) ) +
  # geom_bar(position="dodge", stat="identity", size=0.9, alpha=.3) +
  geom_jitter(aes(y=Trial.Dur), size=4) +
  
  ylab("Trial duration (log10)") + 
  
  # facet_wrap(~Num.Sujet) +
  scale_y_continuous(breaks=round(seq(0, 2.2, by=0.2), 1), expand=c(0,0)) +
  
  # geom_errorbar(aes(ymin=stdmin, ymax=stdmax),width=0.15, size=0.9, position=position_dodge(width=0.01), colour="black") +
  # geom_text(data=txt, aes(label=Reticle.Position, angle=90)) +
  geom_text(data=t1, aes(label=Num.Sujet, colour=ffodg), position=position_jitter(width=0.01,height=0.01) , colour="black") +
  
  # annotate(geom="text", x=1:5, y=0.2, label=c(t3$Reticle.Position)) +
  
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_rect(color="#FFFFFF"),
        # axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        # text=element_text(family="Times New Roman", size=15),
        plot.background=element_rect(fill='#FFFFFF'), strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        # axis.line.x=element_line(colour="#FFFFFF"), axis.line.y=element_line(colour="#000000"), axis.text=element_text(colour="#000000"),
        legend.key=element_blank(), legend.title=element_blank(), legend.position="bottom", # c(0.6, 0.9), legend.justification=c(0, 0.5),
        legend.direction="horizontal", legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line")  ) +
  coord_cartesian(ylim=c(0, 2.2))

# setwd(wd.graphs)
# ggsave("TrialDur_RetPos_EachSubj.png", width=32, height=28, units="cm", dpi=600)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ## ----

