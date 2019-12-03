## 2019 01 11

rm(list=ls(all=TRUE))

(wd = getwd())

# setwd('F:/Projets/5.ContratEric_SAROI/2.Data_collection')
setwd('./2.Data_collection')
wd.data = getwd()

setwd(wd)


library (tidyverse)
library(ggplot2)

setwd(wd.data)
# df = read.csv("aure_2019_01_23_1636.csv")
load("DataSaroi_AllSubjects_AllInfos.RData")

unique(df_all$Target.Jitter.X)
str(df_all$Experiment.Number)

###################################################################################################
### ---- Select 1 bloc of 1 subject                                                           - ----
names(df_all)
df_all$Num.Sujet = as.factor(df_all$Num.Sujet)
levels(df_all$Num.Sujet)
df = df_all %>% filter(Num.Sujet=="S03F", Experiment.Number=="2", Bloc=="2")
#
### ---- Plot 1 (absolute)                                                                    - ----

ggplot (aes (x = Head.Rotation.X, y = Head.Rotation.Y ),
        data = df
        #data = df %>% filter (Trial.Number == 1) %>% slice(1:500)
) +
  geom_point() +
  # Real Target : red cross
  geom_point (aes (x = Real.Target.X, y = Real.Target.Y), shape = 3, size = 6, color = "red")+
  geom_text (aes (x = Real.Target.X, y = Real.Target.Y, 
                  label = paste ("x:",Real.Target.X, "y:",Real.Target.Y) ) ) +
  facet_wrap ( ~ Trial.Number, labeller = "label_both") +
  coord_cartesian ( xlim = c(-25, 25), ylim = c(-25, 25)) +
  theme(aspect.ratio = 1)


### ---- Plot 2 (relative)                                                                    - ----
df2 = df %>% 
  group_by(Num.Sujet, Trial.Number) %>% 
  summarise (first.head.x=first(Head.Rotation.X), 
             first.head.y= first(Head.Rotation.Y),
             relative.target.x=(unique(Real.Target.X)-first.head.x), 
             relative.target.y=(unique(Real.Target.Y)-first.head.y) ) %>% 
  # ## je suis obligé de prendre la "dernière valeur" alors qu'elles devraient être toutes identiques, mais il semble que parfoirs les 3 ou 4 premières soient fausses 
  # slice(row_number() == nb ) %>% 
  ungroup () %>% 
  left_join(df, by=c("Num.Sujet", "Trial.Number") ) %>%
  # filter (Head.Offset.X == first.head.x, Head.Offset.Y == first.head.y) %>% 
  # ## j'ai enlevé les 3 ou 4 premières lignes de chaque essai qui déconnent et comme ça le (maintenant) premier sample est bien à (0, 0)
  mutate (new.head.x =  Head.Rotation.X - first.head.x,
          new.head.y =  Head.Rotation.Y - first.head.y, 
          valid = ifelse(Trial.Skipped==0, "Good trial!", "Skipped trial") )

mypal=c("#000000", "#ffbcbc") # "#b8d1fe"

ggplot(aes(x=new.head.x, y=new.head.y, colour=factor(valid)), data=df2) +
  geom_point() +
  # Target : red cross qui s'étend aux axes pour qu'on vérifie les valeurs
  scale_colour_manual(values=mypal) +
  geom_hline(aes(yintercept=relative.target.y) ) + 
  geom_vline(aes(xintercept=relative.target.x) ) +
  geom_point(aes(x=relative.target.x, y=relative.target.y), shape=3, size=6, color="red") +
  geom_text(aes(x=15, y=-15, label=paste("x:", round(relative.target.x, 2), "\ny:", round(relative.target.y, 2)) ), colour="black" ) +  
  geom_text(aes(x=-20, y=20, label=paste("ret pos: ", Reticle.Position, "\n", valid, sep="")), size=3, colour="blue") +
  facet_wrap(~Trial.Number, labeller="label_both") +
  coord_cartesian(xlim=c(-25, 25), ylim=c(-25, 25)) +
  theme(aspect.ratio=1, legend.position="none") 
#
### ---- Plot 2 (relative)  --  Exp 2 (w reticle)                                             - ----
df2 = df %>% 
  group_by(Num.Sujet, Trial.Number) %>% 
  summarise(first.head.x=first(Head.Rotation.X), first.head.y=first(Head.Rotation.Y),
            relative.target.x=(unique(Real.Target.X)-first.head.x), relative.target.y=(unique(Real.Target.Y)-first.head.y), 
            
            last.head.x=last(Head.Rotation.X), last.head.y=last(Head.Rotation.Y)) %>% 

  ungroup() %>% 
  left_join(df, by=c("Num.Sujet", "Trial.Number") ) %>%
  mutate(new.head.x=Head.Rotation.X-first.head.x, new.head.y=Head.Rotation.Y-first.head.y) %>% #,
  
  group_by(Num.Sujet, Trial.Number) %>%
  mutate(reticle.x=ifelse(Reticle.Position=="right", last(new.head.x)+7, ifelse(Reticle.Position=="left" , last(new.head.x)-7, last(new.head.x))), 
         reticle.y=ifelse(Reticle.Position=="top", last(new.head.y)+7, ifelse(Reticle.Position=="bottom", last(new.head.y)-7, last(new.head.y))), 
         
         valid=ifelse(Trial.Skipped==0, "Good trial!", "Skipped trial")) %>% 
  ungroup()
names(df2)

df2$valid = as.factor(df2$valid)

mypal=c("#b8d1fe", "#ffbcbc")

ggplot(aes(x=new.head.x, y=new.head.y, colour=factor(valid)), data = df2) +
  geom_point() +
  
  scale_colour_manual(values=mypal) +
  
  # Target : red cross qui s'étend aux axes pour qu'on vérifie les valeurs
  geom_hline(aes(yintercept=relative.target.y) ) + geom_vline(aes(xintercept=relative.target.x) ) +
  geom_point(aes(x=relative.target.x, y=relative.target.y), shape=3, size=7, color="red")+
  geom_text(aes(x=17, y=-17, label=paste("x:",round(relative.target.x, 2), "\ny:", round(relative.target.y, 2) ) ), colour="black" ) +  
  
  geom_point(aes(x=reticle.x, y=reticle.y), size=5, shape=21, color="blue") +
  # geom_text(aes(x=-20, y=20, label=paste("ret pos: ", Reticle.Position, "\n", valid, sep="")), size=3) +
  geom_text(aes(x=-20, y=20, label=paste("ret pos: ", Reticle.Position, sep="")), size=3, colour="black") +
  
  facet_wrap(~Trial.Number, labeller="label_both") +
  coord_cartesian(xlim=c(-25, 25), ylim=c(-25, 25)) +
  theme(aspect.ratio=1, legend.position="none") 


  
###################################################################################################






