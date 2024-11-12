---
title: "Pitch Count and Pitch Type Analysis"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
library(tidyverse)
library(readr)
```

## March 2 - May 4 Games (36 total)

```{r}
count <- 1
datalist = list()
for (file in list.files("~/SoftballAnalytics/Cleaned datasets/")){
  if (str_detect(file, ".csv")){
    file_dir = paste("~/SoftballAnalytics/Cleaned datasets/", file, sep="")
    datalist[[count]] <- read_csv(file_dir)
    print(dim(datalist[[count]]))
    count = count + 1
  }
}
allgames = do.call(rbind, datalist)
print(count)
print(allgames)
```

```{r}
# ```X3_2_19_Luther <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-2-19-Luther.csv")
# X3_2_19_UW_River_Falls <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-2-19-UW-River-Falls.csv")
# X3_3_19_Knox <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-3-19-Knox.csv")
# X3_3_19_Rockford <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-3-19-Rockford.csv")
# X3_22_19_Bowdoin <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-22-19-Bowdoin.csv")
# X3_22_19_Tufts <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-22-19-Tufts.csv")
# X3_23_19_Chicago <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-23-19-Chicago.csv")
# X3_23_19_UW_Whitewater <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-23-19-UW-Whitewater.csv")
# X3_25_19_New_Rochelle <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-25-19-New-Rochelle.csv")
# X3_25_19_Thomas <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-25-19-Thomas.csv")
# X3_26_19_UW_Eau_Claire <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-26-19-UW-Eau-Claire.csv")
# X3_26_19_UW_Oshkosh <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-26-19-UW-Oshkosh.csv")
# X3_31_19_St.Mary1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-31-19-St.Mary1.csv")
# X3_31_19_St.Mary2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/3-31-19-St.Mary2.csv")
# X4_3_19_Gustavus1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-3-19-Gustavus1.csv")
# X4_3_19_Gustavus2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-3-19-Gustavus2.csv")
# X4_4_19_St.Kate1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-4-19-St.Kate1.csv")
# X4_4_19_St.Kate2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-4-19-St.Kate2.csv")
# X4_6_19_St.Thomas1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-6-19-St.Thomas1.csv")
# X4_6_19_St.Thomas2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-6-19-St.Thomas2.csv")
# X4_7_19_Augsburg1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-7-19-Augsburg1.csv")
# X4_7_19_Augsburg2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-7-19-Augsburg2.csv")
# X4_9_19_Hamline1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-9-19-Hamline1.csv")
# X4_9_19_Hamline2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-9-19-Hamline2.csv")
# X4_13_19_Bethel1 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-13-19-Bethel1.csv")
# X4_13_19_Bethel2 <- read_csv("~/SoftballAnalytics/Cleaned datasets/4-13-19-Bethel2.csv")
# X5_4_19_St.Kates <- read_csv("~/SoftballAnalytics/Cleaned datasets/5-4-19-St.Kates.csv")


```{r}
# allgames <- X3_2_19_Luther %>%
#   rbind(X3_2_19_UW_River_Falls) %>%
#   rbind(X3_3_19_Knox) %>%
# rbind(X3_3_19_Rockford) %>%
#   rbind(X3_22_19_Bowdoin) %>%
#   rbind(X3_22_19_Tufts) %>%
#   rbind(X3_23_19_Chicago) %>%
#   rbind(X3_23_19_UW_Whitewater) %>%
#   rbind(X3_25_19_New_Rochelle) %>%
#   rbind(X3_25_19_Thomas) %>%
#   rbind(X3_26_19_UW_Eau_Claire) %>%
#   rbind(X3_26_19_UW_Oshkosh) %>%
#   rbind(X3_31_19_St.Mary1) %>%
#   rbind(X3_31_19_St.Mary2) %>%
#   rbind(X4_3_19_Gustavus1) %>%
#   rbind(X4_3_19_Gustavus2) %>%
#   rbind(X4_4_19_St.Kate1) %>%
#   rbind(X4_4_19_St.Kate2) %>%
#   rbind(X4_6_19_St.Thomas1) %>%
#   rbind(X4_6_19_St.Thomas2) %>%
#   rbind(X4_7_19_Augsburg1) %>%
#   rbind(X4_7_19_Augsburg2) %>%
#   rbind(X4_9_19_Hamline1) %>%
#   rbind(X4_9_19_Hamline2) %>%
#   rbind(X4_13_19_Bethel1) %>%
#   rbind(X4_13_19_Bethel2)
  

#St Olaf Pitching Stats
allgamesA <- allgames %>%
  filter(OlafTop != Top)

#St Olaf Batting Stats
allgamesB <- allgames %>%
  filter(OlafTop == Top)
```

###Opponent Batting Average by Count and by Pitcher: "oppcount"
```{r}
oppcount <- allgamesA %>%
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>%
  group_by(Pitcher, Count_before) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

oppcount[is.na(oppcount)] <- 0

oppcount <- oppcount %>%  
  mutate(battingave=nHit/ nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(oppcount) <- c("Pitcher", "Count", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```


###St Olaf Batting Average by Count and by Batter: "stocount"
```{r}
stocount <- allgamesB %>%
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>%
  group_by(Batter, Count_before) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

stocount[is.na(stocount)] <- 0

stocount <- stocount %>%  
  mutate(battingave=nHit/ nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(stocount) <- c("Batter", "Count", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```

###St Olaf by Count as a Team: "stoteamcount"
```{r}
stoteamcount <- allgamesB %>% 
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>% #used to eliminate pitches that don't result in a play/something that would add to PA, like "strike one"
  group_by(Count_before) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

#stoteamcount[is.na(stoteamcount)] <- 0

stoteamcount <- stoteamcount %>%  
  mutate(battingave=nHit/nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(stoteamcount) <- c("Count", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
            
```

###St Olaf Batting Average by Pitch Type and by Batter: "stopitchtype"
```{r}
stopitchtype <- allgamesB %>%
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>% 
  group_by(Batter, PitchType) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

stopitchtype[is.na(stopitchtype)] <- 0

stopitchtype <- stopitchtype %>%  
  mutate(battingave=nHit/ nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(stopitchtype) <- c("Batter", "Pitch_Type", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```


###Opponent Batting Average by Pitch Type and by Pitcher: "opppitchtype"
```{r}
opppitchtype <- allgamesA %>%
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>%
  group_by(Pitcher, PitchType) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

opppitchtype[is.na(opppitchtype)] <- 0

opppitchtype <- opppitchtype %>%  
  mutate(battingave=nHit/ nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(opppitchtype) <- c("Pitcher", "Pitch_Type", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```

###St Olaf OBP Calculations Test
```{r}
test1 <- allgamesB %>% 
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>% #used to eliminate pitches that don't result in a play/something that would add to PA, like "strike one"
  group_by(Count_before) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

test1[is.na(test1)] <- 0

test1 <- test1 %>%  
  mutate(battingave=nHit/nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(test1) <- c("Count", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```

```{r}
stooverall <- allgamesB %>%
  filter(Out == 1 | Hit == 1 | Error == 1 | Walk == 1 | HBP == 1) %>%
  group_by(Batter) %>%
  summarise(nOut = sum(Out),
            nHit = sum(Hit),
            nError = sum(Error),
            nWalk = sum(Walk),
            nHBP = sum(HBP),
            nAB = sum(Out+Hit+Error - Sacrifice),
            nPA = sum(Out+Hit+Error+Walk+HBP))

stooverall[is.na(stooverall)] <- 0

stooverall <- stooverall %>%  
  mutate(battingave=nHit/ nAB) %>%
  mutate(obp=(nHit+nWalk+nHBP)/nPA)

colnames(stooverall) <- c("Batter", "Out", "Hit", "Error", "Walk", "HBP", "AB", "PA", "AVG", "OBP")
```



```{r}
stoteamcount
oppcount
stocount
opppitchtype
stooverall
```


