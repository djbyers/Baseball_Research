---
title: "Clean-iscore"
output: html_document
---

```{r}
library(tidyverse)
library(readr)
library(stringr)
#install.packages('xml2')
library('xml2')
library(rvest)
library(tibble)
```


```{r}
folderlist_feb <- c("2-23-19 vs Martin-Luther1", "2-23-19 vs Martin-Luther2")

folderlist_march <- c("3-3-19 vs Rockford","3-3-19 vs Knox","3-26-19 vs UW-Oshkosh", "3-26-19 vs UW-Eau-Claire", "3-25-19 vs Thomas", "3-25-19 vs New-Rochelle", "3-23-19 vs UW-Whitewater", "3-23-19 vs Chicago", "3-22-19 vs Tufts", "3-22-19 vs Bowdoin", "3-2-19 vs UW-River-Falls", "3-2-19 vs Luther", "3-31-19 vs St.Mary1", "3-31-19 vs St.Mary2")

folderlist_april <- c("4-13-19 vs Bethel1", "4-13-19 vs Bethel2", "4-3-19 vs Gustavus1", "4-3-19 vs Gustavus2", "4-4-19 vs St.Kate1", "4-4-19 vs St.Kate2", "4-6-19 vs St.Thomas1","4-6-19 vs St.Thomas2", "4-7-19 vs Augsburg1", "4-7-19 vs Augsburg2", "4-9-19 vs Hamline1", "4-9-19 vs Hamline2" , "4-19-19 vs Concordia1", "4-19-19 vs Concordia2", "4-20-19 vs Macalester1", "4-20-19 vs Macalester2", "4-27-19 vs St.Ben1", "4-27-19 vs St.Ben2")

folderlist_may <- c("5-3-19 vs St.Thomas", "5-4-19 vs Hamline", "5-4-19 vs St.Kates")
```

###################
###Write out files ####
```{r}

for (folder in folderlist_feb) {
  
  date = str_split(folder,patter=" ")[[1]][1]
  visitorteam = str_split(folder,patter=" ")[[1]][3]
  
  
  ###Load in data
  
  VisitorBatting <- read_csv(paste(c("~/SoftballAnalytics/",folder,"/statsVisitorBatting.csv"),collapse=""))
  HomeBatting <- read_csv(paste(c("~/SoftballAnalytics/",folder,"/statsHomeBatting.csv"),collapse=""))
  VisitorPitching <- read_csv(paste(c("~/SoftballAnalytics/",folder,"/statsVisitorPitching.csv"),collapse=""))
  HomePitching <- read_csv(paste(c("~/SoftballAnalytics/",folder,"/statsHomePitching.csv"),collapse=""))
  game1 <- read_html(paste(c("~/SoftballAnalytics/",folder,"/pitchbypitch.html"),collapse=""))   
  
  
  ###Load html into csv
  
  
  tables <- html_nodes(game1, css = "table") 
  html_table(tables, header = FALSE, fill = TRUE)    # find the right table
  game1_data <- html_table(tables, header = FALSE, fill = TRUE)[[3]]    

  as_tibble(game1_data)
  gm1 <- as_tibble(game1_data) %>%
    select(-X1, -X3) %>%
    rename(outcome = X2, pitchtype = X4)
  
  
  gm1$VsTeam = visitorteam
  gm1$Date = date
  
  
  ###step 1 - Create info column
  
  info <- c()
  pitchcount = 0
  val_old = ""
  PitchNum = c()
  
  for (i in gm1$outcome) {
    val <- ifelse(str_detect(str_sub(i, 1, 3), "\\([0-9]\\)") , i, val)
    pitch = ifelse(val != val_old, 0, pitch+1)
    info <- c(info, val)
    PitchNum = c(PitchNum, pitch)
    
    val_old = val
  }
  
  gm1_1 <-gm1 %>%
    cbind(info) %>%
    cbind(PitchNum)
  
  
  ###step 2 - count, pitch type
  
  gm1_2 <- gm1_1 %>%
    mutate(Count_after = ifelse(str_detect(pitchtype, "[0-3] - [0-3]"),str_sub(pitchtype, 1, 5),NA),
           PitchType = ifelse(str_detect(str_sub(pitchtype, 1, 5), "[0-3] - [0-3]"),str_sub(pitchtype, start=6),pitchtype)
    ) %>%
    filter(PitchNum != 0)
  
  Count_before <- c("0 - 0", gm1_2$Count_after[1:nrow(gm1_2)-1])
  Count_before[is.na(Count_before)] <- "0 - 0"
  
  gm1_2$Count_before = Count_before
  
  
  ###step 3 - extract pitcher - batter and inning
  
  VisitorBatter <- subset(VisitorBatting$Name, VisitorBatting$Name != "TOTALS")
  VisitorPitcher <- subset(VisitorPitching$Name, VisitorPitching$Name != "TOTALS")
  HomeBatter <- subset(HomeBatting$Name, HomeBatting$Name != "TOTALS")
  HomePitcher <- subset(HomePitching$Name, HomePitching$Name != "TOTALS")
  
  batter <- paste(c(VisitorBatter, HomeBatter),collapse="|")
  pitcher <- paste(c(VisitorPitcher, HomePitcher),collapse="|")
  inning <- paste(c("1stP", "2ndP", "3rdP", "4thP", "5thP", "6thP", 
                    "7thP", "8thP", "9thP"),collapse="|")
  
  
  gm1_3 <- gm1_2 %>%
    mutate(Batter = str_extract(info, batter),
           Pitcher = str_extract(str_sub(info, start = 10), pitcher),
           Inning = as.numeric(str_sub(str_extract(info, inning), 1, 1))
    ) %>%
    mutate(VisScore = as.numeric(str_sub(str_extract(info, "Vis: [0-9]|Vis: [1-9][0-9]"), -1, -1)),
           HomeScore = as.numeric(str_sub(str_extract(info, "Home: [0-9]|Home: [1-9][0-9]"), -1, -1)),
           Top = ifelse(str_detect(info, "Top"), 1, 0)
    ) %>%
    select(-pitchtype, -info) 
  
  
  
  ###Step 4 - extract outs
  
  outlist <- paste(c("out", "caught stealing", "sacrifice","pops up","picked off"),collapse="|")
  hitlist <- paste(c("single", "double", "triple", "home run","Homerun","homerun"),collapse="|")
  
  gm1_4 <- gm1_3 %>%
    mutate(Out = ifelse(str_detect(outcome,"but the third strike is dropped"),ifelse(str_detect(gsub("but the third strike is dropped", "", outcome),outlist), 1, 0), ifelse(str_detect(outcome,outlist), 1, 0)),
           Hit=ifelse((str_detect(outcome,"hits")) & (str_detect(outcome, hitlist)),1,0),
           Error = ifelse(str_detect(outcome, "reaches base due to an error"), 1, 0), 
           Sacrifice = ifelse(str_detect(outcome, "sacrifice"), 1, 0),
           Walk = ifelse(str_detect(outcome, "walk"), 1, 0),
           HBP = ifelse(str_detect(outcome, "hit by the pitch"), 1, 0),
           K = ifelse(str_detect(outcome, "strikes out"), 1, 0)
           ) 
  
  
  ###Step 5 - count outs for each inning
  
  val_old <- 0
  outlist <- c()
  
  for (i in gm1_4$Out) {
    val <- ifelse(i==0, val_old, val_old+1)
    outlist <- c(outlist, val)
    val_old <- ifelse(val==3, 0, val)
  }
  
  gm1_4$OutCount <- outlist
  
  gm1_4$Top1 
  
  
  #Find visitor / top =1 
  
  visdata = html_table(tables, header = FALSE, fill = TRUE)[[2]] 
  
  ####Write files
  
  gm1_5 <- gm1_4 %>%
    mutate(OlafTop = ifelse(visdata[1,2] == "St. Olaf College", 1, 0))
  

  
  ###Write files
  write_csv(gm1_5, paste(c("~/SoftballAnalytics/Cleaned datasets/",date,"-",visitorteam,".csv"),collapse=""))
  
}
```




