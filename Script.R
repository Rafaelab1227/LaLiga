setwd("~/Documents/Marta/MÁSTER DATA SCIENCE/Programming in R")
data2=read.csv2("Database Programming R project.csv")
library(dplyr)
library(readxl)
library(reshape2)
library(magrittr)
library(tidyr)
library(lubridate)

#-------------------------------------------------- Probabilidad de Ganar en casa y fuera respecto al número de victorias en casa y fuera -----------------

#Selecciono solo los equipos, los resultados y las apuestas
HomeWins_HomeBets <- select(data2, HomeTeam, FTR, B365H, BWH, IWH, PSH, WHH, VCH)
AwayWins_AwayBets <- select(data2, AwayTeam, FTR, B365A, BWA, IWA, PSA, WHA, VCA)


#Selecciono solo las victorias en casa
HomeWins_HomeBets_Filter <- filter(as.data.frame(HomeWins_HomeBets), FTR=="H")


#Selecciono solo las victorias fuera
AwayWins_AwayBets_Filter <- filter(as.data.frame(AwayWins_AwayBets), FTR=="A")



#Convert my list in numeric type for operating
HomeWins_HomeBets_Filter[,3:8] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))

#Convert my list in numeric type for operating
AwayWins_AwayBets_Filter[,3:8] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))

#Calculo las probabilidades de ganar en casa
Prob_HomeWins <- mutate(1/(HomeWins_HomeBets_Filter[,3:8]))

#Calculo las probabilidades de ganar fuera
Prob_AwayWins <- mutate(1/(AwayWins_AwayBets_Filter[,3:8]))

#Selecciono los equipos para unirnos a la base anterior de las probabilidades
Teams_Results_HomeWins <- select(HomeWins_HomeBets_Filter, HomeTeam, FTR)
Prob_HomeWins_BetsHome <- cbind(Teams_Results_HomeWins, Prob_HomeWins)

#Selecciono los equipos para unirnos a la base anterior de las probabilidades
Teams_Results_AwayWins <- select(AwayWins_AwayBets_Filter, AwayTeam, FTR)
Prob_AwayWins_BetsAway <- cbind(Teams_Results_AwayWins, Prob_AwayWins)

#Agrupo las probabilidades por equipos
Prob_HomeWins_BetsHome_t <- Prob_HomeWins_BetsHome %>% group_by(HomeTeam,B365H,BWH,IWH,PSH,WHH,VCH) %>% summarise(count=n())

#Agrupo las probabilidades por equipos
Prob_AwayWins_BetsAway_t <- Prob_AwayWins_BetsAway %>% group_by(AwayTeam,B365A,BWA,IWA,PSA,WHA,VCA) %>% summarise(count=n())

#Calculo la media de las probabilidades por equipo
Prob_HomeWins_BetsHome_mean <- Prob_HomeWins_BetsHome_t %>% group_by(HomeTeam) %>% summarise_all(mean) %>% select(-count)
Prob_HomeWins_BetsHome_mean$Prob_HomeWins_mean=rowMeans(Prob_HomeWins_BetsHome_mean[,2:7])
Prob_HomeWins_mean_t <- select(Prob_HomeWins_BetsHome_mean, HomeTeam, Prob_HomeWins_mean)

#Calculo la media de las probabilidades por equipo
Prob_AwayWins_BetsAway_mean <- Prob_AwayWins_BetsAway_t %>% group_by(AwayTeam) %>% summarise_all(mean) %>% select(-count)
Prob_AwayWins_BetsAway_mean$Prob_AwayWins_mean=rowMeans(Prob_AwayWins_BetsAway_mean[,2:7])
Prob_AwayWins_mean_t <- select(Prob_AwayWins_BetsAway_mean, AwayTeam, Prob_AwayWins_mean)

#Añado columna nueva de número total de victorias en casa y fuera
Results <- select(data, HomeTeam, AwayTeam, FTR)

N_HomeWins <- filter(as.data.frame(Results), FTR=="H")
N_AwayWins <- filter(as.data.frame(Results), FTR=="A")

N_HomeWins_Team <- N_HomeWins %>% group_by(HomeTeam) %>% summarise(count=n())
names(N_HomeWins_Team)[2]=c("N_HomeWins")
N_AwayWins_Team <- N_AwayWins %>% group_by(AwayTeam) %>% summarise(count=n())
names(N_AwayWins_Team)[2]=c("N_AwayWins")

names(Total_Data_Prob)[2]=c('Mean Prob of HomeWins')
names(Total_Data_Prob)[4]=c('Mean Prob of AwayWins')


#Resumen del total y renombre de columnas
Total_Data_Prob <- cbind(Prob_HomeWins_mean_t, Prob_AwayWins_mean_t)
Total_Data_Prob2 <- select(Total_Data_Prob, -AwayTeam)
names(Total_Data_Prob2)[2]=c('Mean Prob of HomeWins')
names(Total_Data_Prob2)[3]=c('Mean Prob of AwayWins')
names(Total_Data_Prob2)[1]=c("Team")
Total_Data_Prob2_Long <- melt(Total_Data_Prob2)
names(Total_Data_Prob2_Long)[2]=c("Probs")
names(Total_Data_Prob2_Long)[3]=c("Value1")

Total_Data_N <- cbind(N_HomeWins_Team, N_AwayWins_Team)
Total_Data_N2 <- select(Total_Data_N, -AwayTeam)
names(Total_Data_N2)[1]=c("Team2")
names(Total_Data_N2)[2]=c("N of Home Wins")
names(Total_Data_N2)[3]=c("N of Away Wins")
Total_Data_N2_Long <- melt(Total_Data_N2)
names(Total_Data_N2_Long)[2]=c("Quantity")
names(Total_Data_N2_Long)[3]=c("Value2")

Total_Data_VF1 <- cbind(Total_Data_Prob2_Long, Total_Data_N2_Long)
Total_Data_VF1 <- select(Total_Data_VF1, -Team2)

names(Total_Data_VF1)


#Gráfico Probabilidad de ganar vs n de victorias (Home/Away)
library(ggplot2)
library(gtable)
library(grid)

ggplot(Total_Data_VF1) +
  geom_bar(aes(x=Team, y=Value1*15, fill = Probs), alpha=0.7, stat="identity", position = "dodge") +
  geom_line(aes(x= Team, y = Value2, colour = Quantity, group = Quantity)) +
  scale_fill_manual(values=c('steelblue1', 'skyblue4')) +
  scale_color_manual(values=c('red4', 'salmon')) +
  scale_y_continuous(sec.axis = sec_axis(~./15, name= "Probability of Win (Home/Away)")) +
  xlab("Teams") + ylab("N of Wins (Home/Away)") +
  ggtitle("Probability of Win vs N of Wins (Home/Away)") +
  theme()
  
  


#--------------------------------------------------- Probabilidad de ganar por equipo y por casa de apuesta, teniendo en cuenta todos los partidos -----------------


#Cambio de nombre de las columnas para que al hacer el rbind tengan el mismo nombre
colnames=names(Prob_HomeWins_BetsHome_t)
colnames(Prob_AwayWins_BetsAway_t)=colnames

#Uno las dos bases de Prob_HomeWins_BetsHome_t y Prob_AwayWins_BetsAway_t
Prob_Wins_Match <- rbind(Prob_HomeWins_BetsHome_t, Prob_AwayWins_BetsAway_t)
Prob_Wins_Match <- select(Prob_Wins_Match, -count)

#calculo de la media de todas las casas de apuestas
Prob_Wins_Match$Prob_Wins_Match_mean= (rowMeans(Prob_Wins_Match[,2:7])*100)

Prob_Wins_Team <- cbind(Prob_Wins_Match[1], Prob_Wins_Match[8])
names(Prob_Wins_Team)[1]=c("Team")

Total_Data_VF2 <- Prob_Wins_Team

#Gráfico Probabilidad ganar por equipo (BoxPlot)
library(ggplot2)
library(gtable)
library(grid)

ggplot(Total_Data_VF2) +
  geom_boxplot(aes(x=Team, y=Prob_Wins_Match_mean, color=Team)) +
  ggtitle("Probability of Win per Team")


#------------------------------------------------------- Beneficio Casa Apuesta por Season--------------------------------------------------------------------

#Selecciono partido (HomeTeam), fecha y apuestas (todas)

Date_and_bets <- select(data2, HomeTeam, Date, B365H, B365A, B365D, BWH, BWA, BWD, IWH, IWA, IWD, PSH, PSA, PSD, WHH, WHA, WHD, VCH, VCA, VCD)

Date_and_bets[,3:20] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))


#Calculo del mean por partido
Date_and_bets[,3] <- (1/(Date_and_bets[,3]))
Date_and_bets[,4] <- (1/(Date_and_bets[,4]))
Date_and_bets[,5] <- (1/(Date_and_bets[,5]))
Date_and_bets[,6] <- (1/(Date_and_bets[,6]))
Date_and_bets[,7] <- (1/(Date_and_bets[,7]))
Date_and_bets[,8] <- (1/(Date_and_bets[,8]))
Date_and_bets[,9] <- (1/(Date_and_bets[,9]))
Date_and_bets[,10] <- (1/(Date_and_bets[,10]))
Date_and_bets[,11] <- (1/(Date_and_bets[,11]))
Date_and_bets[,12] <- (1/(Date_and_bets[,12]))
Date_and_bets[,13] <- (1/(Date_and_bets[,13]))
Date_and_bets[,14] <- (1/(Date_and_bets[,14]))
Date_and_bets[,15] <- (1/(Date_and_bets[,15]))
Date_and_bets[,16] <- (1/(Date_and_bets[,16]))
Date_and_bets[,17] <- (1/(Date_and_bets[,17]))
Date_and_bets[,18] <- (1/(Date_and_bets[,18]))
Date_and_bets[,19] <- (1/(Date_and_bets[,19]))
Date_and_bets[,20] <- (1/(Date_and_bets[,20]))


#Lo calculo en %
Date_and_bets[,21] <- (1-(rowSums(Date_and_bets[,3:5])))
Date_and_bets[,22] <- Date_and_bets[,21]*-100
Date_and_bets[,23] <- (1-(rowSums(Date_and_bets[,6:8])))
Date_and_bets[,24] <- Date_and_bets[,23]*-100  
Date_and_bets[,25] <- (1-(rowSums(Date_and_bets[,9:11])))
Date_and_bets[,26] <- Date_and_bets[,25]*-100  
Date_and_bets[,27] <- (1-(rowSums(Date_and_bets[,12:14])))
Date_and_bets[,28] <- Date_and_bets[,27]*-100  
Date_and_bets[,29] <- (1-(rowSums(Date_and_bets[,15:17])))
Date_and_bets[,30] <- Date_and_bets[,29]*-100  
Date_and_bets[,31] <- (1-(rowSums(Date_and_bets[,18:20])))
Date_and_bets[,32] <- Date_and_bets[,31]*-100


#Cambio de nombre de las columnas
names(Date_and_bets)[22]=c("PBenB365")
names(Date_and_bets)[24]=c("PBenBW")
names(Date_and_bets)[26]=c("PBenIW")
names(Date_and_bets)[28]=c("PBenPS")
names(Date_and_bets)[30]=c("PBenWH")
names(Date_and_bets)[32]=c("PBenVC")

Date_and_bets[,2] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))

Date_and_bets[,2] = format(as.Date(Date_and_bets[,2]), "%d-%m-%y") 

Date_and_bets$Month = format(as.Date(Date_and_bets[,2]), "%m-%y") 

BrandBenefit_date <- select(Date_and_bets, PBenB365, PBenBW, PBenIW, PBenPS, PBenWH, PBenVC, Month)

BrandBenefit_mean <- BrandBenefit_date %>% group_by(Month) %>% summarise_all(mean)

BrandBenefit_mean_Long <- melt(BrandBenefit_mean)

Total_BrandBenefit_mean <- BrandBenefit_mean_Long %>% group_by(variable) %>% summarise_all(mean)

Total_Data_VF3 <- select(Total_BrandBenefit_mean, -Month)

names(Total_Data_VF3 )[1]=c("BetBrand")
names(Total_Data_VF3 )[2]=c("PBen")

#Gráfico Beneficio Casa Apuesta por mes
library(ggplot2)
library(gtable)
library(grid)

ggplot(Total_Data_VF3, aes(x = "", y = PBen,, fill = reorder(BetBrand,PBen))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(round(PBen/100,3))), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Percentage of Benefit per BetBrand (%)") +
  theme(axis.text.x= element_blank(), axis.ticks=element_blank())
  

#Faltas, Goles y Tiros por meses -------------------------------------------------

Matchs_and_FGS <- select(data2, HomeTeam, Date, FTHG, FTAG, HS, AS, HF, AF)

Matchs_and_FGS[,3:6] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))

Matchs_and_FGS[,2] = format(as.Date(Matchs_and_FGS[,2]), "%d-%m-%y") 

Matchs_and_FGS$Month = format(as.Date(Matchs_and_FGS[,2]), "%m-%y") 

Matchs_and_FGS$Total_Fouls <- (Matchs_and_FGS[,7]) + (Matchs_and_FGS[,8])

Matchs_and_FGS$Total_Shots <- (Matchs_and_FGS[,5]) + (Matchs_and_FGS[,6])

Matchs_and_FGS$Total_Goals <- (Matchs_and_FGS[,3]) + (Matchs_and_FGS[,4])


Matchs_and_TotalFGS <- select(Matchs_and_FGS, HomeTeam, Month, Total_Fouls, Total_Goals, Total_Shots)

Matchs_and_TotalFGS_Month <- select(Matchs_and_TotalFGS, Month, Total_Fouls, Total_Goals, Total_Shots)

TotalF_Month <- aggregate(Matchs_and_TotalFGS_Month$Total_Fouls, by=list(Category=Matchs_and_TotalFGS_Month$Month), FUN=sum)
names(TotalF_Month)[1]=c("Month")
names(TotalF_Month)[2]=c("Total_Fouls")

TotalS_Month <- aggregate(Matchs_and_TotalFGS_Month$Total_Shots, by=list(Category=Matchs_and_TotalFGS_Month$Month), FUN=sum)
names(TotalS_Month)[1]=c("Month3")
names(TotalS_Month)[2]=c("Total_Shots")

TotalG_Month <- aggregate(Matchs_and_TotalFGS_Month$Total_Goals, by=list(Category=Matchs_and_TotalFGS_Month$Month), FUN=sum)
names(TotalG_Month)[1]=c("Month2")
names(TotalG_Month)[2]=c("Total_Goals")

TotalFGS_Month <- cbind(TotalF_Month, TotalG_Month, TotalS_Month)
TotalFGS_Month <- select(TotalFGS_Month, -Month2, -Month3)


#Calculo del mean por partido
TotalFGS_Month[1,5] <- (TotalFGS_Month[1,2]/40)
TotalFGS_Month[1,6] <- (TotalFGS_Month[1,3]/40)
TotalFGS_Month[1,7] <- (TotalFGS_Month[1,4]/40)
TotalFGS_Month[2,5] <- (TotalFGS_Month[2,2]/48)
TotalFGS_Month[2,6] <- (TotalFGS_Month[2,3]/48)
TotalFGS_Month[2,7] <- (TotalFGS_Month[2,4]/48)
TotalFGS_Month[3,5] <- (TotalFGS_Month[3,2]/37)
TotalFGS_Month[3,6] <- (TotalFGS_Month[3,3]/37)
TotalFGS_Month[3,7] <- (TotalFGS_Month[3,4]/37)
TotalFGS_Month[4,5] <- (TotalFGS_Month[4,2]/54)
TotalFGS_Month[4,6] <- (TotalFGS_Month[4,3]/54)
TotalFGS_Month[4,7] <- (TotalFGS_Month[4,4]/54)
TotalFGS_Month[5,5] <- (TotalFGS_Month[5,2]/32)
TotalFGS_Month[5,6] <- (TotalFGS_Month[5,3]/32)
TotalFGS_Month[5,7] <- (TotalFGS_Month[5,4]/32)
TotalFGS_Month[6,5] <- (TotalFGS_Month[6,2]/20)
TotalFGS_Month[6,6] <- (TotalFGS_Month[6,3]/20)
TotalFGS_Month[6,7] <- (TotalFGS_Month[6,4]/20)
TotalFGS_Month[7,5] <- (TotalFGS_Month[7,2]/45)
TotalFGS_Month[7,6] <- (TotalFGS_Month[7,3]/45)
TotalFGS_Month[7,7] <- (TotalFGS_Month[7,4]/45)
TotalFGS_Month[8,5] <- (TotalFGS_Month[8,2]/35)
TotalFGS_Month[8,6] <- (TotalFGS_Month[8,3]/35)
TotalFGS_Month[8,7] <- (TotalFGS_Month[8,4]/35)
TotalFGS_Month[9,5] <- (TotalFGS_Month[9,2]/30)
TotalFGS_Month[9,6] <- (TotalFGS_Month[9,3]/30)
TotalFGS_Month[9,7] <- (TotalFGS_Month[9,4]/30)
TotalFGS_Month[10,5] <- (TotalFGS_Month[10,2]/39)
TotalFGS_Month[10,6] <- (TotalFGS_Month[10,3]/39)
TotalFGS_Month[10,7] <- (TotalFGS_Month[10,4]/30)


TotalFGS_Month_mean <- select(TotalFGS_Month,Month, V5,V6,V7)
names(TotalFGS_Month_mean)[2]=c("Total_Fouls")
names(TotalFGS_Month_mean)[3]=c("Total_Goals")
names(TotalFGS_Month_mean)[4]=c("Total_Shots")

#Ordenarlo por orden cronologico?????????????????????????????????????????????????????????????????????????

Total_Data_VF4 <- melt(TotalFGS_Month_mean)

Total_Data_VF4$Month <- as.Date(paste("01-",Total_Data_VF4$Month,sep=""), "%d-%m-%y")

#Gráfico trayectoria de Faltas, Goles y Tiros por meses ---------------------------------
library(ggplot2)
library(gtable)
library(grid)


ggplot(Total_Data_VF4, aes(x=Month, y=value, group=variable, color=variable)) +
  geom_line() +
  scale_color_manual(values=c('navyblue', 'steelblue1', "red4")) +
  xlab("Month") + ylab("Mean") +
  ggtitle("Mean of Foals, Goals and Shots per month") +
  theme()

#Ganar respecto half-time (ganando, empatando y perdiendo) -----------------------------

Wins_vs_HT <- select(data2, HomeTeam, AwayTeam, FTR, HTR)

Wins_vs_HT_Home <- select(Wins_vs_HT, HomeTeam, FTR, HTR)
Wins_vs_HT_Away <- select(Wins_vs_HT, AwayTeam, FTR, HTR)

#Count del Win-Win
Wins_vs_HT_WWH <- filter(as.data.frame(Wins_vs_HT_Home), FTR=="H", HTR == "H")
Total_vs_HT_WWH <- Wins_vs_HT_WWH %>% group_by(HomeTeam) %>% summarise(count=n())
names(Total_vs_HT_WWH)[1]=c("Team")
names(Total_vs_HT_WWH)[2]=c("WWH")
  
Wins_vs_HT_WWA <- filter(as.data.frame(Wins_vs_HT_Away), FTR=="A", HTR == "A")
Total_vs_HT_WWA <- Wins_vs_HT_WWA %>% group_by(AwayTeam) %>% summarise(count=n())
names(Total_vs_HT_WWA)[1]=c("Team")
names(Total_vs_HT_WWA)[2]=c("WWA")

#lo unimos y calculamos total
Total_Wins_vs_HT_WW <- left_join(Total_vs_HT_WWH , Total_vs_HT_WWA)

Total_Wins_vs_HT_WW[,2:3] %<>% mutate_if(is.factor, funs(as.numeric(as.character(.))))

Total_Wins_vs_HT_WW$Total_WW <- (Total_Wins_vs_HT_WW[,2]) + (Total_Wins_vs_HT_WW[,3])

Total_WW_Team <- select(Total_Wins_vs_HT_WW, Team, Total_WW)


#Count del Draw-Win
Wins_vs_HT_DWH <- filter(as.data.frame(Wins_vs_HT_Home), FTR=="H", HTR == "D")
Total_vs_HT_DWH <- Wins_vs_HT_DWH %>% group_by(HomeTeam) %>% summarise(count=n())
names(Total_vs_HT_DWH)[1]=c("Team")
names(Total_vs_HT_DWH)[2]=c("DWH")

Wins_vs_HT_DWA <- filter(as.data.frame(Wins_vs_HT_Away), FTR=="A", HTR == "D")
Total_vs_HT_DWA <- Wins_vs_HT_DWA %>% group_by(AwayTeam) %>% summarise(count=n())
names(Total_vs_HT_DWA)[1]=c("Team")
names(Total_vs_HT_DWA)[2]=c("DWA")

#Lo unimos
Total_Wins_vs_HT_DW <- left_join(Total_vs_HT_DWH , Total_vs_HT_DWA) %>% 
  mutate(DWA = ifelse(is.na(DWA), 0, DWA))


Total_Wins_vs_HT_DW$Total_DW <- (Total_Wins_vs_HT_DW[,2]) + (Total_Wins_vs_HT_DW[,3])

Total_DW_Team <- select(Total_Wins_vs_HT_DW, Team, Total_DW)


#Calculo del Win-Loss
Wins_vs_HT_LWH <- filter(as.data.frame(Wins_vs_HT_Home), FTR=="H", HTR == "A")
Total_vs_HT_LWH <- Wins_vs_HT_LWH %>% group_by(HomeTeam) %>% summarise(count=n())
names(Total_vs_HT_LWH)[1]=c("Team")
names(Total_vs_HT_LWH)[2]=c("LWH")

Wins_vs_HT_LWA <- filter(as.data.frame(Wins_vs_HT_Away), FTR=="A", HTR == "H")
Total_vs_HT_LWA <- Wins_vs_HT_LWA %>% group_by(AwayTeam) %>% summarise(count=n())
names(Total_vs_HT_LWA)[1]=c("Team")
names(Total_vs_HT_LWA)[2]=c("LWA")

#Lo unimos
Total_Wins_vs_HT_LW <- left_join(Total_vs_HT_LWH , Total_vs_HT_LWA) %>% 
  mutate(LWA = ifelse(is.na(LWA), 0, LWA))

Total_Wins_vs_HT_LW$Total_LW <- (Total_Wins_vs_HT_LW[,2]) + (Total_Wins_vs_HT_LW[,3])

Total_LW_Team <- select(Total_Wins_vs_HT_LW, Team, Total_LW)


#Lo unimos todo

Total_WW_DW_Team <- left_join(Total_WW_Team, Total_DW_Team) %>% 
  mutate(Total_DW = ifelse(is.na(Total_DW$DWH), 0, Total_DW$DWH))

Total_WW_DW_LW_Team <- left_join(Total_WW_Team, left_join(Total_DW_Team, Total_LW_Team)) %>% 
  mutate(Total_DW = ifelse(is.na(Total_DW$DWH), 0, Total_DW$DWH),
         Total_LW = ifelse(is.na(Total_LW$LWH), 0, Total_LW$LWH),
         Total_WW = ifelse(is.na(Total_WW$WWH), 0, Total_WW$WWH)) %>% 
  data.frame() 

colnames(Total_WW_DW_LW_Team)


Total_Data_VF5 <-melt(Total_WW_DW_LW_Team)

ggplot(Total_Data_VF5, aes(x=Team, y=value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c('navyblue', 'steelblue1', "skyblue4")) +
  xlab("Team") + ylab("N of Wins") +
  ggtitle("Wins vs Half Time Result") +
  theme()


  

