#Call required packages and functions
library(Lahman)
library(plyr)
source("FindIP.R")
source("ComputeFIPC.R")
source("ComputeFIP.R")
source("ComputeOPS.R")
source("ComputeDER.R")

#Set the year to go back and grab data for
#Grabs data from 'year' to present
year = 1950

#Load data
#Requies data from team table and pitching table
team.year = subset(Teams, yearID >= year)
pitch.year = subset(Pitching, yearID >= year)

#Find innings pitched, compute FIP constant, and compute FIP for every team in every year
pitch.year = FindIP(pitch.year)
pitch.year.FIPC = ddply(pitch.year, .(yearID), ComputeFIPC)
FIP.df = ddply(pitch.year.FIPC, .(yearID, teamID), ComputeFIP)

#Compute team OPS for every year
OPS.df = ddply(team.year, .(yearID, teamID), ComputeOPS)

#Compute team DER for every year
DER.df = ddply(pitch.year, .(yearID, teamID), ComputeDER)

#Number of wins for each team in each year
W.df = data.frame(yearID = team.year$yearID, teamID = team.year$teamID, W = team.year$W)

#Order the data by year and alphabetically by team name
FIP.df = FIP.df[with(FIP.df, order(yearID, teamID)),]
dimnames(FIP.df)[[2]][3] = "FIP"
OPS.df = OPS.df[with(OPS.df, order(yearID, teamID)),]
dimnames(OPS.df)[[2]][3] = "OPS"
DER.df = DER.df[with(DER.df, order(yearID, teamID)),]
dimnames(DER.df)[[2]][3] = "DER"
W.df = W.df[with(W.df, order(yearID, teamID)),]

#Save data for easy labeling
W = W.df$W
FIP = FIP.df$FIP
OPS = OPS.df$OPS
DER = DER.df$DER

#Save all data in one data frame
Data = data.frame(FIP = FIP.df$FIP, OPS = OPS.df$OPS, DER = DER.df$DER, W = W.df$W)


#Perform linear regression with and without interaction terms
Coeff1 = lm(W~FIP + OPS + DER) # model 1
Coeff2 = lm(W~FIP + OPS + DER + FIP*OPS) # model 2
Coeff3 = lm(W~FIP + OPS + DER + FIP*DER) # model 3
Coeff4 = lm(W~FIP + OPS + DER + OPS*DER) # model 4
Coeff5 = lm(W~FIP + OPS + DER + FIP*OPS + FIP*DER) # model 5
Coeff6 = lm(W~FIP + OPS + DER + FIP*OPS + OPS*DER) # model 6
Coeff7 = lm(W~FIP + OPS + DER + FIP*DER + OPS*DER) # model 7
Coeff8 = lm(W~FIP + OPS + DER + FIP*OPS + OPS*DER + FIP*DER) # model 8
Coeff9 = lm(W~FIP) # model 9
Coeff10 = lm(W~OPS) # model 10
Coeff11 = lm(W~DER) # model 11
Coeff12 = lm(W~DER + OPS) # model 12
Coeff13 = lm(W~DER + FIP) # model 13
Coeff14 = lm(W~FIP + OPS) # model 14


#Read test data
Data.Adv.2016 = read.csv("2016.csv")
Data.Bas.2016 = read.csv("2016Basic.csv")
InputAdvanced = data.frame(FIP = Data.Adv.2016$FIP, OPS = Data.Adv.2016$OPS, DER = Data.Adv.2016$DER)
InputBasic = data.frame(AVG = Data.Bas.2016$AVG, ERA = Data.Bas.2016$ERA, FP = Data.Bas.2016$FP)
TrueWins = data.frame(W = Data.Adv.2016$W)


#Test using FIP, OPS, and DER as predictors. Save test error in 'P'
P = seq(0,10,1)
FIP = InputAdvanced$FIP
OPS = InputAdvanced$OPS
DER = InputAdvanced$DER

#Predict wins for model 1
PredWins = as.numeric(Coeff1$coefficients[1]) + as.numeric(Coeff1$coefficients[2])*FIP + as.numeric(Coeff1$coefficients[3])*OPS + as.numeric(Coeff1$coefficients[4])*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[1] = P1

#Predict wins for model 2
PredWins = as.numeric(Coeff2$coefficients[1]) + as.numeric(Coeff2$coefficients[2])*FIP + as.numeric(Coeff2$coefficients[3])*OPS + as.numeric(Coeff2$coefficients[4])*DER + as.numeric(Coeff2$coefficients[5])*FIP*OPS
P1 = mean((PredWins - TrueWins$W)^2)
P[2] = P1

#Predict wins for model 3
PredWins = as.numeric(Coeff3$coefficients[1]) + as.numeric(Coeff3$coefficients[2])*FIP + as.numeric(Coeff3$coefficients[3])*OPS + as.numeric(Coeff3$coefficients[4])*DER + as.numeric(Coeff3$coefficients[5])*FIP*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[3] = P1

#Predict wins for model 4
PredWins = as.numeric(Coeff4$coefficients[1]) + as.numeric(Coeff4$coefficients[2])*FIP + as.numeric(Coeff4$coefficients[3])*OPS + as.numeric(Coeff4$coefficients[4])*DER + as.numeric(Coeff4$coefficients[5])*OPS*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[4] = P1

#Predict wins for model 5
PredWins = as.numeric(Coeff5$coefficients[1]) + as.numeric(Coeff5$coefficients[2])*FIP + as.numeric(Coeff5$coefficients[3])*OPS + as.numeric(Coeff5$coefficients[4])*DER + as.numeric(Coeff5$coefficients[5])*FIP*OPS + as.numeric(Coeff5$coefficients[6])*FIP*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[5] = P1

#Predict wins for model 6
PredWins = as.numeric(Coeff6$coefficients[1]) + as.numeric(Coeff6$coefficients[2])*FIP + as.numeric(Coeff6$coefficients[3])*OPS + as.numeric(Coeff6$coefficients[4])*DER + as.numeric(Coeff6$coefficients[5])*FIP*OPS + as.numeric(Coeff6$coefficients[6])*OPS*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[6] = P1

#Predict wins for model 7
PredWins = as.numeric(Coeff7$coefficients[1]) + as.numeric(Coeff7$coefficients[2])*FIP + as.numeric(Coeff7$coefficients[3])*OPS + as.numeric(Coeff7$coefficients[4])*DER + as.numeric(Coeff7$coefficients[5])*FIP*DER + as.numeric(Coeff7$coefficients[6])*OPS*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[7] = P1

#Predict wins for model 8
PredWins = as.numeric(Coeff8$coefficients[1]) + as.numeric(Coeff8$coefficients[2])*FIP + as.numeric(Coeff8$coefficients[3])*OPS + as.numeric(Coeff8$coefficients[4])*DER + as.numeric(Coeff8$coefficients[5])*FIP*OPS + as.numeric(Coeff8$coefficients[6])*OPS*DER + + as.numeric(Coeff8$coefficients[7])*FIP*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[8] = P1

#Predict wins for model 9
PredWins = as.numeric(Coeff9$coefficients[1]) + as.numeric(Coeff9$coefficients[2])*FIP
P1 = mean((PredWins - TrueWins$W)^2)
P[9] = P1

#Predict wins for model 10
PredWins = as.numeric(Coeff10$coefficients[1]) + as.numeric(Coeff10$coefficients[2])*OPS
P1 = mean((PredWins - TrueWins$W)^2)
P[10] = P1

#Predict wins for model 11
PredWins = as.numeric(Coeff11$coefficients[1]) + as.numeric(Coeff11$coefficients[2])*DER
P1 = mean((PredWins - TrueWins$W)^2)
P[11] = P1

#Predict wins for model 12
PredWins = as.numeric(Coeff12$coefficients[1]) + as.numeric(Coeff12$coefficients[2])*DER + as.numeric(Coeff12$coefficients[3])*OPS
P1 = mean((PredWins - TrueWins$W)^2)
P[12] = P1

#Predict wins for model 13
PredWins = as.numeric(Coeff13$coefficients[1]) + as.numeric(Coeff13$coefficients[2])*DER + as.numeric(Coeff13$coefficients[3])*FIP
P1 = mean((PredWins - TrueWins$W)^2)
P[13] = P1

#Predict wins for model 14
PredWins = as.numeric(Coeff14$coefficients[1]) + as.numeric(Coeff14$coefficients[2])*FIP + as.numeric(Coeff14$coefficients[3])*OPS
P1 = mean((PredWins - TrueWins$W)^2)
P[14] = P1

#Use team era, average, and fielding percentage as predictors
ERA = team.year$ERA
AVG = with(team.year,H/AB)
FP = team.year$FP

#Save all data in one data frame
Databasic = data.frame(ERA = ERA, AVG = AVG, FP = FP, W = W.df$W)

#Perform linear regression with and without interaction terms
CoeffB1 = lm(W~ERA+AVG+FP) # model 1
CoeffB2 = lm(W~ERA + AVG + FP + ERA*AVG) # model 2
CoeffB3 = lm(W~ERA + AVG + FP + ERA*FP) # model 3
CoeffB4 = lm(W~ERA + AVG + FP + AVG*FP) # model 4
CoeffB5 = lm(W~ERA + AVG + FP + ERA*AVG + ERA*FP) # model 5
CoeffB6 = lm(W~ERA + AVG + FP + ERA*AVG + AVG*FP) # model 6
CoeffB7 = lm(W~ERA + AVG + FP + ERA*FP + AVG*FP) # model 7
CoeffB8 = lm(W~ERA + AVG + FP + ERA*AVG + AVG*FP + ERA*FP) # model 8
CoeffB9 = lm(W~ERA) # model 9
CoeffB10 = lm(W~AVG) # model 10
CoeffB11 = lm(W~FP) # model 11
CoeffB12 = lm(W~FP + AVG) # model 12
CoeffB13 = lm(W~FP + ERA) # model 13
CoeffB14 = lm(W~ERA + AVG) # model 14


#Test using ERA, AVG, and FP as predictors. Save test error in 'P'
PBasic = seq(0,13,1)
ERA = InputBasic$ERA
AVG = InputBasic$AVG
FP = InputBasic$FP

#Predict wins for model 1
PredWins = as.numeric(CoeffB1$coefficients[1]) + as.numeric(CoeffB1$coefficients[2])*ERA + as.numeric(CoeffB1$coefficients[3])*AVG + as.numeric(CoeffB1$coefficients[4])*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[1] = P1

#Predict wins for model 2
PredWins = as.numeric(CoeffB2$coefficients[1]) + as.numeric(CoeffB2$coefficients[2])*ERA + as.numeric(CoeffB2$coefficients[3])*AVG + as.numeric(CoeffB2$coefficients[4])*FP + as.numeric(CoeffB2$coefficients[5])*ERA*AVG
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[2] = P1

#Predict wins for model 3
PredWins = as.numeric(CoeffB3$coefficients[1]) + as.numeric(CoeffB3$coefficients[2])*ERA + as.numeric(CoeffB3$coefficients[3])*AVG + as.numeric(CoeffB3$coefficients[4])*FP + as.numeric(CoeffB3$coefficients[5])*ERA*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[3] = P1

#Predict wins for model 4
PredWins = as.numeric(CoeffB4$coefficients[1]) + as.numeric(CoeffB4$coefficients[2])*ERA + as.numeric(CoeffB4$coefficients[3])*AVG + as.numeric(CoeffB4$coefficients[4])*FP + as.numeric(CoeffB4$coefficients[5])*AVG*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[4] = P1

#Predict wins for model 5
PredWins = as.numeric(CoeffB5$coefficients[1]) + as.numeric(CoeffB5$coefficients[2])*ERA + as.numeric(CoeffB5$coefficients[3])*AVG + as.numeric(CoeffB5$coefficients[4])*FP + as.numeric(CoeffB5$coefficients[5])*ERA*AVG + as.numeric(CoeffB5$coefficients[6])*ERA*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[5] = P1

#Predict wins for model 6
PredWins = as.numeric(CoeffB6$coefficients[1]) + as.numeric(CoeffB6$coefficients[2])*ERA + as.numeric(CoeffB6$coefficients[3])*AVG + as.numeric(CoeffB6$coefficients[4])*FP + as.numeric(CoeffB6$coefficients[5])*ERA*AVG + as.numeric(CoeffB6$coefficients[6])*AVG*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[6] = P1

#Predict wins for model 7
PredWins = as.numeric(CoeffB7$coefficients[1]) + as.numeric(CoeffB7$coefficients[2])*ERA + as.numeric(CoeffB7$coefficients[3])*AVG + as.numeric(CoeffB7$coefficients[4])*FP + as.numeric(CoeffB7$coefficients[5])*ERA*FP + as.numeric(CoeffB7$coefficients[6])*AVG*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[7] = P1

#Predict wins for model 8
PredWins = as.numeric(CoeffB8$coefficients[1]) + as.numeric(CoeffB8$coefficients[2])*ERA + as.numeric(CoeffB8$coefficients[3])*AVG + as.numeric(CoeffB8$coefficients[4])*FP + as.numeric(CoeffB8$coefficients[5])*ERA*AVG + as.numeric(CoeffB8$coefficients[6])*AVG*FP + + as.numeric(CoeffB8$coefficients[7])*ERA*FP
PP1 = mean((PredWins - TrueWins$W)^2)
PBasic[8] = P1

#Predict wins for model 9
PredWins = as.numeric(CoeffB9$coefficients[1]) + as.numeric(CoeffB9$coefficients[2])*ERA
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[9] = P1

#Predict wins for model 10
PredWins = as.numeric(CoeffB10$coefficients[1]) + as.numeric(CoeffB10$coefficients[2])*AVG
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[10] = P1

#Predict wins for model 11
PredWins = as.numeric(CoeffB11$coefficients[1]) + as.numeric(CoeffB11$coefficients[2])*FP
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[11] = P1

#Predict wins for model 12
PredWins = as.numeric(CoeffB12$coefficients[1]) + as.numeric(CoeffB12$coefficients[2])*FP + as.numeric(CoeffB12$coefficients[3])*AVG
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[12] = P1

#Predict wins for model 13
PredWins = as.numeric(CoeffB13$coefficients[1]) + as.numeric(CoeffB13$coefficients[2])*FP + as.numeric(CoeffB13$coefficients[3])*ERA
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[13] = P1

#Predict wins for model 14
PredWins = as.numeric(CoeffB14$coefficients[1]) + as.numeric(CoeffB14$coefficients[2])*ERA + as.numeric(CoeffB14$coefficients[3])*AVG
P1 = mean((PredWins - TrueWins$W)^2)
PBasic[14] = P1

#Data with seasons of 162 games
Teams = subset(Teams, yearID >= 2000)

pitch.year = subset(Pitching, yearID >= 2000)
Wins = Teams$W

pitch.year = FindIP(pitch.year)
pitch.year.FIPC = ddply(pitch.year, .(yearID), ComputeFIPC)
FIP.df = ddply(pitch.year.FIPC, .(yearID, teamID), ComputeFIP)
FIP.df = FIP.df[with(FIP.df, order(yearID, teamID)),]
dimnames(FIP.df)[[2]][3] = "FIP"
W.df = data.frame(yearID = Teams$yearID, teamID = Teams$teamID, W = Teams$W)
W.df = W.df[with(W.df, order(yearID, teamID)),]
FIP = FIP.df$FIP
Data = data.frame(FIP = FIP.df$FIP, W = W.df$W)


#Perform linear regression with and without interaction terms
Coeff1 = lm(W~FIP, data = Data) # model 1
