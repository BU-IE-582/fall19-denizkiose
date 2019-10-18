install.packages("dplyr")
install.packages("fidtistriplus")
install.packages("MASS")

library(dplyr) 

library(fitdistrplus)

library(MASS)

#Task 1 
matches <- read.csv(file="c:/Users/deniz/Desktop/matches.csv", header=TRUE, sep=",") #reading data from csv file

matches_148 = matches[matches$league_id == 148, ] #selecting matches with league id 148
match_id148 = matches_148[,c("match_id")]  #saving English Premier League Data

hist3Data = matches_148[,c("match_hometeam_score")]-matches_148[,c("match_awayteam_score")]  #Calculating data for 3rd histogram

# par(mfrow=c(1,3))  #paring three histograms 

###################################Home Goals Histogram###################################

hist(matches_148[,c("match_hometeam_score")],28,main=" ",xlab="Home Goals",ylab="Number of Games", col = "red", ylim = c(0,220)) 
axis(side=1,at = c(1,3,5,7))

print('It is shown further that for home goals normal distribution is not fit well; similar distribution is observed for away scores, too.
      So Poisson distrbution with lambda 1.23 as given from best distrbution ft is used for calculation of the theoretical number of games at each bin. P(X=x) = lambda^x e(-lambda)/x!')

dd <- na.omit(matches_148[,c("match_hometeam_score")])
fitPoissonHome <- fitdistr(dd, densfun="Poisson")
#create values from a real poisson distribution with lambda found from best fit distribution
distHome = dpois(0:8, lambda = 1.60)
qpois(20, lambda = 1.60)
#multiply them by `sum(dd)` for scaling
distHome = distHome * 0.635*sum(dd)


#add the line plot
lines(x = 0:8, distHome,type = "o")


P_0h = 1.60^0*exp(-1.60)/1*NROW(matches_148) #analytical representation of calculations of expected number of games from pıosson distribution also given in distHome
P_1h = 1.60^1*exp(-1.60)/1*NROW(matches_148)
P_2h = 1.60^2*exp(-1.60)/2*NROW(matches_148)
P_3h = 1.60^3*exp(-1.60)/6*NROW(matches_148)
P_4h = 1.60^4*exp(-1.60)/24*NROW(matches_148)
P_5h = 1.60^5*exp(-1.60)/120*NROW(matches_148)
P_6h = 1.60^6*exp(-1.60)/720*NROW(matches_148)
P_7h = 1.60^7*exp(-1.60)/5040*NROW(matches_148)
P_8h = 1.60^8*exp(-1.60)/40320*NROW(matches_148)



matches_H0= NROW((matches_148[matches_148$match_hometeam_score == 0, ]))
matches_H1= NROW(matches_148[matches_148$match_hometeam_score == 1, ])
matches_H2= NROW(matches_148[matches_148$match_hometeam_score == 2, ])
matches_H3= NROW(matches_148[matches_148$match_hometeam_score == 3, ])
matches_H4= NROW(matches_148[matches_148$match_hometeam_score == 4, ])
matches_H5= NROW(matches_148[matches_148$match_hometeam_score == 5, ])
matches_H6= NROW(matches_148[matches_148$match_hometeam_score == 6, ])
matches_H7= NROW(matches_148[matches_148$match_hometeam_score == 7, ])
matches_H8= NROW(matches_148[matches_148$match_hometeam_score == 8, ])

Ph = c(P_0h,P_1h,P_2h,P_3h,P_4h,P_5h,P_6h,P_7h,P_8h)
Th =  c(matches_H0, matches_H1, matches_H2,matches_H3,matches_H4,matches_H5,matches_H6, matches_H7,matches_H8)

print('The number of games expected for each quantile is shown by the line on the histogram describing Home Goals and the dots represent the theoretical number of matches calculated. The height of each bin is the true value of the matches. Theoretically calculated and true values can be listed for 0,1,2,3,4,5,6,7 and 8 goals respectively as follows' )
print('Theoretical Number of Matches for Home Goal Distribution:')
print(Ph)
print('True Number of Matches for Home Goal Distribution:')
print(Th)

###################################Away Goals Histogram###################################
hist(matches_148[,c("match_awayteam_score")],main=" ",xlab="Away Goals",ylab="Number of Games",ylim = c(0,220), col = "red")
axis(side=1,at = c(1,3,5))


dd2 <- na.omit(matches_148[,c("match_awayteam_score")])
fitPoissonAway <- fitdistr(dd2, densfun="Poisson")

#create values from a real poisson distribution with lambda found from best fit distribution
distAway = dpois(0:6, lambda = 1.23)

#multiply them by `sum(dd)` for scaling
distAway = distAway * 0.827*sum(dd2)

#add the line plot
lines(x = 0:6, distAway, type = "o")

P_0a = 1.23^0*exp(-1.23)/1*NROW(matches_148) #analytical representation of calculations of expected number of games from pıosson distribution also given in distHome
P_1a = 1.23^1*exp(-1.23)/1*NROW(matches_148)
P_2a = 1.23^2*exp(-1.23)/2*NROW(matches_148)
P_3a = 1.23^3*exp(-1.23)/6*NROW(matches_148)
P_4a = 1.23^4*exp(-1.23)/24*NROW(matches_148)
P_5a = 1.23^5*exp(-1.23)/120*NROW(matches_148)
P_6a = 1.23^6*exp(-1.23)/720*NROW(matches_148)


matches_A0= NROW((matches_148[matches_148$match_awayteam_score == 0, ]))
matches_A1= NROW(matches_148[matches_148$match_awayteam_score == 1, ])
matches_A2= NROW(matches_148[matches_148$match_awayteam_score == 2, ])
matches_A3= NROW(matches_148[matches_148$match_awayteam_score == 3, ])
matches_A4= NROW(matches_148[matches_148$match_awayteam_score == 4, ])
matches_A5= NROW(matches_148[matches_148$match_awayteam_score == 5, ])
matches_A6= NROW(matches_148[matches_148$match_awayteam_score == 6, ])


Pa = c(P_0a,P_1a,P_2a,P_3a,P_4a,P_5a,P_6a)
Ta =  c(matches_A0, matches_A1, matches_A2,matches_A3,matches_A4,matches_A5,matches_A6)

print('The number of games expected for each quantile is shown by the line on the histogram describing Away Goals and the dots represent the theoretical number of matches calculated. The height of each bin is the true value of the matches. Theoretically calculated and true values can be listed for 0,1,2,3,4,5,6,7 and 8 goals respectively as follows' )
print('Theoretical Number of Matches for Away Goal Distribution:')
print(Pa)
print('True Number of Matches for Away Goal Distribution:')
print(Ta)

###########################Showing that best fitting normal distribution does not fit good the data so and Poisson is chosen###################################

hist(matches_148[,c("match_hometeam_score")],main=" ",xlab="Home Goals",ylab="Number of Games",ylim = c(0,220), col = "red")
axis(side=1,at = c(1,3,5))

x <- seq(0, 15, length=1000)
y <- dnorm(x, mean=1.60, sd=1.31)*750
lines(x, y, type="l", lwd=1)

print('It is hereby seen that normal distribution does not fit well on Home Goals due to first increasing then decreasing character(similarly for Away Goals)')


###################################Home - Away Goals Histogram###################################
hist(hist3Data,main=" ",xlab="Home Goals – Away Goals",ylab="Number of Games", col = "red")


#Task 2

odds_data <- read.csv(file="c:/Users/deniz/Desktop/bets.csv", header=TRUE, sep=",")
matches_148 = matches[matches$league_id == 148, ] #selecting matches with league id 148
match_id148 = matches_148[,c("match_id")]
odds_dataid = odds_data[,c("match_id")]

##########finding odd information for league id: 148

findNaN <- match(odds_dataid,match_id148) 

k = 0
j = 0
z = 0
for(i in 1:length(findNaN)){
  k = k +1
  if(is.na(findNaN[i]) == "FALSE"){
    j = j+1
    z[j] <- k
  }}



task2Odds = odds_data[z,] #differentiating between draw, home team win and away team win odds (x,1 and 2 respectively)

task2Odds_x = task2Odds[task2Odds$variable == "odd_x", ]
task2Odds_1 = task2Odds[task2Odds$variable == "odd_1", ]
task2Odds_2 = task2Odds[task2Odds$variable == "odd_2", ]

#Tipico, Leonbets,Babibet,10Bet are the bookmakers selected
#Probability Calculations for all four bookmarkers
#Tipico
Odds_x_Tipico = task2Odds_x[task2Odds_x$odd_bookmakers  == "Tipico", ]
Odds_1_Tipico = task2Odds_1[task2Odds_1$odd_bookmakers  == "Tipico", ]
Odds_2_Tipico = task2Odds_2[task2Odds_2$odd_bookmakers  == "Tipico", ]

Odds_x_Tipico_match_id = Odds_x_Tipico[,"match_id"]
Odds_1_Tipico_match_id = Odds_1_Tipico[,"match_id"]
Odds_2_Tipico_match_id = Odds_2_Tipico[,"match_id"]

# match(Odds_x_Tipico_match_id,Odds_1_Tipico_match_id) control if match ids match
Pwin_Tipico = 1/Odds_1_Tipico[,"value"]
Pdraw_Tipico = 1/Odds_x_Tipico[,"value"]
Ploose_Tipico = 1/Odds_2_Tipico[,"value"]

TipicoFirst = rbind(Pwin_Tipico,Ploose_Tipico,Pdraw_Tipico)
TipicoSecond = colSums(TipicoFirst)

ProbTipico = sweep(TipicoFirst, 2, TipicoSecond, FUN = '/')

#Leonbets
Odds_x_Leonbets = task2Odds_x[task2Odds_x$odd_bookmakers  == "Leonbets", ]
Odds_1_Leonbets = task2Odds_1[task2Odds_1$odd_bookmakers  == "Leonbets", ]
Odds_2_Leonbets = task2Odds_2[task2Odds_2$odd_bookmakers  == "Leonbets", ]

Odds_x_Leonbets_match_id = Odds_x_Leonbets[,"match_id"]
Odds_1_Leonbets_match_id = Odds_1_Leonbets[,"match_id"]
Odds_2_Leonbets_match_id = Odds_2_Leonbets[,"match_id"]

Pwin_Leonbets = 1/Odds_1_Leonbets[,"value"]
Pdraw_Leonbets = 1/Odds_x_Leonbets[,"value"]
Ploose_Leonbets = 1/Odds_2_Leonbets[,"value"]

LeonbetsFirst = rbind(Pwin_Leonbets,Ploose_Leonbets,Pdraw_Leonbets)
LeonbetsSecond = colSums(LeonbetsFirst)

ProbLeonbets = sweep(LeonbetsFirst, 2, LeonbetsSecond, FUN = '/')

#Babibet
Odds_x_Babibet = task2Odds_x[task2Odds_x$odd_bookmakers  == "Babibet", ]
Odds_1_Babibet = task2Odds_1[task2Odds_1$odd_bookmakers  == "Babibet", ]
Odds_2_Babibet = task2Odds_2[task2Odds_2$odd_bookmakers  == "Babibet", ]

Odds_x_Babibet_match_id = Odds_x_Babibet[,"match_id"]
Odds_1_Babibet_match_id = Odds_1_Babibet[,"match_id"]
Odds_2_Babibet_match_id = Odds_2_Babibet[,"match_id"]

Pwin_Babibet = 1/Odds_1_Babibet[,"value"]
Pdraw_Babibet = 1/Odds_x_Babibet[,"value"]
Ploose_Babibet = 1/Odds_2_Babibet[,"value"]

BabibetFirst = rbind(Pwin_Babibet,Ploose_Babibet,Pdraw_Babibet)
BabibetSecond = colSums(BabibetFirst)

ProbBabibet = sweep(BabibetFirst, 2, BabibetSecond, FUN = '/')

#10Bet
Odds_x_10Bet = task2Odds_x[task2Odds_x$odd_bookmakers  == "10Bet", ]
Odds_1_10Bet = task2Odds_1[task2Odds_1$odd_bookmakers  == "10Bet", ]
Odds_2_10Bet = task2Odds_2[task2Odds_2$odd_bookmakers  == "10Bet", ]

Odds_x_10Bet_match_id = Odds_x_10Bet[,"match_id"]
Odds_1_10Bet_match_id = Odds_1_10Bet[,"match_id"]
Odds_2_10Bet_match_id = Odds_2_10Bet[,"match_id"]

Pwin_10Bet = 1/Odds_1_10Bet[,"value"]
Pdraw_10Bet = 1/Odds_x_10Bet[,"value"]
Ploose_10Bet = 1/Odds_2_10Bet[,"value"]

o10BetFirst = rbind(Pwin_10Bet,Ploose_10Bet,Pdraw_10Bet)
o10BetSecond = colSums(o10BetFirst)

Prob10Bet = sweep(o10BetFirst, 2, o10BetSecond, FUN = '/')


#Finding the values to be plot in Task2_3a

par(mfrow=c(2,2)) 

PHW_AW_Tipico = ProbTipico[1,]-ProbTipico[2,]
PT_Tipico = ProbTipico[3,]
plot(PHW_AW_Tipico,PT_Tipico)

PHW_AW_Leonbets = ProbLeonbets[1,]-ProbLeonbets[2,]
PT_Leonbets = ProbLeonbets[3,]
plot(PHW_AW_Leonbets,PT_Leonbets)


PHW_AW_Babibet = ProbBabibet[1,]-ProbBabibet[2,]
PT_Babibet = ProbBabibet[3,]

plot(PHW_AW_Babibet,PT_Babibet)

PHW_AW_10Bet = Prob10Bet [1,]-Prob10Bet [2,]
PT_10Bet  = Prob10Bet [3,]

plot(PHW_AW_10Bet ,PT_10Bet )
#in order to check find how many instances lie in interval by cut
#Attention I accounted [,) but this gives [,) so there might be slight differences

t_Tipico = table(cut(PHW_AW_Tipico, breaks = seq.int(from = -1, to = 1, by = 0.2))) #just for checking whether intervals cut space correctly

par(mfrow=c(2,2))

k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Tipico)


match_idFirstIntervalTipico = matrix(NA,ind11,1)
match_idSecondIntervalTipico = matrix(NA,ind11,1)
match_idThirdIntervalTipico = matrix(NA,ind11,1)
match_idFourthIntervalTipico = matrix(NA,ind11,1)
match_idFifthIntervalTipico = matrix(NA,ind11,1)
match_idSixthIntervalTipico = matrix(NA,ind11,1)
match_idSeventhIntervalTipico = matrix(NA,ind11,1)
match_idEighthIntervalTipico = matrix(NA,ind11,1)
match_idNinethIntervalTipico = matrix(NA,ind11,1)
match_idTenthIntervalTipico = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Tipico[g] >= -1)&&(PHW_AW_Tipico[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalTipico[k1] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.8)&&(PHW_AW_Tipico[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalTipico[k2] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.6)&&(PHW_AW_Tipico[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalTipico[k3] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.4)&&(PHW_AW_Tipico[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalTipico[k4] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.2)&&(PHW_AW_Tipico[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalTipico[k5] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0)&&(PHW_AW_Tipico[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalTipico[k6] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.2)&&(PHW_AW_Tipico[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalTipico[k7] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.4)&&(PHW_AW_Tipico[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalTipico[k8] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.6)&&(PHW_AW_Tipico[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalTipico[k9] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.8)&&(PHW_AW_Tipico[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalTipico[k10] <- Odds_1_Tipico_match_id[g]
  }
}


match_idFirstIntervalTipico = na.omit(match_idFirstIntervalTipico)
match_idSecondIntervalTipico = na.omit(match_idSecondIntervalTipico)
match_idThirdIntervalTipico = na.omit(match_idThirdIntervalTipico)
match_idFourthIntervalTipico = na.omit(match_idFourthIntervalTipico)
match_idFifthIntervalTipico = na.omit(match_idFifthIntervalTipico)
match_idSixthIntervalTipico = na.omit(match_idSixthIntervalTipico)
match_idSeventhIntervalTipico = na.omit(match_idSeventhIntervalTipico)
match_idEighthIntervalTipico = na.omit(match_idEighthIntervalTipico)
match_idNinethIntervalTipico = na.omit(match_idNinethIntervalTipico)
match_idTenthIntervalTipico = na.omit(match_idTenthIntervalTipico)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalTipico)>=1) {
  score1IntervalTipico = matrix(0,2,length(match_idFirstIntervalTipico))
  e = 1
  for(e in 1:length(match_idFirstIntervalTipico)){
    score1IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,9]
    score1IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,10]
    
  }
  
  diffScore1IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore1IntTipico = NA
}


if(length(match_idSecondIntervalTipico)>=1) {
  score2IntervalTipico = matrix(0,2,length(match_idSecondIntervalTipico))
  e = 1
  for(e in 1:length(match_idSecondIntervalTipico)){
    score2IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,9]
    score2IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,10]
    
  }
  
  diffScore2IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore2IntTipico = NA
}

if(length(match_idThirdIntervalTipico)>=1){
  score3IntervalTipico = matrix(0,2,length(match_idThirdIntervalTipico))
  e = 1
  for(e in 1:length(match_idThirdIntervalTipico)){
    score3IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,9]
    score3IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,10]
    
  }
  
  diffScore3IntTipico = score3IntervalTipico[1,]-score3IntervalTipico[2,]
} else {
  diffScore3IntTipico <- NA}

if(length(match_idFourthIntervalTipico)>=1){
  score4IntervalTipico = matrix(0,2,length(match_idFourthIntervalTipico))
  
  for(e in 1:length(match_idFourthIntervalTipico)){
    score4IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,9]
    score4IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,10]
    
  }
  
  diffScore4IntTipico = score4IntervalTipico[1,]-score4IntervalTipico[2,]
} else {
  diffScore4IntTipico <- NA}

if(length(match_idFifthIntervalTipico)>=1){
  score5IntervalTipico = matrix(0,2,length(match_idFifthIntervalTipico))
  
  for(e in 1:length(match_idFifthIntervalTipico)){
    score5IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,9]
    score5IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,10]
    
  }
  
  diffScore5IntTipico = score5IntervalTipico[1,]-score5IntervalTipico[2,]
} else {
  diffScore5IntTipico <- NA}

if(length(match_idSixthIntervalTipico)>=1){
  score6IntervalTipico = matrix(0,2,length(match_idSixthIntervalTipico))
  
  for(e in 1:length(match_idSixthIntervalTipico)){
    score6IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,9]
    score6IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,10]
    
  }
  
  diffScore6IntTipico = score6IntervalTipico[1,]-score6IntervalTipico[2,]
} else {
  diffScore6IntTipico <- NA}

if(length(match_idSeventhIntervalTipico)>=1){
  score7IntervalTipico = matrix(0,2,length(match_idSeventhIntervalTipico))
  
  for(e in 1:length(match_idSeventhIntervalTipico)){
    score7IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,9]
    score7IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,10]}
  diffScore7IntTipico = score7IntervalTipico[1,]-score7IntervalTipico[2,]
} else {
  diffScore7IntTipico <- NA
}

e = 1
if(length(match_idEighthIntervalTipico)>=1){
  score8IntervalTipico = matrix(0,2,length(match_idEighthIntervalTipico))
  
  for(e in 1:length(match_idEighthIntervalTipico)){
    score8IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,9]
    score8IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,10]}
  diffScore8IntTipico = score8IntervalTipico[1,]-score8IntervalTipico[2,]
} else {
  diffScore8IntTipico = NA
}

e = 1
if(length(match_idNinethIntervalTipico)>=1){
  score9IntervalTipico = matrix(0,2,length(match_idNinethIntervalTipico))
  
  for(e in 1:length(match_idNinethIntervalTipico)){
    score9IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,9]
    score9IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,10]}
  diffScore9IntTipico = score9IntervalTipico[1,]-score9IntervalTipico[2,]
} else {
  diffScore9IntTipico = NA
}

e = 1
if(length(match_idTenthIntervalTipico)>=1){
  score10IntervalTipico = matrix(0,2,length(match_idTenthIntervalTipico))
  
  for(e in 1:length(match_idTenthIntervalTipico)){
    score10IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,9]
    score10IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,10]}
  diffScore10IntTipico = score10IntervalTipico[1,]-score10IntervalTipico[2,]
} else {
  diffScore10IntTipico = NA
}


#In if (is.na(diffScore2IntTipico) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntTipico) == "FALSE"){
  draw1Tipico = length(which(diffScore1IntTipico==0))/length(diffScore1IntTipico)
} else {
  draw1Tipico = 0} 

if (is.na(diffScore2IntTipico) == "FALSE"){
  draw2Tipico = length(which(diffScore2IntTipico==0))/length(diffScore2IntTipico)
}else {
  draw2Tipico = 0} 

if (is.na(diffScore3IntTipico) == "FALSE"){
  draw3Tipico = length(which(diffScore3IntTipico==0))/length(diffScore3IntTipico)
} else {
  draw3Tipico = 0} 

if (is.na(diffScore4IntTipico) == "FALSE"){
  draw4Tipico = length(which(diffScore4IntTipico==0))/length(diffScore4IntTipico)
} else {
  draw4Tipico = 0} 

if (is.na(diffScore5IntTipico) == "FALSE"){
  draw5Tipico = length(which(diffScore5IntTipico==0))/length(diffScore5IntTipico)
} else {
  draw5Tipico = 0} 

if (is.na(diffScore6IntTipico) == "FALSE"){
  draw6Tipico = length(which(diffScore6IntTipico==0))/length(diffScore6IntTipico)
} else {
  draw6Tipico = 0} 

if (is.na(diffScore7IntTipico) == "FALSE"){
  draw7Tipico = length(which(diffScore7IntTipico==0))/length(diffScore7IntTipico)
} else {
  draw7Tipico = 0} 

if (is.na(diffScore8IntTipico) == "FALSE"){
  draw8Tipico = length(which(diffScore8IntTipico==0))/length(diffScore8IntTipico)
} else {
  draw8Tipico = 0} 

if (is.na(diffScore9IntTipico) == "FALSE"){
  draw9Tipico = length(which(diffScore9IntTipico==0))/length(diffScore9IntTipico)
} else {
  draw9Tipico = 0} 

if (is.na(diffScore10IntTipico) == "FALSE"){
  draw10Tipico = length(which(diffScore10IntTipico==0))/length(diffScore10IntTipico)
} else {
  draw10Tipico = 0} 


drawRealTipico = c(draw1Tipico,draw2Tipico,draw3Tipico,draw4Tipico,draw5Tipico,draw6Tipico,draw7Tipico,draw8Tipico,draw9Tipico,draw10Tipico)



plot(PHW_AW_Tipico,PT_Tipico, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Tipico",ylim = c(0,0.6))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealTipico,lty = 3, type = "b", pch = 19, col = "red")

print('Probability of winning of home team minus that of away team plotted against the probability of draw for Tipico bookmarker is shown on the figure. The red dots represent the actual values of draws. It can be seen that although the majority of the probability of draw games (average taken within intervals) is lower than the probability calculated based on the odds there are instances where the number of real draws is greater. Note that one point near to calculated probability close to 0.9 is not shown because the curve of probabilities calculated by odds was not clearly observable. It can be then concluded that if someone is thinking that the game going to be draw (so always playing for draw) will most probably loose but there is also the probability to win which is exceptional but high at some regions.')


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Leonbets)


match_idFirstIntervalLeonbets = matrix(NA,ind11,1)
match_idSecondIntervalLeonbets = matrix(NA,ind11,1)
match_idThirdIntervalLeonbets = matrix(NA,ind11,1)
match_idFourthIntervalLeonbets = matrix(NA,ind11,1)
match_idFifthIntervalLeonbets = matrix(NA,ind11,1)
match_idSixthIntervalLeonbets = matrix(NA,ind11,1)
match_idSeventhIntervalLeonbets = matrix(NA,ind11,1)
match_idEighthIntervalLeonbets = matrix(NA,ind11,1)
match_idNinethIntervalLeonbets = matrix(NA,ind11,1)
match_idTenthIntervalLeonbets = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Leonbets[g] >= -1)&&(PHW_AW_Leonbets[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalLeonbets[k1] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.8)&&(PHW_AW_Leonbets[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalLeonbets[k2] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.6)&&(PHW_AW_Leonbets[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalLeonbets[k3] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.4)&&(PHW_AW_Leonbets[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalLeonbets[k4] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.2)&&(PHW_AW_Leonbets[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalLeonbets[k5] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0)&&(PHW_AW_Leonbets[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalLeonbets[k6] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.2)&&(PHW_AW_Leonbets[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalLeonbets[k7] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.4)&&(PHW_AW_Leonbets[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalLeonbets[k8] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.6)&&(PHW_AW_Leonbets[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalLeonbets[k9] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.8)&&(PHW_AW_Leonbets[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalLeonbets[k10] <- Odds_1_Leonbets_match_id[g]
  }
}


match_idFirstIntervalLeonbets = na.omit(match_idFirstIntervalLeonbets)
match_idSecondIntervalLeonbets = na.omit(match_idSecondIntervalLeonbets)
match_idThirdIntervalLeonbets = na.omit(match_idThirdIntervalLeonbets)
match_idFourthIntervalLeonbets = na.omit(match_idFourthIntervalLeonbets)
match_idFifthIntervalLeonbets = na.omit(match_idFifthIntervalLeonbets)
match_idSixthIntervalLeonbets = na.omit(match_idSixthIntervalLeonbets)
match_idSeventhIntervalLeonbets = na.omit(match_idSeventhIntervalLeonbets)
match_idEighthIntervalLeonbets = na.omit(match_idEighthIntervalLeonbets)
match_idNinethIntervalLeonbets = na.omit(match_idNinethIntervalLeonbets)
match_idTenthIntervalLeonbets = na.omit(match_idTenthIntervalLeonbets)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalLeonbets)>=1) {
  score1IntervalLeonbets = matrix(0,2,length(match_idFirstIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idFirstIntervalLeonbets)){
    score1IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,9]
    score1IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore1IntLeonbets = score1IntervalLeonbets[1,]-score1IntervalLeonbets[2,]
} else {
  diffScore1IntLeonbets = NA
}


if(length(match_idSecondIntervalLeonbets)>=1) {
  score2IntervalLeonbets = matrix(0,2,length(match_idSecondIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idSecondIntervalLeonbets)){
    score2IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,9]
    score2IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore2IntLeonbets = score2IntervalLeonbets[1,]-score2IntervalLeonbets[2,]
} else {
  diffScore2IntLeonbets = NA
}

if(length(match_idThirdIntervalLeonbets)>=1){
  score3IntervalLeonbets = matrix(0,2,length(match_idThirdIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idThirdIntervalLeonbets)){
    score3IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,9]
    score3IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore3IntLeonbets = score3IntervalLeonbets[1,]-score3IntervalLeonbets[2,]
} else {
  diffScore3IntLeonbets <- NA}

if(length(match_idFourthIntervalLeonbets)>=1){
  score4IntervalLeonbets = matrix(0,2,length(match_idFourthIntervalLeonbets))
  
  for(e in 1:length(match_idFourthIntervalLeonbets)){
    score4IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,9]
    score4IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore4IntLeonbets = score4IntervalLeonbets[1,]-score4IntervalLeonbets[2,]
} else {
  diffScore4IntLeonbets <- NA}

if(length(match_idFifthIntervalLeonbets)>=1){
  score5IntervalLeonbets = matrix(0,2,length(match_idFifthIntervalLeonbets))
  
  for(e in 1:length(match_idFifthIntervalLeonbets)){
    score5IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,9]
    score5IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore5IntLeonbets = score5IntervalLeonbets[1,]-score5IntervalLeonbets[2,]
} else {
  diffScore5IntLeonbets <- NA}

if(length(match_idSixthIntervalLeonbets)>=1){
  score6IntervalLeonbets = matrix(0,2,length(match_idSixthIntervalLeonbets))
  
  for(e in 1:length(match_idSixthIntervalLeonbets)){
    score6IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,9]
    score6IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore6IntLeonbets = score6IntervalLeonbets[1,]-score6IntervalLeonbets[2,]
} else {
  diffScore6IntLeonbets <- NA}

if(length(match_idSeventhIntervalLeonbets)>=1){
  score7IntervalLeonbets = matrix(0,2,length(match_idSeventhIntervalLeonbets))
  
  for(e in 1:length(match_idSeventhIntervalLeonbets)){
    score7IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,9]
    score7IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,10]}
  diffScore7IntLeonbets = score7IntervalLeonbets[1,]-score7IntervalLeonbets[2,]
} else {
  diffScore7IntLeonbets <- NA
}

e = 1
if(length(match_idEighthIntervalLeonbets)>=1){
  score8IntervalLeonbets = matrix(0,2,length(match_idEighthIntervalLeonbets))
  
  for(e in 1:length(match_idEighthIntervalLeonbets)){
    score8IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,9]
    score8IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,10]}
  diffScore8IntLeonbets = score8IntervalLeonbets[1,]-score8IntervalLeonbets[2,]
} else {
  diffScore8IntLeonbets = NA
}

e = 1
if(length(match_idNinethIntervalLeonbets)>=1){
  score9IntervalLeonbets = matrix(0,2,length(match_idNinethIntervalLeonbets))
  
  for(e in 1:length(match_idNinethIntervalLeonbets)){
    score9IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,9]
    score9IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,10]}
  diffScore9IntLeonbets = score9IntervalLeonbets[1,]-score9IntervalLeonbets[2,]
} else {
  diffScore9IntLeonbets = NA
}

e = 1
if(length(match_idTenthIntervalLeonbets)>=1){
  score10IntervalLeonbets = matrix(0,2,length(match_idTenthIntervalLeonbets))
  
  for(e in 1:length(match_idTenthIntervalLeonbets)){
    score10IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,9]
    score10IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,10]}
  diffScore10IntLeonbets = score10IntervalLeonbets[1,]-score10IntervalLeonbets[2,]
} else {
  diffScore10IntLeonbets = NA
}


#In if (is.na(diffScore2IntLeonbets) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntLeonbets) == "FALSE"){
  draw1Leonbets = length(which(diffScore1IntLeonbets==0))/length(diffScore1IntLeonbets)
} else {
  draw1Leonbets = 0} 

if (is.na(diffScore2IntLeonbets) == "FALSE"){
  draw2Leonbets = length(which(diffScore2IntLeonbets==0))/length(diffScore2IntLeonbets)
}else {
  draw2Leonbets = 0} 

if (is.na(diffScore3IntLeonbets) == "FALSE"){
  draw3Leonbets = length(which(diffScore3IntLeonbets==0))/length(diffScore3IntLeonbets)
} else {
  draw3Leonbets = 0} 

if (is.na(diffScore4IntLeonbets) == "FALSE"){
  draw4Leonbets = length(which(diffScore4IntLeonbets==0))/length(diffScore4IntLeonbets)
} else {
  draw4Leonbets = 0} 

if (is.na(diffScore5IntLeonbets) == "FALSE"){
  draw5Leonbets = length(which(diffScore5IntLeonbets==0))/length(diffScore5IntLeonbets)
} else {
  draw5Leonbets = 0} 

if (is.na(diffScore6IntLeonbets) == "FALSE"){
  draw6Leonbets = length(which(diffScore6IntLeonbets==0))/length(diffScore6IntLeonbets)
} else {
  draw6Leonbets = 0} 

if (is.na(diffScore7IntLeonbets) == "FALSE"){
  draw7Leonbets = length(which(diffScore7IntLeonbets==0))/length(diffScore7IntLeonbets)
} else {
  draw7Leonbets = 0} 

if (is.na(diffScore8IntLeonbets) == "FALSE"){
  draw8Leonbets = length(which(diffScore8IntLeonbets==0))/length(diffScore8IntLeonbets)
} else {
  draw8Leonbets = 0} 

if (is.na(diffScore9IntLeonbets) == "FALSE"){
  draw9Leonbets = length(which(diffScore9IntLeonbets==0))/length(diffScore9IntLeonbets)
} else {
  draw9Leonbets = 0} 

if (is.na(diffScore10IntLeonbets) == "FALSE"){
  draw10Leonbets = length(which(diffScore10IntLeonbets==0))/length(diffScore10IntLeonbets)
} else {
  draw10Leonbets = 0} 


drawRealLeonbets = c(draw1Leonbets,draw2Leonbets,draw3Leonbets,draw4Leonbets,draw5Leonbets,draw6Leonbets,draw7Leonbets,draw8Leonbets,draw9Leonbets,draw10Leonbets)



plot(PHW_AW_Leonbets,PT_Leonbets, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Leonbets",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealLeonbets,lty = 3, type = "o",pch = 19, col = "red")

print('It is seen that mainly true number of draw matches is lower than what given from bookmarker Leonbets. The probabilities should be biased so as not for a person always playing for draw matches the probability to win to be low')


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Babibet)


match_idFirstIntervalBabibet = matrix(NA,ind11,1)
match_idSecondIntervalBabibet = matrix(NA,ind11,1)
match_idThirdIntervalBabibet = matrix(NA,ind11,1)
match_idFourthIntervalBabibet = matrix(NA,ind11,1)
match_idFifthIntervalBabibet = matrix(NA,ind11,1)
match_idSixthIntervalBabibet = matrix(NA,ind11,1)
match_idSeventhIntervalBabibet = matrix(NA,ind11,1)
match_idEighthIntervalBabibet = matrix(NA,ind11,1)
match_idNinethIntervalBabibet = matrix(NA,ind11,1)
match_idTenthIntervalBabibet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Babibet[g] >= -1)&&(PHW_AW_Babibet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalBabibet[k1] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.8)&&(PHW_AW_Babibet[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalBabibet[k2] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.6)&&(PHW_AW_Babibet[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalBabibet[k3] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.4)&&(PHW_AW_Babibet[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalBabibet[k4] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.2)&&(PHW_AW_Babibet[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalBabibet[k5] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0)&&(PHW_AW_Babibet[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalBabibet[k6] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.2)&&(PHW_AW_Babibet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalBabibet[k7] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.4)&&(PHW_AW_Babibet[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalBabibet[k8] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.6)&&(PHW_AW_Babibet[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalBabibet[k9] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.8)&&(PHW_AW_Babibet[g]< 1)){
    k10 = k10+1
    match_idTenthIntervalBabibet[k10] <- Odds_1_Babibet_match_id[g]
  }
}


match_idFirstIntervalBabibet = na.omit(match_idFirstIntervalBabibet)
match_idSecondIntervalBabibet = na.omit(match_idSecondIntervalBabibet)
match_idThirdIntervalBabibet = na.omit(match_idThirdIntervalBabibet)
match_idFourthIntervalBabibet = na.omit(match_idFourthIntervalBabibet)
match_idFifthIntervalBabibet = na.omit(match_idFifthIntervalBabibet)
match_idSixthIntervalBabibet = na.omit(match_idSixthIntervalBabibet)
match_idSeventhIntervalBabibet = na.omit(match_idSeventhIntervalBabibet)
match_idEighthIntervalBabibet = na.omit(match_idEighthIntervalBabibet)
match_idNinethIntervalBabibet = na.omit(match_idNinethIntervalBabibet)
match_idTenthIntervalBabibet = na.omit(match_idTenthIntervalBabibet)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalBabibet)>=1){
  score1IntervalBabibet = matrix(0,2,length(match_idFirstIntervalBabibet))
  e = 1
  for(e in 1:length(match_idFirstIntervalBabibet)){
    score1IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,9]
    score1IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,10]
    
  }
  
  diffScore1IntBabibet = score1IntervalBabibet[1,]-score1IntervalBabibet[2,]}else {
    diffScore1IntBabibet <- NA}


if(length(match_idSecondIntervalBabibet)>=1) {
  score2IntervalBabibet = matrix(0,2,length(match_idSecondIntervalBabibet))
  e = 1
  for(e in 1:length(match_idSecondIntervalBabibet)){
    score2IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,9]
    score2IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,10]
    
  }
  
  diffScore2IntBabibet = score2IntervalBabibet[1,]-score2IntervalBabibet[2,]
} else {
  diffScore2IntBabibet = NA
}

if(length(match_idThirdIntervalBabibet)>=1){
  score3IntervalBabibet = matrix(0,2,length(match_idThirdIntervalBabibet))
  e = 1
  for(e in 1:length(match_idThirdIntervalBabibet)){
    score3IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,9]
    score3IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,10]
    
  }
  
  diffScore3IntBabibet = score3IntervalBabibet[1,]-score3IntervalBabibet[2,]
} else {
  diffScore3IntBabibet <- NA}

if(length(match_idFourthIntervalBabibet)>=1){
  score4IntervalBabibet = matrix(0,2,length(match_idFourthIntervalBabibet))
  
  for(e in 1:length(match_idFourthIntervalBabibet)){
    score4IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,9]
    score4IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore4IntBabibet = score4IntervalBabibet[1,]-score4IntervalBabibet[2,]
} else {
  diffScore4IntBabibet <- NA}

if(length(match_idFifthIntervalBabibet)>=1){
  score5IntervalBabibet = matrix(0,2,length(match_idFifthIntervalBabibet))
  
  for(e in 1:length(match_idFifthIntervalBabibet)){
    score5IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,9]
    score5IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore5IntBabibet = score5IntervalBabibet[1,]-score5IntervalBabibet[2,]
} else {
  diffScore5IntBabibet <- NA}

if(length(match_idSixthIntervalBabibet)>=1){
  score6IntervalBabibet = matrix(0,2,length(match_idSixthIntervalBabibet))
  
  for(e in 1:length(match_idSixthIntervalBabibet)){
    score6IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,9]
    score6IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore6IntBabibet = score6IntervalBabibet[1,]-score6IntervalBabibet[2,]
} else {
  diffScore6IntBabibet <- NA}

if(length(match_idSeventhIntervalBabibet)>=1){
  score7IntervalBabibet = matrix(0,2,length(match_idSeventhIntervalBabibet))
  
  for(e in 1:length(match_idSeventhIntervalBabibet)){
    score7IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,9]
    score7IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,10]}
  diffScore7IntBabibet = score7IntervalBabibet[1,]-score7IntervalBabibet[2,]
} else {
  diffScore7IntBabibet <- NA
}

e = 1
if(length(match_idEighthIntervalBabibet)>=1){
  score8IntervalBabibet = matrix(0,2,length(match_idEighthIntervalBabibet))
  
  for(e in 1:length(match_idEighthIntervalBabibet)){
    score8IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,9]
    score8IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,10]}
  diffScore8IntBabibet = score8IntervalBabibet[1,]-score8IntervalBabibet[2,]
} else {
  diffScore8IntBabibet = NA
}

e = 1
if(length(match_idNinethIntervalBabibet)>=1){
  score9IntervalBabibet = matrix(0,2,length(match_idNinethIntervalBabibet))
  
  for(e in 1:length(match_idNinethIntervalBabibet)){
    score9IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,9]
    score9IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,10]}
  diffScore9IntBabibet = score9IntervalBabibet[1,]-score9IntervalBabibet[2,]
} else {
  diffScore9IntBabibet = NA
}

e = 1
if(length(match_idTenthIntervalBabibet)>=1){
  score10IntervalBabibet = matrix(0,2,length(match_idTenthIntervalBabibet))
  
  for(e in 1:length(match_idTenthIntervalBabibet)){
    score10IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,9]
    score10IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,10]}
  diffScore10IntBabibet = score10IntervalBabibet[1,]-score10IntervalBabibet[2,]
} else {
  diffScore10IntBabibet = NA
}


#In if (is.na(diffScore2IntBabibet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntBabibet) == "FALSE"){
  draw1Babibet = length(which(diffScore1IntBabibet==0))/length(diffScore1IntBabibet)
} else {
  draw1Babibet = 0} 

if (is.na(diffScore2IntBabibet) == "FALSE"){
  draw2Babibet = length(which(diffScore2IntBabibet==0))/length(diffScore2IntBabibet)
}else {
  draw2Babibet = 0} 

if (is.na(diffScore3IntBabibet) == "FALSE"){
  draw3Babibet = length(which(diffScore3IntBabibet==0))/length(diffScore3IntBabibet)
} else {
  draw3Babibet = 0} 

if (is.na(diffScore4IntBabibet) == "FALSE"){
  draw4Babibet = length(which(diffScore4IntBabibet==0))/length(diffScore4IntBabibet)
} else {
  draw4Babibet = 0} 

if (is.na(diffScore5IntBabibet) == "FALSE"){
  draw5Babibet = length(which(diffScore5IntBabibet==0))/length(diffScore5IntBabibet)
} else {
  draw5Babibet = 0} 

if (is.na(diffScore6IntBabibet) == "FALSE"){
  draw6Babibet = length(which(diffScore6IntBabibet==0))/length(diffScore6IntBabibet)
} else {
  draw6Babibet = 0} 

if (is.na(diffScore7IntBabibet) == "FALSE"){
  draw7Babibet = length(which(diffScore7IntBabibet==0))/length(diffScore7IntBabibet)
} else {
  draw7Babibet = 0} 

if (is.na(diffScore8IntBabibet) == "FALSE"){
  draw8Babibet = length(which(diffScore8IntBabibet==0))/length(diffScore8IntBabibet)
} else {
  draw8Babibet = 0} 

if (is.na(diffScore9IntBabibet) == "FALSE"){
  draw9Babibet = length(which(diffScore9IntBabibet==0))/length(diffScore9IntBabibet)
} else {
  draw9Babibet = 0} 

if (is.na(diffScore10IntBabibet) == "FALSE"){
  draw10Babibet = length(which(diffScore10IntBabibet==0))/length(diffScore10IntBabibet)
} else {
  draw10Babibet = 0} 


drawRealBabibet = c(draw1Babibet,draw2Babibet,draw3Babibet,draw4Babibet,draw5Babibet,draw6Babibet,draw7Babibet,draw8Babibet,draw9Babibet,draw10Babibet)


plot(PHW_AW_Babibet,PT_Babibet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Babibet",ylim = c(0,0.52))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealBabibet,lty = 3, type = "b", pch = 19, col = "red")

print('Except of two points which are above theoretical curve (P(draw) = 0.5 and 0.8 not shown) of calculated by the odds probabilities the rest of true probabilities lies below. So the real probability of getting truw draw values is lower than ')


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_10Bet)


match_idFirstInterval10Bet = matrix(NA,ind11,1)
match_idSecondInterval10Bet = matrix(NA,ind11,1)
match_idThirdInterval10Bet = matrix(NA,ind11,1)
match_idFourthInterval10Bet = matrix(NA,ind11,1)
match_idFifthInterval10Bet = matrix(NA,ind11,1)
match_idSixthInterval10Bet = matrix(NA,ind11,1)
match_idSeventhInterval10Bet = matrix(NA,ind11,1)
match_idEighthInterval10Bet = matrix(NA,ind11,1)
match_idNinethInterval10Bet = matrix(NA,ind11,1)
match_idTenthInterval10Bet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_10Bet[g] >= -1)&&(PHW_AW_10Bet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstInterval10Bet[k1] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.8)&&(PHW_AW_10Bet[g]< -0.6)){
    k2 = k2+1
    match_idSecondInterval10Bet[k2] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.6)&&(PHW_AW_10Bet[g]< -0.4)){
    k3 = k3+1
    match_idThirdInterval10Bet[k3] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.4)&&(PHW_AW_10Bet[g]< -0.2)){
    k4 = k4+1
    match_idFourthInterval10Bet[k4] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.2)&&(PHW_AW_10Bet[g]< 0)){
    k5 = k5+1
    match_idFifthInterval10Bet[k5] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0)&&(PHW_AW_10Bet[g]< 0.2)){
    k6 = k6+1
    match_idSixthInterval10Bet[k6] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.2)&&(PHW_AW_10Bet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhInterval10Bet[k7] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.4)&&(PHW_AW_10Bet[g]< 0.6)){
    k8 = k8+1
    match_idEighthInterval10Bet[k8] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.6)&&(PHW_AW_10Bet[g]< 0.8)){
    k9 = k9+1
    match_idNinethInterval10Bet[k9] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.8)&&(PHW_AW_10Bet[g]< 1)){
    k10 = k10+1
    match_idEighthInterval10Bet[k10] <- Odds_1_10Bet_match_id[g]
  }
}


match_idFirstInterval10Bet = na.omit(match_idFirstInterval10Bet)
match_idSecondInterval10Bet = na.omit(match_idSecondInterval10Bet)
match_idThirdInterval10Bet = na.omit(match_idThirdInterval10Bet)
match_idFourthInterval10Bet = na.omit(match_idFourthInterval10Bet)
match_idFifthInterval10Bet = na.omit(match_idFifthInterval10Bet)
match_idSixthInterval10Bet = na.omit(match_idSixthInterval10Bet)
match_idSeventhInterval10Bet = na.omit(match_idSeventhInterval10Bet)
match_idEighthInterval10Bet = na.omit(match_idEighthInterval10Bet)
match_idNinethInterval10Bet = na.omit(match_idNinethInterval10Bet)
match_idTenthInterval10Bet = na.omit(match_idTenthInterval10Bet)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstInterval10Bet)>=1) {
  score1Interval10Bet = matrix(0,2,length(match_idFirstInterval10Bet))
  e = 1
  for(e in 1:length(match_idFirstInterval10Bet)){
    score1Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,9]
    score1Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,10]
    
  }
  
  diffScore1Int10Bet = score1Interval10Bet[1,]-score1Interval10Bet[2,]
} else {
  diffScore1Int10Bet = NA
}


if(length(match_idSecondInterval10Bet)>=1) {
  score2Interval10Bet = matrix(0,2,length(match_idSecondInterval10Bet))
  e = 1
  for(e in 1:length(match_idSecondInterval10Bet)){
    score2Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,9]
    score2Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,10]
    
  }
  
  diffScore2Int10Bet = score2Interval10Bet[1,]-score2Interval10Bet[2,]
} else {
  diffScore2Int10Bet = NA
}

if(length(match_idThirdInterval10Bet)>=1){
  score3Interval10Bet = matrix(0,2,length(match_idThirdInterval10Bet))
  e = 1
  for(e in 1:length(match_idThirdInterval10Bet)){
    score3Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,9]
    score3Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,10]
    
  }
  
  diffScore3Int10Bet = score3Interval10Bet[1,]-score3Interval10Bet[2,]
} else {
  diffScore3Int10Bet <- NA}

if(length(match_idFourthInterval10Bet)>=1){
  score4Interval10Bet = matrix(0,2,length(match_idFourthInterval10Bet))
  
  for(e in 1:length(match_idFourthInterval10Bet)){
    score4Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,9]
    score4Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,10]
    
  }
  
  diffScore4Int10Bet = score4Interval10Bet[1,]-score4Interval10Bet[2,]
} else {
  diffScore4Int10Bet <- NA}

if(length(match_idFifthInterval10Bet)>=1){
  score5Interval10Bet = matrix(0,2,length(match_idFifthInterval10Bet))
  
  for(e in 1:length(match_idFifthInterval10Bet)){
    score5Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,9]
    score5Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,10]
    
  }
  
  diffScore5Int10Bet = score5Interval10Bet[1,]-score5Interval10Bet[2,]
} else {
  diffScore5Int10Bet <- NA}

if(length(match_idSixthInterval10Bet)>=1){
  score6Interval10Bet = matrix(0,2,length(match_idSixthInterval10Bet))
  
  for(e in 1:length(match_idSixthInterval10Bet)){
    score6Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,9]
    score6Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,10]
    
  }
  
  diffScore6Int10Bet = score6Interval10Bet[1,]-score6Interval10Bet[2,]
} else {
  diffScore6Int10Bet <- NA}

if(length(match_idSeventhInterval10Bet)>=1){
  score7Interval10Bet = matrix(0,2,length(match_idSeventhInterval10Bet))
  
  for(e in 1:length(match_idSeventhInterval10Bet)){
    score7Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,9]
    score7Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,10]}
  diffScore7Int10Bet = score7Interval10Bet[1,]-score7Interval10Bet[2,]
} else {
  diffScore7Int10Bet <- NA
}

e = 1
if(length(match_idEighthInterval10Bet)>=1){
  score8Interval10Bet = matrix(0,2,length(match_idEighthInterval10Bet))
  
  for(e in 1:length(match_idEighthInterval10Bet)){
    score8Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,9]
    score8Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,10]}
  diffScore8Int10Bet = score8Interval10Bet[1,]-score8Interval10Bet[2,]
} else {
  diffScore8Int10Bet = NA
}

e = 1
if(length(match_idNinethInterval10Bet)>=1){
  score9Interval10Bet = matrix(0,2,length(match_idNinethInterval10Bet))
  
  for(e in 1:length(match_idNinethInterval10Bet)){
    score9Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,9]
    score9Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,10]}
  diffScore9Int10Bet = score9Interval10Bet[1,]-score9Interval10Bet[2,]
} else {
  diffScore9Int10Bet = NA
}

e = 1
if(length(match_idTenthInterval10Bet)>=1){
  score10Interval10Bet = matrix(0,2,length(match_idTenthInterval10Bet))
  
  for(e in 1:length(match_idTenthInterval10Bet)){
    score10Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,9]
    score10Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,10]}
  diffScore10Int10Bet = score10Interval10Bet[1,]-score10Interval10Bet[2,]
} else {
  diffScore10Int10Bet = NA
}


#In if (is.na(diffScore2Int10Bet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1Int10Bet) == "FALSE"){
  draw110Bet = length(which(diffScore1Int10Bet==0))/length(diffScore1Int10Bet)
} else {
  draw110Bet = 0} 

if (is.na(diffScore2Int10Bet) == "FALSE"){
  draw210Bet = length(which(diffScore2Int10Bet==0))/length(diffScore2Int10Bet)
}else {
  draw210Bet = 0} 

if (is.na(diffScore3Int10Bet) == "FALSE"){
  draw310Bet = length(which(diffScore3Int10Bet==0))/length(diffScore3Int10Bet)
} else {
  draw310Bet = 0} 

if (is.na(diffScore4Int10Bet) == "FALSE"){
  draw410Bet = length(which(diffScore4Int10Bet==0))/length(diffScore4Int10Bet)
} else {
  draw410Bet = 0} 

if (is.na(diffScore5Int10Bet) == "FALSE"){
  draw510Bet = length(which(diffScore5Int10Bet==0))/length(diffScore5Int10Bet)
} else {
  draw510Bet = 0} 

if (is.na(diffScore6Int10Bet) == "FALSE"){
  draw610Bet = length(which(diffScore6Int10Bet==0))/length(diffScore6Int10Bet)
} else {
  draw610Bet = 0} 

if (is.na(diffScore7Int10Bet) == "FALSE"){
  draw710Bet = length(which(diffScore7Int10Bet==0))/length(diffScore7Int10Bet)
} else {
  draw710Bet = 0} 

if (is.na(diffScore8Int10Bet) == "FALSE"){
  draw810Bet = length(which(diffScore8Int10Bet==0))/length(diffScore8Int10Bet)
} else {
  draw810Bet = 0} 

if (is.na(diffScore9Int10Bet) == "FALSE"){
  draw910Bet = length(which(diffScore9Int10Bet==0))/length(diffScore9Int10Bet)
} else {
  draw910Bet = 0} 

if (is.na(diffScore10Int10Bet) == "FALSE"){
  draw1010Bet = length(which(diffScore10Int10Bet==0))/length(diffScore10Int10Bet)
} else {
  draw1010Bet = 0} 


drawReal10Bet = c(draw110Bet,draw210Bet,draw310Bet,draw410Bet,draw510Bet,draw610Bet,draw710Bet,draw810Bet,draw910Bet,draw1010Bet)



plot(PHW_AW_10Bet,PT_10Bet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "10Bet",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawReal10Bet,lty = 3, type = "b",pch = 19, col = "red")

print('All in all by the observation of all four figures plotted based on probabilities calculated using the odd values given by the bookmarkers and the real scores of the games it can be said that Tipico odd probabilities may be the least biased. However, we should be aware of the fact that there is much lower data point number for this bookmarker. Leonbets and 10Bet probabilities can be characterized more biased compared to Babibet  which are also biased.')

#Task 3 Removal of early red cards before 30th minute

myBookings <- read.csv(file="c:/Users/deniz/Desktop/booking.csv", header=TRUE, sep=",")
myStats <- read.csv(file="c:/Users/deniz/Desktop/stats.csv", header=TRUE, sep=",")
odds_data <- read.csv(file="c:/Users/deniz/Desktop/bets.csv", header=TRUE, sep=",")


data_Task3 = merge(matches,odds_data,by='match_id')
data_Task3=merge(data_Task3,myBookings,by='match_id')  #use match id to add stats and bookings info
data_Task3=merge(data_Task3,myStats,by='match_id')

data_Task3 = data_Task3[data_Task3$league_id == 148, ]
data_Task3 = data_Task3[data_Task3$card == "red card", ]

data_Task3$time <- as.numeric(data_Task3$time) 

data_Task3 = data_Task3[data_Task3$time < 30, ]

data_task3removeid = data_Task3[,c("match_id")]

print(' The total number of games removed is:')
print(length(data_task3removeid))

odds_data <- read.csv(file="c:/Users/deniz/Desktop/bets.csv", header=TRUE, sep=",")
matches_148 = matches[matches$league_id == 148, ] #selecting matches with league id 148
match_id148 = matches_148[,c("match_id")]
odds_dataid = odds_data[,c("match_id")]

##########finding odd information for league id: 148

findNaN <- match(odds_dataid,match_id148) 

k = 0
j = 0
z = 0
for(i in 1:length(findNaN)){
  k = k +1
  if(is.na(findNaN[i]) == "FALSE"){
    j = j+1
    z[j] <- k
  }}



task2Odds = odds_data[z,] #differentiating between draw, home team win and away team win odds (x,1 and 2 respectively)

task2Odds_x = task2Odds[task2Odds$variable == "odd_x", ]
task2Odds_1 = task2Odds[task2Odds$variable == "odd_1", ]
task2Odds_2 = task2Odds[task2Odds$variable == "odd_2", ]

#Tipico, Leonbets,Babibet,10Bet are the bookmakers selected
#Probability Calculations for all four bookmarkers
#Tipico
Odds_x_Tipico = task2Odds_x[task2Odds_x$odd_bookmakers  == "Tipico", ]
Odds_1_Tipico = task2Odds_1[task2Odds_1$odd_bookmakers  == "Tipico", ]
Odds_2_Tipico = task2Odds_2[task2Odds_2$odd_bookmakers  == "Tipico", ]

Odds_x_Tipico_match_id = Odds_x_Tipico[,"match_id"]
Odds_1_Tipico_match_id = Odds_1_Tipico[,"match_id"]
Odds_2_Tipico_match_id = Odds_2_Tipico[,"match_id"]

# match(Odds_x_Tipico_match_id,Odds_1_Tipico_match_id) control if match ids match
Pwin_Tipico = 1/Odds_1_Tipico[,"value"]
Pdraw_Tipico = 1/Odds_x_Tipico[,"value"]
Ploose_Tipico = 1/Odds_2_Tipico[,"value"]

TipicoFirst = rbind(Pwin_Tipico,Ploose_Tipico,Pdraw_Tipico)
TipicoSecond = colSums(TipicoFirst)

ProbTipico = sweep(TipicoFirst, 2, TipicoSecond, FUN = '/')

#Leonbets
Odds_x_Leonbets = task2Odds_x[task2Odds_x$odd_bookmakers  == "Leonbets", ]
Odds_1_Leonbets = task2Odds_1[task2Odds_1$odd_bookmakers  == "Leonbets", ]
Odds_2_Leonbets = task2Odds_2[task2Odds_2$odd_bookmakers  == "Leonbets", ]

Odds_x_Leonbets_match_id = Odds_x_Leonbets[,"match_id"]
Odds_1_Leonbets_match_id = Odds_1_Leonbets[,"match_id"]
Odds_2_Leonbets_match_id = Odds_2_Leonbets[,"match_id"]

Pwin_Leonbets = 1/Odds_1_Leonbets[,"value"]
Pdraw_Leonbets = 1/Odds_x_Leonbets[,"value"]
Ploose_Leonbets = 1/Odds_2_Leonbets[,"value"]

LeonbetsFirst = rbind(Pwin_Leonbets,Ploose_Leonbets,Pdraw_Leonbets)
LeonbetsSecond = colSums(LeonbetsFirst)

ProbLeonbets = sweep(LeonbetsFirst, 2, LeonbetsSecond, FUN = '/')

#Babibet
Odds_x_Babibet = task2Odds_x[task2Odds_x$odd_bookmakers  == "Babibet", ]
Odds_1_Babibet = task2Odds_1[task2Odds_1$odd_bookmakers  == "Babibet", ]
Odds_2_Babibet = task2Odds_2[task2Odds_2$odd_bookmakers  == "Babibet", ]

Odds_x_Babibet_match_id = Odds_x_Babibet[,"match_id"]
Odds_1_Babibet_match_id = Odds_1_Babibet[,"match_id"]
Odds_2_Babibet_match_id = Odds_2_Babibet[,"match_id"]

Pwin_Babibet = 1/Odds_1_Babibet[,"value"]
Pdraw_Babibet = 1/Odds_x_Babibet[,"value"]
Ploose_Babibet = 1/Odds_2_Babibet[,"value"]

BabibetFirst = rbind(Pwin_Babibet,Ploose_Babibet,Pdraw_Babibet)
BabibetSecond = colSums(BabibetFirst)

ProbBabibet = sweep(BabibetFirst, 2, BabibetSecond, FUN = '/')

#10Bet
Odds_x_10Bet = task2Odds_x[task2Odds_x$odd_bookmakers  == "10Bet", ]
Odds_1_10Bet = task2Odds_1[task2Odds_1$odd_bookmakers  == "10Bet", ]
Odds_2_10Bet = task2Odds_2[task2Odds_2$odd_bookmakers  == "10Bet", ]

Odds_x_10Bet_match_id = Odds_x_10Bet[,"match_id"]
Odds_1_10Bet_match_id = Odds_1_10Bet[,"match_id"]
Odds_2_10Bet_match_id = Odds_2_10Bet[,"match_id"]

Pwin_10Bet = 1/Odds_1_10Bet[,"value"]
Pdraw_10Bet = 1/Odds_x_10Bet[,"value"]
Ploose_10Bet = 1/Odds_2_10Bet[,"value"]

o10BetFirst = rbind(Pwin_10Bet,Ploose_10Bet,Pdraw_10Bet)
o10BetSecond = colSums(o10BetFirst)

Prob10Bet = sweep(o10BetFirst, 2, o10BetSecond, FUN = '/')


#Finding the values to be plot in Task2_3a



PHW_AW_Tipico = ProbTipico[1,]-ProbTipico[2,]
PT_Tipico = ProbTipico[3,]


PHW_AW_Leonbets = ProbLeonbets[1,]-ProbLeonbets[2,]
PT_Leonbets = ProbLeonbets[3,]



PHW_AW_Babibet = ProbBabibet[1,]-ProbBabibet[2,]
PT_Babibet = ProbBabibet[3,]



PHW_AW_10Bet = Prob10Bet [1,]-Prob10Bet [2,]
PT_10Bet  = Prob10Bet [3,]


#in order to check find how many instances lie in interval by cut
#Attention I accounted [,) but this gives [,) so there might be slight differences


t_Tipico = table(cut(PHW_AW_Tipico, breaks = seq.int(from = -1, to = 1, by = 0.2))) #just for checking whether intervals cut space correctly

par(mfrow=c(2,2))

k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Tipico)


match_idFirstIntervalTipico = matrix(NA,ind11,1)
match_idSecondIntervalTipico = matrix(NA,ind11,1)
match_idThirdIntervalTipico = matrix(NA,ind11,1)
match_idFourthIntervalTipico = matrix(NA,ind11,1)
match_idFifthIntervalTipico = matrix(NA,ind11,1)
match_idSixthIntervalTipico = matrix(NA,ind11,1)
match_idSeventhIntervalTipico = matrix(NA,ind11,1)
match_idEighthIntervalTipico = matrix(NA,ind11,1)
match_idNinethIntervalTipico = matrix(NA,ind11,1)
match_idTenthIntervalTipico = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Tipico[g] >= -1)&&(PHW_AW_Tipico[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalTipico[k1] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.8)&&(PHW_AW_Tipico[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalTipico[k2] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.6)&&(PHW_AW_Tipico[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalTipico[k3] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.4)&&(PHW_AW_Tipico[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalTipico[k4] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.2)&&(PHW_AW_Tipico[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalTipico[k5] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0)&&(PHW_AW_Tipico[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalTipico[k6] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.2)&&(PHW_AW_Tipico[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalTipico[k7] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.4)&&(PHW_AW_Tipico[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalTipico[k8] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.6)&&(PHW_AW_Tipico[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalTipico[k9] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.8)&&(PHW_AW_Tipico[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalTipico[k10] <- Odds_1_Tipico_match_id[g]
  }
}


match_idFirstIntervalTipico = na.omit(match_idFirstIntervalTipico)
match_idSecondIntervalTipico = na.omit(match_idSecondIntervalTipico)
match_idThirdIntervalTipico = na.omit(match_idThirdIntervalTipico)
match_idFourthIntervalTipico = na.omit(match_idFourthIntervalTipico)
match_idFifthIntervalTipico = na.omit(match_idFifthIntervalTipico)
match_idSixthIntervalTipico = na.omit(match_idSixthIntervalTipico)
match_idSeventhIntervalTipico = na.omit(match_idSeventhIntervalTipico)
match_idEighthIntervalTipico = na.omit(match_idEighthIntervalTipico)
match_idNinethIntervalTipico = na.omit(match_idNinethIntervalTipico)
match_idTenthIntervalTipico = na.omit(match_idTenthIntervalTipico)

match_idFirstIntervalTipico = setdiff(match_idFirstIntervalTipico,data_task3removeid)
match_idSecondIntervalTipico = setdiff(match_idSecondIntervalTipico,data_task3removeid)
match_idThirdIntervalTipico= setdiff(match_idThirdIntervalTipico,data_task3removeid)
match_idFourthIntervalTipico = setdiff(match_idFourthIntervalTipico,data_task3removeid)
match_idFifthIntervalTipico = setdiff(match_idFifthIntervalTipico,data_task3removeid)
match_idSixthIntervalTipico = setdiff(match_idSeventhIntervalTipico,data_task3removeid)
match_idEighthtervalTipico = setdiff(match_idEighthIntervalTipico,data_task3removeid)
match_idNinethIntervalTipico = setdiff(match_idNinethIntervalTipico,data_task3removeid)
match_idTenthIntervalTipico = setdiff(match_idTenthIntervalTipico,data_task3removeid)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalTipico)>=1) {
  score1IntervalTipico = matrix(0,2,length(match_idFirstIntervalTipico))
  e = 1
  for(e in 1:length(match_idFirstIntervalTipico)){
    score1IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,9]
    score1IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,10]
    
  }
  
  diffScore1IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore1IntTipico = NA
}


if(length(match_idSecondIntervalTipico)>=1) {
  score2IntervalTipico = matrix(0,2,length(match_idSecondIntervalTipico))
  e = 1
  for(e in 1:length(match_idSecondIntervalTipico)){
    score2IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,9]
    score2IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,10]
    
  }
  
  diffScore2IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore2IntTipico = NA
}

if(length(match_idThirdIntervalTipico)>=1){
  score3IntervalTipico = matrix(0,2,length(match_idThirdIntervalTipico))
  e = 1
  for(e in 1:length(match_idThirdIntervalTipico)){
    score3IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,9]
    score3IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,10]
    
  }
  
  diffScore3IntTipico = score3IntervalTipico[1,]-score3IntervalTipico[2,]
} else {
  diffScore3IntTipico <- NA}

if(length(match_idFourthIntervalTipico)>=1){
  score4IntervalTipico = matrix(0,2,length(match_idFourthIntervalTipico))
  
  for(e in 1:length(match_idFourthIntervalTipico)){
    score4IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,9]
    score4IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,10]
    
  }
  
  diffScore4IntTipico = score4IntervalTipico[1,]-score4IntervalTipico[2,]
} else {
  diffScore4IntTipico <- NA}

if(length(match_idFifthIntervalTipico)>=1){
  score5IntervalTipico = matrix(0,2,length(match_idFifthIntervalTipico))
  
  for(e in 1:length(match_idFifthIntervalTipico)){
    score5IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,9]
    score5IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,10]
    
  }
  
  diffScore5IntTipico = score5IntervalTipico[1,]-score5IntervalTipico[2,]
} else {
  diffScore5IntTipico <- NA}

if(length(match_idSixthIntervalTipico)>=1){
  score6IntervalTipico = matrix(0,2,length(match_idSixthIntervalTipico))
  
  for(e in 1:length(match_idSixthIntervalTipico)){
    score6IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,9]
    score6IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,10]
    
  }
  
  diffScore6IntTipico = score6IntervalTipico[1,]-score6IntervalTipico[2,]
} else {
  diffScore6IntTipico <- NA}

if(length(match_idSeventhIntervalTipico)>=1){
  score7IntervalTipico = matrix(0,2,length(match_idSeventhIntervalTipico))
  
  for(e in 1:length(match_idSeventhIntervalTipico)){
    score7IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,9]
    score7IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,10]}
  diffScore7IntTipico = score7IntervalTipico[1,]-score7IntervalTipico[2,]
} else {
  diffScore7IntTipico <- NA
}

e = 1
if(length(match_idEighthIntervalTipico)>=1){
  score8IntervalTipico = matrix(0,2,length(match_idEighthIntervalTipico))
  
  for(e in 1:length(match_idEighthIntervalTipico)){
    score8IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,9]
    score8IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,10]}
  diffScore8IntTipico = score8IntervalTipico[1,]-score8IntervalTipico[2,]
} else {
  diffScore8IntTipico = NA
}

e = 1
if(length(match_idNinethIntervalTipico)>=1){
  score9IntervalTipico = matrix(0,2,length(match_idNinethIntervalTipico))
  
  for(e in 1:length(match_idNinethIntervalTipico)){
    score9IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,9]
    score9IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,10]}
  diffScore9IntTipico = score9IntervalTipico[1,]-score9IntervalTipico[2,]
} else {
  diffScore9IntTipico = NA
}

e = 1
if(length(match_idTenthIntervalTipico)>=1){
  score10IntervalTipico = matrix(0,2,length(match_idTenthIntervalTipico))
  
  for(e in 1:length(match_idTenthIntervalTipico)){
    score10IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,9]
    score10IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,10]}
  diffScore10IntTipico = score10IntervalTipico[1,]-score10IntervalTipico[2,]
} else {
  diffScore10IntTipico = NA
}


#In if (is.na(diffScore2IntTipico) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntTipico) == "FALSE"){
  draw1Tipico = length(which(diffScore1IntTipico==0))/length(diffScore1IntTipico)
} else {
  draw1Tipico = 0} 

if (is.na(diffScore2IntTipico) == "FALSE"){
  draw2Tipico = length(which(diffScore2IntTipico==0))/length(diffScore2IntTipico)
}else {
  draw2Tipico = 0} 

if (is.na(diffScore3IntTipico) == "FALSE"){
  draw3Tipico = length(which(diffScore3IntTipico==0))/length(diffScore3IntTipico)
} else {
  draw3Tipico = 0} 

if (is.na(diffScore4IntTipico) == "FALSE"){
  draw4Tipico = length(which(diffScore4IntTipico==0))/length(diffScore4IntTipico)
} else {
  draw4Tipico = 0} 

if (is.na(diffScore5IntTipico) == "FALSE"){
  draw5Tipico = length(which(diffScore5IntTipico==0))/length(diffScore5IntTipico)
} else {
  draw5Tipico = 0} 

if (is.na(diffScore6IntTipico) == "FALSE"){
  draw6Tipico = length(which(diffScore6IntTipico==0))/length(diffScore6IntTipico)
} else {
  draw6Tipico = 0} 

if (is.na(diffScore7IntTipico) == "FALSE"){
  draw7Tipico = length(which(diffScore7IntTipico==0))/length(diffScore7IntTipico)
} else {
  draw7Tipico = 0} 

if (is.na(diffScore8IntTipico) == "FALSE"){
  draw8Tipico = length(which(diffScore8IntTipico==0))/length(diffScore8IntTipico)
} else {
  draw8Tipico = 0} 

if (is.na(diffScore9IntTipico) == "FALSE"){
  draw9Tipico = length(which(diffScore9IntTipico==0))/length(diffScore9IntTipico)
} else {
  draw9Tipico = 0} 

if (is.na(diffScore10IntTipico) == "FALSE"){
  draw10Tipico = length(which(diffScore10IntTipico==0))/length(diffScore10IntTipico)
} else {
  draw10Tipico = 0} 


drawRealTipicoT3 = c(draw1Tipico,draw2Tipico,draw3Tipico,draw4Tipico,draw5Tipico,draw6Tipico,draw7Tipico,draw8Tipico,draw9Tipico,draw10Tipico)



plot(PHW_AW_Tipico,PT_Tipico, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Tipico",ylim = c(0,0.6))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealTipico,lty = 3, type = "b", pch = 19, col = "red")
lines(drawRealX, drawRealTipicoT3,lty = 3, type = "b", pch = 19, col = "green")

print('When matches for which red card is given at the first 30 minutes are removed number of draw games based on real data no significant difference is observed except of the decrease for one probability Tipico')

k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Leonbets)


match_idFirstIntervalLeonbets = matrix(NA,ind11,1)
match_idSecondIntervalLeonbets = matrix(NA,ind11,1)
match_idThirdIntervalLeonbets = matrix(NA,ind11,1)
match_idFourthIntervalLeonbets = matrix(NA,ind11,1)
match_idFifthIntervalLeonbets = matrix(NA,ind11,1)
match_idSixthIntervalLeonbets = matrix(NA,ind11,1)
match_idSeventhIntervalLeonbets = matrix(NA,ind11,1)
match_idEighthIntervalLeonbets = matrix(NA,ind11,1)
match_idNinethIntervalLeonbets = matrix(NA,ind11,1)
match_idTenthIntervalLeonbets = matrix(NA,ind11,1)



#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Leonbets[g] >= -1)&&(PHW_AW_Leonbets[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalLeonbets[k1] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.8)&&(PHW_AW_Leonbets[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalLeonbets[k2] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.6)&&(PHW_AW_Leonbets[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalLeonbets[k3] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.4)&&(PHW_AW_Leonbets[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalLeonbets[k4] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.2)&&(PHW_AW_Leonbets[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalLeonbets[k5] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0)&&(PHW_AW_Leonbets[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalLeonbets[k6] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.2)&&(PHW_AW_Leonbets[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalLeonbets[k7] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.4)&&(PHW_AW_Leonbets[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalLeonbets[k8] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.6)&&(PHW_AW_Leonbets[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalLeonbets[k9] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.8)&&(PHW_AW_Leonbets[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalLeonbets[k10] <- Odds_1_Leonbets_match_id[g]
  }
}


match_idFirstIntervalLeonbets = na.omit(match_idFirstIntervalLeonbets)
match_idSecondIntervalLeonbets = na.omit(match_idSecondIntervalLeonbets)
match_idThirdIntervalLeonbets = na.omit(match_idThirdIntervalLeonbets)
match_idFourthIntervalLeonbets = na.omit(match_idFourthIntervalLeonbets)
match_idFifthIntervalLeonbets = na.omit(match_idFifthIntervalLeonbets)
match_idSixthIntervalLeonbets = na.omit(match_idSixthIntervalLeonbets)
match_idSeventhIntervalLeonbets = na.omit(match_idSeventhIntervalLeonbets)
match_idEighthIntervalLeonbets = na.omit(match_idEighthIntervalLeonbets)
match_idNinethIntervalLeonbets = na.omit(match_idNinethIntervalLeonbets)
match_idTenthIntervalLeonbets = na.omit(match_idTenthIntervalLeonbets)

match_idFirstIntervalLeonbets = setdiff(match_idFirstIntervalLeonbets,data_task3removeid)
match_idSecondIntervalLeonbets = setdiff(match_idSecondIntervalLeonbets,data_task3removeid)
match_idThirdIntervalLeonbets = setdiff(match_idThirdIntervalLeonbets,data_task3removeid)
match_idFourthIntervalLeonbets = setdiff(match_idFourthIntervalLeonbets,data_task3removeid)
match_idFifthIntervalLeonbets = setdiff(match_idFifthIntervalLeonbets,data_task3removeid)
match_idSixthIntervalLeonbets= setdiff(match_idSeventhIntervalLeonbets,data_task3removeid)
match_idEighthtervalLeonbets = setdiff(match_idEighthIntervalLeonbets,data_task3removeid)
match_idNinethIntervalLeonbets = setdiff(match_idNinethIntervalLeonbets,data_task3removeid)
match_idTenthIntervalLeonbets = setdiff(match_idTenthIntervalLeonbets,data_task3removeid)

#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalLeonbets)>=1) {
  score1IntervalLeonbets = matrix(0,2,length(match_idFirstIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idFirstIntervalLeonbets)){
    score1IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,9]
    score1IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore1IntLeonbets = score1IntervalLeonbets[1,]-score1IntervalLeonbets[2,]
} else {
  diffScore1IntLeonbets = NA
}


if(length(match_idSecondIntervalLeonbets)>=1) {
  score2IntervalLeonbets = matrix(0,2,length(match_idSecondIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idSecondIntervalLeonbets)){
    score2IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,9]
    score2IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore2IntLeonbets = score2IntervalLeonbets[1,]-score2IntervalLeonbets[2,]
} else {
  diffScore2IntLeonbets = NA
}

if(length(match_idThirdIntervalLeonbets)>=1){
  score3IntervalLeonbets = matrix(0,2,length(match_idThirdIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idThirdIntervalLeonbets)){
    score3IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,9]
    score3IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore3IntLeonbets = score3IntervalLeonbets[1,]-score3IntervalLeonbets[2,]
} else {
  diffScore3IntLeonbets <- NA}

if(length(match_idFourthIntervalLeonbets)>=1){
  score4IntervalLeonbets = matrix(0,2,length(match_idFourthIntervalLeonbets))
  
  for(e in 1:length(match_idFourthIntervalLeonbets)){
    score4IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,9]
    score4IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore4IntLeonbets = score4IntervalLeonbets[1,]-score4IntervalLeonbets[2,]
} else {
  diffScore4IntLeonbets <- NA}

if(length(match_idFifthIntervalLeonbets)>=1){
  score5IntervalLeonbets = matrix(0,2,length(match_idFifthIntervalLeonbets))
  
  for(e in 1:length(match_idFifthIntervalLeonbets)){
    score5IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,9]
    score5IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore5IntLeonbets = score5IntervalLeonbets[1,]-score5IntervalLeonbets[2,]
} else {
  diffScore5IntLeonbets <- NA}

if(length(match_idSixthIntervalLeonbets)>=1){
  score6IntervalLeonbets = matrix(0,2,length(match_idSixthIntervalLeonbets))
  
  for(e in 1:length(match_idSixthIntervalLeonbets)){
    score6IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,9]
    score6IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore6IntLeonbets = score6IntervalLeonbets[1,]-score6IntervalLeonbets[2,]
} else {
  diffScore6IntLeonbets <- NA}

if(length(match_idSeventhIntervalLeonbets)>=1){
  score7IntervalLeonbets = matrix(0,2,length(match_idSeventhIntervalLeonbets))
  
  for(e in 1:length(match_idSeventhIntervalLeonbets)){
    score7IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,9]
    score7IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,10]}
  diffScore7IntLeonbets = score7IntervalLeonbets[1,]-score7IntervalLeonbets[2,]
} else {
  diffScore7IntLeonbets <- NA
}

e = 1
if(length(match_idEighthIntervalLeonbets)>=1){
  score8IntervalLeonbets = matrix(0,2,length(match_idEighthIntervalLeonbets))
  
  for(e in 1:length(match_idEighthIntervalLeonbets)){
    score8IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,9]
    score8IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,10]}
  diffScore8IntLeonbets = score8IntervalLeonbets[1,]-score8IntervalLeonbets[2,]
} else {
  diffScore8IntLeonbets = NA
}

e = 1
if(length(match_idNinethIntervalLeonbets)>=1){
  score9IntervalLeonbets = matrix(0,2,length(match_idNinethIntervalLeonbets))
  
  for(e in 1:length(match_idNinethIntervalLeonbets)){
    score9IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,9]
    score9IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,10]}
  diffScore9IntLeonbets = score9IntervalLeonbets[1,]-score9IntervalLeonbets[2,]
} else {
  diffScore9IntLeonbets = NA
}

e = 1
if(length(match_idTenthIntervalLeonbets)>=1){
  score10IntervalLeonbets = matrix(0,2,length(match_idTenthIntervalLeonbets))
  
  for(e in 1:length(match_idTenthIntervalLeonbets)){
    score10IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,9]
    score10IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,10]}
  diffScore10IntLeonbets = score10IntervalLeonbets[1,]-score10IntervalLeonbets[2,]
} else {
  diffScore10IntLeonbets = NA
}


#In if (is.na(diffScore2IntLeonbets) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntLeonbets) == "FALSE"){
  draw1Leonbets = length(which(diffScore1IntLeonbets==0))/length(diffScore1IntLeonbets)
} else {
  draw1Leonbets = 0} 

if (is.na(diffScore2IntLeonbets) == "FALSE"){
  draw2Leonbets = length(which(diffScore2IntLeonbets==0))/length(diffScore2IntLeonbets)
}else {
  draw2Leonbets = 0} 

if (is.na(diffScore3IntLeonbets) == "FALSE"){
  draw3Leonbets = length(which(diffScore3IntLeonbets==0))/length(diffScore3IntLeonbets)
} else {
  draw3Leonbets = 0} 

if (is.na(diffScore4IntLeonbets) == "FALSE"){
  draw4Leonbets = length(which(diffScore4IntLeonbets==0))/length(diffScore4IntLeonbets)
} else {
  draw4Leonbets = 0} 

if (is.na(diffScore5IntLeonbets) == "FALSE"){
  draw5Leonbets = length(which(diffScore5IntLeonbets==0))/length(diffScore5IntLeonbets)
} else {
  draw5Leonbets = 0} 

if (is.na(diffScore6IntLeonbets) == "FALSE"){
  draw6Leonbets = length(which(diffScore6IntLeonbets==0))/length(diffScore6IntLeonbets)
} else {
  draw6Leonbets = 0} 

if (is.na(diffScore7IntLeonbets) == "FALSE"){
  draw7Leonbets = length(which(diffScore7IntLeonbets==0))/length(diffScore7IntLeonbets)
} else {
  draw7Leonbets = 0} 

if (is.na(diffScore8IntLeonbets) == "FALSE"){
  draw8Leonbets = length(which(diffScore8IntLeonbets==0))/length(diffScore8IntLeonbets)
} else {
  draw8Leonbets = 0} 

if (is.na(diffScore9IntLeonbets) == "FALSE"){
  draw9Leonbets = length(which(diffScore9IntLeonbets==0))/length(diffScore9IntLeonbets)
} else {
  draw9Leonbets = 0} 

if (is.na(diffScore10IntLeonbets) == "FALSE"){
  draw10Leonbets = length(which(diffScore10IntLeonbets==0))/length(diffScore10IntLeonbets)
} else {
  draw10Leonbets = 0} 


drawRealLeonbetsT3 = c(draw1Leonbets,draw2Leonbets,draw3Leonbets,draw4Leonbets,draw5Leonbets,draw6Leonbets,draw7Leonbets,draw8Leonbets,draw9Leonbets,draw10Leonbets)



plot(PHW_AW_Leonbets,PT_Leonbets, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Leonbets",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealLeonbets,lty = 3, type = "o",pch = 19, col = "red")
lines(drawRealX, drawRealLeonbetsT3,lty = 3, type = "o",pch = 19, col = "green")

print('No significance difference is observed for Leonbets as well.')


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Babibet)


match_idFirstIntervalBabibet = matrix(NA,ind11,1)
match_idSecondIntervalBabibet = matrix(NA,ind11,1)
match_idThirdIntervalBabibet = matrix(NA,ind11,1)
match_idFourthIntervalBabibet = matrix(NA,ind11,1)
match_idFifthIntervalBabibet = matrix(NA,ind11,1)
match_idSixthIntervalBabibet = matrix(NA,ind11,1)
match_idSeventhIntervalBabibet = matrix(NA,ind11,1)
match_idEighthIntervalBabibet = matrix(NA,ind11,1)
match_idNinethIntervalBabibet = matrix(NA,ind11,1)
match_idTenthIntervalBabibet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Babibet[g] >= -1)&&(PHW_AW_Babibet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalBabibet[k1] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.8)&&(PHW_AW_Babibet[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalBabibet[k2] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.6)&&(PHW_AW_Babibet[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalBabibet[k3] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.4)&&(PHW_AW_Babibet[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalBabibet[k4] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.2)&&(PHW_AW_Babibet[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalBabibet[k5] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0)&&(PHW_AW_Babibet[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalBabibet[k6] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.2)&&(PHW_AW_Babibet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalBabibet[k7] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.4)&&(PHW_AW_Babibet[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalBabibet[k8] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.6)&&(PHW_AW_Babibet[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalBabibet[k9] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.8)&&(PHW_AW_Babibet[g]< 1)){
    k10 = k10+1
    match_idTenthIntervalBabibet[k10] <- Odds_1_Babibet_match_id[g]
  }
}


match_idFirstIntervalBabibet = na.omit(match_idFirstIntervalBabibet)
match_idSecondIntervalBabibet = na.omit(match_idSecondIntervalBabibet)
match_idThirdIntervalBabibet = na.omit(match_idThirdIntervalBabibet)
match_idFourthIntervalBabibet = na.omit(match_idFourthIntervalBabibet)
match_idFifthIntervalBabibet = na.omit(match_idFifthIntervalBabibet)
match_idSixthIntervalBabibet = na.omit(match_idSixthIntervalBabibet)
match_idSeventhIntervalBabibet = na.omit(match_idSeventhIntervalBabibet)
match_idEighthIntervalBabibet = na.omit(match_idEighthIntervalBabibet)
match_idNinethIntervalBabibet = na.omit(match_idNinethIntervalBabibet)
match_idTenthIntervalBabibet = na.omit(match_idTenthIntervalBabibet)

match_idFirstIntervalBabibet  = setdiff(match_idFirstIntervalBabibet,data_task3removeid)
match_idSecondIntervalBabibet  = setdiff(match_idSecondIntervalBabibet,data_task3removeid)
match_idThirdIntervalBabibet  = setdiff(match_idThirdIntervalBabibet,data_task3removeid)
match_idFourthIntervalBabibet  = setdiff(match_idFourthIntervalBabibet,data_task3removeid)
match_idFifthIntervalBabibet  = setdiff(match_idFifthIntervalBabibet,data_task3removeid)
match_idSixthIntervalBabibet  = setdiff(match_idSeventhIntervalBabibet,data_task3removeid)
match_idEighthtervalBabibet  = setdiff(match_idEighthIntervalBabibet,data_task3removeid)
match_idNinethIntervalBabibet  = setdiff(match_idNinethIntervalBabibet,data_task3removeid)
match_idTenthIntervalBabibet  = setdiff(match_idTenthIntervalBabibet,data_task3removeid)



#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalBabibet)>=1){
  score1IntervalBabibet = matrix(0,2,length(match_idFirstIntervalBabibet))
  e = 1
  for(e in 1:length(match_idFirstIntervalBabibet)){
    score1IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,9]
    score1IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,10]
    
  }
  
  diffScore1IntBabibet = score1IntervalBabibet[1,]-score1IntervalBabibet[2,]}else {
    diffScore1IntBabibet <- NA}


if(length(match_idSecondIntervalBabibet)>=1) {
  score2IntervalBabibet = matrix(0,2,length(match_idSecondIntervalBabibet))
  e = 1
  for(e in 1:length(match_idSecondIntervalBabibet)){
    score2IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,9]
    score2IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,10]
    
  }
  
  diffScore2IntBabibet = score2IntervalBabibet[1,]-score2IntervalBabibet[2,]
} else {
  diffScore2IntBabibet = NA
}

if(length(match_idThirdIntervalBabibet)>=1){
  score3IntervalBabibet = matrix(0,2,length(match_idThirdIntervalBabibet))
  e = 1
  for(e in 1:length(match_idThirdIntervalBabibet)){
    score3IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,9]
    score3IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,10]
    
  }
  
  diffScore3IntBabibet = score3IntervalBabibet[1,]-score3IntervalBabibet[2,]
} else {
  diffScore3IntBabibet <- NA}

if(length(match_idFourthIntervalBabibet)>=1){
  score4IntervalBabibet = matrix(0,2,length(match_idFourthIntervalBabibet))
  
  for(e in 1:length(match_idFourthIntervalBabibet)){
    score4IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,9]
    score4IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore4IntBabibet = score4IntervalBabibet[1,]-score4IntervalBabibet[2,]
} else {
  diffScore4IntBabibet <- NA}

if(length(match_idFifthIntervalBabibet)>=1){
  score5IntervalBabibet = matrix(0,2,length(match_idFifthIntervalBabibet))
  
  for(e in 1:length(match_idFifthIntervalBabibet)){
    score5IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,9]
    score5IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore5IntBabibet = score5IntervalBabibet[1,]-score5IntervalBabibet[2,]
} else {
  diffScore5IntBabibet <- NA}

if(length(match_idSixthIntervalBabibet)>=1){
  score6IntervalBabibet = matrix(0,2,length(match_idSixthIntervalBabibet))
  
  for(e in 1:length(match_idSixthIntervalBabibet)){
    score6IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,9]
    score6IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore6IntBabibet = score6IntervalBabibet[1,]-score6IntervalBabibet[2,]
} else {
  diffScore6IntBabibet <- NA}

if(length(match_idSeventhIntervalBabibet)>=1){
  score7IntervalBabibet = matrix(0,2,length(match_idSeventhIntervalBabibet))
  
  for(e in 1:length(match_idSeventhIntervalBabibet)){
    score7IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,9]
    score7IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,10]}
  diffScore7IntBabibet = score7IntervalBabibet[1,]-score7IntervalBabibet[2,]
} else {
  diffScore7IntBabibet <- NA
}

e = 1
if(length(match_idEighthIntervalBabibet)>=1){
  score8IntervalBabibet = matrix(0,2,length(match_idEighthIntervalBabibet))
  
  for(e in 1:length(match_idEighthIntervalBabibet)){
    score8IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,9]
    score8IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,10]}
  diffScore8IntBabibet = score8IntervalBabibet[1,]-score8IntervalBabibet[2,]
} else {
  diffScore8IntBabibet = NA
}

e = 1
if(length(match_idNinethIntervalBabibet)>=1){
  score9IntervalBabibet = matrix(0,2,length(match_idNinethIntervalBabibet))
  
  for(e in 1:length(match_idNinethIntervalBabibet)){
    score9IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,9]
    score9IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,10]}
  diffScore9IntBabibet = score9IntervalBabibet[1,]-score9IntervalBabibet[2,]
} else {
  diffScore9IntBabibet = NA
}

e = 1
if(length(match_idTenthIntervalBabibet)>=1){
  score10IntervalBabibet = matrix(0,2,length(match_idTenthIntervalBabibet))
  
  for(e in 1:length(match_idTenthIntervalBabibet)){
    score10IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,9]
    score10IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,10]}
  diffScore10IntBabibet = score10IntervalBabibet[1,]-score10IntervalBabibet[2,]
} else {
  diffScore10IntBabibet = NA
}


#In if (is.na(diffScore2IntBabibet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntBabibet) == "FALSE"){
  draw1Babibet = length(which(diffScore1IntBabibet==0))/length(diffScore1IntBabibet)
} else {
  draw1Babibet = 0} 

if (is.na(diffScore2IntBabibet) == "FALSE"){
  draw2Babibet = length(which(diffScore2IntBabibet==0))/length(diffScore2IntBabibet)
}else {
  draw2Babibet = 0} 

if (is.na(diffScore3IntBabibet) == "FALSE"){
  draw3Babibet = length(which(diffScore3IntBabibet==0))/length(diffScore3IntBabibet)
} else {
  draw3Babibet = 0} 

if (is.na(diffScore4IntBabibet) == "FALSE"){
  draw4Babibet = length(which(diffScore4IntBabibet==0))/length(diffScore4IntBabibet)
} else {
  draw4Babibet = 0} 

if (is.na(diffScore5IntBabibet) == "FALSE"){
  draw5Babibet = length(which(diffScore5IntBabibet==0))/length(diffScore5IntBabibet)
} else {
  draw5Babibet = 0} 

if (is.na(diffScore6IntBabibet) == "FALSE"){
  draw6Babibet = length(which(diffScore6IntBabibet==0))/length(diffScore6IntBabibet)
} else {
  draw6Babibet = 0} 

if (is.na(diffScore7IntBabibet) == "FALSE"){
  draw7Babibet = length(which(diffScore7IntBabibet==0))/length(diffScore7IntBabibet)
} else {
  draw7Babibet = 0} 

if (is.na(diffScore8IntBabibet) == "FALSE"){
  draw8Babibet = length(which(diffScore8IntBabibet==0))/length(diffScore8IntBabibet)
} else {
  draw8Babibet = 0} 

if (is.na(diffScore9IntBabibet) == "FALSE"){
  draw9Babibet = length(which(diffScore9IntBabibet==0))/length(diffScore9IntBabibet)
} else {
  draw9Babibet = 0} 

if (is.na(diffScore10IntBabibet) == "FALSE"){
  draw10Babibet = length(which(diffScore10IntBabibet==0))/length(diffScore10IntBabibet)
} else {
  draw10Babibet = 0} 


drawRealBabibetT3 = c(draw1Babibet,draw2Babibet,draw3Babibet,draw4Babibet,draw5Babibet,draw6Babibet,draw7Babibet,draw8Babibet,draw9Babibet,draw10Babibet)


plot(PHW_AW_Babibet,PT_Babibet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Babibet",ylim = c(0,0.52))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealBabibet,lty = 3, type = "b", pch = 19, col = "red")
lines(drawRealX, drawRealBabibetT3,lty = 3, type = "b", pch = 19, col = "green")

print('Similar to the previous bookmarkers no significant but a bit higher for one point number of draw games is observed for Babibet.')


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_10Bet)


match_idFirstInterval10Bet = matrix(NA,ind11,1)
match_idSecondInterval10Bet = matrix(NA,ind11,1)
match_idThirdInterval10Bet = matrix(NA,ind11,1)
match_idFourthInterval10Bet = matrix(NA,ind11,1)
match_idFifthInterval10Bet = matrix(NA,ind11,1)
match_idSixthInterval10Bet = matrix(NA,ind11,1)
match_idSeventhInterval10Bet = matrix(NA,ind11,1)
match_idEighthInterval10Bet = matrix(NA,ind11,1)
match_idNinethInterval10Bet = matrix(NA,ind11,1)
match_idTenthInterval10Bet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_10Bet[g] >= -1)&&(PHW_AW_10Bet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstInterval10Bet[k1] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.8)&&(PHW_AW_10Bet[g]< -0.6)){
    k2 = k2+1
    match_idSecondInterval10Bet[k2] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.6)&&(PHW_AW_10Bet[g]< -0.4)){
    k3 = k3+1
    match_idThirdInterval10Bet[k3] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.4)&&(PHW_AW_10Bet[g]< -0.2)){
    k4 = k4+1
    match_idFourthInterval10Bet[k4] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.2)&&(PHW_AW_10Bet[g]< 0)){
    k5 = k5+1
    match_idFifthInterval10Bet[k5] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0)&&(PHW_AW_10Bet[g]< 0.2)){
    k6 = k6+1
    match_idSixthInterval10Bet[k6] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.2)&&(PHW_AW_10Bet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhInterval10Bet[k7] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.4)&&(PHW_AW_10Bet[g]< 0.6)){
    k8 = k8+1
    match_idEighthInterval10Bet[k8] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.6)&&(PHW_AW_10Bet[g]< 0.8)){
    k9 = k9+1
    match_idNinethInterval10Bet[k9] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.8)&&(PHW_AW_10Bet[g]< 1)){
    k10 = k10+1
    match_idEighthInterval10Bet[k10] <- Odds_1_10Bet_match_id[g]
  }
}


match_idFirstInterval10Bet = na.omit(match_idFirstInterval10Bet)
match_idSecondInterval10Bet = na.omit(match_idSecondInterval10Bet)
match_idThirdInterval10Bet = na.omit(match_idThirdInterval10Bet)
match_idFourthInterval10Bet = na.omit(match_idFourthInterval10Bet)
match_idFifthInterval10Bet = na.omit(match_idFifthInterval10Bet)
match_idSixthInterval10Bet = na.omit(match_idSixthInterval10Bet)
match_idSeventhInterval10Bet = na.omit(match_idSeventhInterval10Bet)
match_idEighthInterval10Bet = na.omit(match_idEighthInterval10Bet)
match_idNinethInterval10Bet = na.omit(match_idNinethInterval10Bet)
match_idTenthInterval10Bet = na.omit(match_idTenthInterval10Bet)

match_idFirstInterval10Bet  = setdiff(match_idFirstInterval10Bet, data_task3removeid)
match_idSecondInterval10Bet  = setdiff(match_idSecondInterval10Bet, data_task3removeid)
match_idThirdInterval10Bet  = setdiff(match_idThirdInterval10Bet, data_task3removeid)
match_idFourthInterval10Bet  = setdiff(match_idFourthInterval10Bet, data_task3removeid)
match_idFifthInterval10Bet  = setdiff(match_idFifthInterval10Bet, data_task3removeid)
match_idSixthInterval10Bet  = setdiff(match_idSeventhInterval10Bet, data_task3removeid)
match_idEighthterval10Bet  = setdiff(match_idEighthInterval10Bet, data_task3removeid)
match_idNinethInterval10Bet  = setdiff(match_idNinethInterval10Bet, data_task3removeid)
match_idTenthInterval10Bet  = setdiff(match_idTenthInterval10Bet, data_task3removeid)



#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstInterval10Bet)>=1) {
  score1Interval10Bet = matrix(0,2,length(match_idFirstInterval10Bet))
  e = 1
  for(e in 1:length(match_idFirstInterval10Bet)){
    score1Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,9]
    score1Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,10]
    
  }
  
  diffScore1Int10Bet = score1Interval10Bet[1,]-score1Interval10Bet[2,]
} else {
  diffScore1Int10Bet = NA
}


if(length(match_idSecondInterval10Bet)>=1) {
  score2Interval10Bet = matrix(0,2,length(match_idSecondInterval10Bet))
  e = 1
  for(e in 1:length(match_idSecondInterval10Bet)){
    score2Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,9]
    score2Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,10]
    
  }
  
  diffScore2Int10Bet = score2Interval10Bet[1,]-score2Interval10Bet[2,]
} else {
  diffScore2Int10Bet = NA
}

if(length(match_idThirdInterval10Bet)>=1){
  score3Interval10Bet = matrix(0,2,length(match_idThirdInterval10Bet))
  e = 1
  for(e in 1:length(match_idThirdInterval10Bet)){
    score3Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,9]
    score3Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,10]
    
  }
  
  diffScore3Int10Bet = score3Interval10Bet[1,]-score3Interval10Bet[2,]
} else {
  diffScore3Int10Bet <- NA}

if(length(match_idFourthInterval10Bet)>=1){
  score4Interval10Bet = matrix(0,2,length(match_idFourthInterval10Bet))
  
  for(e in 1:length(match_idFourthInterval10Bet)){
    score4Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,9]
    score4Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,10]
    
  }
  
  diffScore4Int10Bet = score4Interval10Bet[1,]-score4Interval10Bet[2,]
} else {
  diffScore4Int10Bet <- NA}

if(length(match_idFifthInterval10Bet)>=1){
  score5Interval10Bet = matrix(0,2,length(match_idFifthInterval10Bet))
  
  for(e in 1:length(match_idFifthInterval10Bet)){
    score5Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,9]
    score5Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,10]
    
  }
  
  diffScore5Int10Bet = score5Interval10Bet[1,]-score5Interval10Bet[2,]
} else {
  diffScore5Int10Bet <- NA}

if(length(match_idSixthInterval10Bet)>=1){
  score6Interval10Bet = matrix(0,2,length(match_idSixthInterval10Bet))
  
  for(e in 1:length(match_idSixthInterval10Bet)){
    score6Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,9]
    score6Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,10]
    
  }
  
  diffScore6Int10Bet = score6Interval10Bet[1,]-score6Interval10Bet[2,]
} else {
  diffScore6Int10Bet <- NA}

if(length(match_idSeventhInterval10Bet)>=1){
  score7Interval10Bet = matrix(0,2,length(match_idSeventhInterval10Bet))
  
  for(e in 1:length(match_idSeventhInterval10Bet)){
    score7Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,9]
    score7Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,10]}
  diffScore7Int10Bet = score7Interval10Bet[1,]-score7Interval10Bet[2,]
} else {
  diffScore7Int10Bet <- NA
}

e = 1
if(length(match_idEighthInterval10Bet)>=1){
  score8Interval10Bet = matrix(0,2,length(match_idEighthInterval10Bet))
  
  for(e in 1:length(match_idEighthInterval10Bet)){
    score8Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,9]
    score8Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,10]}
  diffScore8Int10Bet = score8Interval10Bet[1,]-score8Interval10Bet[2,]
} else {
  diffScore8Int10Bet = NA
}

e = 1
if(length(match_idNinethInterval10Bet)>=1){
  score9Interval10Bet = matrix(0,2,length(match_idNinethInterval10Bet))
  
  for(e in 1:length(match_idNinethInterval10Bet)){
    score9Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,9]
    score9Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,10]}
  diffScore9Int10Bet = score9Interval10Bet[1,]-score9Interval10Bet[2,]
} else {
  diffScore9Int10Bet = NA
}

e = 1
if(length(match_idTenthInterval10Bet)>=1){
  score10Interval10Bet = matrix(0,2,length(match_idTenthInterval10Bet))
  
  for(e in 1:length(match_idTenthInterval10Bet)){
    score10Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,9]
    score10Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,10]}
  diffScore10Int10Bet = score10Interval10Bet[1,]-score10Interval10Bet[2,]
} else {
  diffScore10Int10Bet = NA
}


#In if (is.na(diffScore2Int10Bet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1Int10Bet) == "FALSE"){
  draw110Bet = length(which(diffScore1Int10Bet==0))/length(diffScore1Int10Bet)
} else {
  draw110Bet = 0} 

if (is.na(diffScore2Int10Bet) == "FALSE"){
  draw210Bet = length(which(diffScore2Int10Bet==0))/length(diffScore2Int10Bet)
}else {
  draw210Bet = 0} 

if (is.na(diffScore3Int10Bet) == "FALSE"){
  draw310Bet = length(which(diffScore3Int10Bet==0))/length(diffScore3Int10Bet)
} else {
  draw310Bet = 0} 

if (is.na(diffScore4Int10Bet) == "FALSE"){
  draw410Bet = length(which(diffScore4Int10Bet==0))/length(diffScore4Int10Bet)
} else {
  draw410Bet = 0} 

if (is.na(diffScore5Int10Bet) == "FALSE"){
  draw510Bet = length(which(diffScore5Int10Bet==0))/length(diffScore5Int10Bet)
} else {
  draw510Bet = 0} 

if (is.na(diffScore6Int10Bet) == "FALSE"){
  draw610Bet = length(which(diffScore6Int10Bet==0))/length(diffScore6Int10Bet)
} else {
  draw610Bet = 0} 

if (is.na(diffScore7Int10Bet) == "FALSE"){
  draw710Bet = length(which(diffScore7Int10Bet==0))/length(diffScore7Int10Bet)
} else {
  draw710Bet = 0} 

if (is.na(diffScore8Int10Bet) == "FALSE"){
  draw810Bet = length(which(diffScore8Int10Bet==0))/length(diffScore8Int10Bet)
} else {
  draw810Bet = 0} 

if (is.na(diffScore9Int10Bet) == "FALSE"){
  draw910Bet = length(which(diffScore9Int10Bet==0))/length(diffScore9Int10Bet)
} else {
  draw910Bet = 0} 

if (is.na(diffScore10Int10Bet) == "FALSE"){
  draw1010Bet = length(which(diffScore10Int10Bet==0))/length(diffScore10Int10Bet)
} else {
  draw1010Bet = 0} 


drawReal10BetT3 = c(draw110Bet,draw210Bet,draw310Bet,draw410Bet,draw510Bet,draw610Bet,draw710Bet,draw810Bet,draw910Bet,draw1010Bet)



plot(PHW_AW_10Bet,PT_10Bet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "10Bet",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawReal10Bet,lty = 3, type = "b",pch = 19, col = "red")
lines(drawRealX, drawReal10BetT3,lty = 3, type = "b",pch = 19, col = "green")

print('No significance change is observed by the removal of early red card games')

#Task3b

myGoals <- read.csv(file="c:/Users/deniz/Desktop/goals.csv", header=TRUE, sep=",")
odds_data <- read.csv(file="c:/Users/deniz/Desktop/bets.csv", header=TRUE, sep=",")

data_Task3b = merge(matches,odds_data,by='match_id')
data_Task3b = merge(data_Task3b,myGoals,by='match_id')  #use match id to add stats and bookings info


data_Task3b$time <- as.numeric(data_Task3b$time)

data_Task3b = data_Task3b[data_Task3b$league_id == 148, ]
data_Task3b = data_Task3b[data_Task3b$time > 90, ] #removing matches with goals after 90 min

data_task3bremoveid = data_Task3b[,c("match_id")]


matches_148 = matches[matches$league_id == 148, ] #selecting matches with league id 148
match_id148 = matches_148[,c("match_id")]
odds_dataid = odds_data[,c("match_id")]

##########finding odd information for league id: 148

findNaN <- match(odds_dataid,match_id148) 

k = 0
j = 0
z = 0
for(i in 1:length(findNaN)){
  k = k +1
  if(is.na(findNaN[i]) == "FALSE"){
    j = j+1
    z[j] <- k
  }}



task2Odds = odds_data[z,] #differentiating between draw, home team win and away team win odds (x,1 and 2 respectively)

task2Odds_x = task2Odds[task2Odds$variable == "odd_x", ]
task2Odds_1 = task2Odds[task2Odds$variable == "odd_1", ]
task2Odds_2 = task2Odds[task2Odds$variable == "odd_2", ]

#Tipico, Leonbets,Babibet,10Bet are the bookmakers selected
#Probability Calculations for all four bookmarkers
#Tipico
Odds_x_Tipico = task2Odds_x[task2Odds_x$odd_bookmakers  == "Tipico", ]
Odds_1_Tipico = task2Odds_1[task2Odds_1$odd_bookmakers  == "Tipico", ]
Odds_2_Tipico = task2Odds_2[task2Odds_2$odd_bookmakers  == "Tipico", ]

Odds_x_Tipico_match_id = Odds_x_Tipico[,"match_id"]
Odds_1_Tipico_match_id = Odds_1_Tipico[,"match_id"]
Odds_2_Tipico_match_id = Odds_2_Tipico[,"match_id"]

# match(Odds_x_Tipico_match_id,Odds_1_Tipico_match_id) control if match ids match
Pwin_Tipico = 1/Odds_1_Tipico[,"value"]
Pdraw_Tipico = 1/Odds_x_Tipico[,"value"]
Ploose_Tipico = 1/Odds_2_Tipico[,"value"]

TipicoFirst = rbind(Pwin_Tipico,Ploose_Tipico,Pdraw_Tipico)
TipicoSecond = colSums(TipicoFirst)

ProbTipico = sweep(TipicoFirst, 2, TipicoSecond, FUN = '/')

#Leonbets
Odds_x_Leonbets = task2Odds_x[task2Odds_x$odd_bookmakers  == "Leonbets", ]
Odds_1_Leonbets = task2Odds_1[task2Odds_1$odd_bookmakers  == "Leonbets", ]
Odds_2_Leonbets = task2Odds_2[task2Odds_2$odd_bookmakers  == "Leonbets", ]

Odds_x_Leonbets_match_id = Odds_x_Leonbets[,"match_id"]
Odds_1_Leonbets_match_id = Odds_1_Leonbets[,"match_id"]
Odds_2_Leonbets_match_id = Odds_2_Leonbets[,"match_id"]

Pwin_Leonbets = 1/Odds_1_Leonbets[,"value"]
Pdraw_Leonbets = 1/Odds_x_Leonbets[,"value"]
Ploose_Leonbets = 1/Odds_2_Leonbets[,"value"]

LeonbetsFirst = rbind(Pwin_Leonbets,Ploose_Leonbets,Pdraw_Leonbets)
LeonbetsSecond = colSums(LeonbetsFirst)

ProbLeonbets = sweep(LeonbetsFirst, 2, LeonbetsSecond, FUN = '/')

#Babibet
Odds_x_Babibet = task2Odds_x[task2Odds_x$odd_bookmakers  == "Babibet", ]
Odds_1_Babibet = task2Odds_1[task2Odds_1$odd_bookmakers  == "Babibet", ]
Odds_2_Babibet = task2Odds_2[task2Odds_2$odd_bookmakers  == "Babibet", ]

Odds_x_Babibet_match_id = Odds_x_Babibet[,"match_id"]
Odds_1_Babibet_match_id = Odds_1_Babibet[,"match_id"]
Odds_2_Babibet_match_id = Odds_2_Babibet[,"match_id"]

Pwin_Babibet = 1/Odds_1_Babibet[,"value"]
Pdraw_Babibet = 1/Odds_x_Babibet[,"value"]
Ploose_Babibet = 1/Odds_2_Babibet[,"value"]

BabibetFirst = rbind(Pwin_Babibet,Ploose_Babibet,Pdraw_Babibet)
BabibetSecond = colSums(BabibetFirst)

ProbBabibet = sweep(BabibetFirst, 2, BabibetSecond, FUN = '/')

#10Bet
Odds_x_10Bet = task2Odds_x[task2Odds_x$odd_bookmakers  == "10Bet", ]
Odds_1_10Bet = task2Odds_1[task2Odds_1$odd_bookmakers  == "10Bet", ]
Odds_2_10Bet = task2Odds_2[task2Odds_2$odd_bookmakers  == "10Bet", ]

Odds_x_10Bet_match_id = Odds_x_10Bet[,"match_id"]
Odds_1_10Bet_match_id = Odds_1_10Bet[,"match_id"]
Odds_2_10Bet_match_id = Odds_2_10Bet[,"match_id"]

Pwin_10Bet = 1/Odds_1_10Bet[,"value"]
Pdraw_10Bet = 1/Odds_x_10Bet[,"value"]
Ploose_10Bet = 1/Odds_2_10Bet[,"value"]

o10BetFirst = rbind(Pwin_10Bet,Ploose_10Bet,Pdraw_10Bet)
o10BetSecond = colSums(o10BetFirst)

Prob10Bet = sweep(o10BetFirst, 2, o10BetSecond, FUN = '/')


#Finding the values to be plot in Task2_3a



PHW_AW_Tipico = ProbTipico[1,]-ProbTipico[2,]
PT_Tipico = ProbTipico[3,]


PHW_AW_Leonbets = ProbLeonbets[1,]-ProbLeonbets[2,]
PT_Leonbets = ProbLeonbets[3,]



PHW_AW_Babibet = ProbBabibet[1,]-ProbBabibet[2,]
PT_Babibet = ProbBabibet[3,]



PHW_AW_10Bet = Prob10Bet [1,]-Prob10Bet [2,]
PT_10Bet  = Prob10Bet [3,]


#in order to check find how many instances lie in interval by cut
#Attention I accounted [,) but this gives [,) so there might be slight differences

t_Tipico = table(cut(PHW_AW_Tipico, breaks = seq.int(from = -1, to = 1, by = 0.2))) #just for checking whether intervals cut space correctly

par(mfrow=c(2,2))

k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Tipico)


match_idFirstIntervalTipico = matrix(NA,ind11,1)
match_idSecondIntervalTipico = matrix(NA,ind11,1)
match_idThirdIntervalTipico = matrix(NA,ind11,1)
match_idFourthIntervalTipico = matrix(NA,ind11,1)
match_idFifthIntervalTipico = matrix(NA,ind11,1)
match_idSixthIntervalTipico = matrix(NA,ind11,1)
match_idSeventhIntervalTipico = matrix(NA,ind11,1)
match_idEighthIntervalTipico = matrix(NA,ind11,1)
match_idNinethIntervalTipico = matrix(NA,ind11,1)
match_idTenthIntervalTipico = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Tipico[g] >= -1)&&(PHW_AW_Tipico[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalTipico[k1] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.8)&&(PHW_AW_Tipico[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalTipico[k2] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.6)&&(PHW_AW_Tipico[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalTipico[k3] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.4)&&(PHW_AW_Tipico[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalTipico[k4] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= -0.2)&&(PHW_AW_Tipico[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalTipico[k5] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0)&&(PHW_AW_Tipico[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalTipico[k6] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.2)&&(PHW_AW_Tipico[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalTipico[k7] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.4)&&(PHW_AW_Tipico[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalTipico[k8] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.6)&&(PHW_AW_Tipico[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalTipico[k9] <- Odds_1_Tipico_match_id[g]
  }
  else if ( (PHW_AW_Tipico[g] >= 0.8)&&(PHW_AW_Tipico[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalTipico[k10] <- Odds_1_Tipico_match_id[g]
  }
}


match_idFirstIntervalTipico = na.omit(match_idFirstIntervalTipico)
match_idSecondIntervalTipico = na.omit(match_idSecondIntervalTipico)
match_idThirdIntervalTipico = na.omit(match_idThirdIntervalTipico)
match_idFourthIntervalTipico = na.omit(match_idFourthIntervalTipico)
match_idFifthIntervalTipico = na.omit(match_idFifthIntervalTipico)
match_idSixthIntervalTipico = na.omit(match_idSixthIntervalTipico)
match_idSeventhIntervalTipico = na.omit(match_idSeventhIntervalTipico)
match_idEighthIntervalTipico = na.omit(match_idEighthIntervalTipico)
match_idNinethIntervalTipico = na.omit(match_idNinethIntervalTipico)
match_idTenthIntervalTipico = na.omit(match_idTenthIntervalTipico)

match_idFirstIntervalTipico = setdiff(match_idFirstIntervalTipico,data_task3bremoveid) #removal of matches wıth extra time goals
match_idSecondIntervalTipico = setdiff(match_idSecondIntervalTipico,data_task3bremoveid)
match_idThirdIntervalTipico= setdiff(match_idThirdIntervalTipico,data_task3bremoveid)
match_idFourthIntervalTipico = setdiff(match_idFourthIntervalTipico,data_task3bremoveid)
match_idFifthIntervalTipico = setdiff(match_idFifthIntervalTipico,data_task3bremoveid)
match_idSixthIntervalTipico = setdiff(match_idSeventhIntervalTipico,data_task3bremoveid)
match_idEighthtervalTipico = setdiff(match_idEighthIntervalTipico,data_task3bremoveid)
match_idNinethIntervalTipico = setdiff(match_idNinethIntervalTipico,data_task3bremoveid)
match_idTenthIntervalTipico = setdiff(match_idTenthIntervalTipico,data_task3bremoveid)


#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalTipico)>=1) {
  score1IntervalTipico = matrix(0,2,length(match_idFirstIntervalTipico))
  e = 1
  for(e in 1:length(match_idFirstIntervalTipico)){
    score1IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,9]
    score1IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalTipico[e],][1,10]
    
  }
  
  diffScore1IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore1IntTipico = NA
}


if(length(match_idSecondIntervalTipico)>=1) {
  score2IntervalTipico = matrix(0,2,length(match_idSecondIntervalTipico))
  e = 1
  for(e in 1:length(match_idSecondIntervalTipico)){
    score2IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,9]
    score2IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalTipico[e],][1,10]
    
  }
  
  diffScore2IntTipico = score2IntervalTipico[1,]-score2IntervalTipico[2,]
} else {
  diffScore2IntTipico = NA
}

if(length(match_idThirdIntervalTipico)>=1){
  score3IntervalTipico = matrix(0,2,length(match_idThirdIntervalTipico))
  e = 1
  for(e in 1:length(match_idThirdIntervalTipico)){
    score3IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,9]
    score3IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalTipico[e],][1,10]
    
  }
  
  diffScore3IntTipico = score3IntervalTipico[1,]-score3IntervalTipico[2,]
} else {
  diffScore3IntTipico <- NA}

if(length(match_idFourthIntervalTipico)>=1){
  score4IntervalTipico = matrix(0,2,length(match_idFourthIntervalTipico))
  
  for(e in 1:length(match_idFourthIntervalTipico)){
    score4IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,9]
    score4IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalTipico[e],][1,10]
    
  }
  
  diffScore4IntTipico = score4IntervalTipico[1,]-score4IntervalTipico[2,]
} else {
  diffScore4IntTipico <- NA}

if(length(match_idFifthIntervalTipico)>=1){
  score5IntervalTipico = matrix(0,2,length(match_idFifthIntervalTipico))
  
  for(e in 1:length(match_idFifthIntervalTipico)){
    score5IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,9]
    score5IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalTipico[e],][1,10]
    
  }
  
  diffScore5IntTipico = score5IntervalTipico[1,]-score5IntervalTipico[2,]
} else {
  diffScore5IntTipico <- NA}

if(length(match_idSixthIntervalTipico)>=1){
  score6IntervalTipico = matrix(0,2,length(match_idSixthIntervalTipico))
  
  for(e in 1:length(match_idSixthIntervalTipico)){
    score6IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,9]
    score6IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalTipico[e],][1,10]
    
  }
  
  diffScore6IntTipico = score6IntervalTipico[1,]-score6IntervalTipico[2,]
} else {
  diffScore6IntTipico <- NA}

if(length(match_idSeventhIntervalTipico)>=1){
  score7IntervalTipico = matrix(0,2,length(match_idSeventhIntervalTipico))
  
  for(e in 1:length(match_idSeventhIntervalTipico)){
    score7IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,9]
    score7IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalTipico[e],][1,10]}
  diffScore7IntTipico = score7IntervalTipico[1,]-score7IntervalTipico[2,]
} else {
  diffScore7IntTipico <- NA
}

e = 1
if(length(match_idEighthIntervalTipico)>=1){
  score8IntervalTipico = matrix(0,2,length(match_idEighthIntervalTipico))
  
  for(e in 1:length(match_idEighthIntervalTipico)){
    score8IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,9]
    score8IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalTipico[e],][1,10]}
  diffScore8IntTipico = score8IntervalTipico[1,]-score8IntervalTipico[2,]
} else {
  diffScore8IntTipico = NA
}

e = 1
if(length(match_idNinethIntervalTipico)>=1){
  score9IntervalTipico = matrix(0,2,length(match_idNinethIntervalTipico))
  
  for(e in 1:length(match_idNinethIntervalTipico)){
    score9IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,9]
    score9IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalTipico[e],][1,10]}
  diffScore9IntTipico = score9IntervalTipico[1,]-score9IntervalTipico[2,]
} else {
  diffScore9IntTipico = NA
}

e = 1
if(length(match_idTenthIntervalTipico)>=1){
  score10IntervalTipico = matrix(0,2,length(match_idTenthIntervalTipico))
  
  for(e in 1:length(match_idTenthIntervalTipico)){
    score10IntervalTipico[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,9]
    score10IntervalTipico[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalTipico[e],][1,10]}
  diffScore10IntTipico = score10IntervalTipico[1,]-score10IntervalTipico[2,]
} else {
  diffScore10IntTipico = NA
}


#In if (is.na(diffScore2IntTipico) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntTipico) == "FALSE"){
  draw1Tipico = length(which(diffScore1IntTipico==0))/length(diffScore1IntTipico)
} else {
  draw1Tipico = 0} 

if (is.na(diffScore2IntTipico) == "FALSE"){
  draw2Tipico = length(which(diffScore2IntTipico==0))/length(diffScore2IntTipico)
}else {
  draw2Tipico = 0} 

if (is.na(diffScore3IntTipico) == "FALSE"){
  draw3Tipico = length(which(diffScore3IntTipico==0))/length(diffScore3IntTipico)
} else {
  draw3Tipico = 0} 

if (is.na(diffScore4IntTipico) == "FALSE"){
  draw4Tipico = length(which(diffScore4IntTipico==0))/length(diffScore4IntTipico)
} else {
  draw4Tipico = 0} 

if (is.na(diffScore5IntTipico) == "FALSE"){
  draw5Tipico = length(which(diffScore5IntTipico==0))/length(diffScore5IntTipico)
} else {
  draw5Tipico = 0} 

if (is.na(diffScore6IntTipico) == "FALSE"){
  draw6Tipico = length(which(diffScore6IntTipico==0))/length(diffScore6IntTipico)
} else {
  draw6Tipico = 0} 

if (is.na(diffScore7IntTipico) == "FALSE"){
  draw7Tipico = length(which(diffScore7IntTipico==0))/length(diffScore7IntTipico)
} else {
  draw7Tipico = 0} 

if (is.na(diffScore8IntTipico) == "FALSE"){
  draw8Tipico = length(which(diffScore8IntTipico==0))/length(diffScore8IntTipico)
} else {
  draw8Tipico = 0} 

if (is.na(diffScore9IntTipico) == "FALSE"){
  draw9Tipico = length(which(diffScore9IntTipico==0))/length(diffScore9IntTipico)
} else {
  draw9Tipico = 0} 

if (is.na(diffScore10IntTipico) == "FALSE"){
  draw10Tipico = length(which(diffScore10IntTipico==0))/length(diffScore10IntTipico)
} else {
  draw10Tipico = 0} 


drawRealTipicoT3b = c(draw1Tipico,draw2Tipico,draw3Tipico,draw4Tipico,draw5Tipico,draw6Tipico,draw7Tipico,draw8Tipico,draw9Tipico,draw10Tipico)



plot(PHW_AW_Tipico,PT_Tipico, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Tipico",ylim = c(0,0.6))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealTipico,lty = 3, type = "b", pch = 19, col = "red")
lines(drawRealX, drawRealTipicoT3b,lty = 3, type = "b", pch = 19, col = "blue")


k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Leonbets)


match_idFirstIntervalLeonbets = matrix(NA,ind11,1)
match_idSecondIntervalLeonbets = matrix(NA,ind11,1)
match_idThirdIntervalLeonbets = matrix(NA,ind11,1)
match_idFourthIntervalLeonbets = matrix(NA,ind11,1)
match_idFifthIntervalLeonbets = matrix(NA,ind11,1)
match_idSixthIntervalLeonbets = matrix(NA,ind11,1)
match_idSeventhIntervalLeonbets = matrix(NA,ind11,1)
match_idEighthIntervalLeonbets = matrix(NA,ind11,1)
match_idNinethIntervalLeonbets = matrix(NA,ind11,1)
match_idTenthIntervalLeonbets = matrix(NA,ind11,1)



#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Leonbets[g] >= -1)&&(PHW_AW_Leonbets[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalLeonbets[k1] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.8)&&(PHW_AW_Leonbets[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalLeonbets[k2] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.6)&&(PHW_AW_Leonbets[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalLeonbets[k3] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.4)&&(PHW_AW_Leonbets[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalLeonbets[k4] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= -0.2)&&(PHW_AW_Leonbets[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalLeonbets[k5] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0)&&(PHW_AW_Leonbets[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalLeonbets[k6] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.2)&&(PHW_AW_Leonbets[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalLeonbets[k7] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.4)&&(PHW_AW_Leonbets[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalLeonbets[k8] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.6)&&(PHW_AW_Leonbets[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalLeonbets[k9] <- Odds_1_Leonbets_match_id[g]
  }
  else if ( (PHW_AW_Leonbets[g] >= 0.8)&&(PHW_AW_Leonbets[g]< 1)){
    k10 = k10+1
    match_idEighthIntervalLeonbets[k10] <- Odds_1_Leonbets_match_id[g]
  }
}


match_idFirstIntervalLeonbets = na.omit(match_idFirstIntervalLeonbets)
match_idSecondIntervalLeonbets = na.omit(match_idSecondIntervalLeonbets)
match_idThirdIntervalLeonbets = na.omit(match_idThirdIntervalLeonbets)
match_idFourthIntervalLeonbets = na.omit(match_idFourthIntervalLeonbets)
match_idFifthIntervalLeonbets = na.omit(match_idFifthIntervalLeonbets)
match_idSixthIntervalLeonbets = na.omit(match_idSixthIntervalLeonbets)
match_idSeventhIntervalLeonbets = na.omit(match_idSeventhIntervalLeonbets)
match_idEighthIntervalLeonbets = na.omit(match_idEighthIntervalLeonbets)
match_idNinethIntervalLeonbets = na.omit(match_idNinethIntervalLeonbets)
match_idTenthIntervalLeonbets = na.omit(match_idTenthIntervalLeonbets)

match_idFirstIntervalLeonbets = setdiff(match_idFirstIntervalLeonbets,data_task3bremoveid)
match_idSecondIntervalLeonbets = setdiff(match_idSecondIntervalLeonbets,data_task3bremoveid)
match_idThirdIntervalLeonbets = setdiff(match_idThirdIntervalLeonbets,data_task3bremoveid)
match_idFourthIntervalLeonbets = setdiff(match_idFourthIntervalLeonbets,data_task3bremoveid)
match_idFifthIntervalLeonbets = setdiff(match_idFifthIntervalLeonbets,data_task3bremoveid)
match_idSixthIntervalLeonbets= setdiff(match_idSeventhIntervalLeonbets,data_task3bremoveid)
match_idEighthtervalLeonbets = setdiff(match_idEighthIntervalLeonbets,data_task3bremoveid)
match_idNinethIntervalLeonbets = setdiff(match_idNinethIntervalLeonbets,data_task3bremoveid)
match_idTenthIntervalLeonbets = setdiff(match_idTenthIntervalLeonbets,data_task3bremoveid)

#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalLeonbets)>=1) {
  score1IntervalLeonbets = matrix(0,2,length(match_idFirstIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idFirstIntervalLeonbets)){
    score1IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,9]
    score1IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore1IntLeonbets = score1IntervalLeonbets[1,]-score1IntervalLeonbets[2,]
} else {
  diffScore1IntLeonbets = NA
}


if(length(match_idSecondIntervalLeonbets)>=1) {
  score2IntervalLeonbets = matrix(0,2,length(match_idSecondIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idSecondIntervalLeonbets)){
    score2IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,9]
    score2IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore2IntLeonbets = score2IntervalLeonbets[1,]-score2IntervalLeonbets[2,]
} else {
  diffScore2IntLeonbets = NA
}

if(length(match_idThirdIntervalLeonbets)>=1){
  score3IntervalLeonbets = matrix(0,2,length(match_idThirdIntervalLeonbets))
  e = 1
  for(e in 1:length(match_idThirdIntervalLeonbets)){
    score3IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,9]
    score3IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore3IntLeonbets = score3IntervalLeonbets[1,]-score3IntervalLeonbets[2,]
} else {
  diffScore3IntLeonbets <- NA}

if(length(match_idFourthIntervalLeonbets)>=1){
  score4IntervalLeonbets = matrix(0,2,length(match_idFourthIntervalLeonbets))
  
  for(e in 1:length(match_idFourthIntervalLeonbets)){
    score4IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,9]
    score4IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore4IntLeonbets = score4IntervalLeonbets[1,]-score4IntervalLeonbets[2,]
} else {
  diffScore4IntLeonbets <- NA}

if(length(match_idFifthIntervalLeonbets)>=1){
  score5IntervalLeonbets = matrix(0,2,length(match_idFifthIntervalLeonbets))
  
  for(e in 1:length(match_idFifthIntervalLeonbets)){
    score5IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,9]
    score5IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore5IntLeonbets = score5IntervalLeonbets[1,]-score5IntervalLeonbets[2,]
} else {
  diffScore5IntLeonbets <- NA}

if(length(match_idSixthIntervalLeonbets)>=1){
  score6IntervalLeonbets = matrix(0,2,length(match_idSixthIntervalLeonbets))
  
  for(e in 1:length(match_idSixthIntervalLeonbets)){
    score6IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,9]
    score6IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalLeonbets[e],][1,10]
    
  }
  
  diffScore6IntLeonbets = score6IntervalLeonbets[1,]-score6IntervalLeonbets[2,]
} else {
  diffScore6IntLeonbets <- NA}

if(length(match_idSeventhIntervalLeonbets)>=1){
  score7IntervalLeonbets = matrix(0,2,length(match_idSeventhIntervalLeonbets))
  
  for(e in 1:length(match_idSeventhIntervalLeonbets)){
    score7IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,9]
    score7IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalLeonbets[e],][1,10]}
  diffScore7IntLeonbets = score7IntervalLeonbets[1,]-score7IntervalLeonbets[2,]
} else {
  diffScore7IntLeonbets <- NA
}

e = 1
if(length(match_idEighthIntervalLeonbets)>=1){
  score8IntervalLeonbets = matrix(0,2,length(match_idEighthIntervalLeonbets))
  
  for(e in 1:length(match_idEighthIntervalLeonbets)){
    score8IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,9]
    score8IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalLeonbets[e],][1,10]}
  diffScore8IntLeonbets = score8IntervalLeonbets[1,]-score8IntervalLeonbets[2,]
} else {
  diffScore8IntLeonbets = NA
}

e = 1
if(length(match_idNinethIntervalLeonbets)>=1){
  score9IntervalLeonbets = matrix(0,2,length(match_idNinethIntervalLeonbets))
  
  for(e in 1:length(match_idNinethIntervalLeonbets)){
    score9IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,9]
    score9IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalLeonbets[e],][1,10]}
  diffScore9IntLeonbets = score9IntervalLeonbets[1,]-score9IntervalLeonbets[2,]
} else {
  diffScore9IntLeonbets = NA
}

e = 1
if(length(match_idTenthIntervalLeonbets)>=1){
  score10IntervalLeonbets = matrix(0,2,length(match_idTenthIntervalLeonbets))
  
  for(e in 1:length(match_idTenthIntervalLeonbets)){
    score10IntervalLeonbets[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,9]
    score10IntervalLeonbets[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalLeonbets[e],][1,10]}
  diffScore10IntLeonbets = score10IntervalLeonbets[1,]-score10IntervalLeonbets[2,]
} else {
  diffScore10IntLeonbets = NA
}


#In if (is.na(diffScore2IntLeonbets) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntLeonbets) == "FALSE"){
  draw1Leonbets = length(which(diffScore1IntLeonbets==0))/length(diffScore1IntLeonbets)
} else {
  draw1Leonbets = 0} 

if (is.na(diffScore2IntLeonbets) == "FALSE"){
  draw2Leonbets = length(which(diffScore2IntLeonbets==0))/length(diffScore2IntLeonbets)
}else {
  draw2Leonbets = 0} 

if (is.na(diffScore3IntLeonbets) == "FALSE"){
  draw3Leonbets = length(which(diffScore3IntLeonbets==0))/length(diffScore3IntLeonbets)
} else {
  draw3Leonbets = 0} 

if (is.na(diffScore4IntLeonbets) == "FALSE"){
  draw4Leonbets = length(which(diffScore4IntLeonbets==0))/length(diffScore4IntLeonbets)
} else {
  draw4Leonbets = 0} 

if (is.na(diffScore5IntLeonbets) == "FALSE"){
  draw5Leonbets = length(which(diffScore5IntLeonbets==0))/length(diffScore5IntLeonbets)
} else {
  draw5Leonbets = 0} 

if (is.na(diffScore6IntLeonbets) == "FALSE"){
  draw6Leonbets = length(which(diffScore6IntLeonbets==0))/length(diffScore6IntLeonbets)
} else {
  draw6Leonbets = 0} 

if (is.na(diffScore7IntLeonbets) == "FALSE"){
  draw7Leonbets = length(which(diffScore7IntLeonbets==0))/length(diffScore7IntLeonbets)
} else {
  draw7Leonbets = 0} 

if (is.na(diffScore8IntLeonbets) == "FALSE"){
  draw8Leonbets = length(which(diffScore8IntLeonbets==0))/length(diffScore8IntLeonbets)
} else {
  draw8Leonbets = 0} 

if (is.na(diffScore9IntLeonbets) == "FALSE"){
  draw9Leonbets = length(which(diffScore9IntLeonbets==0))/length(diffScore9IntLeonbets)
} else {
  draw9Leonbets = 0} 

if (is.na(diffScore10IntLeonbets) == "FALSE"){
  draw10Leonbets = length(which(diffScore10IntLeonbets==0))/length(diffScore10IntLeonbets)
} else {
  draw10Leonbets = 0} 


drawRealLeonbetsT3b = c(draw1Leonbets,draw2Leonbets,draw3Leonbets,draw4Leonbets,draw5Leonbets,draw6Leonbets,draw7Leonbets,draw8Leonbets,draw9Leonbets,draw10Leonbets)



plot(PHW_AW_Leonbets,PT_Leonbets, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Leonbets",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealLeonbets,lty = 3, type = "o",pch = 19, col = "red")
lines(drawRealX, drawRealLeonbetsT3b,lty = 3, type = "o",pch = 19, col = "blue")



k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_Babibet)


match_idFirstIntervalBabibet = matrix(NA,ind11,1)
match_idSecondIntervalBabibet = matrix(NA,ind11,1)
match_idThirdIntervalBabibet = matrix(NA,ind11,1)
match_idFourthIntervalBabibet = matrix(NA,ind11,1)
match_idFifthIntervalBabibet = matrix(NA,ind11,1)
match_idSixthIntervalBabibet = matrix(NA,ind11,1)
match_idSeventhIntervalBabibet = matrix(NA,ind11,1)
match_idEighthIntervalBabibet = matrix(NA,ind11,1)
match_idNinethIntervalBabibet = matrix(NA,ind11,1)
match_idTenthIntervalBabibet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_Babibet[g] >= -1)&&(PHW_AW_Babibet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstIntervalBabibet[k1] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.8)&&(PHW_AW_Babibet[g]< -0.6)){
    k2 = k2+1
    match_idSecondIntervalBabibet[k2] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.6)&&(PHW_AW_Babibet[g]< -0.4)){
    k3 = k3+1
    match_idThirdIntervalBabibet[k3] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.4)&&(PHW_AW_Babibet[g]< -0.2)){
    k4 = k4+1
    match_idFourthIntervalBabibet[k4] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= -0.2)&&(PHW_AW_Babibet[g]< 0)){
    k5 = k5+1
    match_idFifthIntervalBabibet[k5] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0)&&(PHW_AW_Babibet[g]< 0.2)){
    k6 = k6+1
    match_idSixthIntervalBabibet[k6] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.2)&&(PHW_AW_Babibet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhIntervalBabibet[k7] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.4)&&(PHW_AW_Babibet[g]< 0.6)){
    k8 = k8+1
    match_idEighthIntervalBabibet[k8] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.6)&&(PHW_AW_Babibet[g]< 0.8)){
    k9 = k9+1
    match_idNinethIntervalBabibet[k9] <- Odds_1_Babibet_match_id[g]
  }
  else if ( (PHW_AW_Babibet[g] >= 0.8)&&(PHW_AW_Babibet[g]< 1)){
    k10 = k10+1
    match_idTenthIntervalBabibet[k10] <- Odds_1_Babibet_match_id[g]
  }
}


match_idFirstIntervalBabibet = na.omit(match_idFirstIntervalBabibet)
match_idSecondIntervalBabibet = na.omit(match_idSecondIntervalBabibet)
match_idThirdIntervalBabibet = na.omit(match_idThirdIntervalBabibet)
match_idFourthIntervalBabibet = na.omit(match_idFourthIntervalBabibet)
match_idFifthIntervalBabibet = na.omit(match_idFifthIntervalBabibet)
match_idSixthIntervalBabibet = na.omit(match_idSixthIntervalBabibet)
match_idSeventhIntervalBabibet = na.omit(match_idSeventhIntervalBabibet)
match_idEighthIntervalBabibet = na.omit(match_idEighthIntervalBabibet)
match_idNinethIntervalBabibet = na.omit(match_idNinethIntervalBabibet)
match_idTenthIntervalBabibet = na.omit(match_idTenthIntervalBabibet)

match_idFirstIntervalBabibet  = setdiff(match_idFirstIntervalBabibet,data_task3bremoveid)
match_idSecondIntervalBabibet  = setdiff(match_idSecondIntervalBabibet,data_task3bremoveid)
match_idThirdIntervalBabibet  = setdiff(match_idThirdIntervalBabibet,data_task3bremoveid)
match_idFourthIntervalBabibet  = setdiff(match_idFourthIntervalBabibet,data_task3bremoveid)
match_idFifthIntervalBabibet  = setdiff(match_idFifthIntervalBabibet,data_task3bremoveid)
match_idSixthIntervalBabibet  = setdiff(match_idSeventhIntervalBabibet,data_task3bremoveid)
match_idEighthtervalBabibet  = setdiff(match_idEighthIntervalBabibet,data_task3bremoveid)
match_idNinethIntervalBabibet  = setdiff(match_idNinethIntervalBabibet,data_task3bremoveid)
match_idTenthIntervalBabibet  = setdiff(match_idTenthIntervalBabibet,data_task3bremoveid)



#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstIntervalBabibet)>=1){
  score1IntervalBabibet = matrix(0,2,length(match_idFirstIntervalBabibet))
  e = 1
  for(e in 1:length(match_idFirstIntervalBabibet)){
    score1IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,9]
    score1IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFirstIntervalBabibet[e],][1,10]
    
  }
  
  diffScore1IntBabibet = score1IntervalBabibet[1,]-score1IntervalBabibet[2,]}else {
    diffScore1IntBabibet <- NA}


if(length(match_idSecondIntervalBabibet)>=1) {
  score2IntervalBabibet = matrix(0,2,length(match_idSecondIntervalBabibet))
  e = 1
  for(e in 1:length(match_idSecondIntervalBabibet)){
    score2IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,9]
    score2IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSecondIntervalBabibet[e],][1,10]
    
  }
  
  diffScore2IntBabibet = score2IntervalBabibet[1,]-score2IntervalBabibet[2,]
} else {
  diffScore2IntBabibet = NA
}

if(length(match_idThirdIntervalBabibet)>=1){
  score3IntervalBabibet = matrix(0,2,length(match_idThirdIntervalBabibet))
  e = 1
  for(e in 1:length(match_idThirdIntervalBabibet)){
    score3IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,9]
    score3IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idThirdIntervalBabibet[e],][1,10]
    
  }
  
  diffScore3IntBabibet = score3IntervalBabibet[1,]-score3IntervalBabibet[2,]
} else {
  diffScore3IntBabibet <- NA}

if(length(match_idFourthIntervalBabibet)>=1){
  score4IntervalBabibet = matrix(0,2,length(match_idFourthIntervalBabibet))
  
  for(e in 1:length(match_idFourthIntervalBabibet)){
    score4IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,9]
    score4IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFourthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore4IntBabibet = score4IntervalBabibet[1,]-score4IntervalBabibet[2,]
} else {
  diffScore4IntBabibet <- NA}

if(length(match_idFifthIntervalBabibet)>=1){
  score5IntervalBabibet = matrix(0,2,length(match_idFifthIntervalBabibet))
  
  for(e in 1:length(match_idFifthIntervalBabibet)){
    score5IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,9]
    score5IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idFifthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore5IntBabibet = score5IntervalBabibet[1,]-score5IntervalBabibet[2,]
} else {
  diffScore5IntBabibet <- NA}

if(length(match_idSixthIntervalBabibet)>=1){
  score6IntervalBabibet = matrix(0,2,length(match_idSixthIntervalBabibet))
  
  for(e in 1:length(match_idSixthIntervalBabibet)){
    score6IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,9]
    score6IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSixthIntervalBabibet[e],][1,10]
    
  }
  
  diffScore6IntBabibet = score6IntervalBabibet[1,]-score6IntervalBabibet[2,]
} else {
  diffScore6IntBabibet <- NA}

if(length(match_idSeventhIntervalBabibet)>=1){
  score7IntervalBabibet = matrix(0,2,length(match_idSeventhIntervalBabibet))
  
  for(e in 1:length(match_idSeventhIntervalBabibet)){
    score7IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,9]
    score7IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idSeventhIntervalBabibet[e],][1,10]}
  diffScore7IntBabibet = score7IntervalBabibet[1,]-score7IntervalBabibet[2,]
} else {
  diffScore7IntBabibet <- NA
}

e = 1
if(length(match_idEighthIntervalBabibet)>=1){
  score8IntervalBabibet = matrix(0,2,length(match_idEighthIntervalBabibet))
  
  for(e in 1:length(match_idEighthIntervalBabibet)){
    score8IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,9]
    score8IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idEighthIntervalBabibet[e],][1,10]}
  diffScore8IntBabibet = score8IntervalBabibet[1,]-score8IntervalBabibet[2,]
} else {
  diffScore8IntBabibet = NA
}

e = 1
if(length(match_idNinethIntervalBabibet)>=1){
  score9IntervalBabibet = matrix(0,2,length(match_idNinethIntervalBabibet))
  
  for(e in 1:length(match_idNinethIntervalBabibet)){
    score9IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,9]
    score9IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idNinethIntervalBabibet[e],][1,10]}
  diffScore9IntBabibet = score9IntervalBabibet[1,]-score9IntervalBabibet[2,]
} else {
  diffScore9IntBabibet = NA
}

e = 1
if(length(match_idTenthIntervalBabibet)>=1){
  score10IntervalBabibet = matrix(0,2,length(match_idTenthIntervalBabibet))
  
  for(e in 1:length(match_idTenthIntervalBabibet)){
    score10IntervalBabibet[1,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,9]
    score10IntervalBabibet[2,e] <- matches_148[matches_148$match_id == match_idTenthIntervalBabibet[e],][1,10]}
  diffScore10IntBabibet = score10IntervalBabibet[1,]-score10IntervalBabibet[2,]
} else {
  diffScore10IntBabibet = NA
}


#In if (is.na(diffScore2IntBabibet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1IntBabibet) == "FALSE"){
  draw1Babibet = length(which(diffScore1IntBabibet==0))/length(diffScore1IntBabibet)
} else {
  draw1Babibet = 0} 

if (is.na(diffScore2IntBabibet) == "FALSE"){
  draw2Babibet = length(which(diffScore2IntBabibet==0))/length(diffScore2IntBabibet)
}else {
  draw2Babibet = 0} 

if (is.na(diffScore3IntBabibet) == "FALSE"){
  draw3Babibet = length(which(diffScore3IntBabibet==0))/length(diffScore3IntBabibet)
} else {
  draw3Babibet = 0} 

if (is.na(diffScore4IntBabibet) == "FALSE"){
  draw4Babibet = length(which(diffScore4IntBabibet==0))/length(diffScore4IntBabibet)
} else {
  draw4Babibet = 0} 

if (is.na(diffScore5IntBabibet) == "FALSE"){
  draw5Babibet = length(which(diffScore5IntBabibet==0))/length(diffScore5IntBabibet)
} else {
  draw5Babibet = 0} 

if (is.na(diffScore6IntBabibet) == "FALSE"){
  draw6Babibet = length(which(diffScore6IntBabibet==0))/length(diffScore6IntBabibet)
} else {
  draw6Babibet = 0} 

if (is.na(diffScore7IntBabibet) == "FALSE"){
  draw7Babibet = length(which(diffScore7IntBabibet==0))/length(diffScore7IntBabibet)
} else {
  draw7Babibet = 0} 

if (is.na(diffScore8IntBabibet) == "FALSE"){
  draw8Babibet = length(which(diffScore8IntBabibet==0))/length(diffScore8IntBabibet)
} else {
  draw8Babibet = 0} 

if (is.na(diffScore9IntBabibet) == "FALSE"){
  draw9Babibet = length(which(diffScore9IntBabibet==0))/length(diffScore9IntBabibet)
} else {
  draw9Babibet = 0} 

if (is.na(diffScore10IntBabibet) == "FALSE"){
  draw10Babibet = length(which(diffScore10IntBabibet==0))/length(diffScore10IntBabibet)
} else {
  draw10Babibet = 0} 


drawRealBabibetT3b = c(draw1Babibet,draw2Babibet,draw3Babibet,draw4Babibet,draw5Babibet,draw6Babibet,draw7Babibet,draw8Babibet,draw9Babibet,draw10Babibet)


plot(PHW_AW_Babibet,PT_Babibet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "Babibet",ylim = c(0,0.52))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawRealBabibet,lty = 3, type = "b", pch = 19, col = "red")
lines(drawRealX, drawRealBabibetT3b,lty = 3, type = "b", pch = 19, col = "blue")



k1 = 0 #initiation of variables 
k2 = 0
k3 = 0
k4 = 0
k5 = 0
k6 = 0
k7 = 0
k8 = 0
k9 = 0
k10 = 0
g = 0

ind11 = length(PHW_AW_10Bet)


match_idFirstInterval10Bet = matrix(NA,ind11,1)
match_idSecondInterval10Bet = matrix(NA,ind11,1)
match_idThirdInterval10Bet = matrix(NA,ind11,1)
match_idFourthInterval10Bet = matrix(NA,ind11,1)
match_idFifthInterval10Bet = matrix(NA,ind11,1)
match_idSixthInterval10Bet = matrix(NA,ind11,1)
match_idSeventhInterval10Bet = matrix(NA,ind11,1)
match_idEighthInterval10Bet = matrix(NA,ind11,1)
match_idNinethInterval10Bet = matrix(NA,ind11,1)
match_idTenthInterval10Bet = matrix(NA,ind11,1)

#finding odds at each interval

for (g in 1:ind11){
  if ( (PHW_AW_10Bet[g] >= -1)&&(PHW_AW_10Bet[g]< -0.8) ){
    k1 = k1+1
    match_idFirstInterval10Bet[k1] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.8)&&(PHW_AW_10Bet[g]< -0.6)){
    k2 = k2+1
    match_idSecondInterval10Bet[k2] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.6)&&(PHW_AW_10Bet[g]< -0.4)){
    k3 = k3+1
    match_idThirdInterval10Bet[k3] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.4)&&(PHW_AW_10Bet[g]< -0.2)){
    k4 = k4+1
    match_idFourthInterval10Bet[k4] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= -0.2)&&(PHW_AW_10Bet[g]< 0)){
    k5 = k5+1
    match_idFifthInterval10Bet[k5] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0)&&(PHW_AW_10Bet[g]< 0.2)){
    k6 = k6+1
    match_idSixthInterval10Bet[k6] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.2)&&(PHW_AW_10Bet[g]< 0.4)){
    k7 = k7+1
    match_idSeventhInterval10Bet[k7] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.4)&&(PHW_AW_10Bet[g]< 0.6)){
    k8 = k8+1
    match_idEighthInterval10Bet[k8] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.6)&&(PHW_AW_10Bet[g]< 0.8)){
    k9 = k9+1
    match_idNinethInterval10Bet[k9] <- Odds_1_10Bet_match_id[g]
  }
  else if ( (PHW_AW_10Bet[g] >= 0.8)&&(PHW_AW_10Bet[g]< 1)){
    k10 = k10+1
    match_idEighthInterval10Bet[k10] <- Odds_1_10Bet_match_id[g]
  }
}


match_idFirstInterval10Bet = na.omit(match_idFirstInterval10Bet)
match_idSecondInterval10Bet = na.omit(match_idSecondInterval10Bet)
match_idThirdInterval10Bet = na.omit(match_idThirdInterval10Bet)
match_idFourthInterval10Bet = na.omit(match_idFourthInterval10Bet)
match_idFifthInterval10Bet = na.omit(match_idFifthInterval10Bet)
match_idSixthInterval10Bet = na.omit(match_idSixthInterval10Bet)
match_idSeventhInterval10Bet = na.omit(match_idSeventhInterval10Bet)
match_idEighthInterval10Bet = na.omit(match_idEighthInterval10Bet)
match_idNinethInterval10Bet = na.omit(match_idNinethInterval10Bet)
match_idTenthInterval10Bet = na.omit(match_idTenthInterval10Bet)

match_idFirstInterval10Bet  = setdiff(match_idFirstInterval10Bet, data_task3bremoveid)
match_idSecondInterval10Bet  = setdiff(match_idSecondInterval10Bet, data_task3bremoveid)
match_idThirdInterval10Bet  = setdiff(match_idThirdInterval10Bet, data_task3bremoveid)
match_idFourthInterval10Bet  = setdiff(match_idFourthInterval10Bet, data_task3bremoveid)
match_idFifthInterval10Bet  = setdiff(match_idFifthInterval10Bet, data_task3bremoveid)
match_idSixthInterval10Bet  = setdiff(match_idSeventhInterval10Bet, data_task3bremoveid)
match_idEighthterval10Bet  = setdiff(match_idEighthInterval10Bet, data_task3bremoveid)
match_idNinethInterval10Bet  = setdiff(match_idNinethInterval10Bet, data_task3bremoveid)
match_idTenthInterval10Bet  = setdiff(match_idTenthInterval10Bet, data_task3bremoveid)



#finding difference in home and away goals to be used in finding which matches are actually draw
if(length(match_idFirstInterval10Bet)>=1) {
  score1Interval10Bet = matrix(0,2,length(match_idFirstInterval10Bet))
  e = 1
  for(e in 1:length(match_idFirstInterval10Bet)){
    score1Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,9]
    score1Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFirstInterval10Bet[e],][1,10]
    
  }
  
  diffScore1Int10Bet = score1Interval10Bet[1,]-score1Interval10Bet[2,]
} else {
  diffScore1Int10Bet = NA
}


if(length(match_idSecondInterval10Bet)>=1) {
  score2Interval10Bet = matrix(0,2,length(match_idSecondInterval10Bet))
  e = 1
  for(e in 1:length(match_idSecondInterval10Bet)){
    score2Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,9]
    score2Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSecondInterval10Bet[e],][1,10]
    
  }
  
  diffScore2Int10Bet = score2Interval10Bet[1,]-score2Interval10Bet[2,]
} else {
  diffScore2Int10Bet = NA
}

if(length(match_idThirdInterval10Bet)>=1){
  score3Interval10Bet = matrix(0,2,length(match_idThirdInterval10Bet))
  e = 1
  for(e in 1:length(match_idThirdInterval10Bet)){
    score3Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,9]
    score3Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idThirdInterval10Bet[e],][1,10]
    
  }
  
  diffScore3Int10Bet = score3Interval10Bet[1,]-score3Interval10Bet[2,]
} else {
  diffScore3Int10Bet <- NA}

if(length(match_idFourthInterval10Bet)>=1){
  score4Interval10Bet = matrix(0,2,length(match_idFourthInterval10Bet))
  
  for(e in 1:length(match_idFourthInterval10Bet)){
    score4Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,9]
    score4Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFourthInterval10Bet[e],][1,10]
    
  }
  
  diffScore4Int10Bet = score4Interval10Bet[1,]-score4Interval10Bet[2,]
} else {
  diffScore4Int10Bet <- NA}

if(length(match_idFifthInterval10Bet)>=1){
  score5Interval10Bet = matrix(0,2,length(match_idFifthInterval10Bet))
  
  for(e in 1:length(match_idFifthInterval10Bet)){
    score5Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,9]
    score5Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idFifthInterval10Bet[e],][1,10]
    
  }
  
  diffScore5Int10Bet = score5Interval10Bet[1,]-score5Interval10Bet[2,]
} else {
  diffScore5Int10Bet <- NA}

if(length(match_idSixthInterval10Bet)>=1){
  score6Interval10Bet = matrix(0,2,length(match_idSixthInterval10Bet))
  
  for(e in 1:length(match_idSixthInterval10Bet)){
    score6Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,9]
    score6Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSixthInterval10Bet[e],][1,10]
    
  }
  
  diffScore6Int10Bet = score6Interval10Bet[1,]-score6Interval10Bet[2,]
} else {
  diffScore6Int10Bet <- NA}

if(length(match_idSeventhInterval10Bet)>=1){
  score7Interval10Bet = matrix(0,2,length(match_idSeventhInterval10Bet))
  
  for(e in 1:length(match_idSeventhInterval10Bet)){
    score7Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,9]
    score7Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idSeventhInterval10Bet[e],][1,10]}
  diffScore7Int10Bet = score7Interval10Bet[1,]-score7Interval10Bet[2,]
} else {
  diffScore7Int10Bet <- NA
}

e = 1
if(length(match_idEighthInterval10Bet)>=1){
  score8Interval10Bet = matrix(0,2,length(match_idEighthInterval10Bet))
  
  for(e in 1:length(match_idEighthInterval10Bet)){
    score8Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,9]
    score8Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idEighthInterval10Bet[e],][1,10]}
  diffScore8Int10Bet = score8Interval10Bet[1,]-score8Interval10Bet[2,]
} else {
  diffScore8Int10Bet = NA
}

e = 1
if(length(match_idNinethInterval10Bet)>=1){
  score9Interval10Bet = matrix(0,2,length(match_idNinethInterval10Bet))
  
  for(e in 1:length(match_idNinethInterval10Bet)){
    score9Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,9]
    score9Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idNinethInterval10Bet[e],][1,10]}
  diffScore9Int10Bet = score9Interval10Bet[1,]-score9Interval10Bet[2,]
} else {
  diffScore9Int10Bet = NA
}

e = 1
if(length(match_idTenthInterval10Bet)>=1){
  score10Interval10Bet = matrix(0,2,length(match_idTenthInterval10Bet))
  
  for(e in 1:length(match_idTenthInterval10Bet)){
    score10Interval10Bet[1,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,9]
    score10Interval10Bet[2,e] <- matches_148[matches_148$match_id == match_idTenthInterval10Bet[e],][1,10]}
  diffScore10Int10Bet = score10Interval10Bet[1,]-score10Interval10Bet[2,]
} else {
  diffScore10Int10Bet = NA
}


#In if (is.na(diffScore2Int10Bet) == "FALSE") { :
# the condition has length > 1 and only the first element will be used

if (is.na(diffScore1Int10Bet) == "FALSE"){
  draw110Bet = length(which(diffScore1Int10Bet==0))/length(diffScore1Int10Bet)
} else {
  draw110Bet = 0} 

if (is.na(diffScore2Int10Bet) == "FALSE"){
  draw210Bet = length(which(diffScore2Int10Bet==0))/length(diffScore2Int10Bet)
}else {
  draw210Bet = 0} 

if (is.na(diffScore3Int10Bet) == "FALSE"){
  draw310Bet = length(which(diffScore3Int10Bet==0))/length(diffScore3Int10Bet)
} else {
  draw310Bet = 0} 

if (is.na(diffScore4Int10Bet) == "FALSE"){
  draw410Bet = length(which(diffScore4Int10Bet==0))/length(diffScore4Int10Bet)
} else {
  draw410Bet = 0} 

if (is.na(diffScore5Int10Bet) == "FALSE"){
  draw510Bet = length(which(diffScore5Int10Bet==0))/length(diffScore5Int10Bet)
} else {
  draw510Bet = 0} 

if (is.na(diffScore6Int10Bet) == "FALSE"){
  draw610Bet = length(which(diffScore6Int10Bet==0))/length(diffScore6Int10Bet)
} else {
  draw610Bet = 0} 

if (is.na(diffScore7Int10Bet) == "FALSE"){
  draw710Bet = length(which(diffScore7Int10Bet==0))/length(diffScore7Int10Bet)
} else {
  draw710Bet = 0} 

if (is.na(diffScore8Int10Bet) == "FALSE"){
  draw810Bet = length(which(diffScore8Int10Bet==0))/length(diffScore8Int10Bet)
} else {
  draw810Bet = 0} 

if (is.na(diffScore9Int10Bet) == "FALSE"){
  draw910Bet = length(which(diffScore9Int10Bet==0))/length(diffScore9Int10Bet)
} else {
  draw910Bet = 0} 

if (is.na(diffScore10Int10Bet) == "FALSE"){
  draw1010Bet = length(which(diffScore10Int10Bet==0))/length(diffScore10Int10Bet)
} else {
  draw1010Bet = 0} 


drawReal10BetT3b = c(draw110Bet,draw210Bet,draw310Bet,draw410Bet,draw510Bet,draw610Bet,draw710Bet,draw810Bet,draw910Bet,draw1010Bet)



plot(PHW_AW_10Bet,PT_10Bet, xlab = "P(Home win)-P(Away win)", ylab = "P(draw)",
     main = "10Bet",ylim = c(0,0.35))
drawRealX = c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
lines(drawRealX, drawReal10Bet,lty = 3, type = "b",pch = 19, col = "red")
lines(drawRealX, drawReal10BetT3b,lty = 3, type = "b",pch = 19, col = "blue")

print('Although no significant change is seen for each bookmarker when extra time goals are removed there are mostly cases for which number of draw games are increased for Tipico,Leonbets and Babibet. This may be explained as the contribution of extra goals has an effect.')
print('The number of games removed is:')
print(length(data_task3bremoveid))
print('out of 2705700')

print('All in all geometric distribution fits well to both Homa and Away goals with best calculated lambda by fitdistrplus library. Home-Away Goals are almost normally distrbuted. Probabilities set by the fourth selected bookmarkers are more biased for Leonbets and 10Bet and less for Tipico and Babibet but are sill biased. At long run it is expected for someone betting at draw games to loose. Removal of early red cards (first 30 min) do not have any significance effect on average real time raw game number factor calculation but more significant effect is observed for extra time goals which may be characterized as outliers.')


