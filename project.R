rm(list=ls()) 

library('codetools')
library('Matrix')
library('glmnet')
library('readr')
library('rpart')
library('Rfast')

matches <- read_csv(file="C:/Users/Onur/Desktop/proje/matches/matches.csv", col_names=TRUE, col_types = NULL ) 
bets <- read_csv(file="C:/Users/Onur/Desktop/proje/bets/bets.csv", col_names=TRUE, col_types = NULL ) 
booking <- read_csv(file="C:/Users/Onur/Desktop/proje/booking/booking.csv", col_names=TRUE, col_types = NULL ) 
goals <- read_csv(file="C:/Users/Onur/Desktop/proje/goals/goals.csv", col_names=TRUE, col_types = NULL ) 
stats <- read_csv(file="C:/Users/Onur/Desktop/proje/stats/stats.csv", col_names=TRUE, col_types = NULL ) 

# # 21. Round 
# round_21 <- matrix(c(273302, 273303,273306,273308,273309,273305,273307,273310,273301,273304))
# 
# bet_21_1x2 = matrix(c(0),10,3)
# for(i in 1:10){
# bet_21 <- bets[bets$match_id == 273302,]
# bet_21_1 <- bet_21[bet_21$variable == "odd_1",]
# bet_21_x <- bet_21[bet_21$variable == "odd_x",]
# bet_21_2 <- bet_21[bet_21$variable == "odd_2",]
# 
# bet_21_1x2[i,1] = mean(bet_21_1[["value"]])
# bet_21_1x2[i,2] = mean(bet_21_x[["value"]])
# bet_21_1x2[i,3] = mean(bet_21_2[["value"]])
# }
# 
# probability_1x2 = matrix(c(0),10,3)
# probability_1x2 = 1 / bet_21_1x2
# probability_1x2_rowsum = rowSums(probability_1x2)
# probability_1x2 = sweep(probability_1x2,1,probability_1x2_rowsum, FUN = '/')

premier_league_matches <- matches[matches$league_id == 148 , ]
training_data = premier_league_matches 
training_data = matches


# #################################################################################################################3
# training_data$lastFiveGoals = NA
# for(i in 1:NROW(training_data)){
#   #homeId = myData$match_hometeam_id[i]
#   ind = which(training_data$match_hometeam_id %in% training_data$match_hometeam_id[i] )
#   matchIds = training_data$match_id[ind]
#   matchId = training_data$match_id[i]
#   
#   k = 1
#   for(j in 1:NROW(matchIds)){
#     if(matchId == matchIds[j]){
#       break
#     }
#     else {k = k + 1}}
#   
#   lastScores = NA
#   for(l in 0:k){
#     
#     indd = which(training_data$match_id %in% training_data$match_id[ind[l]] ) 
#     lastScores[l] = training_data$match_hometeam_score[indd]
#   }
#   
#   lengthLastScore = NROW(lastScores)
#   
#   if(lengthLastScore>5){
#     training_data$lastFiveGoals[i] = mean(lastScores[(lengthLastScore-1):(lengthLastScore-5)])
#   }
# }
###############################################################################################################3

training_data$y <- c(0)
for(i in 1:(NROW(training_data))) {
  
  if (is.na(training_data$match_hometeam_score[i])) {
    
  } else if (training_data$match_hometeam_score[i] > training_data$match_hometeam_score[i] ) {
    training_data$y[i] <- 0
  } else if (training_data$match_hometeam_score[i] == training_data$match_awayteam_score[i]){
    training_data$y[i] <- 0.5
  } else if (training_data$match_hometeam_score[i] < training_data$match_awayteam_score[i]) {
    training_data$y[i] <- 1
  }
  
  print(i)
}

#############################################################################################################

training_data$odd_1 <- c(0)
training_data$odd_x <- c(0)
training_data$odd_2 <- c(0)

for(i in 1:(NROW(training_data))){
  bet_1 <- bets[bets$match_id == training_data$match_id[i] & bets$variable == "odd_1" ,]
  bet_x <- bets[bets$match_id == training_data$match_id[i] & bets$variable == "odd_x" ,]
  bet_2 <- bets[bets$match_id == training_data$match_id[i] & bets$variable == "odd_2" ,]
  
  if(NROW(bet_1)<10){
    training_data$odd_1[i] = NA
    training_data$odd_x[i] = NA
    training_data$odd_2[i] = NA
  } else {
    training_data$odd_1[i] = mean(bet_1[(NROW(bet_1)-10):NROW(bet_1),][["value"]])
    training_data$odd_x[i] = mean(bet_x[(NROW(bet_x)-10):NROW(bet_x),][["value"]])
    training_data$odd_2[i] = mean(bet_2[(NROW(bet_2)-10):NROW(bet_2),][["value"]])
  }
  print(i)
}

training_data$home_last_5_games_points <- c(0)
training_data$away_last_5_games_points <- c(0)
training_data$home_last_5_home_games_points <- c(0)
training_data$away_last_5_away_games_points <- c(0)
training_data$home_last_5_games_avg_goals <- c(0)
training_data$away_last_5_games_avg_goals <- c(0)
training_data$home_last_5_home_games_avg_goals <- c(0)
training_data$away_last_5_away_games_avg_goals <- c(0)

################################################################################################################3

for(i in 1:(NROW(training_data))){
  
  hometeam_id = training_data$match_hometeam_id[i]
  awayteam_id = training_data$match_awayteam_id[i]
  
  temp_past_data = training_data[1:i,]
  
  ## LAST 5 MATCHES ##
  temp_hometeam_matches = temp_past_data[temp_past_data$match_hometeam_id == hometeam_id | temp_past_data$match_awayteam_id == hometeam_id,]
  temp_awayteam_matches = temp_past_data[temp_past_data$match_awayteam_id  == awayteam_id |temp_past_data$match_hometeam_id == awayteam_id,]  
  
  if (NROW(temp_hometeam_matches) < 6 | NROW(temp_awayteam_matches) < 6){
    training_data$home_last_5_games_points[i] = NA
    training_data$away_last_5_games_points[i] = NA
    training_data$home_last_5_games_avg_goals[i] = NA
    training_data$away_last_5_games_avg_goals[i] = NA
  } else {
    
    hometeam_last_5_matches = temp_hometeam_matches[(NROW(temp_hometeam_matches)-5):(NROW(temp_hometeam_matches)-1),]
    awayteam_last_5_matches = temp_awayteam_matches[(NROW(temp_awayteam_matches)-5):(NROW(temp_awayteam_matches)-1),]
    
    hometeam_last_5_matches_home = hometeam_last_5_matches[hometeam_last_5_matches$match_hometeam_id == hometeam_id,]
    hometeam_last_5_matches_away = hometeam_last_5_matches[hometeam_last_5_matches$match_awayteam_id == hometeam_id,]
    
    awayteam_last_5_matches_home = awayteam_last_5_matches[awayteam_last_5_matches$match_hometeam_id == awayteam_id,]
    awayteam_last_5_matches_away = awayteam_last_5_matches[awayteam_last_5_matches$match_awayteam_id == awayteam_id,]
    
    training_data$home_last_5_games_points[i] =  3*NROW(hometeam_last_5_matches_home[hometeam_last_5_matches_home$y==0,]) + 1*NROW(hometeam_last_5_matches_home[hometeam_last_5_matches_home$y==0.5,]) + 3*NROW(hometeam_last_5_matches_away[hometeam_last_5_matches_away$y==1,]) + 1*NROW(hometeam_last_5_matches_away[hometeam_last_5_matches_away$y==0.5,])
    training_data$away_last_5_games_points[i] =  3*NROW(awayteam_last_5_matches_home[awayteam_last_5_matches_home$y==0,]) + 1*NROW(awayteam_last_5_matches_home[awayteam_last_5_matches_home$y==0.5,])  + 3*NROW(awayteam_last_5_matches_away[awayteam_last_5_matches_away$y==1,]) + 1*NROW(awayteam_last_5_matches_away[awayteam_last_5_matches_away$y==0.5,])
    
    training_data$home_last_5_games_avg_goals[i] = (colSums(as.matrix(hometeam_last_5_matches_home$match_hometeam_score)) + colSums(as.matrix(hometeam_last_5_matches_away$match_awayteam_score))) / 5
    training_data$away_last_5_games_avg_goals[i] = (colSums(as.matrix(awayteam_last_5_matches_home$match_hometeam_score)) + colSums(as.matrix(awayteam_last_5_matches_away$match_awayteam_score))) / 5
  }
  ## LAST 5 HOME/ 5 AWAY MATCHES
  
  temp_hometeam_home_matches = temp_past_data[temp_past_data$match_hometeam_id == hometeam_id ,]
  temp_awayteam_away_matches = temp_past_data[temp_past_data$match_awayteam_id == awayteam_id ,]
  
  if (NROW(temp_hometeam_home_matches) < 6 | NROW(temp_awayteam_away_matches) < 6){
    training_data$home_last_5_home_games_points[i] = NA
    training_data$away_last_5_away_games_points[i] = NA
    training_data$home_last_5_home_games_avg_goals[i] = NA 
    training_data$away_last_5_away_games_avg_goals[i] = NA
  } else {
    hometeam_last_5_home_matches = temp_hometeam_home_matches[(NROW(temp_hometeam_home_matches)-5):(NROW(temp_hometeam_home_matches)-1),]
    awayteam_last_5_away_matches = temp_awayteam_away_matches[(NROW(temp_awayteam_away_matches)-5):(NROW(temp_awayteam_away_matches)-1),]
    
    training_data$home_last_5_home_games_points[i] =  3*NROW(hometeam_last_5_home_matches[hometeam_last_5_home_matches$y==0,]) + 1*NROW(hometeam_last_5_home_matches[hometeam_last_5_home_matches$y==0.5,])
    training_data$away_last_5_away_games_points[i] = 3*NROW(awayteam_last_5_away_matches[awayteam_last_5_away_matches$y==1,]) + 1*NROW(awayteam_last_5_away_matches[awayteam_last_5_away_matches$y==0.5,])
    
    training_data$home_last_5_home_games_avg_goals[i] = (colSums(as.matrix(hometeam_last_5_home_matches$match_hometeam_score))) / 5
    training_data$away_last_5_away_games_avg_goals[i] = (colSums(as.matrix(awayteam_last_5_away_matches$match_awayteam_score))) / 5
    
  }
}


training_data_2 = training_data[,18:29]
training_data_omit = na.omit(training_data_2)

training_set = training_data_omit[1:2400,]
test_set = training_data_omit[2401:2900,]


cvfit = cv.glmnet(as.matrix(training_set[,2:12]), as.matrix(training_set$y))
treeFit = rpart(y~odd_1+odd_x+odd_2+home_last_5_games_points+away_last_5_games_points+home_last_5_home_games_points+away_last_5_away_games_points+home_last_5_games_avg_goals+away_last_5_games_avg_goals+home_last_5_home_games_avg_goals+away_last_5_away_games_avg_goals,data=training_set,method='anova')
# coef(cvfit,s = "lambda.min")
# treeFit <- randomForest(y~odd_1+odd_x+odd_2+home_last_5_games_points+away_last_5_games_points+home_last_5_home_games_points+away_last_5_away_games_points+home_last_5_games_avg_goals+away_last_5_games_avg_goals+home_last_5_home_games_avg_goals+away_last_5_away_games_avg_goals, data = training_set, ntree = 500, nodesize =5, mtry = 5)

predict_test_lasso = predict(cvfit, as.matrix(test_set[,2:12]), s = "lambda.min")

predict_test_lasso = predict(Lassofit, as.matrix(test_set[,2:12]), s = 0.0005)
predict_test_tree = predict(treeFit,newdata = test_set[,2:12])
predict_test_ensemble = ( predict_test_lasso  + predict_test_tree ) / 2

predict_result_lasso = matrix(c(0),500,1)
predict_result_tree = matrix(c(0),500,1)
predict_result_ensemble = matrix(c(0),500,1)
misclass_lasso = matrix(c(0),500,1)
misclass_tree = matrix(c(0),500,1)
misclass_ensemble = matrix(c(0),500,1)

for(i in 1:500){
  if(predict_test_lasso[i] < 0.40){
    predict_result_lasso[i] = 0
  } else if(predict_test_lasso[i] > 0.60){
    predict_result_lasso[i] = 1
  } else{
    predict_result_lasso[i] = 0.5
  }
  
  
  if(predict_test_tree[i] < 0.40){
    predict_result_tree[i] = 0
  } else if(predict_test_tree[i] > 0.60){
    predict_result_tree[i] = 1
  }else{
    predict_result_tree[i] = 0.5
  }
  
  if(predict_test_ensemble[i] < 0.40){
    predict_result_ensemble[i] = 0
  } else if(predict_test_ensemble[i] > 0.60){
    predict_result_ensemble[i] = 1
  } else{
    predict_result_ensemble[i] = 0.5
  }
  
  if(test_set$y[i] == predict_result_lasso[i]){
    misclass_lasso[i] = 1
  }
  if(test_set$y[i] == predict_result_tree[i]){
    misclass_tree[i] = 1
  }
  if(test_set$y[i] == predict_result_ensemble[i]){
    misclass_ensemble[i] = 1
  }
  
}

misclass_results = data.frame("misclass_lasso" = misclass_lasso, "misclass_tree" = misclass_tree, "misclass_ensemble" = misclass_ensemble)
misclass_results_colsum = colSums((misclass_results))

ensemble_odds = data.frame("odd_1" = matrix(c(0),500,1),"odd_x" = matrix(c(0),500,1) ,"odd_2"= matrix(c(0),500,1) )
for(i in 1:500) {
  ensemble_odds[i,1] = exp(-abs(predict_test_ensemble[i]-0))
  ensemble_odds[i,2] = exp(-abs(predict_test_ensemble[i]-0.5))
  ensemble_odds[i,3] = exp(-abs(predict_test_ensemble[i]-1))
}
scale_ensemble_odds = rowSums(ensemble_odds)

ensemble_odds_scaled = ensemble_odds
ensemble_odds_scaled[,1] = ensemble_odds_scaled[,1] / scale_ensemble_odds
ensemble_odds_scaled[,2] = ensemble_odds_scaled[,2] / scale_ensemble_odds
ensemble_odds_scaled[,3] = ensemble_odds_scaled[,3] / scale_ensemble_odds

## RPS CALC ## 

RPS_ensemble = data.frame("RPS_ensemble" = matrix(c(0),500,1) )

for(i in 1:500) {
  
  if(test_set$y[i] == 0){
    RPS_ensemble[i,1] = ((ensemble_odds_scaled[i,1] - 1)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2] - 1)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2]+ensemble_odds_scaled[i,3] - 1)^2 ) / 2
  } else if (test_set$y[i] == 0.5){
    RPS_ensemble[i,1] = ((ensemble_odds_scaled[i,1] - 0)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2] - 1)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2]+ensemble_odds_scaled[i,3] - 1)^2 ) / 2
  } 
  else if (test_set$y[i] == 1){
    RPS_ensemble[i,1] = ((ensemble_odds_scaled[i,1] - 0)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2] - 0)^2 + (ensemble_odds_scaled[i,1]+ensemble_odds_scaled[i,2]+ensemble_odds_scaled[i,3] - 1)^2 ) / 2
  } 
  
}
mean(RPS_ensemble[["RPS_ensemble"]])


# cvfit = cv.glmnet(as.matrix(training_data_omit[,2:12]), as.matrix(training_data_omit$y))
# treeFit = rpart(y~odd_1+odd_x+odd_2+home_last_5_games_points+away_last_5_games_points+home_last_5_home_games_points+away_last_5_away_games_points+home_last_5_games_avg_goals+away_last_5_games_avg_goals+home_last_5_home_games_avg_goals+away_last_5_away_games_avg_goals,data=training_data_omit,method='anova')
# 
# cvfit = cv.glmnet(as.matrix(training_data[400:500,19:29]), as.matrix(training_data$y[400:500]))
# predict_deneme = predict(cvfit, matrix(c(3.82,3.75,1.91),1,3), s = "lambda.min")
# odd1 = exp(-abs(predict_deneme-0))
# oddx = exp(-abs(predict_deneme-0.5))
# odd2 = exp(-abs(predict_deneme-1))
# scale = odd1 + oddx + odd2
# odd1 = odd1 / scale
# oddx = oddx / scale
# odd2 = odd2 / scale

predict_test_lasso_NN = matrix(c(0),500,1)
training_set = training_data_omit[1:2500,]
test_set = training_data_omit[2501:3000,]

for(i in 1:500){
  
  # all_set = rbind(training_set,test_set[i,]) 
  # training_set_scaled = scale(all_set)  
  # dist = dista(transpose(as.matrix(training_set_scaled[NROW(training_set_scaled),2:12])),training_set_scaled[1:(NROW(training_set_scaled)-1),2:12], type = "euclidean",k=0,index = FALSE, trans = FALSE)
  
  dist = dista(test_set[i,],training_set,type = "euclidean",k=0,index=FALSE,trans = FALSE)
  ind_sort = order(dist)
  
  cvfit = cv.glmnet(as.matrix(training_set[ind_sort[1:45],2:12]), as.matrix(training_set$y[ind_sort[1:45]]))
  predict_test_lasso_NN[i,1] = predict(cvfit, as.matrix(test_set[i,2:12]), s = "lambda.min")
  
  training_set = rbind(training_set,test_set[i,])
  print(i)
}
predict_test_lasso = predict_test_lasso_NN