
#Install package if you need to... 
#devtools::install_github(("abresler/nbastatR"))
source("INTERNAL PACKAGE OR FUNCTION PATH")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,magrittr,tidyr,readr,stringr,RODBC,data.table,broom,purrr,caret,mlbench,nbastatR,mboost,randomForest,arm)

mypath <- "INTERNALPATH"

seasons_select <- c(1987:2019)

mydata <- fread(paste0(mypath,"/NBADATA1987_2019.csv")) 

# mydata <- fread(paste0(mypath,"/NBADATA1987_2019.csv"))

# 
# mydata <- game_logs(seasons = seasons_select, result_types = "player",
#               season_types = "Regular Season", nest_data = F,
#               assign_to_environment = TRUE, return_message = TRUE)
# 
# fwrite(mydata, file = paste0(getwd(),"/nbadata.csv"))

#get game ids we are interested in
# games <- seasons_schedule(seasons = seasons_select) 
# fwrite(games, file = paste0(mypath,"/seasongames.csv"))

# games <- fread(paste0(mypath,"/seasongames.csv"))

# game_ids <- games %>%
#   select(idGame)

## get box scores for each game
# testdata <- box_scores(game_ids = 29100086,
#                        box_score_types =  "Advanced",
#                                            #"Scoring", "Misc", "Usage", "Advanced", "Four Factors", "Tracking"),
#                        result_types = "player",
#                        join_data = TRUE,
#                        assign_to_environment = TRUE,
#                        return_message = TRUE)


# awards <- bref_awards(c("Most Valuable Player","Defensive Player of the Year","Sixth Man of the Year","Rookie of the Year"))
# fwrite(awards, file = paste0(mypath,"/awards.csv"))

awardsvotes <- bref_awards_votes(seasons = seasons_select)

#
# 
# awards <- fread(paste0(mypath,"/awards.csv")) %>%
#   mutate(pSeason = paste(namePlayer,slugSeason, sep = ": "))


#get new awards dataframe with columns for pseason and slugAward

dataMVPVotes <- dataMVPVotes %>% filter(rankVotes <= 10) %>% mutate(pSeason = paste(namePlayer,slugSeason, sep = ": "))
dataDPOYVotes <- dataDPOYVotes %>% filter(rankVotes <= 10) %>% mutate(pSeason = paste(namePlayer,slugSeason, sep = ": "))
dataROYVotes <- dataROYVotes %>% filter(rankVotes <= 10) %>% mutate(pSeason = paste(namePlayer,slugSeason, sep = ": "))
dataSMOYVotes <- dataSMOYVotes %>% filter(rankVotes <= 10) %>% mutate(pSeason = paste(namePlayer,slugSeason, sep = ": "))

#get award winners only
MVPw <- dataMVPVotes %>% filter(rankVotes == 1) 
DPOYw <- dataDPOYVotes %>% filter(rankVotes == 1)
ROYw <- dataROYVotes %>% filter(rankVotes == 1) 
SMOYw <- dataSMOYVotes %>% filter(rankVotes == 1) 


#creating variables to identify award winners in each season
mydata.p <- mydata %>%
  mutate(pSeason = paste(namePlayer,slugSeason, sep = ": ")) 

#identify award winners for each season -----this was a bear because of logical operators in dplyr. I just wanted it done :-)
mydata.allGames <- mydata.p %>%
  mutate(isMVP = if_else(pSeason 
                         %in%
                           dataMVPVotes$pSeason,
                         1, 
                         0),
         isROY = if_else(pSeason
                         %in%
                           dataROYVotes$pSeason,
                         1, 
                         0),
         isDPOY = if_else(pSeason
                          %in%
                            dataDPOYVotes$pSeason,
                          1, 
                          0),
         isSMOY = if_else(pSeason
                          %in%
                            dataSMOYVotes$pSeason,
                          1, 
                          0),
         isMVPwin = if_else(pSeason
                            %in%
                              MVPw$pSeason,
                            1,
                            0),
         isROYwin = if_else(pSeason
                            %in%
                              ROYw$pSeason,
                            1,
                            0),
         isDPOYwin = if_else(pSeason
                             %in%
                               DPOYw$pSeason,
                             1,
                             0),
         isSMOYwin = if_else(pSeason
                             %in%
                               SMOYw$pSeason,
                             1,
                             0)
  )

#write.csv(mydata.allGames, file = paste0(getwd(),"/mydataFinal.csv"))

### BEGIN MOVING AVERAGE SCRIPT

#calculating the season average across each game played for each player. 
#will be essential to calculate probabilities to win various awards over entire seasons.


mydata.rollingaverage <- mydata.allGames %>%
  dplyr::select(1:2,6:10,21:23,26,28:34,36:50,57:66) %>% #only want identifying information and stats
  group_by(slugSeason,idPlayer) %>%
  arrange(slugSeason, numberGamePlayerSeason) %>%
  mutate(ppg = cummean(pts), #rolling average of stats
         fgpct = cumsum(fgm) / cumsum(fga),
         fg2pct = cumsum(fg2m) / cumsum(fg2a),
         fg3pct = cumsum(fg3m) / cumsum(fg3a),
         ftpct = cumsum(ftm) / cumsum(fta),
         orpg = cummean(oreb),
         drpg = cummean(dreb),
         rpg = cummean(treb),
         apg = cummean(ast),
         spg = cummean(stl),
         bpg = cummean(blk),
         topg = cummean(tov),
         mpg = cummean(minutes)
  )

rm(dataDPOYVotes,dataMVPVotes,dataMIPVotes,dataROYVotes,dataSMOYVotes,DPOYw,MVPw,ROYw,SMOYw,awardsvotes,mydata.p,mydata.allGames)

#get the last game for each player season to get the season average
mydata.lastgame <- mydata.rollingaverage %>%
  group_by(pSeason) %>%
  summarize(numberGamePlayerSeason = max(numberGamePlayerSeason)) %>%
  inner_join(mydata.rollingaverage, by = c("pSeason","numberGamePlayerSeason"))

#now create dataset for model based on seasons who will win MVP
season.df <- mydata.lastgame %>%
  dplyr::select(1,4,36:40,42,44:56) %>%
  mutate(isMVP = as.factor(isMVP),
         isDPOY = as.factor(isDPOY),
         isSMOY = as.factor(isSMOY),
         isROY = as.factor(isROY)) %>%
  replace(., is.na(.), 0)

#replace all NaN with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

season.df[is.nan(season.df)] <- 0





#create training and test sets to train model

train.size <- floor(.75 * nrow(season.df))
set.seed(123) #for reproducibility
season.train <- sample(seq_len(nrow(season.df)), size = train.size)

train.df <- season.df[season.train,]
test.df <- season.df[-season.train,]



####################### MVP MODEL #########################
set.seed(123)
control <- trainControl(method="cv", number=10)
fit.mvp <- train(isMVP ~ ., data = train.df %>%
                   select(-pSeason,-slugSeason,-isDPOY,-isSMOY,-isROY), method="glm", metric="Accuracy", trControl=control, na.action = na.omit) #using GLM for speed


# accuracy metrics
set.seed(123)
predictions.mvp <- predict(fit.mvp, newdata=test.df)
confusionMatrix(predictions.mvp, test.df$isMVP)


################# DPOY MODEL ##########################

set.seed(123)
control <- trainControl(method="cv", number=10)
fit.dpoy <- train(isDPOY ~ ., data = train.df %>%
                    dplyr::select(-pSeason,-slugSeason,-isMVP,-isSMOY,-isROY), method="rf", metric="Accuracy", trControl=control, na.action = na.omit) #random forest more accurate



set.seed(123)
predictions.dpoy <- predict(fit.dpoy, newdata=test.df)
confusionMatrix(predictions.dpoy, test.df$isDPOY)



#TODO: ROY and SMOY models later......


#save model to be run later

saveRDS(fit.mvp, paste0(mypath,"/mvpModel.rds"))

saveRDS(fit.dpoy, paste0(mypath,"/dpoyModel.rds"))





##### LOAD MODELS ########


fit.dpoy <- readRDS(paste0(mypath,"/dpoyModel.rds"))
fit.mvp <- readRDS(paste0(mypath,"/mvpModel.rds"))



#apply predictions (get probabilities) for each season for MVP and DPOY
season.awards <- season.df %>%
  nest(-slugSeason) %>%
  mutate(pred = map(data, ~predict(fit.mvp,newdata = .,type = 'prob'))) %>%
  unnest(data,pred) %>%
  dplyr::select(-`0`) %>%
  rename(predMVP = `1`) %>%
  nest(-slugSeason) %>%
  mutate(pred = map(data, ~predict(fit.dpoy,newdata = .,type = 'prob'))) %>%
  unnest(data,pred) %>%
  dplyr::select(-`0`) %>%
  rename(predDPOY = `1`)


#apply predictions (get probabilities) for each game of each season for MVP and DPOY


mydata.rollingaverage <- mydata.rollingaverage %>%
  replace(., is.na(.), 0)

#replace all NaN with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

mydata.rollingaverage[is.nan(mydata.rollingaverage)] <- 0


#create a running prediction for each game of the season
running.awards <- mydata.rollingaverage %>%
  ungroup() %>%
  nest(-slugSeason,-numberGamePlayerSeason) %>%
  mutate(pred = map(data, ~predict(fit.mvp,newdata = .,type = 'prob'))) %>%
  unnest(data,pred) %>%
  dplyr::select(-`0`) %>%
  rename(predMVP = `1`) %>%
  nest(-slugSeason) %>%
  mutate(pred = map(data, ~predict(fit.dpoy,newdata = .,type = 'prob'))) %>%
  unnest(data,pred) %>%
  dplyr::select(-`0`) %>%
  rename(predDPOY = `1`)


#write the final predictions

fwrite(season.awards, paste0(mypath,"/seasonpreds.csv"))
fwrite(running.awards, paste0(mypath,"/runningpreds.csv"))
