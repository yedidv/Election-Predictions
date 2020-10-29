source('finalfunctions.r')


##### INITIAL PREDICTIONS: USING POLLING TO PREDICT THE FINAL ELECTIONS ##### 

### Read Polling CSV File 
polling <- ReadCsv('presidential_polls_2020.csv')

## Subset the Dataframe to remove NE-1, NE-2, ME-1, ME-2
polling <- subset.data.frame(polling,  !(polling$state %in% c('NE-1', 'ME-2', 'NE-2', 'ME-1')))
tail(polling) 


polling$party <- ifelse(polling$candidate_name  == unique(polling$candidate_name)[1], 'democrat', 'republican')
polling$logpct <- log(polling$pct) 
polling$loghouse <- log(polling$house_adjusted_pct) 
polling$logtrendhouse <- log(polling$trend_and_house_adjusted_pct) 



### Iterate through the data, for each state collect the trends for trump, biden, and see if they are
### statistically different from each other. If they are, then pick the one with the highest mean as the winner. 
### Start with percentages 
polling_pct_winner <- PollingResults(polling, 0) 
UsMapPlot(polling_pct_winner, 'Polling Predictions') 

## Count how many states each candidate won. 
dem_winner_count <- CountWinner(polling_pct_winner) 
rep_winner_count <- CountWinner(polling_pct_winner) 


## House Adjusted Polling Averages
polling_house_pct_winner <- PollingResults(polling, 'house')
UsMapPlot(polling_pct_winner, 'Polling Predictions- House Adjusted') 

## Count how many states each candidate won. 
dem_winner_house_count <- CountWinner(polling_pct_winner) 
rep_winner_house_count <- CountWinner(polling_pct_winner) 


## House and Trend Adjusted Polling Averages
polling_house_trend_pct_winner <- PollingResults(polling, 'trend_and_house')
UsMapPlot(polling_house_trend_pct_winner, 'Polling Predictions- House and Trend Adjusted') 

## Count how many states each candidate won. 
dem_winner_house_trend_count <- CountWinner(polling_pct_winner) 
rep_winner_house_trend_count <- CountWinner(polling_pct_winner) 


##### Prediction 2: Using Historical Data to Try and Predict Elections ####### 
historical_polls <- ReadCsv('1976_2016_president.csv')
head(historical_polls)
## Convert to percentages of votes per state to make data comparison easier. 
historical_polls$percentvotes <- historical_polls$candidatevotes / historical_polls$totalvotes

## We determine that any candidate with less than 5% of the votes is insignificant towards the final results. 
historical_polls <- subset.data.frame(historical_polls, percentvotes > 0.05) 
historical_polls$log <- log(historical_polls$percentvotes) 
parties <- unique(historical_polls$party)
head(parties) 
head(historical_polls) 

### We can see we have election results for all years since 1976.
## We can put this data through the HistoricalPrediction function created
## which would return the predicted winner for each state based on historical data
historical_winner <- HistoricalPrediction(historical_polls) 
UsMapPlot(historical_winner, 'Average Historical Elections')

## Count how many states each candidate won. 
dem_winner_house_historical_count_ <- CountWinner(historical_winner) 
rep_winner_house_historical_count <- CountWinner(historical_winner) 

## In order to find states we are sure will go to Trump or to Biden, we can 
## merge the two tables of the winner of the polls and of this historical polls
## in a full outer join. 
average_polling_merge <- merge(x = polling_house_trend_pct_winner, 
                               y =  historical_winner, by = 'state', 
                               all = TRUE)
head(average_polling_merge) 

## This determines the states where both the polling and the historical data predict
## the same winner, meaning we can say with high confidence that these states are 
## accurately predicted. 
average_polling_merge$winner <- ifelse((average_polling_merge$winner.x == average_polling_merge$winner.y), 
                                       ifelse((average_polling_merge$winner.x == 'democrat'), 
                                              'democrat',
                                              'republican'), 
                                       'No Winner') 
UsMapPlot(average_polling_merge, 'Polling vs Average Joint Predictions') 


######## Prediction 3: Using health and economic data to predict the outcomes for the remaining states. #######



## Read unemployment file - contains unemployment, gdp, and inflation rates for every year
unemployment <- ReadCsv('unemployment.csv') 
head(unemployment) 

## Unemployment data during election year. Year must be a multiple of 4 and after the year 1975 
## (to fit with the historical data set) 
unemployment_electionyear <- subset.data.frame(unemployment, (year %% 4 == 0) & (year > 1975) )
head(unemployment_electionyear) 


## Put through a function that outputs a dataframe with the popular vote winner for each election, 
## what the popular vote was, and the election cycle. 
winner_economy <- WinnerPopular(historical_polls)  

## Merge data for the winner with the data for the 
winner_economy <- merge(x = unemployment_electionyear,
                        y = winner_economy, 
                        by = 'year', 
                        all = TRUE)


head(winner_economy) 

## We find there is a moderate correlation between the democratic popular vote and the unemployment factors
summary(lm(dem_popular ~ unemployment_rate + gdp_growth + inflation, data = winner_economy))
summary(lm(rep_popular ~ unemployment_rate + gdp_growth + inflation, data = winner_economy))
summary(lm(party ~ unemployment_rate + gdp_growth + inflation, data = winner_economy))

dem_lm <- lm(dem_popular ~ unemployment_rate + gdp_growth + inflation, data = winner_economy)

dem_popular_winning <- 


