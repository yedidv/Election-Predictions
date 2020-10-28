
SelectCandidateState <- function(df, party_name, state_name){
  ## Input the dataframe, the candidate we want to filter by, and the state we want to filter by. 
  ## Returns a subset of the polling data that meets the specified parameters 
  
  return(subset.data.frame(df, (party == party_name) & (state == state_name)))
}


PollingResults <- function(polling_data, adjusted){ 
  ### Iterate through the data, for each state collect the trends for trump, biden, and see if they are
  ### statistically different from each other. If they are, then pick the one with the highest mean as the winner. 
  
  
  polling_winner_columns <- c("state", 'p_Value', 'winner')
  polling_winner <- data.frame(matrix(ncol = length(polling_winner_columns), nrow = length(unique(polling$state))))
  colnames(polling_winner) <- polling_winner_columns
  
  
  i <- 1
  for (state in unique(polling_data$state)){
    trump_data <- SelectCandidateState(polling_data, unique(polling$party)[2], state)
    biden_data <- SelectCandidateState(polling_data, unique(polling$party)[1], state)
    
    ## Select which polling data we are using 
    if(adjusted == 'trend_and_house'){
      trump_polling <- trump_data$logtrendhouse
      biden_polling <- biden_data$logtrendhouse
    }
    else if(adjusted == 'house'){
      trump_polling <- trump_data$loghouse
      biden_polling <- biden_data$loghouse
    }
    else if(adjusted == 0) { 
      trump_polling <- trump_data$logpct
      biden_polling <- biden_data$logpct
    }
    else{
      return('Invalid polling data')
    }
    
    ## Perform t test to see if the data is significant
    p_test <- t.test(trump_polling, biden_polling)[3]
    winning_candidate <- ifelse(p_test <= 0.025, ifelse(mean(biden_polling) > mean(trump_polling), 'democrat', 'republican'), 'No Winner')
    
    
    polling_winner$state[i] <- state
    polling_winner$p_Value[i] <- p_test
    polling_winner$winner[i] <- winning_candidate
    i <- i + 1
    
  }
  
  return(polling_winner) 
  
}



HistoricalPrediction <- function(historical_polls){
  states <- unique(historical_polls$state) 
  historical_winner_columns <- c("state", 'p_value', 'winner')
  historical_winner<- data.frame(matrix(ncol = length(historical_winner_columns), nrow = length(states)))
  colnames(historical_winner) <- historical_winner_columns 
  head(historical_winner) 
  
  
  ## Create a for loop to group together the data for each state and make a prediction
  ## based on the p - value of the t - test. 
  ## Box plots were made for each state's data to see if it was normal. It was shown not to be, 
  ## so we took the log transform of the data to ensure the normality assumption holds when performing
  ## the t-test. 
  j <- 1
  for (state in states){ 
    dem <- SelectCandidateState(historical_polls, 'democrat', state)
    rep <- SelectCandidateState(historical_polls, 'republican', state)
    p_value <- t.test(dem$log, rep$log)[3] 
    winner <- ifelse(p_value < 0.05, ifelse(mean(dem$percentvotes) > mean(rep$percentvotes), 'democrat', 'republican'), 'No Winner') 
    
    historical_winner$state[j] <- state
    historical_winner$p_value[j] <- p_value 
    historical_winner$winner[j] <- winner
    
    j <- j + 1
  }
  return(historical_winner) 
  
}


WinnerPopular <- function(historical_polls){
  winner_columns <- c('party', 'year', 'dem_popular', 'rep_popular')
  winner_economy <- data.frame(matrix(ncol = length(winner_columns),
                                      nrow = length(unique(historical_polls$year))))
  colnames(winner_economy) <- winner_columns
  
  
  j <- 1
  for (cycle in unique(historical_polls$year)){
    
    winner <- subset.data.frame(historical_polls, year == cycle) 
    dem <- subset.data.frame(winner, party == 'democrat') 
    rep <- subset.data.frame(winner, party == 'republican') 
    dem_popular <- sum(dem$candidatevotes) / sum(dem$totalvotes) 
    rep_popular <- sum(rep$candidatevotes) / sum(rep$totalvotes) 
    winner <- ifelse(dem_popular > rep_popular, 1, 0) 
    
    winner_economy$year[j] <- cycle
    winner_economy$dem_popular[j] <- dem_popular
    winner_economy$rep_popular[j] <- rep_popular
    winner_economy$party[j] <- ifelse(mean(dem_popular) > mean(rep_popular), 1, 0)
    j <- j + 1
    
  }
  
  return(winner_economy)
}



UsMapPlot <- function(winning_data, data_type){ 
  ### US Map plot for the predictions
  library(ggplot2) 
  library(usmap) 
  us_map <- plot_usmap(data = winning_data, values = 'winner', regions = 'states', labels = TRUE)
  us_map <- us_map + theme(legend.position = 'right')
  us_map <- us_map + scale_fill_manual(name = data_type, 
                                       values = c('democrat' = '#2E74C0', 'republican' = '#CB454A', 'No Winner' = 'Green'))
  return(us_map) 
}



ReadCsv <- function(path){ 
  return (as.data.frame(read.csv(path)))
}



