## Loading of libraries needed to analyse and predict the data

library(stringr)
library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
library(caret)

## Loading of data that will be used to predict the movie ratings
edx <- readRDS("~/Documents/R/Movielens_capstone/edx.rds")
validation <- readRDS("~/Documents/R/Movielens_capstone/validation.rds")

## 2.1 Data preparation
# Make a separate table with movieId and title per movie to tidy up the data
# Keep timestamp, movieId, userID and ratings as the rating could be modelled
# by accounting for the variation in movie title, genre, time stamp and user.

movie_lookup <- data.frame(ID=edx$movieId,title=edx$title) %>% distinct() # make a
# new data frame that saves movieId and title, and use the distinct function to
# extract the unique movie ID's and names

edx$title<- NULL #Remove the title column from the test set to tidy
validation$title <- NULL #Remove the title column from the validation set to tidy

## 2.2 Analysis section
## 2.2.1 Data exploration
dim(edx)[1] # returns the amount of ratings
min(edx$rating) # lowest rating for a movie
max(edx$rating) # highest rating for a movie
uid<- edx %>%            # load the test set
  group_by(userId) %>%   # group per user
  summarize(count = n()) # count the amount of ratings per user
dim(uid)[1] # amount of unique users
max(uid[,2]) # highest amount of ratings for a single user
stars <- edx %>% 
  group_by(rating) %>%   # group per rating
  summarize(count = n()) # count the occurence of every rating
stars %>% ggplot(aes(x= rating,y = count)) + geom_col(color='black')  + xlab("Rating") +
   ylab("Count") + ggtitle("Occurences of ratings in the movielens data set") 
# create the graph that visualizes the rating distribution in the data set

## 2.2.2 Modelling
RMSE <- function(true_ratings, predicted_ratings){ # creates the RMSE-function
  sqrt(mean((true_ratings - predicted_ratings)^2))
} 

## 2.2.2.1 simple model (mean)
mu_model <- mean(edx$rating) # calculate the average rating of all ratings
mu_model # display the average rating

simple_rmse<-RMSE(validation$rating,mu_model) # calculate the RMSE
simple_frame<-data.frame(complexity="overall average",RMSE=simple_rmse) # create data frame to build the results table later
simple_frame # show the result

mean_ex<-seq(0.5,5,0.1) # create an array that covers the entire rating spectrum with a step of 0.1

#calculate the RMSE values for the entire spectrum
rmse_values <- sapply(mean_ex, function(i){ # use the sapply function to reiterate for every value of mean_ex
  RMSE(validation$rating,i)
})

#make a data frame of the variable so ggplot can use it
rmse_values<-rmse_values %>% 
  as.data.frame() %>%        # make a data frame so that it can be used for plotting
  mutate(rating=mean_ex)     # add the mean_ex vector so that it can be used as x axis to plot the graph

#plot the graph
rmse_values %>% ggplot(aes(x=rating, y = .)) + geom_point() + 
  xlab("Rating value") + ylab("RMSE") + ggtitle("RMSE for various rating values")


## 2.2.2.2 Movie effect

# calculate the average rating for each movie
average_rating_movie <- edx %>% group_by(movieId) %>% # group by movie 
  summarize(arm = mean(rating))                       # calculate average per movie

# Create a plot which shows the distribution of average movie ratings
average_rating_movie %>% ggplot(aes(x=arm)) + 
  geom_histogram(binwidth = 0.5, colour='black') +
  xlab("Average rating") + ylab("Occurences") + 
  ggtitle("Distribution of average rating per movie")

# calculate the bias per movie, the bias = avg. rating per movie - total avg. rating
movie_bias <- edx %>%                     # load the data set
  group_by(movieId) %>%                   # group the ratings per movieID
  summarize(b_i = mean(rating - mu_model))# calculate the bias per movie

# plot the bias distribution for the report
movie_bias %>% ggplot(aes(x=movie_bias$b_i)) + 
  geom_histogram(binwidth = 0.5, colour='black') +
  xlab("Bias") + ylab("Occurences") + 
  ggtitle("Distribution of movie bias")


predicted_ratings <- mu_model + validation %>% #load both the mu_model and validation data set as we want to be predict the validation
  left_join(movie_bias, by='movieId') %>% # add the movie_bias column to the predicted_ratings
  pull(b_i)                               # access the b_i column to save it in the variable


model_1_rmse <- RMSE(predicted_ratings, validation$rating) # calculate the RMSE by comparing predicted and actual ratings of the validation set
movie_frame<-data.frame(complexity="movie bias",RMSE=model_1_rmse) # create data frame to build the results table later
movie_frame # show the result

## 2.2.2.3 User effect

# calculate the average rating per user

user_bias <- edx %>%                             # load edx
  left_join(movie_bias, by='movieId') %>%        # add the movie bias column to the data frame
  group_by(userId) %>%                           # group the results by userID
  summarize(b_u = mean(rating - mu_model - b_i)) # calculate the average bias per user

#display the user preference figure
edx %>%                                         # load edx
  group_by(userId) %>%                          # group ratings per user
  summarize(user_avg = mean(rating)) %>%        # calculate the average rating per user
  filter(n()>=100) %>%                          # remove users that have less than 100 ratings
  ggplot(aes(user_avg)) +                       # prep ggplot w aesthetics
  geom_histogram(bins = 20, color = "black") +  # create a histogram
  xlab("Average rating") + ylab("Occurences") + # label the axes
  ggtitle("Average rating per user")            # create a title

predicted_ratings <- validation %>%        # load the validation data set
  left_join(movie_bias, by='movieId') %>%  # add the movie bias to the data frame
  left_join(user_bias, by='userId') %>%    # add the user bias to the data frame
  mutate(pred = mu_model + b_i + b_u) %>%  # add the predicted rating column per observation
  pull(pred)                               # access the pred column to save it in the variable

model_2_rmse <- RMSE(predicted_ratings, validation$rating) # calculate the RMSE
user_frame<-data.frame(complexity="user and movie bias",RMSE=model_2_rmse) # create data frame to build the results table later
user_frame # show the result

# 3. Results section
result_table<-bind_rows(simple_frame,movie_frame,user_frame) # construct a final overview of all the RMSEs
result_table # display the final overview of the results
cat("The final RMSE is:", model_2_rmse)

max_user<- uid$userId[which.max(uid$count)] # user ID with highest amount of ratings

genres_max_user <- edx[edx$userId==max_user,] %>%    # get only the data of the userId with the highest rating count
  separate_rows(genres, sep = "\\|") %>%             # separate the string and only keep the first word before the backslash
  group_by(genres) %>%                               # group the ratings per genre
  summarize(avg_rating=mean(rating),count = n()) %>% # calculate the average rating per genre
  arrange(desc(avg_rating)) %>%                      # rearrange the genres in a descending order
  filter(count > 100)                                # filter out the genres that have less than 100 ratings

genres_max_user    # display the list

#Average rating for the movie "Forrest Gump"
edx %>%filter(movieId==356) %>% summarize(mean(rating))

#plot the time variability of "Forrest Gump"'s average rating
edx %>%filter(movieId==356) %>%                                      # load the edx data frame and filter out the ratings for forrest gump
  mutate(years=year(as.POSIXct(timestamp, origin="1970-01-01"))) %>% # convert the timestamp to the YMD-HMS format and extract the year
  group_by(years) %>%                                                # group ratings per year 
  summarize(avg_rating=mean(rating)) %>%                             #calc the average rating per year
  ggplot(aes(x=years,y=avg_rating)) + geom_line() +                  # define the aesthetics and the line plot
  xlim(1995, 2010)+                                                  # limit the axes
  xlab("Year") + ylab("Average yearly rating") +                     # label the axes
  ggtitle("Time variation of the average yearly rating for Forrest Gump") # create a title
