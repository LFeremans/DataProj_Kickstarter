#######################################################
# Start of code for the kickstarter Capstone project #
#######################################################

# Load the required packages, if some of these are not yet installed please use the install.package() function

if(!require(stringr)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(dslabs)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(dplyr)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(lubridate)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(tidyverse)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(e1071)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(tidytext)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it
if(!require(devtools)) install.packages("caret", repos = "http://cran.us.r-project.org") # check if library is installed, if not install it

library(stringr) # load library
library(dslabs) # load library
library(dplyr) # load library
library(lubridate) # load library
library(tidyverse) # load library
library(caret) # load library
library(e1071) # load library
library(tidytext) # load library
library(devtools) # load library

# 1. Overview
#-------------

# 2. Prediction approach
#------------------------
  
## 2.1: Kaggle Kickstarter data set:
# IMPORTANT: the original zipe file was downloaded from this link
# https://www.kaggle.com/codename007/funding-successful-projects/downloads/funding-successful-projects-on-kickstarter.zip/1
# and uploaded to my GitHub repository:
# https://github.com/LFeremans/DataProj_Kickstarter
# the train.csv file is zipped as otherwise it was too big to be uploaded to github

# The zip file that can be downloaded from the Kaggle link above contains a test set, training set and a sample submission set
# the train set is used to train and validate the machine learning and the test.csv set is used to look at its variables.
# the submission.csv set is not used in this project

# download the kickstarter train set from my Github: https://github.com/LFeremans/DataProj_Kickstarter
temp <- tempfile() # create a temporary file
download.file("https://github.com/LFeremans/DataProj_Kickstarter/raw/master/train.csv.zip", 
              temp) # download the zip and put it in the temp folder
con <- unz(temp, "train.csv") # unzip the train.csv.zip
kickstarter <- read_csv(con) # read the train.csv and save it into kickstarter
unlink(temp) # unlink the temporary file
rm(con,temp) # remove the variable


### 2.1.1 Kaggle train set basic exploration

# Get the dimensions of the data set
dim(kickstarter)[1] # amount of projects
dim(kickstarter)[2] # amount of variables
print(object.size(kickstarter),units="Mb") # size of the kickstarter data set

# Show the column names and their type
str(kickstarter)

### 2.1.2 Kaggle train set basic exploration

#Import the kaggle test set
urlfile<-'https://github.com/LFeremans/DataProj_Kickstarter/raw/master/test.csv' # Download the file from my GitHub repository
kickstarter_test<-read_csv(urlfile) # read it to a data frame
rm(urlfile)

# Show the column names and their type
str(kickstarter_test)
# show the dimensions of the Kaggle test set
dim(kickstarter_test)
# remove the test set as it is of no further use and takes up memory
rm(kickstarter_test)

## 2.1.3 Data cleaning

# clean the timestamps to a YMD-HMS format
kickstarter <- kickstarter %>% mutate(created_at=as.POSIXct(created_at, origin="1970-01-01"), #modify to YMD-HMS format
       launched_at=as.POSIXct(launched_at, origin="1970-01-01"), #modify to YMD-HMS format
       state_changed_at=as.POSIXct(state_changed_at, origin="1970-01-01"), #modify to YMD-HMS format
       deadline=as.POSIXct(deadline, origin="1970-01-01")) #modify to YMD-HMS format

# Show the keywords being separated by dashes and replacing the dashes by spaces
head(kickstarter$keywords)
kickstarter$keywords<-str_replace_all(kickstarter$keywords, "-", " ")
head(kickstarter$keywords)

### 2.1.4 In-depth data exploration

# Average succes rate on kickstarter
mean(kickstarter$final_status)

min(kickstarter$created_at) # First data at which a project was created
max(kickstarter$deadline)   # last deadline of the project
max(kickstarter$deadline)-min(kickstarter$created_at) # time between first creation and last deadline

# Calculate time delta between states in days
kickstarter<-kickstarter %>% 
  mutate(launch_delta=time_length(interval( created_at, launched_at), "day"), # calculate days difference between creation and launch
                  deadline_delta=time_length(interval( launched_at, deadline), "day"), # calculate days difference between launch and deadline
                  state_delta=time_length(interval( deadline, state_changed_at),"day")) # calculate days difference between deadline and state change

mean(kickstarter$deadline_delta) # average amount of days between launching and deadline of projects
min(kickstarter$deadline_delta) # minimum amount of days between launching and deadline of projects
max(kickstarter$deadline_delta) # maximum amount of days between launching and deadline of projects

bb <- kickstarter                   # make a dummy variable to not alter kickstarter
bb$idu <- as.numeric(row.names(bb)) # add the rownumber as a variable to make the plot below

#example of different timelines for different projects
stat_progr<-data.frame(idx=bb$idu[40:50],ca=bb$created_at[40:50],la=bb$launched_at[40:50],
               dl=bb$deadline[40:50],sca=bb$state_changed_at[40:50]) # create a data frame which stores the identity of projects 40 to 50
# and four other variables to create the timelines
rm(bb) # remove the variables as they are of no further use

ggplot(data=stat_progr,aes(x = idx,fill=idx))+ # load the data and define the x of the aes
  geom_line(aes(y=ca,colour="CreatedAt"))+       # create a lineplot of the creation date
  geom_line(aes(y=la,colour='LaunchedAt'))+      # create a lineplot of the launch date
  geom_line(aes(y=dl,colour='Deadline'))+        # create a lineplot of the deadline date
  geom_line(aes(y=sca,colour='StateChangedAt'))+ # create a lineplot of the state change date
  scale_color_manual(name="Legend",              # add the legend and its colours
                     values=c(CreatedAt='blue', LaunchedAt='green',
                              Deadline='red', StateChangedAt='black')) +
  xlab("Project #") + ylab("Timestamp") +       # add the x and y axis label
  ggtitle("Comparison of timestamps for different events") # add the title
rm(stat_progr)

# disable communication analysis
dis_comm <- kickstarter %>% filter(disable_communication==TRUE) # filter out the projects where the communication is disabled
dim(dis_comm)[1]               # amount of projects with disabled communication
unique(dis_comm$final_status)  # do zero and one appear?
rm(dis_comm)                   # remove the variable as it is of no further use

# Country analysis
unique(kickstarter$country)                        # list of unique countries
countries<-kickstarter %>% group_by(country) %>%   # group by country
  summarize(succes=mean(final_status),count=n()) %>% # calculate the average succesrate per country and the amount of projects
  arrange(desc(succes))                            # arrange in descending succes rate
countries %>% ggplot(aes(x=reorder(country,-succes),y=succes)) +  # define aes
  geom_col(colour='black')+                        # make a column plot
  ylab("Succes rate (%)") + xlab("Country") +  # label the y and x axis
  ggtitle("Average succes rate per country")   # add the title
countries          # show the result
rm(countries)      # remove the variable


# Currency analysis
unique(kickstarter$currency)      # list of unique currencies
currencies<-kickstarter %>% group_by(currency) %>% # group by currency
  summarize(succes=mean(final_status),count=n()) %>% # calculate the average succesrate per currency and the amount of projects
  arrange(desc(succes))                  # arrange in descending succes rate
currencies %>% ggplot(aes(x=reorder(currency,-succes),y=succes)) + # define aes and use reorder to respect descending order of succes
  geom_col(colour='black')+                  # make a column plot
  ylab("Succes rate (%)") +  xlab("Currency") + # label the y and x axis
  ggtitle("Average succes rate per currency") # add the title
currencies          # show the result
rm(currencies)      # remove the variable

#### 2.1.4.1 Backers analysis
min(kickstarter$backers_count) # minimal amount of backers for a project
max(kickstarter$backers_count) # maximal amount of backers for a project
sum(kickstarter$backers_count) # total amount of backings (some persons might back diff projects)
sum(kickstarter$backers_count)/dim(kickstarter)[1]

backers <- kickstarter %>%    
  mutate(hist_backers=ifelse(backers_count> 1000,1000,backers_count)) # If a project has more than 1000 backers, remove the number and use 1000,
# this can be interpreted as having an open right limit for the interval of higher than 1000 backers
backers <- hist(backers$hist_backers,xlab="Backers") # display the histogram of backers vs frequency

kickstarter %>% # plot the average succes rate vs amount of backers 
  mutate(interval = cut(x=backers_count,breaks=c(0,1:9 %o% 10^(1:5)), include.lowest = TRUE)) %>% # add to each observation in which interval of amount of backers it belongs
  # the interval limits are powers of 10
  group_by(interval) %>%  # group the data set by the newly created intervals
  summarise( avg = mean(final_status)) %>% # calculate the average succes rate per interval
  ggplot(aes(x=c(0,1:9 %o% 10^(1:5))[1:37],y=avg)) +  # define aes to plot the succes rate vs amount of backers
  geom_point() + # use the geom_points to display the result
  scale_x_log10() + xlab("Amount of backers") + ylab("Average succes rate for interval") + # add the labels and scale the x axis to a log-10 scale
  ggtitle("Amount of backers vs average succes rate") # add the title
rm(backers) # remove the variable


#### 2.1.4.2 Currency and country

concur<-as.data.frame(sapply(seq(1:11),function(x){ # make a data frame that shows which country pays with which currency
  b<-unique(kickstarter$country)                    #get all the country names
  a<-kickstarter%>% filter(kickstarter$country==b[x]) # get all the currencies per country
  unique(a$currency) # show the different currencies per country
},simplify=FALSE))

#rename the column names of concur to show currency vs country
concur <- data.frame(country=unique(kickstarter$country), # unique countries
  currency_used =c("USD","GBP","CAD","AUD","NZD","EUR","SEK","EUR","NOK","DKK","EUR")) # all their currencies read from the concur above
concur # show result
rm(concur) # remove variable as it is no longer needed

#### 2.1.4.3 Goals analysis

# recalculated currency, exchange rates of 29/05/2019, put in new column rate_goal
currency_LUT <- data.frame(currency=c(unique(kickstarter$currency)), # make a LUT data frame that contains the currency vs rate
                           rate= c(1, 1.27, 0.74, 0.69, 0.65, 1.12, 0.10,0.11,0.15))

kickstarter <- kickstarter %>% # add the rate_goal variable to kickstarter that containts the goal in USD
  mutate(rate_goal=goal*currency_LUT$rate[match(currency,currency_LUT$currency)])

min(kickstarter$rate_goal)  # minimal goal in USD
mean(kickstarter$rate_goal) # average goal in USD
max(kickstarter$rate_goal)  # maximal goal in USD

goals <- kickstarter %>% mutate(hist_goal=ifelse(rate_goal> 1e+5,1e+5,rate_goal)) # create a new dataframe that has the kickstarter data
# and which has a variable that contains the rate_goal limited to 1e+5, every goal bigger than this is set to 1e+5
goals <- hist(goals$hist_goal) # create a histogram with breaks that shows the amount of projects per interval
goals$mids   # display middle values of intervals
goals$counts # display amount of projects per interval

kickstarter %>% mutate(interval = cut(rate_goal,                   # add an interval variable which dictates in which interval the goal is situated
                                      breaks=goals$breaks,         # use the breaks from the histogram created above to define the intervals to use
                                      include.lowest = TRUE)) %>%
  group_by(interval) %>%                     # group by the intervals
  summarise( avg = mean(final_status)) %>%   # calculate the average succes rate per interval
  ggplot(aes(x=c(goals$mids,100250),y=avg)) + # define the aes variables and use the middle values of intervals as x values
  geom_line() + xlab("Average goal of the interval") + ylab("Succes rate") +  # use geom_line and add labels
  ggtitle("Size of goal vs. succes rate (cutoff 1e+5)") # add title
rm(goals, currency_LUT) # remove variable as it is no longer needed

#### 2.1.4.4 Text analysis:

head(kickstarter$name) # show the first 6 project names

head(kickstarter$desc) # show the first 6 project descriptions
nchar(kickstarter$desc[1:20]) # show the length of the first 20 descriptions

head(kickstarter$keywords) # show the first 6 project keyword strings
unique(kickstarter$name==kickstarter$keywords) # show that not all the project names and keywords are equal

print(object.size(kickstarter$desc),units="Mb")
print(object.size(kickstarter$name),units="Mb")
print(object.size(kickstarter$keywords),units="Mb")

keyw<- kickstarter  # save the kickstarter set in a dummy variable for manipulation
keyw$goal<-NULL  # remove the unneccessary variable (column)
keyw$disable_communication<-NULL # remove the unneccessary variable (column)
keyw$country<-NULL # remove the unneccessary variable (column)
keyw$currency<-NULL # remove the unneccessary variable (column)
keyw$launched_at<-NULL # remove the unneccessary variable (column)
keyw$backers_count<-NULL # remove the unneccessary variable (column)
keyw$name<-NULL # remove the unneccessary variable (column)
keyw$desc<-NULL # remove the unneccessary variable (column)
keyw$deadline <- NULL # remove the unneccessary variable (column)
keyw$state_changed_at <- NULL # remove the unneccessary variable (column)
keyw$created_at <- NULL # remove the unneccessary variable (column)
keyw$launch_delta <- NULL # remove the unneccessary variable (column)
keyw$state_delta <- NULL # remove the unneccessary variable (column)
keyw$deadline_delta <- NULL # remove the unneccessary variable (column)
keyw$rate_goal <- NULL # remove the unneccessary variable (column)
head(keyw)

#Create the Look Up Table to rank words
keyw<- keyw %>% unnest_tokens(word,keywords) #unnest all the words
garb1 <- keyw %>% group_by(word) %>% summarize(succes=mean(final_status)) # calculate succes rate per word
garb2 <- keyw %>% count(word) # count amount of words
unique(garb1$word==garb2$word) # shows that all the words are the same otherwise unique would return true and false
keyword_LUT<-data.frame(word=garb1$word,n=garb2$n,succes=garb1$succes) %>% # merge the two tables into one that contains word, occurence and succes rate
  arrange(desc(n)) %>% # arrange in descending order
  filter(!word %in% stop_words$word)%>% mutate(rank=seq(1:65142)) # remove common (stop) words (f.ex. a, the, ...) and attach rank (rank 1 is highest rank, most occurences)
to_rm<-keyword_LUT %>% filter(n<=650)                            # find words with less than 650 occurences
keyword_LUT<-keyword_LUT[-min(to_rm$rank):-last(to_rm$rank),] # remove words that have less than 650 occurences
keyword_LUT[35,]<- c("other",1,0.33,35) # add a last row that uses "other" as a filling word
rm(garb1,garb2,to_rm) # remove unnecessary variables

head(keyword_LUT) # show the 6 first words with count and rank
sum(as.numeric(keyword_LUT$n)) # sum the occurences of the first 34 words

keyw<-keyw %>% mutate(rank=sapply(seq(1:dim(keyw)[1]),function(x){ # apply a function to the unnest word column that assigns the rank to every word
  a<-min(grep(keyw$word[x],keyword_LUT$word),na.rm=TRUE) # for every project find the word with the highest rank( most occurences)
  ifelse(a==Inf,35,a) # if no identified keyword of the project return in the top 34 words, use the dummy word "other" to fill  
}))

keyw_sol<-keyw %>% group_by(project_id) %>% # create a solutions vector that groups by project id
  summarize(first=min(rank))%>%  # finds the highest rank per project
  mutate(top_word=keyword_LUT$word[first]) # finds the word associated to the highest rank and adds it to the keyw_sol

kickstarter <- kickstarter %>% 
  mutate(top_word=keyw_sol$top_word[match(x=keyw_sol$project_id,table = kickstarter$project_id)]) # match project id from keyw_sol and kickstarter to add the correct top word to the kickstarter vector
kickstarter$word<-NULL # remove the word column

rm(keyw,keyw_sol,keyword_LUT) # remove variable as it is no longer needed

print(object.size(kickstarter$top_word),units="Mb") # show the new size of the top_words vector


## 2.2 Method analysis

kick<-kickstarter # create a dummy variable in which to store the original data set @ to be used for further use
kick$project_id<-NULL # remove the unneccessary variable (column)
kick$goal<-NULL # remove the unneccessary variable (column)
kick$name<-NULL # remove the unneccessary variable (column)
kick$desc<-NULL # remove the unneccessary variable (column)
kick$keywords<-NULL # remove the unneccessary variable (column)
kick$country<-NULL # remove the unneccessary variable (column)
kick$backers_count<-NULL # remove the unneccessary variable (column)
kick$state_changed_at <- NULL # remove the unneccessary variable (column)
kick$created_at <- NULL # remove the unneccessary variable (column)
kick$launch_delta <- NULL # remove the unneccessary variable (column)
kick$state_delta <- NULL # remove the unneccessary variable (column)
kick$deadline <- NULL # remove the unneccessary variable (column)
kick<-kick[c(1,2,7,6,3,5,4)] # rearrange to a more logical order
head(kick) # display the final data frame

# Modelling
# validation set will be 10% of the total data
set.seed(1) # set the seed to always have the same train and test set
test_index <- createDataPartition(y = kickstarter$final_status, times = 1, p = 0.1, list = FALSE) # create test_indexes with 90% train and 10% test
train <- kick[-test_index,] # create the train set by NOT taking the test_indexes from the entire set
test <- kick[test_index,] # create the test set by taking the test_indexes from the entire set

### 2.2.3 Average succes model:

mean(train$final_status) # average succes rating of the entire training data set
set.seed(5) # set the seed to have a reproducible outcome on different machines and at different moments in time
pred_avg<- sample(c(0,1), replace=TRUE, prob=c(1-mean(train$final_status),1-mean(train$final_status)), # sample vector with 0 and 1 with probability p0= 1-p1 and p1= average succesof the data set
               size= dim(as.data.frame(test$final_status))[1]) # sample the amount of times equal to the size of the test set
CF_avg<-confusionMatrix(table(pred_avg, test$final_status),positive="1") # make a confusion matrix to look at the different parameters
CF_avg # display the confusion matrix

### 2.2.4 GLM modelling

train_thou<-train[75000:80000,] # define the test sample to get an estimate of running time for the entire model

# GLM fitting
set.seed(1) # set the seed to have a reproducible outcome on different machines and at different moments in time
fit_glm_thou <- glm(final_status ~ . , data=train_thou, family = "binomial") # train the final status as function of all variables with the binomial family
pred_glm_thou <- predict(fit_glm_thou, test, type = 'response') # predict the values for each observation of the test set
y_hat_glm_thou <- factor(ifelse(pred_glm_thou > 0.5, 1,0)) # as prediction is not categorical, observations above 0.5 are seen as succes and below as fail
CF_glm_thou<- confusionMatrix(table(y_hat_glm_thou, test$final_status),positive="1") # save the reults as a confusion matrix
CF_glm_thou # display the confusion matrix

set.seed(1) # set the seed to have a reproducible outcome on different machines and at different moments in time
fit_glm <- glm(final_status ~ . , data=train, family = "binomial") # train the final status as function of all variables with the binomial family
pred_glm <- predict(fit_glm, test, type = 'response') # predict the values for each observation of the test set
y_hat_glm <- factor(ifelse(pred_glm > 0.5, 1,0)) # as prediction is not categorical, observations above 0.5 are seen as succes and below as fail
CF_glm<-confusionMatrix(table(y_hat_glm, test$final_status),positive="1")  # save the reults as a confusion matrix
CF_glm # show the confusion matrix

var_glm<-data.frame(var=rownames(varImp(fit_glm)),overall=varImp(fit_glm)[,1]) %>% # save a dataframe with the variable importances
  arrange(desc(overall)) # order the variables by descending importance

var_glm[1:10,] %>% ggplot(aes(x=reorder(var,-overall),y=overall))  + geom_col() + # take the 10 most important variables and plot them on columns in descending order of importance
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # rotate the x labels 90 degrees
  xlab("variable") + ylab("importance") + title("Importance of different varibles (GLM)") # add labels and a title

CF_glm_thou$overall[["Accuracy"]] # accuracy score of the model with only 5000 observations
CF_glm$overall[["Accuracy"]]      # accuracy score of the model with all observations


GLM_var<-sapply(seq(0.01,0.67,0.01), function(x){ # rerun different cut-off numbers to see how the results change
  y_hat_glm_vary <- factor(ifelse(pred_glm > x, 1,0)) # as prediction is not categorical, observations above 0.5 are seen as succes and below as fail
  c<-confusionMatrix(table(y_hat_glm_vary, test$final_status),positive="1")$overall["Accuracy"] # save the accuracy, define 1 as being the positive factor for specificity and sensitivity
  se<-sensitivity(table(y_hat_glm_vary, test$final_status),positive="1") # save the sensitivity, define 1 as being the positive factor for sensitivity
  sp<-specificity(table(y_hat_glm_vary, test$final_status),positive="1") # save the specificity, define 1 as being the positive factor for specificity
  res <- c(Cut_off=x,c,Sensitivity=se,Specificity=sp) # create a table of the results
  return(res) # return the results
})

GLM_var<-data.frame(Cut_off=GLM_var[1,], Accuracy=GLM_var[2,], Sensitivity=GLM_var[3,], Specificity=GLM_var[4,]) # create a data frame for ggplot

GLM_var %>% ggplot(aes(x = Cut_off,fill=Cut_off))+ # load the data and define the x of the aes and the fill
  geom_line(aes(y=Accuracy,colour="Accuracy"))+       # create a lineplot of the accuracy
  geom_line(aes(y=Sensitivity,colour='Sensitivity'))+ # create a lineplot of the sensitivity
  geom_line(aes(y=Specificity,colour='Specificity'),linetype="dashed")+ # create a lineplot of the specificity
  scale_color_manual(name="Legend",              # add the legend and its colours
                     values=c(Accuracy='blue', Sensitivity='green',
                              Specificity='red')) +
  xlab("Cut_off") + ylab("Metric") +       # add the x and y axis label
  ggtitle("Comparison of accuracy, sensitivity and specificity vs cut-off") # add the title



### 2.2.5 SVM modelling

#5000 variables limited model kernel testing
kernels<- factor(c("linear","polynomial","radial","sigmoid")) # make a list of the different kernels
kerns_thou<- sapply(kernels,function(x){ #apply the SVM on different kernels by using sapply
  fit_svm_thou <- svm(final_status ~ . - top_word, data=train_thou,kernel=kernels[x],type="C-classification") # fit the SVM while varying the kernels while dropping the top_word
  pred_svm_thou <- predict(fit_svm_thou, test, type = 'response') # predict the values
  confusionMatrix(table(pred_svm_thou, test$final_status),positive="1")$overall["Accuracy"] # return the accuracy
})
kerns_thou <- data.frame(kernels,accuracy=kerns_thou) # create a data frame to order the results
kerns_thou # display results

# full sets models and CF for different kernels, this may take a very long time to execute (1 hour on a 2012 Macbook Pro)
fit_svm_lin <- svm(final_status ~ . - top_word, data=train,kernel="linear",type="C-classification") # fit the SVM for a linear kernel and the entire data set
pred_svm_lin <- predict(fit_svm_lin, test, type = 'response') # predict the values
CF_svm_lin<-confusionMatrix(table(pred_svm_lin, test$final_status), positive = "1") # return the confusion matrix with 1 being the positive for sensitivity and specificity
CF_svm_lin # show the confusion matrix

fit_svm_poly <- svm(final_status ~ . - top_word, data=train,kernel="polynomial",type="C-classification") # fit the SVM for a polynomial kernel and the entire data set
pred_svm_poly <- predict(fit_svm_poly, test, type = 'response') # predict the values
CF_svm_poly<-confusionMatrix(table(pred_svm_poly, test$final_status),positive="1") # return the confusion matrix with 1 being the positive for sensitivity and specificity
CF_svm_poly # show the confusion matrix

fit_svm_rad <- svm(final_status ~ . - top_word, data=train,kernel="radial",type="C-classification") # fit the SVM for a radial kernel and the entire data set
pred_svm_rad <- predict(fit_svm_rad, test, type = 'response') # predict the values
CF_svm_rad<-confusionMatrix(table(pred_svm_rad, test$final_status),positive="1") # return the confusion matrix with 1 being the positive for sensitivity and specificity
CF_svm_rad # show the confusion matrix

fit_svm_sig <- svm(final_status ~ . - top_word, data=train,kernel="sigmoid",type="C-classification") # fit the SVM for a sigmoidal kernel and the entire data set
pred_svm_sig <- predict(fit_svm_sig, test, type = 'response') # predict the values
CF_svm_sig<-confusionMatrix(table(pred_svm_sig, test$final_status),positive="1") # return the confusion matrix with 1 being the positive for sensitivity and specificity
CF_svm_sig # show the confusion matrix

# 3. results
# make a data frame that is used to discuss the results
overview <- data.frame( type = c("average", "glm","svm lin","svm poly", "svm rad", "svm_sig"), # add the types
                        A=c(3714,7247,7367,7367,7367,5071), # add A
                        B=c(1691,3330,3446,3446,3446,2320), # add B
                        C=c(3653,120,0,0,0,2296), # add C
                        D=c(1755,116,0,0,0,1126), # add D
                        accuracy=c(0.51,0.68,0.68,0.68,0.68,0.57), # add accuracies
                        sensitivity=c(0.51,0.03,0.0,0.0,0.0,0.33), # add sensitivities
                        specificity=c(0.50,0.98,1.0,1.0,1.0,0.68)) %>% # add specificities
  mutate(neutral=A+C,win=D,loss=B) # add the neutral, win and loss variables
overview

mean(test$final_status) # show the overall average succes of the test set

# get the confusionMatric for the 5000 observations polynomial kernel SVM model
set.seed(1) # set the seed
fit_svm_thou_poly <- svm(final_status ~ . - top_word, data=train_thou,kernel="polynomial",type="C-classification") # fit the SVM for a polynomial kernel and the 500 observations data set
pred_svm_thou_poly <- predict(fit_svm_thou_poly, test, type = 'response') # predict the values
confusionMatrix(table(pred_svm_thou_poly, test$final_status),positive="1") # return the confusion matrix with 1 being the positive for sensitivity and specificity

#########################
#####    END     ########
#########################