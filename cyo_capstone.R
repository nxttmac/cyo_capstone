##########################################################
# title: "Predicting the outcome of a Chess game"
# author: "Andrew Ertell"
# date: "12/08/2020"
##########################################################

# Install required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

# 3 significant digits
options(digits = 3)

dl = tempfile()

unzip("games.csv.zip", "games.csv", overwrite = TRUE)

data_raw <- read_csv("games.csv")

# The last row has a parsing problem
data_raw <- data_raw[1:20022,]

# filter out some of the unnecessary columns
data <- data_raw %>% mutate(winner = factor(winner), 
                        victory_status = factor(victory_status),
                        opening_eco = factor(opening_eco)) %>%
                 select(rated, turns, victory_status, winner, white_rating, 
                       black_rating, opening_eco, opening_name, opening_ply, increment_code)

##################
# Data Exploration
##################

# How likely is each outcome?
table(data$winner)
data %>% group_by(winner) %>%
  summarize(win_pct = n()/nrow(data))
# White wins 49.9% of the time (does not win 50.1%)

# store the white win avg in a variable
white_win_mu = mean(data$winner == "white")

## NUMBER OF TURNS
# Average turns in a game
mean(data$turns)

# Histogram of turns in a game
data %>% ggplot(aes(turns)) + geom_histogram()

# This is a plot of the white win pct by turns
data %>% group_by(turns) %>% summarize(white_win_pct = mean(winner == "white"), n= n()) %>%
  ggplot(aes(turns, white_win_pct)) + geom_point() +
  ylab("white win pct")
# The "turns" are not turn pairs - let's create a variable for that

# This shows the win pct by # of turn pairs.  There is a filter applied to only include
# turn_pairs with at lease 50 games
data %>% mutate(turn_pair = ceiling(turns/2)) %>%
  group_by(turn_pair) %>%
  summarize(white_win_pct = mean(winner == "white"), n = n()) %>%
  filter(n >= 50) %>%
  ggplot(aes(turn_pair, white_win_pct)) + geom_point() + geom_smooth() + 
  xlab("turn pair") + ylab("white win pct")
# There seems to be a definite advantage for white in a shorter game

# This is a way of looking at the length of time in a game
game_length_time <- data$increment_code %>% strsplit(split="\\+")
temp <- sapply(game_length_time, head, 1)

# plotting the white win pct for time increments with at least 100 occurances
data %>% mutate(length = temp) %>% group_by(length) %>%
  mutate(white_win_pct = mean(winner == "white"), n = n()) %>%
  filter(n >= 100) %>% 
  ggplot(aes(x = reorder(length, as.numeric(length)), white_win_pct)) + geom_point()
# Note: there doesn't seem to be a discernible pattern here

## RATING
# The max rating is 2700
max(data$white_rating)

# The mean rating for each color
mean(data$white_rating)
mean(data$black_rating)

# white players have a _slightly_ higher average rating...
# Do better players know to select white, or does white have a better win pct
# because the average rating is a little higher?

# Histogram of white rating in RATED games
rated <- data %>% filter(rated == "TRUE") %>% ggplot(aes(white_rating)) + geom_histogram() + 
  ggtitle("Rated")
# Histogram of white rating in CASUAL (non-rated) games
casual <- data %>% filter(rated == "FALSE") %>% ggplot(aes(white_rating)) + geom_histogram() + ggtitle("Casual")
# Notice: There is a big spike around 1500.  This may be a default or initial rating
library(gridExtra)
grid.arrange(rated, casual, nrow = 1)

# Create a variable (rating_delta) that is the difference between white and black rating
# and makes a histogram
data %>% mutate(rating_delta = white_rating - black_rating) %>% 
  ggplot(aes(rating_delta)) + geom_histogram()

# Descriptive stats about rating delta
all_games <- data %>% mutate(rating_delta = white_rating - black_rating) %>% 
  summarize(average_rating_delta = mean(rating_delta),
            sd_rating_delta = sd(rating_delta))

# Add to a table
rating_delta_table <- data.frame(type = "All games", 
                                 average = all_games$average_rating_delta, 
                                 sd = all_games$sd_rating_delta)

# Rating delta in RATED games only
rated_games <- data %>% filter(rated == TRUE) %>% 
  mutate(rating_delta = white_rating - black_rating) %>% 
  summarize(average_rating_delta = mean(rating_delta),
            sd_rating_delta = sd(rating_delta))

rating_delta_table <- bind_rows(rating_delta_table, data_frame(type = "Rated Games", 
                                                        average = rated_games$average_rating_delta, 
                                                        sd = rated_games$sd_rating_delta))

# ratings delta in CASUAL (non-rated) games only
casual_games <- data %>% filter(rated == FALSE) %>% 
  mutate(rating_delta = white_rating - black_rating) %>% 
  summarize(average_rating_delta = mean(rating_delta),
            sd_rating_delta = sd(rating_delta))

rating_delta_table <- bind_rows(rating_delta_table, data_frame(type = "Casual Games", 
                                                        average = casual_games$average_rating_delta, 
                                                        sd = casual_games$sd_rating_delta))

rating_delta_table %>% knitr::kable()

# Plotting the win rate for white by rating delta
wins_by_rating_delta_all <- data %>% mutate(rating_delta = white_rating - black_rating) %>%
  filter(rating_delta >= -1000 & rating_delta <= 1000) %>%
  mutate(rating_delta_group = cut(rating_delta, seq(-1000, 1000, 25), ordered_result = TRUE)) %>%
  group_by(rating_delta_group) %>% summarize(n = n(), white_win_pct = mean(winner == "white")) %>%
  ggplot(aes(rating_delta_group, white_win_pct)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
  geom_point() + ggtitle("All Games") + xlab("Rating Group") + ylab("white win pct")

# Plotting the win rate for White by rating delta for RATED games only
wins_by_rating_delta_rated <- data %>% mutate(rating_delta = white_rating - black_rating) %>%
  filter(rating_delta >= -1000 & rating_delta <= 1000 & rated == "TRUE") %>%
  mutate(rating_delta_group = cut(rating_delta, seq(-1000, 1000, 25), ordered_result = TRUE)) %>% 
  group_by(rating_delta_group) %>% summarize(n = n(), white_win_pct = mean(winner == "white")) %>%
  ggplot(aes(rating_delta_group, white_win_pct)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
  geom_point() + ggtitle("Rated Games") + xlab("Rating Group") + ylab("white win pct")

# Plotting the win rate for White by rating delta for CASUAL (non-rated) games only
wins_by_rating_delta_casual <- data %>% mutate(rating_delta = white_rating - black_rating) %>%
  filter(rating_delta >= -1000 & rating_delta <= 1000 & rated == "FALSE") %>%
  mutate(rating_delta_group = cut(rating_delta, seq(-1000, 1000, 25), ordered_result = TRUE)) %>% 
  group_by(rating_delta_group) %>% summarize(n = n(), white_win_pct = mean(winner == "white")) %>%
  ggplot(aes(rating_delta_group, white_win_pct)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
  geom_point() + ggtitle("Casual Games") + xlab("Rating Group") + ylab("white win pct")

grid.arrange(wins_by_rating_delta_all, wins_by_rating_delta_rated, wins_by_rating_delta_casual, 
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)),
             heights = c(1.5, 1),
             nrow = 2, ncol = 2)

# Notice: There seems to be a stronger relationship for RATED games
# There are just over 16k RATED games in the dataset
data %>% filter(rated == "TRUE") %>% nrow()

## OPENING
# Types of chess openings (https://en.wikipedia.org/wiki/Encyclopaedia_of_Chess_Openings#Main_ECO_codes)
# Opening type doesn't SEEM like it makes much difference
data %>% mutate(opening_type = factor(substr(opening_eco, 1, 1))) %>%
  group_by(opening_type) %>%
  summarize(white_win_pct = mean(winner == "white"), n = n()) %>%
  ggplot(aes(opening_type, white_win_pct)) + geom_point() + 
  xlab("Opening Type") + ylab("white win pct")

# White win rate by opening (only considering openings used over 100 times)
data %>% group_by(opening_name) %>% summarize(n = n(), average = mean(winner == "white")) %>%
  filter(n >= 100) %>% arrange(desc(average)) %>%
  ggplot(aes(x = reorder(opening_name, -average), y = average)) + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  geom_point() + ylab("white win pct") + xlab("opening name")

#######
# Start making an algorithm!
#######

# Setting a seed for stable results
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

data <- as.data.frame(data) %>% mutate(rated = factor(rated),
                                       opening_eco = factor(opening_eco),
                                       turn_pair = ceiling(turns/2),
                                       opening_type = factor(substr(opening_eco, 1, 1)),
                                       rating_delta = white_rating - black_rating) %>%
                                select(rated, turns, victory_status, winner, white_rating, 
                                       black_rating, opening_ply, rating_delta,
                                       opening_eco, turn_pair, opening_type)

test_index <- createDataPartition(y = data$winner, times = 1, p = 0.1, list = FALSE)

train_data <- data[-test_index,]
temp <- data[test_index,]

validation <- temp %>%
  semi_join(train_data, by = "turn_pair") %>%
  semi_join(train_data, by = "opening_type")

removed <- anti_join(temp, validation)
train_data <- rbind(train_data, removed)

# make a test set from the train set below
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = train_data$rating, times = 1, p = 0.2, list = FALSE)
train <- train_data[-test_index,]
temp <- train_data[test_index,]

test <- temp %>%
  semi_join(train, by = "turn_pair") %>%
  semi_join(train, by = "opening_type")

removed <- anti_join(temp, test)
train <- rbind(train, removed)

# Let's try random guessing
predicted_naive <- sample(c(1,3), nrow(train_data), replace = TRUE)
mean(predicted_naive == as.numeric(train_data$winner))
## [0.473] Random guessing will be right about 1/2 the time

# Note: Cannot use GLM because there are 3 classes

# LDA with one variable
train_lda_rating_delta <- train(winner ~ rating_delta, method = "lda", data = train)
y_hat_lda_rating_delta <- predict(train_lda_rating_delta, test)  
confusionMatrix(y_hat_lda_rating_delta, reference = test$winner)$overall["Accuracy"]
## [0.609]

# QDA with one variable
train_qda_rating_delta <- train(winner ~ rating_delta, method = "qda", data = train)
y_hat_qda_rating_delta <- predict(train_qda_rating_delta, test)  
confusionMatrix(y_hat_qda_rating_delta, reference = test$winner)$overall["Accuracy"]
## [0.61]

# LDA with more variables
x <- data.frame( rating_delta = train$rating_delta, 
                 opening_ply = train$opening_ply,
                 turn_pair = train$turn_pair, 
                 victory_status = as.numeric(train$victory_status),
                 opening_eco = as.numeric(train$opening_eco), 
                 rated = as.numeric(train$rated))
y <- train$winner

train_lda_all <- train(x, y, method = "lda")

x_test <- data.frame( rating_delta = test$rating_delta,
                      opening_ply = test$opening_ply,
                      turn_pair = test$turn_pair, 
                      victory_status = as.numeric(test$victory_status),
                      opening_eco = as.numeric(test$opening_eco), 
                      rated = as.numeric(test$rated))
y_test <- test$winner

y_hat_lda_all <- predict(train_lda_all, x_test)  
confusionMatrix(factor(y_hat_lda_all), 
                                    reference = test$winner)$overall["Accuracy"]
## [0.645]

# QDA with more variables
train_qda_all <- train(x, y, method="qda")
y_hat_qda_all <- predict(train_qda_all, x_test)
confusionMatrix(factor(y_hat_qda_all), reference = y_test)
## Accuracy: [0.657]

# KNN with cross-validation
control <- trainControl(method = "cv", number = 10, p = .9)
set.seed(1, sample.kind = "Rounding")
train_knn_cv <- train(x, y,
                      method = "knn",
                      tuneGrid = data.frame(k = seq(21, 141, 10)),
                      trControl = control)
train_knn_cv$bestTune

y_hat_knn_cv <- predict(train_knn_cv, x_test)
confusionMatrix(y_hat_knn_cv, reference = y_test)
## Accuracy: [0.609] w k = 121
confusionMatrix(y_hat_knn_cv, reference = y_test)$byClass[,"Sensitivity"]
## KNN never predicts a draw

# R Part
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(x, y,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart, x_test)
confusionMatrix(y_hat_rpart, reference = y_test)$overall["Accuracy"]
## Accuracy: [0.656]

# train_rpart$finalModel

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# Random Forest
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method="cv", number = 5)
train_rf <- train(x, y,
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(1, 5, 1)),
                  ntree = 101,
                  trControl = control
                  )
train_rf$bestTune
y_hat_rf <- predict(train_rf, x_test) 
confusionMatrix(y_hat_rf, reference = y_test)$overall["Accuracy"]
confusionMatrix(y_hat_rf, reference = y_test)
## Accuracy: [0.668]

varImp(train_rf)

#######
# Ensemble
#######

# Build an ensemble from the models
ensemble_df <- data.frame(lda = as.numeric(y_hat_lda_all), 
                          qda = as.numeric(y_hat_qda_all), 
                          knn_cv = as.numeric(y_hat_knn_cv),
                          rpart = as.numeric(y_hat_rpart), 
                          rf = as.numeric(y_hat_rf))

# get mode function - the mode is the majority vote
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# create modes vector 
modes <- vector(mode = "numeric", length = length(ensemble_df[,1]))

# Loop through data frame and get "majority vote"
for (row in 1:nrow(ensemble_df)) {
  modes[row] <- getmode(as.numeric(ensemble_df[row,]))
}

# factor the modes vector
modes_factored <- factor(modes, 
                         levels = c(1, 2, 3), 
                         labels = c("black", "draw", "white"))

confusionMatrix(modes_factored, reference = y_test)
## Accuracy: [0.66]

#######
# Validation
#######

# Prep the validation dataset
x_validation <- data.frame(
  rating_delta = validation$rating_delta, 
  opening_ply = validation$opening_ply,
  turn_pair = validation$turn_pair, 
  victory_status = as.numeric(validation$victory_status),
  opening_eco = as.numeric(validation$opening_eco), 
  rated = as.numeric(validation$rated))

# Validation outcomes
y_validation <- validation$winner

# Predict the outcomes with each model  
y_hat_lda_all_validation <- predict(train_lda_all, x_validation)
y_hat_qda_all_validation <- predict(train_qda_all, x_validation)
y_hat_knn_cv_validation <- predict(train_knn_cv, x_validation)
y_hat_rpart_validation <- predict(train_rpart, x_validation)
y_hat_rf_validation <- predict(train_rf, x_validation)

confusionMatrix(y_hat_lda_all_validation, reference = y_validation)$overall["Accuracy"]
confusionMatrix(y_hat_qda_all_validation, reference = y_validation)$overall["Accuracy"]
confusionMatrix(y_hat_knn_cv_validation, reference = y_validation)$overall["Accuracy"]
confusionMatrix(y_hat_rpart_validation, reference = y_validation)$overall["Accuracy"]
confusionMatrix(y_hat_rf_validation, reference = y_validation)$overall["Accuracy"]

# Build a dataframe with all model predictions
ensemble_df_validation <- data.frame(
  lda = as.numeric(y_hat_lda_all_validation),  
  qda = as.numeric(y_hat_qda_all_validation),
  knn_cv = as.numeric(y_hat_knn_cv_validation),
  rpart = as.numeric(y_hat_rpart_validation), 
  rf = as.numeric(y_hat_rf_validation))

# create modes vector 
modes_validation <- vector(mode = "numeric", 
                           length = length(ensemble_df_validation[,1]))

# Loop through data frame and get "majority vote"
for (row in 1:nrow(ensemble_df_validation)) {
  modes_validation[row] <- getmode(as.numeric(ensemble_df_validation[row,]))
}

# factor the modes vector
modes_factored_validation <- factor(modes_validation, 
                         levels = c(1, 2, 3), 
                         labels = c("black", "draw", "white"))

confusionMatrix(modes_factored_validation, reference = y_validation)
# Accuracy: [0.669]
