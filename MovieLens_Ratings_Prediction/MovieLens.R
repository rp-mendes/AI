# Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Saving the data to the disk
save(edx, file = "edx.RData")
save(validation, file = "validation.RData")

#Loading libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
library(caret)
library(kableExtra)

# Configuring the theme for the plots
plots_theme <- theme_classic() + theme(
  plot.title = element_text(color = "#0099F8", size = 16, face = "bold"),
  axis.title = element_text(size = 10)
)


# Tabulate class of variables and first 5 rows included in edx dataset
rbind((lapply(edx, class)), head(edx)) %>%
  kable(caption = "edx dataset: variable class and first 5 rows", align = 'ccclll', booktabs = T,format = "latex", linesep = "") %>%
  row_spec(1, hline_after = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "hold_position"))


ratings <- edx %>% group_by(rating) %>% summarise(count = n())
frequent_ratings <- ratings %>% arrange(desc(count)) %>% head(2) %>% select(rating) %>% arrange(rating) %>% pull()


# Number of ratings
ratings %>% arrange(rating) %>%
  kable(col.names = c("Rating", "Count"), caption = "Distribution per Rating", align = 'cc', booktabs = T,format = "pandoc", linesep = "") %>%
  row_spec(1, hline_after = T, font_size=3) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "hold_position"))

# Plotting the ratings in a histogram
edx %>% ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.2, color = "#000000", fill = "#0099F8") +
  scale_y_continuous(breaks = c(1000000, 2000000), labels = c("1", "2")) +
  scale_x_continuous(breaks = seq(0.5, max(edx$rating), 0.5)) +
  labs(
    title = "Distribution of Ratings",
    x = "Rating", 
    y = "Count(in millions)") +
  plots_theme


# Most given ratings
wholeStars <- edx %>% group_by(rating) %>% summarise(count = n()) %>% filter(rating %in% c(1.0, 2.0, 3.0, 4.0, 5.0)) %>% summarise(total = sum(count)) %>% pull()
halfStars <- edx %>% group_by(rating) %>% summarise(count = n()) %>% filter(rating %in% c(0.5, 1.5, 2.5, 3.5, 4.5)) %>% summarise(total = sum(count)) %>% pull()

stars <- data.frame(Type = c("Whole Stars", "Half Stars"), Values = c(wholeStars, halfStars))

stars %>% 
  ggplot(aes(Type, y=Values/1000000)) + 
  geom_bar(stat = 'identity', color = "#000000", fill = "#0099F8") +
  geom_text(aes(label=Values), size = 3, vjust = -0.5) +
  labs(
    title = "Whole vs Half Stars",
    y = "Count(in millions)") +
  plots_theme +
  theme(axis.title.x = element_blank())


print(paste('This is the number of unique movies in the dataset: ', edx %>% summarise(movies = n_distinct(movieId)) %>% pull()))

print(paste('There are ', edx %>% select(genres) %>% distinct() %>% separate_rows(genres, sep = "\\|") %>% n_distinct(.$genres), 'different genres in the dataset.'))

# Number of movies per genre
movies_genre <- edx %>% select(movieId, genres) %>% distinct() %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarise(count = n())

print(movies_genre %>% arrange(., count))

# Plotting the genres in a histogram
movies_genre %>%
  ggplot(aes(genres, x = reorder(genres, count), y = count)) + 
  geom_bar(stat = 'identity', color = "#000000", fill = "#0099F8") +
  coord_flip() +
  geom_text(aes(label=count), vjust=0.5, hjust = -0.1, size=3) +
  labs(
    title = "Movies by Genre",
    x = "Genre", 
    y = "# Movies") +
  plots_theme

# Top Rated Movies
# Separate individual genres and ranking them by the total number of ratings in the edx dataset
print(edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), rating = round(mean(rating), 2)) %>%
  arrange(desc(count)))

# Get the movie with the greatest number of ratings
print(edx %>% group_by(movieId, title) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head())

# Users

print(paste('This is the number of unique users in the dataset: ', edx %>% summarise(users = n_distinct(userId)) %>% pull()))

# Creating the Test Set

# Creating training and testing sets from edx
set.seed(1234, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Ensure userId and movieId are also in the training set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)
# Remove temporary files to tidy environment
rm(test_index, temp, removed) 

rmse_objective <- 0.86490

# Average Model

# Calculate the average rating across all movies in the training set
mu_hat <- mean(train_set$rating)
# Calculate RMSE for this model
simple_avg_rmse <- RMSE(test_set$rating, mu_hat)

print(paste('Average: ', mu_hat))
print(paste('RMSE using the average model: ', simple_avg_rmse))

# Random Forest

library(randomForest)
library(doParallel)

predictors_train <- train_set %>% select(userId, movieId, timestamp, title, genres)
predictors_test <- test_set %>% select(userId, movieId, timestamp, title, genres)

# The code below looks for the number of trees that reports the lower RMSE. It is commented since it takes a long time to run.
# cluster <- makePSOCKcluster(7)
# registerDoParallel(cluster)

# rf_result <- randomForest(x = predictors_train, y = train_set$rating, xtest = predictors_test, ytest = test_set$rating, do.trace = TRUE)
# print(rf_result)

# stopCluster(cluster)

print('RMSE using the random forest model: 0.9986')

# Linear Regression

# User Effect
user_avg <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat))

lr_predicted_u <- test_set %>%
  left_join(user_avg, by='userId') %>%
  mutate(predicted = mu_hat + b_u) %>%
  .$predicted

lr_u_rmse <- RMSE(lr_predicted_u, test_set$rating)

print(paste('By considering the user, we obtain an RMSE of ', lr_u_rmse))

# Movie Effect

movie_avg <- train_set %>%
  left_join(user_avg, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu_hat - b_u))

lr_predicted_m <- test_set %>%
  left_join(user_avg, by='userId') %>%
  left_join(movie_avg, by='movieId') %>%
  mutate(predicted = mu_hat + b_u + b_m) %>%
  .$predicted

lr_um_rmse <- RMSE(lr_predicted_m, test_set$rating)

print(paste('By considering both the user and the movie, we obtain an RMSE of ', lr_um_rmse))

# Genre Effect

genre_avg <- train_set %>%
  left_join(user_avg, by='userId') %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_u - b_m))

lr_predicted_g <- test_set %>%
  left_join(user_avg, by='userId') %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(genre_avg, by='genres') %>%
  mutate(predicted = mu_hat + b_u + b_m + b_g) %>%
  .$predicted

lr_umg_rmse <- RMSE(lr_predicted_g, test_set$rating)

print(paste('By considering the user, the movie and the genre, we obtain an RMSE of ', lr_umg_rmse))

# Date Effect

library(lubridate)
train_set$year <- year(as_datetime(train_set$timestamp))
test_set$year <- year(as_datetime(test_set$timestamp))

year_avg <- train_set %>%
  left_join(user_avg, by='userId') %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(genre_avg, by='genres') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu_hat - b_u - b_m - b_g))

lr_predicted_y <- test_set %>%
  left_join(user_avg, by='userId') %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(genre_avg, by='genres') %>%
  left_join(year_avg, by='year') %>%
  mutate(predicted = mu_hat + b_u + b_m + b_g + b_y) %>%
  .$predicted

lr_umgy_rmse <- RMSE(lr_predicted_y, test_set$rating)

print(paste('By considering the user, the movie, the genre, and the year, we obtain an RMSE of ', lr_umgy_rmse))

# Regularization

# Determining the optimal lambda
lambda <- seq(0, 10, 0.25)

sim_rmse <- sapply(lambda, function(l) {
  b_u <- train_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat) / (n() + l))
  
  b_m <- train_set %>%
    left_join(b_u, by='userId') %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat - b_u) / (n() + l))
  
  b_g <- train_set %>%
    left_join(b_u, by='userId') %>%
    left_join(b_m, by='movieId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu_hat - b_u - b_m) / (n() + l))
  
  b_y <- train_set %>%
    left_join(b_u, by='userId') %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_g, by='genres') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu_hat - b_u - b_m - b_g) / (n() + l))
  
  predicted <- train_set %>%
    left_join(b_u, by='userId') %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    mutate(predicted = mu_hat + b_u + b_m + b_g + b_y) %>%
    .$predicted
  
  return(RMSE(predicted, train_set$rating))
})

qplot(lambda, sim_rmse)

# Sticking to the lambda that yields the minimum RMSE:

lambda <- lambda[which.min((sim_rmse))]

# Using the optimal lambda to perform regularization
b_u <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_hat) / (n() + lambda))

b_m <- train_set %>%
  left_join(b_u, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu_hat - b_u) / (n() + lambda))

b_g <- train_set %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu_hat - b_u - b_m) / (n() + lambda))

b_y <- train_set %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  left_join(b_g, by='genres') %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu_hat - b_u - b_m - b_g) / (n() + lambda))

predicted <- test_set %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_y, by='year') %>%
  mutate(predicted = mu_hat + b_u + b_m + b_g + b_y) %>%
  .$predicted

lr_reg_rmse <- RMSE(predicted, test_set$rating)

print(paste('The RMSE after regularization is ', lr_reg_rmse))

# Matrix Factorization

# Calculating the residuals in both training and testing datasets
# Calculating the user effect again without regularization
b_u <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat))

# Calculating the movie effect again without regularization                 
b_m <- train_set %>%
  left_join(b_u, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu_hat - b_u))

# Calculating the residuals on the training set
train_set <- train_set %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  mutate(residual=rating - mu_hat - b_u - b_m)

# Calculating the residuals on the testing set
test_set <- test_set %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  mutate(residual=rating - mu_hat - b_u - b_m)

# Using the recosystem package to help with matrix factorization
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(recosystem)

# Providing training data
train_reco <- data_memory(user_index = train_set$userId, item_index = train_set$movieId, rating = train_set$residual, index1 = T)

# Providing test data
test_reco <- data_memory(user_index = test_set$userId, item_index = test_set$movieId, rating = test_set$residual, index1 = T)

# Creating the model
recommender <- Reco()

# The code below optimizes the parameters to minimize the RMSE. It is commented because it takes a long time to run.

# Setting the seed to always obtain the same result
set.seed(100)

# Tune the parameters
# reco_parameters <- recommender$tune(train_reco, opts = list(dim = c(10, 20, 30), 
#                                                            costp_11 = 0,
#                                                            costq11 = 0,
#                                                            lrate = c(0.05, 0.1, 0.2),
#                                                            nthread = 2))

# Using the optimal parameters that have been previously calculated
suppressWarnings(recommender$train(train_reco, opts = c(dim = 30,
                                                        costp11 = 0,
                                                        costp12 = 0.01,
                                                        costq11 = 0,
                                                        costq12 = 0.1,
                                                        lrate = 0.05,
                                                        verbose = FALSE)))

# Make predictions using the test data
reco_test_predictions <- recommender$predict(test_reco, out_memory()) + mu_hat + test_set$b_u + test_set$b_m

# If any prediction went beyond 5, set it to 5
index <- which(reco_test_predictions > 5)
reco_test_predictions[index] <- 5

# If any prediction is less than 0.5, set it to 0.5
index <- which(reco_test_predictions < 0.5)
reco_test_predictions[index] <- 0.5

# Calculate the RMSE
lr_matrix_rmse <- RMSE(reco_test_predictions, test_set$rating)

print(paste('The RMSE using matrix factorization is ', lr_matrix_rmse))

# Calculate the final RMSE on the validation (edx) dataset.

# Calculating the user effect again without regularization
b_u <- edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat))

# Calculating the movie effect again without regularization                 
b_m <- edx %>%
  left_join(b_u, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu_hat - b_u))

# Calculating the residuals on the edx set
edx <- edx %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  mutate(residual=rating - mu_hat - b_u - b_m)

# Calculating the residuals on the validation set
validation <- validation %>%
  left_join(b_u, by='userId') %>%
  left_join(b_m, by='movieId') %>%
  mutate(residual=rating - mu_hat - b_u - b_m)

# Providing training data
edx_reco <- data_memory(user_index = edx$userId, item_index = edx$movieId, rating = edx$residual, index1 = T)

# Providing test data
validation_reco <- data_memory(user_index = validation$userId, item_index = validation$movieId, rating = validation$residual, index1 = T)

# Creating the model
final_recommender <- Reco()

# Setting the seed to always obtain the same result
set.seed(100)

# Training the model using the edx set and the same parameters obtained on the tuning phase
suppressWarnings(final_recommender$train(edx_reco, opts = c(dim = 30,
                                                        costp11 = 0,
                                                        costp12 = 0.01,
                                                        costq11 = 0,
                                                        costq12 = 0.1,
                                                        lrate = 0.05,
                                                        verbose = FALSE)))

# Making predictions on the validation set
reco_final_predictions <- final_recommender$predict(validation_reco, out_memory()) + mu_hat + validation$b_u + validation$b_m

# If any prediction went beyond 5, set it to 5
index <- which(reco_final_predictions > 5)
reco_final_predictions[index] <- 5

# If any prediction is less than 0.5, set it to 0.5
index <- which(reco_final_predictions < 0.5)
reco_final_predictions[index] <- 0.5

# Calculate the RMSE
final_rmse <- RMSE(reco_final_predictions, validation$rating)

print(paste('The final RMSE, obtained on the validation set, was ', final_rmse))
