#load libraries
library(tidyverse)

#load data files
train_x <- data.frame(read_csv("ks_training_X.csv"))
train_y <- data.frame(read_csv("ks_training_y.csv"))
test_x <- data.frame(read_csv("ks_test_X.csv"))
baby_names <- data.frame(read_csv("baby_names_.csv"))
unemployment <- data.frame(read_csv("unemployment.csv"))
sp500 <- data.frame(read_csv("dow10y_new.csv"))

#Combine train and test data for cleaning, feature engineering
train_test_x <- rbind(train_x, test_x)

#Exploratory visualizations
box(train_test_x$goal, main='Goal distribution',
     ylab = 'Frequency', xlab='goal' )

barplot(table(train_test_x$category_name))
#--------------------------------------------------------------------------------------------------------
#FEATURE ENGINEERING

#cleaning the reward_amounts
lists <- train_test_x$reward_amounts
#Replacing nulls and gibberish with 0
lists <- ifelse(is.na(lists), 0, lists)
lists <- ifelse(lists =="B,A,D", 0, lists)
lists <- ifelse(lists=="NA",0,lists)
#Creating summary stats for the list of items
train_test_x$reward_sum <- sapply(strsplit(lists,','), function(x) sum(type.convert(x)))
train_test_x$reward_min <- sapply(strsplit(lists,','), function(x) min(type.convert(x)))
train_test_x$reward_max <- sapply(strsplit(lists,','), function(x) max(type.convert(x)))
train_test_x$reward_count <- sapply(strsplit(lists,','), function(x) length(type.convert(x)))
#Extracting top most tag from the tags column
train_test_x$Top_Tag <- gsub("([A-Za-z]+).*", "\\1", train_test_x$tag_names)

#Separating first name and last name, while dropping the last name
train_test_x <- train_test_x %>%
  separate(creator_name, into = ("name"), extra = "drop") %>%
  mutate(name = str_to_title(name))

#Joining external baby_names dataset, assigning all nulls as none. 
 train_test_x <- train_test_x %>%
   left_join(baby_names, by = "name") %>%
  mutate(creator_gender = ifelse(is.na(percent_female), "None", percent_female),
         creator_gender = ifelse(creator_gender>0.5, "Female", "Male"),
         creator_gender = as.factor(creator_gender))

filtered_data <- train_test_x %>%
  select(-c(creator_id, name, color_foreground, color_background, isbwImg1,
            accent_color, captions, reward_amounts, reward_descriptions, tag_names, smiling_creator,
            minage_creator, maxage_project, percent_female)) %>% 
  mutate(location_type = ifelse(location_type == "Estate", "Miscellaneous", location_type)) %>%
  group_by(category_name) %>%
  mutate(category_name_freq = n()) %>%
  ungroup() %>%
  mutate(category_name = ifelse(category_name_freq < 500, 'Other', category_name),
         category_name = as.factor(category_name),
         #Creating date difference between launched_at and deadline
         date_diff = as.Date(deadline, format = '%m/%d/%Y') -
                     as.Date(launched_at, format = '%m/%d/%Y'),
         date_diff = as.double(str_replace(date_diff,"days", "")),
         #Creating bins based on the more popular periods 
         date_diff_bin = case_when(
           date_diff <= 15 ~ "<15 days",
           date_diff > 15 & date_diff <= 30 ~ "15-30 days",
           date_diff > 30 & date_diff <= 45 ~ "30-45 days",
           date_diff > 45 & date_diff <= 60 ~ "45-60 days",
           date_diff >60 ~ ">60 days",
           TRUE ~  "other"),
         #Creating bins based on quartile splits
         goalsize = case_when(
           goal < 2000 ~ "Q1",
           goal <= 2000 & goal >5000 ~ "Q2", 
           goal <=5000  & goal > 12500 ~ "Q3", 
           TRUE~ "Q4"),
         #Handling nulls by assinging a value
         isTextPic = as.factor(ifelse(is.na(isTextPic), "None", isTextPic)),
         isLogoPic = as.factor(ifelse(is.na(isLogoPic), "None", isLogoPic)),
         isCalendarPic = as.factor(ifelse(is.na(isCalendarPic), "None", isCalendarPic)),
         isDiagramPic = as.factor(ifelse(is.na(isDiagramPic), "None", isDiagramPic)),
         isShapePic = as.factor(ifelse(is.na(isShapePic), "None", isShapePic)),
         #Extracting month and year from the three date fields
         launch_month = as.factor(format(as.Date(launched_at, format = '%m/%d/%Y'), "%m")),
         launch_year = as.factor(format(as.Date(launched_at, format = '%m/%d/%Y'), "%Y")),
         deadline_month = as.factor(format(as.Date(deadline, format = '%m/%d/%Y'), "%m")),
         deadline_year = as.factor(format(as.Date(deadline, format = '%m/%d/%Y'), "%Y")),
         created_month = as.double(format(as.Date(created_at, format = '%m/%d/%Y'), "%m")),
         created_year = as.double(format(as.Date(created_at, format = '%m/%d/%Y'), "%Y")),
         #factoring data_diff_bin
         date_diff_bin = as.factor(date_diff_bin),
         #Extracting states from location_slug
         state = sub("^.*-\\s*", "", location_slug),
         #setting state as a factor
         state = as.factor(toupper(state)),
         #Extracting cities from location_slug
         city_name = substr(location_slug, 1, nchar(location_slug)-3),
         #Extracting the first tag from the column called tags
         Top_Tag = ifelse(is.na(Top_Tag), "Miscellaneous", Top_Tag)) %>%
  #binning those levels that fall below the frequency of 130
  group_by(Top_Tag) %>%
  mutate(top_tag_freq = n()) %>%
  ungroup() %>%
  mutate(Top_Tag = ifelse(top_tag_freq < 130, 'Miscellaneous', Top_Tag),
         location_slug = NULL,
         deadline = NULL,
         launched_at = NULL,
         category_name_freq = NULL,
         top_tag_freq = NULL) %>%
  #binning those levels falling below a frequency of 300
  group_by(city_name) %>%
  mutate(city_freq = n()) %>%
  ungroup() %>%
  mutate(city_name = as.factor(ifelse(city_freq < 300, 'Other', city_name)),
         city_freq = NULL)

#joining the dow external data 
filtered_data <- filtered_data %>%
  left_join(sp500, by = c("created_month", "created_year"))

#Joing the unemployment external data
filtered_data <- filtered_data %>%
  left_join(unemployment, by = c("created_month", "created_year")) %>%
  mutate(
         created_month = as.factor(created_month),
         created_year = as.factor(created_year),
         created_at = NULL)

#----------------------------------------------------------------------------------------------------
#BAG OF WORDS
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)

prep_func = tolower
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}
tok_func = cleaning_tokenizer
# Iterate over the individual documents and convert them to tokens
# Uses the functions defined above.
it_train = itoken(filtered_data$blurb, 
                  preprocessor = prep_func, 
                  tokenizer = tok_func, 
                  ids = filtered_data$id, 
                  progressbar = FALSE)
# Create the vocabulary from the itoken object
vocab = create_vocabulary(it_train, ngram = c(1L, 2L))
#vocab = prune_vocabulary(vocab)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)
#N<-10
#m <- as.matrix(dtm_train)
#v <- sort(colSums(m), decreasing=TRUE)
#head(v, N)
dtm_train_bin <- dtm_train>0+0
dtm_train_small <- dtm_train_bin[,c('help', 'new', 'film', 'will', 'music')]
                                    #'make', 'art', 'world', 'stori', 'game )]
#dtm_train_small <- dtm_train_bin[,1:5]
# Convert the small sparse matrix into a dense one
dense = as.matrix(dtm_train_small)+0
filtered_data <- cbind(filtered_data, dense)

#Remove blurb once the frequent words have been removed 
filtered_data <- filtered_data %>%
  select(-c(blurb))

#------------------------------------------------------------------------------------------------------
#Splitting into Training and Testing, Before Dummification
train <- split(filtered_data,cumsum(1:nrow(filtered_data)%in%97421))$`0`
test <- split(filtered_data,cumsum(1:nrow(filtered_data)%in%97421))$`1`
#Clears the memory, since vectors take up a lot of space
rm(train_test_x)
rm(train_x)
rm(test_x)
#join the training y to the training x file
#also turn two of the target variables into factors
processed_train <- train %>%
  left_join(train_y, by = "id") %>%
  mutate(success = as.factor(success),
         big_hit = NULL,
         backers_count = NULL)

#-----------------------------------------------------------------------------------------------------
#DUMMIFICATION
library(dplyr)
library(caret)

#Getting the dummies for the training set
dummy_x <- dummyVars(~ . + goal:category_name, data = filtered_data)
#one hot encoding
one_hot_lc_x <- data.frame(predict(dummy_x, newdata = filtered_data))

#Removing target variables we are not working on
train_y <- train_y %>%
  mutate(big_hit = NULL,
         backers_count = NULL)

#Getting the dummies for the training set
dummy_y <- dummyVars(~ . , data = train_y)
one_hot_lc_y <- data.frame(predict(dummy_y, newdata = train_y))

#Dropping successNO and factoring successYES
one_hot_lc_y <- one_hot_lc_y %>%
  mutate(successNO = NULL,
         successYES = as.factor(successYES))

#------------------------------------------------------------------------------------------------------
#Splitting into Training and Testing, After Dummification
train_dummy <- split(one_hot_lc_x,cumsum(1:nrow(one_hot_lc_x)%in%97421))$`0`
test_dummy <- split(one_hot_lc_x,cumsum(1:nrow(one_hot_lc_x)%in%97421))$`1`

#Clears the memory, since vectors take up a lot of space
rm(train_test_x)
rm(filtered_data)

#join the training y to the training x file
#also turn two of the target variables into factors
 one_hot_lc <- train_dummy %>%
   left_join(one_hot_lc_y, by = "id")
 
 #Removing id after the dummification process is done for merged, test and train
 one_hot_lc <- one_hot_lc %>%
   select(-c(id))

 test_dummy <- test_dummy %>%
   select(-c(id))
 
 train_dummy <- train_dummy %>%
   select(-c(id))
 
 #Removing id after the dummification process is done
 test <- test %>%
   mutate(-c(id))
 
 train <- train %>%
   mutate(-c(id))

#------------------------------------------------------------------------------------------------------------------
rm(train_y)
set.seed(1)
#sample(max, n) will randomly select n instances from a list spanning from 1 to max
train_insts = sample(nrow(processed_train), .7*nrow(processed_train))

#separate labeled data into training and validation 
data_train <- processed_train[train_insts,]
data_valid <- processed_train[-train_insts,]

#-----------------------------------------------------------------------------------------------------------------
#LOGISTIC REGRESSION MODEL

#Logistic Model 
logistic_model <- glm(success~.,data = data_train, family = "binomial")
summary(logistic_model)
#Get predictions and accuracies
logistic_preds <- predict(logistic_model, newdata = data_valid, type = "response") #we need response so we dont get logit
logistic_classification <- ifelse(logistic_preds > .5, "YES" , "NO") #0.5 is the cut off
correct_logistic_classifications <- ifelse(logistic_classification == data_valid$success, 1, 0)#counts the number of correct classification
logistic_acc = sum(correct_logistic_classifications)/length(correct_logistic_classifications)
logistic_acc

#--------------------------------------------------------------------------------------------------------------------------
#BOOSTING

library(xgboost)
#Data for Train and valid sets, finding accuracy of model
data_boost <- one_hot_lc %>%
  select(-c(successYES))
#Set seed to get same splits every run
set.seed(1)
#Splitting dummified Train into train and valid sets, converting to matrix 
data_boost_train <- as.matrix(data_boost[train_insts,])
data_boost_valid <- as.matrix(data_boost[-train_insts,])
#Assigning target variables 
y<-data.frame(one_hot_lc$successYES)
y_train <- y[train_insts,]
y_valid <- y[-train_insts,]

#Boosting model
bst <- xgboost(data = data_boost_train, label = as.numeric(as.character(y_train)), max.depth = 2, eta = 1, nrounds = 1000,  objective = "binary:logistic")
#Prediction and Evaluation 
bst_pred <- predict(bst, data_boost_valid)
bst_classifications <- ifelse(bst_pred > 0.5, 1, 0)
bst_acc <- mean(ifelse(bst_classifications == y_valid, 1, 0))
bst_acc

#-----------------------------------------------FITTING CURVES FOR XGBOOST Start-------------------------------------------

accuracy <- function(classifications, actuals){
  bst_acc <- mean(ifelse(classifications == actuals, 1, 0))
  return(bst_acc)
}
#Function to that returns accuracy of model
predict_and_eval <- function(treename, pred_data, y){
  bst_pred_valid <- predict(treename, pred_data)
  classifications <- ifelse(bst_pred_valid > 0.5, 1, 0)
  acc <- accuracy(classifications, y) #compute the accuracy
  return(acc)
}
#list of split sizes
sizes <- c(2,  4,  6,  8, 10, 15, 20, 25, 30, 35, 40)
k <- length(sizes)

#to store training and validation accuracies for the different trees
va_acc_storage <- rep(0, length(sizes)) 
tr_acc_storage <- rep(0, length(sizes))
#loop to calculate the accuracies for each tree size
for(i in 1:k){
  #running the tree model for different splits 
  pruned_tree = xgboost(data = data_boost_train, label = as.numeric(as.character(y_train)), max.depth = sizes[i], eta = 1, 
                        nrounds = 50, objective = "binary:logistic")
  
  #Calling the prediction and accuracy calculating functions
  va_acc <- predict_and_eval(pruned_tree, data_boost_valid, y_valid) #predict and get accuracy
  tr_acc <- predict_and_eval(pruned_tree, data_boost_train, y_train) #repeat in training data

  #store the two accuracies
  va_acc_storage[i] <- va_acc
  tr_acc_storage[i] <- tr_acc
}

#plotting the two lines - training and testing
plot(sizes, va_acc_storage, type = 'l', col = 'red', ylim = c(0.700, 1.00),
     main = "Tree depth vs accuracy", xlab = "Depth", ylab = "Accuracies",)
lines(sizes, tr_acc_storage, col = 'blue')


#-----------------------------------------------FITTING CURVES FOR XGBOOST End-------------------------------------------
#XGBOOST TEST PREDICTIONS

#The entire dummy Training and Testing data in the form of a matrix 
data_boost_train_matrix <- as.matrix(train_dummy)
data_boost_test_matrix <- as.matrix(test_dummy)
y <- as.matrix(one_hot_lc_y$successYES)
#Running the tree
bst <- xgboost(data = data_boost_train_matrix, label = as.numeric(as.character(y)), max.depth = 2, eta = 1, nrounds = 1000,  objective = "binary:logistic")
#Getting Predictions and Accuracies
bst_pred <- predict(bst, data_boost_test_matrix)
bst_classifications <- ifelse(bst_pred > .5, "YES", "NO")

#--------------------------------------------------------------------------------------------------
#RANDOM FOREST MODEL
library(ranger)
#Training the ranger model 
rf.mod <- ranger(success~.+goalsize:city_name,
                       data=data_train,
                       mtry=15, ntree=1000,
                       importance="impurity",
                       probability = TRUE)
#Getting the predictions and accuracies 
rf_preds <- predict(rf.mod, newdata=data_valid)
rf_acc <- mean(ifelse(rf_preds==data_valid$success,1,0))
rf_acc

#--------------------------------------------------------------------------------------------------
#CROSS VALIDATION FOR THE RANGER MODEL

#remove blurb from data_train
data_train <- data_train %>%
  select(-c(blurb))
#create random forest with ranger function
range.mod <- ranger(success~ .,
                    data = data_train,
                    mtry=20,
                    num.trees=1000,
                    importance="impurity",
                    probability = TRUE)
#remove blurb from data_valid
data_valid <- data_valid %>%
  select(-c(blurb))
#make predictions for validation data
range_preds <- predict(range.mod, data=data_valid)$predictions[,2]
range_classifications <- ifelse(range_preds>0.5, "YES", "NO")
#check accuracy of ranger model 
range_acc <- mean(ifelse(range_classifications == data_valid$success, 1, 0))
range_acc
vip(range.mod)

ranger_formula <- success~ .
# Define a function that trains and predicts probabilities and classifies based on a cutoff c
tr_pred <- function(train_data, valid_data, model_formula){
  trained_model <- ranger(data = train_data, model_formula, mtry=20,
                          num.trees=1000,
                          importance="impurity",
                          probability = TRUE) 
  predictions <- predict(trained_model, data = valid_data, type = "response") 
  return(predictions)
}

rpred<-tr_pred(data_train, data_valid, ranger_formula)
# Define a function that uses scores to classify based on a cutoff c
classify <- function(scores, c){
  classifications <- ifelse(scores > c, "YES" , "NO") 
  return(classifications) 
}
classify(rpred$predictions[,2],0.5)
#CROSS VALIDATION FOR RANGER
#randomly shuffle the training data
labeled_shuffle <- data_train[sample(nrow(data_train)),]
#define k = the number of folds
k <- 10

#separate data into k equally-sized folds
#cut() will assign "fold numbers" to each instance
folds <- cut(seq(1,nrow(labeled_shuffle)),breaks=k,labels=FALSE)

# Make a vectors of zeros to store your performance in each fold using the rep function
range_acc = rep(0, k)
range_sensitivity = rep(0, k)
range_specificity = rep(0, k)

# To start with, I recombined some code from above to make a new function
# Given a list containing the elements of a confusion matrix, compute the accuracy, sensitivity, specificity and return as another list.

compute_metrics <- function(confusion_matrix){
  
  #get the TP, TN, FP, FN
  TP <- confusion_matrix[1]
  TN <- confusion_matrix[2]
  FP <- confusion_matrix[3]
  FN <- confusion_matrix[4]
  
  #compute Accuracy, TPR, TNR
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  TPR <- TP/(TP+FN)
  TNR <- TN/(TN+FP)
  
  #return a list of metrics
  return(c(accuracy, TPR, TNR))
}

#Define a function to make a confusion matrix
confusion_maker <- function(actuals, classifications){
  
  CM <- table(actuals, classifications)
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  
  return(c(TP, TN, FP, FN))
  
}

# Here's the actual cross-validation code
# Use a for loop to repeat k times
for(i in 1:k){
  #Segment your data by fold using the which() function 
  valid_inds <- which(folds==i,arr.ind=TRUE)
  valid_fold <- labeled_shuffle[valid_inds, ]
  train_fold <- labeled_shuffle[-valid_inds, ]
  
  # save the actual labels for the validation fold
  fold_actuals <- valid_fold$success
  
  #train, predict, classify as usual
  preds1 <- tr_pred(train_fold, valid_fold, ranger_formula)$predictions[,2]
  class1 <- classify(preds1, .5)
  
  #make a confusion matrix and retrieve the metrics
  CM1 <- confusion_maker(fold_actuals, class1)
  metrics1 <- compute_metrics(CM1)
  
  #compute_metrics() will return a list where accuracy is the first element, TPR, is the second, TNR is the third. Save these in the storage vectors defined above.
  range_acc[i] <- metrics1[1]
  range_sensitivity[i] <- metrics1[2]
  range_specificity[i] <- metrics1[3]
}

# Compute the means across folds for each vector
CV_accuracy_model1 <- mean(range_acc)
CV_sensitivity_model1 <- mean(range_sensitivity)
CV_specificity_model1 <- mean(range_specificity)

#--------------------------------------------------------------------------------------------------
#LASSO AND RIDGE
library(glmnet)

#Changing to Matrix form
data_lr_x <- model.matrix(success~.,processed_train)
#Preference is a factor
data_lr_y <- processed_train$success
#Set seed to get the same split every run
set.seed(1)
#getting the instances for splitting
index <- sample(nrow(processed_train),.7*nrow(processed_train))
#predictor variables
x_train <- data_lr_x[index,]
x_valid <- data_lr_x[-index,]
#Target variables 
y_train <- data_lr_y[index]
y_valid <- data_lr_y[-index]

#values for lambda
grid <- 10^seq(7,-7,length=100)
#Fold for cross validation
k<-5

#Training the models ridge and lasso 
cv.glm.out.lasso <- cv.glmnet(x_train, y_train, family="binomial", alpha=1, lambda=grid, nfolds=k)
cv.glm.out.ridge <- cv.glmnet(x_train, y_train, family="binomial", alpha=0, lambda=grid, nfolds=k)

#get the lambda that gave the lowest cross-validated error
bestlam_lasso <- cv.glm.out.lasso$lambda.min
bestlam_ridge <- cv.glm.out.ridge$lambda.min

#Predictions for Lasso
pred_lasso <- predict(cv.glm.out.lasso, s=bestlam_lasso, newx = x_valid,type="response")
classifications_lasso <- ifelse(pred > .5, "YES" , "NO") #0.5 is the cut off
#Predictions for Ridge
pred_ridge <- predict(cv.glm.out.ridge, s=bestlam_ridge, newx = x_valid,type="response")
classifications_ridge <- ifelse(pred > .5, "YES" , "NO")
#Acuuracy for Lasso
correct_classifications_lasso <- ifelse(classifications1 == y_valid, 1, 0)#counts correct classifications
accuracy_lasso = sum(correct_classifications_lasso)/length(correct_classifications_lasso)
accuracy_lasso
#Accuracy for Ridge
correct_classifications_ridge <- ifelse(classifications1 == y_valid, 1, 0)#counts correct classifications
accuracy_ridge = sum(correct_classifications_ridge)/length(correct_classifications_ridge)
accuracy_ridge
#-------------------------------------------------------------------------------------------------------------------------------------
#NEURAL NETWORK

# Neural Networks
#install.packages('neuralnet')
#install.packages('mlbench')
#install.packages('keras')
#install.packages('magrittr')

library(keras)
#install_keras(tensorflow = "gpu")
library(mlbench)
library(magrittr)
library(caret)

DF <- data_train
DF <- DF %>%
  select(-c(id))
DF <- DF %>%
  mutate(success = ifelse((success == 'YES'),1,0))
# Convert all characters to factors
DF <- as.data.frame(unclass(DF),stringsAsFactors=TRUE)
#split target and data
DF_Y <- as.matrix(subset(DF,select = success))
DF <- subset(DF,select = -success)
# Create Dummies using one hot encoding
dummy <- dummyVars( ~ . , data=DF)
one_hot_DF <- data.frame(predict(dummy, newdata = DF))
one_hot_DF <- one_hot_DF %>%
  select(-c(location_type.Miscellaneous))

#scale using min max normalization
max = apply(one_hot_DF, 2 , max)
min = apply(one_hot_DF, 2 , min)
one_hot_DF = as.data.frame(scale(one_hot_DF, center = min, scale = max - min))
one_hot_DF <- as.matrix.data.frame(one_hot_DF)

#split training data and target


# Repeat all above for validation data
DF_valid <- data_valid
DF_valid <- DF_valid %>%
  select(-c(id))
DF_valid <- DF_valid %>%
  mutate(success = ifelse((success == 'YES'),1,0))
# Convert all characters to factors
DF_valid <- as.data.frame(unclass(DF_valid),stringsAsFactors=TRUE)
#split target and data
DF_valid_Y <- as.matrix(subset(DF_valid,select = success))
DF_valid <- subset(DF_valid,select = -success)
# Create Dummies using one hot encoding
dummy_valid <- dummyVars( ~ . , data=DF_valid)
one_hot_DF_valid <- data.frame(predict(dummy_valid, newdata = DF_valid))

#scale using min max normalization
max = apply(one_hot_DF_valid, 2 , max)
min = apply(one_hot_DF_valid, 2 , min)
one_hot_DF_valid = as.data.frame(scale(one_hot_DF_valid, center = min, scale = max - min))
one_hot_DF_valid <- as.matrix.data.frame(one_hot_DF_valid)


# Defining the MLP Model

model <- keras_model_sequential()
model %>%
  layer_dense(units = 2000, activation = 'leaky_relu', input_shape = c(352)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_dense(units = 500, activation = 'leaky_relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 100, activation = 'leaky_relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid')

#compile the Model

model %>% compile(loss='binary_crossentropy',
                  optimizer = optimizer_adam(learning_rate = 0.0005),
                  metrics = 'accuracy')

#Fitting our model

mymodel <- model %>%
  fit(one_hot_DF,
      DF_Y,
      epochs = 30,
      batch_size = 1000,
      validation_split = 0.1)

#evaluating the accuracy and loss
model %>%
  evaluate(one_hot_DF_valid,DF_valid_Y)

# Predictions and Truth Table
pred <- model %>% predict(one_hot_DF_valid)
pred <- ifelse(pred > 0.5, 1, 0)
tab1 <- table(pred, DF_valid_Y)
tab1

accuracy_nn <- (tab1['0','0']+tab1['1','1'])/(tab1['0','0']+tab1['1','1']+tab1['0','1']+tab1['1','0'])
accuracy_nn
#--------------------------------------------------------------------------------------------------------------------------

#outputting the test predictions
write.table(bst_classifications, "success_group9.csv", row.names = FALSE)
