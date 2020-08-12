###########################
# Create dataset
###########################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("https://www.kaggle.com/pavanraj159/predicting-a-pulsar-star/download", dl)

star <- unzip(dl,)
dl

dl <- read.csv("C:/Users/stevefuckyou/Desktop/pulsar_stars.csv")


# Tidy up colmn names
colnames(dl) <-  c("MeanIP","SdIP","ExcKurtIP","SkewIP","MeanCurve","SdCurve","ExcKurtCurve","SkewCurve","tClass")

set.seed(19, sample.kind = "Rounding")

# SPlit data into Train and test set
test_index <- createDataPartition(y = dl$tClass, times = 1, p = 0.1, list = FALSE)
small_test_index <- createDataPartition(y = dl$tClass, times = 1, p = 0.9, list = FALSE)
train_set <- dl[-test_index,]
small_train_set <- dl[-small_test_index,]
test_set <- dl[test_index,]

# Due to size of the data I'm going to use a small segment to train my ensemble model
small_train_set$tClass <- as.factor(small_train_set$tClass)


head(dl)

### Analysis ###

# No. of columns
ncol(dl)

#No. of rows
nrow(dl)

#No. of pulsars
sum(dl$tClass)

# Summaries of Mean, Median, 1st & 3rd quartile and min and max
summary(dl)

# Summary of the false Pulsar signals
dl %>%
  filter(tClass == 0) %>%
  summary(.)


# Summary of the real Pulsar signals
dl %>%
  filter(tClass == 1) %>%
  summary(.)

# Check for NA's
sum(is.na(dl))

colnames(dl)

# Define measure
measure <- c("MeanIP","SdIP","ExcKurtIP","SkewIP","MeanCurve","SdCurve","ExcKurtCurve","SkewCurve")

dl %>%
  gather(Variable,results,all_of(measure)) %>%
  ggplot(aes(Variable,results, fill = factor(tClass))) +
  geom_boxplot() + 
  scale_y_continuous(trans = "log2") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Boxplot of one example
dl %>%
  gather(Variable,results,all_of(measure)) %>%
  filter(Variable == "MeanIP") %>%
  ggplot(aes(Variable,results, fill = factor(tClass))) +
  geom_boxplot()

# To enable colour we need to turn our Pulsars into a factor
dl$tClass <- as.factor(dl$tClass)

# Plotting all pairs against eacch other
my_cols <- c("#0716FC","#FC0707") 
lower_cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

upper_panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[dl$tClass])
}
pairs(dl[-9],
      lower.panel = lower_cor,
      upper.panel = upper_panel)


## Distributions ##

# Checking distribution of all values
grp1 <- c("MeanIP","SdIP")
grp2 <- c("SkewCurve","SdCurve")
grp3 <- c("ExcKurtCurve","SdIP")
grp4 <- c("MeanCurve","SkewIP")
grp5 <- c("ExcKurtIP")
dl %>% 
  gather(Variable,results,all_of(grp1)) %>%
  ggplot(aes(results, fill = factor(tClass))) +
  geom_density(alpha = 0.4)+
  facet_grid(.~ Variable,scales = "free")
dl %>% 
  gather(Variable,results,all_of(grp2)) %>%
  ggplot(aes(results, fill = factor(tClass))) +
  geom_density(alpha = 0.4)+
  facet_grid(.~ Variable,scales = "free")
dl %>% 
  gather(Variable,results,all_of(grp3)) %>%
  ggplot(aes(results, fill = factor(tClass))) +
  geom_density(alpha = 0.4)+
  facet_grid(.~ Variable,scales = "free")
dl %>% 
  gather(Variable,results,all_of(grp4)) %>%
  ggplot(aes(results, fill = factor(tClass))) +
  geom_density(alpha = 0.4)+
  facet_grid(.~ Variable,scales = "free")
dl %>% 
  gather(Variable,results,all_of(grp5)) %>%
  ggplot(aes(results, fill = factor(tClass))) +
  geom_density(alpha = 0.4)+
  facet_grid(.~ Variable,scales = "free")


# Geom point comparing Mean IP with SD IP by Class
dl %>%
  ggplot(aes(MeanIP,SdIP,color = factor(tClass))) +
  geom_point()

### Results ###

# Making Pulsar observations into factor
train_set$tClass <- as.factor(train_set$tClass)
test_set$tClass <- as.factor(test_set$tClass)

# Check these have been converted into factors
class(train_set$tClass)
class(test_set$tClass)

# Checking factor levels
levels(train_set$tClass)
levels(test_set$tClass)


# Classififcation by quartiles
Pulsar_quartiles <- dl %>% 
  filter(tClass == 1) %>%
  select(all_of(measure))

# Get quantiles for each measurement
Pul_qunts <- apply(Pulsar_quartiles, 2,quantile)


# Dividing into groups depending on which quartiles we're selecting. Group A 3rd quartile of Pulsars is max 1st quartile of non pulsars distribution is min and vice versa for Group B.
Grpa <- c("MeanIP","SkewCurve","ExcKurtCurve","SdIP")
Grpb <- c("SdCurve","MeanCurve","SkewIP","ExcKurtIP")

# 3rd quartils for Group A Pulsar distribution
Pul_max_a <- Pul_qunts[4,Grpa]

# 1st quartile for Group B Pulsar distribution
Pul_min_b <- Pul_qunts[2,Grpb]

# Predict Pulsar if observation between 1st - 3rd quartile for Group A observations and 2nd - 4th Group B
predict_Pulsar <- train_set %>% 
  mutate(a = ifelse(MeanIP<Pul_max_a,1,0),
         b = ifelse(SkewCurve<Pul_max_a,1,0),
         c = ifelse(ExcKurtCurve<Pul_max_a,1,0),
         d = ifelse(SdCurve>Pul_min_b,1,0),
         e = ifelse(MeanCurve>Pul_min_b,1,0),
         f = ifelse(SkewIP>Pul_min_b,1,0),
         g = ifelse(ExcKurtIP>Pul_min_b,1,0)) %>%
  select(tClass,a,b,c,d,e,f,g) %>%
  mutate(pred = ifelse(a+b+c+d+e+f+g>3.5,1,0))

# Calculate accuracy
mean(predict_Pulsar$pred == predict_Pulsar$tClass)

# Calculate confusion matrix
confusionMatrix(as.factor(predict_Pulsar$pred), as.factor(train_set$tClass), positive = "1")


##  First KNN model ## 

train_knn <- train(tClass ~ ., method = "knn", data = train_set)
knn_pred <- predict(train_knn, test_set)
mean(knn_pred == test_set$tClass)


# Get information about which parameters can be tuned
getModelInfo("knn")
modelLookup("knn")

# PLot default Neighbors values
ggplot(train_knn, highlight = TRUE)

# Set control to use cross validation. This will run the model 10 times with a different 90% of the data each time
control <- trainControl(method = "cv", number = 10, p = .9)
# We are starting with 9 nearest neighbours ad working our way up to 51
train_knn_cv <- train(tClass ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(9, 51, 2)),
                      trControl = control)
#Now we plot the results to see the most accurate method
ggplot(train_knn_cv, highlight = TRUE)

# We run the model with 11 nearest neighbours
train_knn_cv <- train(tClass ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = 11),
                      trControl = control)

# Run predictions on test set
train_knn_cv_pred <- predict(train_knn_cv,test_set)

#Calculate accuracy, sensitivity, specificty etc
confusionMatrix(train_knn_cv_pred,as.factor(test_set$tClass),positive = "1")


# Train Random Forest
Train_RF <- train(tClass ~ .,
                  method = "Rborist",
                  nTree = 5,
                  data = train_set)

# Apply model to test set
RF_Pred <- predict(Train_RF,test_set)

# Accuracy
mean(RF_Pred == test_set$tClass)

# Confusion matrix
confusionMatrix(RF_Pred,as.factor(test_set$tClass),positive = "1")

# Tuning parameters for random forest
getModelInfo("Rborist")
modelLookup("Rborist")

# Train Random Forest
Train_RF_tun <- train(tClass ~ .,
                  method = "Rborist",
                  nTree = 5,
                  tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                  data = train_set)

#Now we plot the results to see the most accurate method
ggplot(Train_RF_tun, highlight = TRUE)

# Train Random Forest
Train_RF_tun_top <- train(tClass ~ .,
                      method = "Rborist",
                      nTree = 5,
                      tuneGrid = data.frame(predFixed = 2, minNode = 50),
                      data = train_set)

# Apply model to test set
RF_Pred_tun <- predict(Train_RF_tun_top,test_set)

# Confusion matrix
confusionMatrix(RF_Pred_tun,as.factor(test_set$tClass),positive = "1")

# Ensemble model

# Define models
models <- c("svmLinear", "gamLoess", "qda", "knn","Rborist")

# Apply each model to the data
ensemble <- lapply(models,function(model){
  print(model)
  train(tClass ~ .,method = model, data = small_train_set)
})

# Generating a matrix of predictions for the test set data
ensemble_pred <- sapply(ensemble, function(object) 
  predict(object, test_set))
dim(ensemble_pred)

# Accuracy for each model in the ensemble
accuracy <- colMeans(ensemble_pred == test_set$tClass)
accuracy
mean(accuracy)

# building the ensemble prediction model based on majority decision
maj <- rowMeans(ensemble_pred == "1")
y_hat <- ifelse(maj > 0.5, "1", "0")
mean(y_hat == test_set$tClass)

# Confusion matrix on ensemble model
confusionMatrix(as.factor(y_hat),as.factor(test_set$tClass),positive = "1")


