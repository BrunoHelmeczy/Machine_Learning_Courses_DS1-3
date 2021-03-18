rm(list=ls())

library(tidyverse)
library(data.table)
library(caret)
library(gbm)
library(rpart.plot)
library(MLeval)
library(pROC)
library(caTools)
library(ggplot2)
library(viridis)
library(ggthemes)
library(dplyr)





# 1) TREE ENSEMBLE MODELS - 7points ----
df <- as_tibble(ISLR::OJ)
# Target var = Purchase


# 1 a) Create a training data of 75% and keep 25% of the data as a test set. 
#### Sample vs Holdout sets ####
set.seed(1)
train_indices <- as.integer(createDataPartition(df$Purchase, p = 0.7, list = FALSE))
data_train <- df[train_indices, ]
data_holdout <- df[-train_indices, ]

# train control is 5 fold cross validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE,
                              summaryFunction = twoClassSummary, 
                              classProbs = T,
                              savePredictions = T) 

# Train a decision tree as a benchmark model. 
        # Plot the final model and interpret the result 
            # -> (using rpart and rpart.plot is an easier option)


#### CART ####
set.seed(1234)
cart_model <- train(
  Purchase ~ .,
  data = data_train,
  method = "rpart",
  metric = "ROC",
  trControl = train_control,
  tuneGrid= expand.grid(cp = 0.0005))

# Tree Plot
rpart.plot(cart_model$finalModel)



# Evaluation
x <- evalm(cart_model)
# ROC Curve + AUC Values
x$roc
# Confusion Matrix
table(cart_model$pred[,c("obs")],cart_model$pred[,c("pred")])

# Pred Accuracy
mean(cart_model$pred[,c("obs")] == cart_model$pred[,c("pred")])


# 1 b) Investigate Tree Ensemble Models -----
      # RF, GBM, XGBoost -> Try different tuning parameters
      # Select best-model w cross-validation

rf_tune_grid <- expand.grid(
  .mtry = 2:10,
  .splitrule = "gini",
  .min.node.size = seq(5,45,by = 5) 
)


#### RAND FOREST ####
set.seed(1234)
rf_model_1b <- train(
  Purchase ~ .,
  data = data_train,
  method = "ranger",
  metric = "ROC",
  trControl = train_control,
  tuneGrid= rf_tune_grid,
  importance = "impurity"
  )

rf_model_1b$bestTune

#### GBM ####
gbm_grid <-  expand.grid(
  interaction.depth = c(1:5)*2-1, # complexity of the tree
  n.trees = 500, # number of iterations, i.e. trees
  shrinkage = c(1,5,10)*0.0001, # learning rate: how quickly the algorithm adapts
  n.minobsinnode = seq(5,25,by = 5) # the minimum number of training set samples in a node to commence splitting
)

set.seed(1234)
gbm_model_1b <- train(
  Purchase ~ .,
  data = data_train,
  method = "gbm",
  metric = "ROC",
  trControl = train_control,
  tuneGrid= gbm_grid)

x <- evalm(gbm_model_1b)
x$roc

 #### XGBoost ####
 xgb_grid <-  expand.grid(
   nrounds=c(500,750),
   max_depth = (2:4)*2+1,
   eta = c(0.03,0.05, 0.06),
   gamma = c(0.01),
   colsample_bytree = seq(75,95,by =10 )/100,  
   min_child_weight = (1:5)/10,
   subsample = c(0.75)
 )
 
set.seed(1234)
 xgb_model_1b <- train(
   Purchase ~ .,
   data = data_train,
   method = "xgbTree", # xgbDART / xgbLinear / xgbTree
   metric = "ROC",
   trControl = train_control,
   tuneGrid= xgb_grid
   )
 
#----------- 

models <- list(cart_model,rf_model_1b,gbm_model_1b,xgb_model_1b)


plots <- list()
findf <- x$stdres$`Group 1`[,1] %>% cbind() %>% as.data.frame()
for (model in 1:length(models)) {
  x <- evalm(models[[model]])
  plots[[model]] <- x$roc
  df <- x$stdres$`Group 1`[,1] %>% cbind() %>% as.data.frame()
  names <-  x$stdres$`Group 1` %>% rownames()
  rownames(df) <- names
  findf <- cbind(findf,df)

}
findf <- findf[,2:5]
colnames(findf) <- c("CART","RandForest","GBM","XGBoost")
findf
plots



# 1 c) Compare models' performance - CARET -> resamples() ----
      # set.seed() same 4 all 3 models
      # Either model has significantly different predictive power?

final_models <-
  list("CART" = cart_model,
       "Random_Forest" = rf_model_1b,
       "GBM" = gbm_model_1b,
       "XGBoost" = xgb_model_1b)

results <- resamples(final_models) %>% summary()

# 1 d) Choose best model & Plot ROC curve for best model based on test set. ----
      # Calculate & Interpret AUC

CV_AUC_folds <- list()
for (model_name in names(final_models)) {
  
  auc <- list()
  model <- final_models[[model_name]]
  fold <- "Fold1"
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <- model$pred %>% filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$CH)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

CV_AUC <- list()
for (model_name in names(final_models)) {
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

AUCs <- CV_AUC %>% rbind() 

## ROC Plot with built-in package 

rf_pred <- predict(rf_model_1b, data_holdout, type="prob")
colAUC(rf_pred, data_holdout$Purchase, plotROC = TRUE)

## ROC plot with own function
colAUC(rf_pred, data_holdout$Purchase, plotROC = TRUE)
data_holdout[,"best_model_pred"] <- rf_pred[,"CH"]

roc_obj_holdout <- roc(data_holdout$Purchase, data_holdout$best_model_pred)

createRocPlot <- function(r, plot_name) {
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  
  roc_plot <- ggplot(data = all_coords, aes(x = fpr, y = tpr)) +
    geom_line(color='blue', size = 0.7) +
    geom_area(aes(fill = 'red', alpha=0.4), alpha = 0.3, position = 'identity', color = 'blue') +
    scale_fill_viridis(discrete = TRUE, begin=0.6, alpha=0.5, guide = FALSE) +
    xlab("False Positive Rate (1-Specifity)") +
    ylab("True Positive Rate (Sensitivity)") +
    geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0.01)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0.01, 0)) + 
    theme_tufte() +
    labs(x = "False Positive Rate (1-Specifity)",
         y = "True Positive Rate (Sensitivity)",
         title = plot_name)
  
  roc_plot
}



FinModelROC <- createRocPlot(roc_obj_holdout, "ROC curve for best model (RF)")


# 1 e) Inspect variable importance plots for the 3 models ----
    # Similar variables found most important by all 3 models ? 


VarImpPlots <- function(named_model_list) {
  lapply(names(named_model_list), function(x) {
    VarImp_Plots <- list()
    df <- varImp(named_model_list[[x]])$importance
    df$names <- df %>% rownames()
    
    VarImp_Plots[[x]] <- df %>% 
      ggplot(aes(reorder(names,Overall), Overall) ) +
      geom_point(color = "blue") +
      geom_segment(aes(x=names,xend=names,y=0,yend=Overall), color = "blue") +
      theme_tufte() + 
      coord_flip() +
      labs(x = "Predictor Variables", y = "Relative Importance",
           title = paste0(x, " Model - Var. Importance Plot"))
    
    return(VarImp_Plots)
  })
  
}
VarImpPlots(final_models)


# 2) VARIABLE IMPORTANCE PROFILES - 6points ----

df <- as_tibble(ISLR::Hitters) %>% drop_na(Salary) %>% 
  mutate(log_salary = log(Salary), Salary = NULL)

# 2 a) Train 2 RFs - 1 sampling 2 vars randomly / split -> 1 sampling 10 ----
      # Use whole data & no cross-validation
      # Inspect variable importance profiles
    # What do you see - 
      # how important the first few variables are relative to each other?
train_control <- trainControl(method = "none", savePredictions = T)

rf_model_2a_2 <- train(
  log_salary ~ .,
  data = df, 
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(
    .mtry = 2)
  )

rf_model_2a_10 <- train(
  log_salary ~ .,
  data = df, 
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(
    .mtry = 10) 
)

RFs_2a <- list("Rand_Forest_2Vars" = rf_model_2a_2,
               "Rand_Forest_10Vars" = rf_model_2a_10)

VarImpPlots(RFs_2a)

# 2 b) Either model is more extreme fr 2a) -> Give Intuition how mtry relates


# 2 c) Same as 2a), estimate 2 GBMs - vary sampling of trees ----
        # 0.1 & 1 for bag.fraction / sample_rate parameter
          # Hold all other params fixed
            # grow 500 trees -> max depth = 5 -> learning rate = 0.1
      # Compare variable importance plots for 2 models
        # Intuition on why 1 var importance profile more extreme then the other?

h2o.init()

set.seed(1234)
gbm_h2o_2c_0.1 <- train(
  log_salary ~ .,
  data = df,
  method = "gbm_h2o",
  trControl = train_control,
  tuneGrid= expand.grid(
    ntrees = 500,
    max_depth = 5,
    min_rows = 5,
    learn_rate = 0.1,
    col_sample_rate = 0.1)
  )

set.seed(1234)
gbm_h2o_2c_1 <- train(
  log_salary ~ .,
  data = df,
  method = "gbm_h2o",
  trControl = train_control,
  tuneGrid= expand.grid(
    ntrees = 500,
    max_depth = 5,
    min_rows = 5,
    learn_rate = 0.1,
    col_sample_rate = 1)
)

GBMs_2c <- list("GBM_10%_Sampling" = gbm_h2o_2c_0.1,
               "GBM_100%_Sampling" = gbm_h2o_2c_1)

VarImpPlots(GBMs_2c)
h2o.shutdown()
Y
# 3) STACKING - 10 points ----data <- read_csv("KaggleV2-May-2016.csv") ----
myurl <- "https://raw.githubusercontent.com/BrunoHelmeczy/Machine_Learning_Courses_DS1-3/main/DS2/KaggleV2-May-2016.csv"
data <- read_csv(myurl)

# some data cleaning
data <- select(data, -one_of(c("PatientId", "AppointmentID", "Neighbourhood"))) %>%
  janitor::clean_names()

# for binary prediction, the target variable must be a factor + generate new variables
data <- mutate(
  data,
  no_show = factor(no_show, levels = c("Yes", "No")),
  handcap = ifelse(handcap > 0, 1, 0),
  across(c(gender, scholarship, hipertension, alcoholism, handcap), factor),
  hours_since_scheduled = as.numeric(appointment_day - scheduled_day)
)

# clean up a little bit
data <- filter(data, between(age, 0, 95), hours_since_scheduled >= 0) %>%
  select(-one_of(c("scheduled_day", "appointment_day", "sms_received")))

# data <- data %>% mutate(gender = factor(gender, levels = c("F","M"), labels = c(1,2)))
# data <- data[,c("no_show")] %>% cbind(sapply(data %>% select(-no_show), as.numeric)) %>% as.data.frame()
# data <- sapply(data, as.numeric) %>% as.data.frame()

# 3 a) Train / Validation / Test Sets - 5% / 45 / 50 -----

# CreateDataPartition
#### Sample vs Holdout sets ####
set.seed(1)
test_indices <- as.integer(createDataPartition(data$no_show, p = 0.5, list = FALSE))
data_test <- data[test_indices, ]
data_model <- data[-test_indices, ]


set.seed(1)
train_indices <- as.integer(createDataPartition(data_model$no_show, p = 0.1,list = FALSE))
data_train <- data_model[train_indices,]
data_validate <- data_model[-train_indices,]
data_model <- NULL

# train control is 5 fold cross validation ----
MyFolds <- createFolds(data_train$no_show,k = 5) 

train_control <- trainControl(index = MyFolds,
                              verboseIter = FALSE,
                              summaryFunction = twoClassSummary, 
                              classProbs = T,
                              savePredictions = "final") 

# 3 b) Train benchmark model of choice - rf / gbm / glm ----
  # Evaluate on validation set - lets do GLM - Elastic Net


# My E-Net Grid & Model ----
ENet_tunegrid <- expand.grid(
 "alpha"  = seq(0, 1, by = 0.1),
 "lambda" = 10^seq(2,-5,length=100)
)

Formula <- paste0("no_show ~ ", data %>% select(-no_show) %>% colnames() %>% paste(collapse = " + "))
 
Benchmark_GLM <- train(
  formula(Formula),
  data = data_train,
  method = "glmnet",
  family = "binomial",
  preProcess = c("center","scale"),
  trControl = train_control,
  tuneGrid = ENet_tunegrid,
  na.action=na.exclude
)

x <- evalm(Benchmark_GLM)

Preds <- Benchmark_GLM$pred[,"pred"][Benchmark_GLM$pred$alpha == Benchmark_GLM$bestTune[,"alpha"] & Benchmark_GLM$pred$lambda == Benchmark_GLM$bestTune[,"lambda"]]
Obss <- Benchmark_GLM$pred[,"obs"][Benchmark_GLM$pred$alpha == Benchmark_GLM$bestTune[,"alpha"] & Benchmark_GLM$pred$lambda == Benchmark_GLM$bestTune[,"lambda"]]

table(Preds,Obss)

Benchmark_GLM$pred[,"pred"][Benchmark_GLM$pred$lambda == Benchmark_GLM$bestTune[,"lambda"]]
Benchmark_GLM$bestTune[,"lambda"]

table(data_train$no_show)

# 3 c) Build min 3 models of different model families - w cross-validation ----
    # keep cross-validated predictions

#### 1) Random Forest ####
rf_tune_grid <- expand.grid(
  .mtry = 2:5,
  .splitrule = "gini",
  .min.node.size = seq(5,45,by = 5) 
)

RF_model_3c <- train(
  formula(Formula),
  data = data_train,
  method = "ranger",
  metric = "ROC",
  trControl = train_control,
  tuneGrid = rf_tune_grid,
  na.action=na.exclude
)

x <- RF_model_3c %>% evalm()
x$roc
x$stdres



#### 2) GBM ####
gbm_grid <-  expand.grid(
  interaction.depth = c(1:5)*2-1, # complexity of the tree
  n.trees = 500, # number of iterations, i.e. trees
  shrinkage = c(1,5,10)*0.0001, # learning rate: how quickly the algorithm adapts
  n.minobsinnode = seq(5,25,by = 5) # the minimum number of training set samples in a node to commence splitting
)

set.seed(1234)
GBM_model_3c <- train(
  formula(Formula),
  data = data_train,
  method = "gbm",
  metric = "ROC",
  trControl = train_control,
  tuneGrid= gbm_grid)

x <- evalm(GBM_model_3c)
x$roc


#### 3) XGBoost ####
xgb_grid <-  expand.grid(
  nrounds=c(500,750),
  max_depth = (2:4)*2+1,
  eta = c(0.03,0.05, 0.06),
  gamma = c(0.01),
  colsample_bytree = seq(75,95,by =10 )/100,  
  min_child_weight = (1:5)/10,
  subsample = c(0.75)
)

set.seed(1234)
XGB_model_3c <- train(
  formula(Formula),
  data = data_train,
  method = "xgbTree", # xgbDART / xgbLinear / xgbTree
  metric = "ROC",
  trControl = train_control,
  tuneGrid= xgb_grid
)


x <- XGB_model_3c %>% evalm()
x$roc
x$stdres



#### 4) KNN ####

set.seed(1234)
KNN_model_3c <- train(
  formula(Formula),
  data = data_train,
  method = "knn", 
  metric = "ROC",
  preProcess = c("center","scale"),
  trControl = train_control,
  tuneGrid= data.frame(k = c((1:21)*2-1))
)

#### 5) Radial SVM ####
set.seed(1234)
SVM_Radial_3c <- train(
  formula(Formula), 
  data = data_train, 
  method = "svmRadial",
  metric = "ROC",
  trControl=train_control,
  tuneLength = 10
)

# 3 d) Evaluate Validation set performance of each model ----
final_models_3c <- list(
  "Benchmark_Enet" = Benchmark_GLM,
  "Random_Forest"  = RF_model_3c,
  "Grad_Boost"     = GBM_model_3c,
  "XGBoost"        = XGB_model_3c,
  "KNN"            = KNN_model_3c,
  "SVM_Non_Lin"    = SVM_Radial_3c
)


results <- resamples(final_models_3c) %>% summary()

resamples(final_models_3c) %>% xyplot()

# Old E.g. -----
CV_AUC_folds <- list()

model_name <- names(final_models_3c)[1]
for (model_name in names(final_models_3c)) {
  
  auc <- list()
  model <- final_models_3c[[model_name]]
  fold <- "Fold1"
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <- model$pred %>% filter(Resample == fold)
    TuneParams <- model$bestTune %>% colnames()

    for (param in TuneParams) {
      cv_fold <- cv_fold[cv_fold[,param] == model$bestTune[,param],]
    }

    roc_obj <- roc(cv_fold$obs, cv_fold$No)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

CV_AUC <- list()
for (model_name in names(final_models_3c)) {
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

AUCs <- CV_AUC %>% cbind() 
colnames(AUCs) <- "Model_AUC"

#-----------
model <- names(final_models_3c)[1]
for (model in names(final_models_3c)) {
  Pred <- predict(final_models_3c[[model]],newdata = data_validate, type = "prob")
  data_validate[,paste0("Pred_",model)] <- Pred[,""]
  
  colAUC(Pred, data_validate$no_show, plotROC = TRUE)
}

rf_pred <- predict(rf_model_1b, data_holdout, type="prob")
colAUC(rf_pred, data_holdout$Purchase, plotROC = TRUE)

roc(data_validate[,paste0("Pred_",model)], data_validate$no_show)

## ROC plot with own function
colAUC(rf_pred, data_holdout$Purchase, plotROC = TRUE)
data_holdout[,"best_model_pred"] <- rf_pred[,"CH"]

roc_obj_holdout <- roc(data_holdout$Purchase, data_holdout$best_model_pred)

createRocPlot <- function(r, plot_name) {
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  
  roc_plot <- ggplot(data = all_coords, aes(x = fpr, y = tpr)) +
    geom_line(color='blue', size = 0.7) +
    geom_area(aes(fill = 'red', alpha=0.4), alpha = 0.3, position = 'identity', color = 'blue') +
    scale_fill_viridis(discrete = TRUE, begin=0.6, alpha=0.5, guide = FALSE) +
    xlab("False Positive Rate (1-Specifity)") +
    ylab("True Positive Rate (Sensitivity)") +
    geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0.01)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0.01, 0)) + 
    theme_tufte() +
    labs(x = "False Positive Rate (1-Specifity)",
         y = "True Positive Rate (Sensitivity)",
         title = plot_name)
  
  roc_plot
}



FinModelROC <- createRocPlot(roc_obj_holdout, "ROC curve for best model (RF)")


# 3 e) How large are correlations of predicted scores on the validation set ----
    # produced by the base learners



# 3 f) Create Stacked ensemble model from the base learners. ----

#install.packages("caretEnsemble")
#library(caretEnsemble)



glmEnsemble <- caretStack(
  as.caretList(final_models_3c),
  method = "glm",
  metric = "ROC",
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final",
    classProbs = T,
    summaryFunction = twoClassSummary
  )
)


RF_Ensemble <- caretStack(
  as.caretList(final_models_3c),
  method = "ranger",
  metric = "ROC",
  tuneGrid = expand.grid(
    .mtry = 2:6,
    .splitrule = "gini",
    .min.node.size = seq(5,45,by = 5)), 
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final",
    classProbs = T,
    summaryFunction = twoClassSummary
  )
)
RF_Ensemble$ens_model

final_models_3c[['Lin_Ensemble']] <- NULL

glmEnsemble %>% summary()
RF_Ensemble %>% summary()
RF_Ensemble$ens_model$pred
glmEnsemble %>% class()

resamples(final_models_3c)

evalm(glmEnsemble)

# 3 g) Evaluate ensembles on validation set - Did prediction improve ?


# 3 h) Evaluate best performing model on test set. 
    # How does performance compare to validation set?
      