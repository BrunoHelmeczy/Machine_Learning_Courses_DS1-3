library(tidyverse)
library(datasets)
library(MASS)
library(ISLR)
library(caret)
library(data.table)
library(ggthemes)
library(ggridges)

rm(list=ls())
options(scipen=999)

df <- readRDS(url('http://www.jaredlander.com/data/manhattan_Train.rds')) %>%
  mutate(logTotalValue = log(TotalValue)) %>%
  drop_na() %>% dplyr::select(-TotalValue)

### 1) Supervised Learning with Penalized Models & PCA ----
    # Property Values in Manhattan - R 4 Everyone
    #   Predict log-Property Value = logTotalValue

# Function to check distinct values & class of all columns - anytime
ColsUniques <- function(dataframe = df) {
  rbindlist(lapply(1:length(dataframe), function(x) {
    tl <- list()
    tl[['name']] <- colnames(dataframe)[x]
    tl[['distinct']] <- nrow(unique(dataframe[,x]))
    tl[['class']] <- dataframe[,x][[1]] %>% class()
    return(tl)
  }))  
}
ColsUni1 <- ColsUniques()

# Dummies : Area measures -> has, or not 
# logicals / binaries
ColsUni1$name[ColsUni1$distinct == 2]


    # Zone Distr 2-4 -> 90+ %  Missing value
df$ZoneDist4 <- NULL
df$ZoneDist3 <- NULL
df$ZoneDist2 <- NULL

# Binary Factors -> Yes = 1 No = 0
df <- df %>% mutate(
  IrregularLot     = ifelse(IrregularLot == "Yes",1,0),
  Landmark         = ifelse(Landmark == "Yes",1,0),
  HistoricDistrict = ifelse(HistoricDistrict == "Yes" ,1,0),
  High             = ifelse(High == T,1,0)
) %>% mutate(
  Council    = as.factor(Council),
  PolicePrct = as.factor(PolicePrct),
  HealthArea = as.factor(HealthArea)
)

ColsUni2 <- ColsUniques()

# a) Short EDA on data -> Find possible predictors ----

# 2 functions to check all histograms / boxplots / LOESS-es in dataset
Hists <- function() {
  ColsUni2 <- ColsUniques()
  
  lapply(df %>% dplyr::select(-matches("id")) %>% colnames(),function(x) {
    if( ( ColsUni2$class[ColsUni2$name == x] %in% c("integer","numeric") ) & (df[,x] %>% unique() %>% nrow() >2 )  ) {
    plot <- df %>% ggplot(aes_string(x = x)) + 
        geom_histogram( color = "red", fill = "blue") + theme_tufte() + 
        labs(title = paste0("NYC Properties ",x," Distribution")) 
    print(plot)
    } else {
      plot <- df %>% ggplot(aes_string(x = x ) ) + 
        geom_bar(color = "red", fill = "blue") + coord_flip() + theme_tufte() + 
        labs(title = paste0("NYC Properties ",x," Distribution"))  
      print(plot)
    }
  })
  
}
# Hists()

Boxes_n_Scatters <- function() {
  ColsUni1 <- ColsUniques()
  
# Box Plots
#  x <- "SchoolDistrict"
  lapply(df %>% dplyr::select(-matches("id|logTotalValue")) %>%  colnames(), function(x) {
      if (  (ColsUni1$class[ColsUni1$name == x] %in% c("factor","logical") ) | (df[,x] %>% unique() %>% nrow() == 2)  ) {
    plot <- df %>% ggplot()  +
          geom_boxplot(aes_string(y = x, x = "logTotalValue", group = x),color = "red", fill = "blue") +
          theme_tufte() +  
          labs(title = paste0("Title ",x))
    print(plot)
    print(paste0("Printing plot: ",x))
      } 
    })
  
# Scatters 
  xvars <- df %>% dplyr::select(
    matches(paste(ColsUni1$name[ColsUni1$class %in% 
                                  c("numeric","integer")],collapse = "|"))) %>%
    dplyr::select(-matches("id|logTotalValue")) %>% colnames()
  
  for (i in xvars) {
    plot <- df[,c(i,"logTotalValue")] %>% 
      ggplot(aes_string(x = i, y = "logTotalValue")) +
      geom_smooth() + geom_point(size = 0.1)  +
      labs(title = paste0("Title ",i))
    
    print(plot)  
    print(paste0("Printing plot: ",i))
  }
  
}
 Boxes_n_Scatters()

# By default with raw variables, we see Area-related variables to be highly skewed, ----
  #   while in all cases there are a number of zero values. When such areas values are zero,
  #   it implies there is no such aspect of the building, e.g. no garage.
  #   It is in and of itself a value driver if a garage exists 
      #   (reflected by all '_Zero' ending variables) 
  #   While also flags those observations where the raw values were zero before log-transforms
    # we thus can interpret coefficient parameters as 
      #   % higher area being associated with on average % higher total value  

df <- df %>% mutate(
  BuiltFAR_Zero   = ifelse(BuiltFAR == 0, 1, 0),
  BldgDepth_Zero  = ifelse(BldgDepth == 0,1,0),
  BldgFront_Zero  = ifelse(BldgFront == 0 , 1,0),
  LotDepth_Zero   = ifelse(LotDepth == 0,1,0),
  LotFront_Zero   = ifelse(LotFront == 0,1,0),
  UnitsTotal_Zero = ifelse(UnitsTotal == 0,1,0),
  UnitsRes_Zero   = ifelse(UnitsRes == 0,1,0),
  NumFloors_Zero  = ifelse(NumFloors == 0,1,0),
  NumBldgs_Zero   = ifelse(NumBldgs == 0,1,0),
  OtherArea_Zero  = ifelse(OtherArea == 0,1,0),
  FactryArea_Zero = ifelse(FactryArea == 0,1,0),
  StrgeArea_Zero  = ifelse(StrgeArea == 0,1,0),
  GarageArea_Zero = ifelse(GarageArea == 0,1,0),
  RetailArea_Zero = ifelse(RetailArea == 0,1,0),
  OfficeArea_Zero = ifelse(OfficeArea == 0,1,0),
  ResArea_Zero    = ifelse(ResArea == 0,1,0),
  ComArea_Zero    = ifelse(ComArea == 0,1,0),
  BldgArea_Zero   = ifelse(BldgArea == 0,1,0),
  LotArea_Zero    = ifelse(LotArea == 0,1,0),
  Easements       = ifelse(Easements > 0,1,0)
)

# log-transforms: ----
# BuiltFAR, BldgDepth, BldgFront, LotDepth, LotFront, UnitTotal, UnitsRes,
# NumFloors, NumBldgs, OtherArea, FactryArea, StrgeArea, GarageArea, RetailArea,
# OfficeArea, ResArea, ComArea, BldgArea, LotArea, Easements


df <- df %>% mutate(
  BuiltFAR_ln   = log(BuiltFAR    + 0.01),
  BldgDepth_ln  = log(BldgDepth   + 0.01),
  BldgFront_ln  = log(BldgFront   + 0.01),
  LotDepth_ln   = log(LotDepth    + 0.01),
  LotFront_ln   = log(LotFront    + 0.01),
  UnitsTotal_ln = log(UnitsTotal  + 0.01),
  UnitsRes_ln   = log(UnitsRes    + 0.01),
  NumFloors_ln  = log(NumFloors   + 0.01),
  NumBldgs_ln   = log(NumBldgs    + 0.001),
  OtherArea_ln  = log(OtherArea   + 0.01),
  FactryArea_ln = log(FactryArea  + 0.01),
  StrgeArea_ln  = log(StrgeArea   + 0.01),
  GarageArea_ln = log(GarageArea  + 0.01),
  RetailArea_ln = log(RetailArea  + 0.01),
  OfficeArea_ln = log(OfficeArea  + 0.01),
  ResArea_ln    = log(ResArea     + 0.01),
  ComArea_ln    = log(ComArea     + 0.01),
  BldgArea_ln   = log(BldgArea    + 0.01),
  LotArea_ln    = log(LotArea     + 0.01)) %>% 
  dplyr::select(-c(BuiltFAR, BldgDepth, BldgFront, LotDepth, LotFront, UnitsTotal,
            UnitsRes, NumFloors, NumBldgs, OtherArea, FactryArea, StrgeArea,
            GarageArea, RetailArea, OfficeArea, ResArea, ComArea, BldgArea, LotArea))

Boxes_n_Scatters()    

# Correls w logTotalValue
Cols <- ColsUniques()

Y_Cors <- df %>% dplyr::select(matches(Cols$name[Cols$class %in% c("numeric", "integer")])) %>% 
  dplyr::select(-matches("id|logTotalValue")) %>% colnames() %>% 
  lapply(function(x) {
    tl <- list()
    tl[['Colname']] <- x
    tl[['Corr_w_Y_abs']] <- cor(df[,x],df[,"logTotalValue"]) %>% round(2) %>% abs()
    return(tl)
  }) %>% rbindlist() %>% as.data.frame() %>% arrange(desc(Corr_w_Y_abs))


Y_Cors[1:15,] %>% ggplot(aes(x = reorder(Colname, Corr_w_Y_abs), y = Corr_w_Y_abs,
                      color = "red")) +
  geom_point(size = 8) + 
  geom_col(width = 0.1, position = "dodge", fill  = "red") +
  geom_text(aes(label = Corr_w_Y_abs), size = 3, color = "black") + 
  theme_tufte() + 
  theme(legend.position = c(2,2),legend.title = element_text(size = 8)
        ,legend.text = element_text(size = 8),legend.key.size = unit(2,"mm")) +
  coord_flip() +
  labs(title = "Top Correlations with Target-Y Variable",
       y = "Absolute Correlation",
       x = "Continuous X variables")
  
# Top 3 correlating variables are LotArea, LotFront & Commercial Area, 
  # w Number of Units in a Building & total Building area making the top 5

# We can also expect that a garages value increases, 
    # given how large the building is, how much commercial area is there, etc.
  #   all-in-all I hypothesize a positive feedback loop between all area related variables
    #   I model this via interactions between all area related variables 
        #   -> ( 10 * 9 ) / 2 = 45 interactions 
Vars <- df %>% dplyr::select(matches("area_ln")) %>% colnames()

Interactions <- NULL
Counter <- 1
for (i in 1:length(Vars)) {
  for (j in (i+1):(length(Vars)) ) {
    Interactions[Counter] <- paste(Vars[i],Vars[j],sep =  " * ")
    Counter <- Counter + 1
  }
}
Interactions <- paste(Interactions[1:45],collapse =  " + " ) 


# b) Create Training - Test sets -> 70 test - 30 training ----
#### Sample vs Holdout sets
set.seed(1234)
train_indices <- as.integer(createDataPartition(df$logTotalValue, p = 0.3, list = FALSE))
data_train <- df[train_indices, ]
data_holdout <- df[-train_indices, ]

# train control is 10 fold cross validation
train_control <- trainControl(method = "cv",number = 10,verboseIter = FALSE)  


#### c) Use Lin Reg 2 predict logTotalValue -> 10-fold CV to assess predictive power ----
Vars <- df %>% dplyr::select(-matches("id|logTotalValue")) %>% colnames()

Formula <- formula(paste0("logTotalValue ~",paste(Vars, collapse = " + ")," + ", Interactions))

#### OLS ####
set.seed(1234)
ols_model <- train(
  formula(Formula),
  data = data_train,
  method = "lm",
  trControl = train_control)

summary(ols_model)


CV_RMSE <- ols_model$resample[,1] %>% mean()
CV_MAE <- ols_model$resample[,3] %>% mean()
CV_R2 <- ols_model$resample[,2] %>% mean()

data_holdout$ols_pred <- predict(ols_model, newdata = data_holdout)
# Holdout
Hd_MAE <- MAE(data_holdout$ols_pred, data_holdout$logTotalValue)
Hd_RMSE <- RMSE(data_holdout$ols_pred, data_holdout$logTotalValue)

# OLS Summary - Cross-Validation & Holdout data
OLS_Summ <- as.data.frame(rbind(cbind(CV_MAE,CV_RMSE,CV_R2),cbind(Hd_MAE,Hd_RMSE,CV_R2)))
rownames(OLS_Summ) <- c("CV_Stats","Holdout_Stats")
colnames(OLS_Summ) <- c("MAE","RMSE","R^2")
OLS_Summ

# d) Now use Penalized linear Models - LASSO / Ridge / Elastic Net ----
    # Does best model improve results vs c)
features <- setdiff(names(df), c("logTotalValue","ID"))

### GLMNET Versions ----

# glmnet needs inputs as a matrix. model.matrix: handles factor variables
# -1: we do not need the intercept as glmnet will automatically include it
x_train <- model.matrix( ~ . -1, data_train[, features, with = FALSE])
dim(x_train)

# how much penalty do we want to apply? select with CV
lambda_grid <- 10^seq(2,-5,length=100)  

# Ridge -> alpha = 0 ----
set.seed(1234)
system.time({
ridge_model <- cv.glmnet(
  x = x_train, y = data_train[["logTotalValue"]], 
  lambda = lambda_grid,
  family = "gaussian", # for continuous response
  alpha = 0,  # the ridge model
  nfolds = 10
)
})

plot(ridge_model, xvar = "lambda", main = "Collapsing Coefs. in RIDGE")

best_lambda <- ridge_model$lambda.min
message(paste0("The optimally chosen penalty parameter: ", best_lambda))

# RMSE for Highest Good Enough & Best Fits
ridge_model[[2]][c(54,79)]^2

highest_good_enough_lambda <- ridge_model$lambda.1se
message(paste0("The highest good enough penalty parameter: ", highest_good_enough_lambda))

# LASSO GLM version ----
set.seed(1234)
system.time({
  lasso_model <- cv.glmnet(
    x = x_train, y = data_train[["logTotalValue"]], 
    lambda = lambda_grid,
    family = "gaussian", # for continuous response
    alpha = 1,  # the ridge model
    nfolds = 10
  )
})

plot(lasso_model, xvar = "lambda", main = "Collapsing Coefs. in LASSO")

best_lambda <- lasso_model$lambda.min
message(paste0("The optimally chosen penalty parameter: ", best_lambda))

# RMSE for Highest Good Enough & Best Fits
lasso_model[[2]][c(67,77)]^2

highest_good_enough_lambda <- lasso_model$lambda.1se
message(paste0("The highest good enough penalty parameter: ", highest_good_enough_lambda))









### CARET versions ----
### LASSO ----

lasso_tune_grid <- expand.grid(
  "alpha" = c(1),
  "lambda" = 10^seq(2,-5,length=100)
)

set.seed(1234)
system.time({
lasso_fit <- train(
  Formula,
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid = lasso_tune_grid,
  trControl = train_control
)
})

lasso_fit %>% ggplot() +xlim(c(0,1))

lasso_fit$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = Rsquared), color = "red") + 
  geom_line(aes(y = MAE), color = "green") + xlim(c(0,1))

### RIDGE ----
ridge_tune_grid <- expand.grid(
  "alpha" = c(0),
  "lambda" = 10^seq(2,-5,length=100)  
)

set.seed(1234)
system.time({
ridge_fit <- train(
  Formula,
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid = ridge_tune_grid,
  trControl = train_control
)
})

ridge_fit %>% ggplot()

ridge_fit$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE)) +
  geom_line(aes(y = Rsquared)) + 
  geom_line(aes(y = MAE))


### Elastic Net ----
enet_tune_grid <- expand.grid(
  "alpha" = seq(0, 1, by = 0.1),
  "lambda" = union(lasso_tune_grid[["lambda"]], ridge_tune_grid[["lambda"]])
)

set.seed(1234)
system.time({
  enet_fit <- train(
    Formula,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid = enet_tune_grid,
    trControl = train_control
  )  
})

enet_fit$results %>% arrange(RMSE)

lasso_fit$bestTune
ridge_fit$bestTune
enet_fit$bestTune

# Based on summary table of Cross-Validated results above, 
  #   OLS has lowest MAE, Elastic Net has Lowest RMSE & Highest R^2, 
      # while dropping 17 variables vs the OLS model


# e)  Which model is "simplest one still good enough" ? ----
    #   Explore adding: selectionFunction = "oneSE" to trainControl

# OLS beats all simplest still good enough models 
  #   -> Elastic Net Came very close in RMSE & R2 while using 48 less predictors
    #     vs OLS difference in RMSE: 0.0031 / R2: 0.0013
      # i.e. on average 0.3% more off
    #     vs Elastic Net RMSE; 0.0052 / R2: 0.0022

train_control_1se <- trainControl(method = "cv",number = 10,
                                 verboseIter = FALSE,
                                 selectionFunction = "oneSE")  

lasso_tune_grid <- expand.grid(
  "alpha" = c(1),
  "lambda" = 10^seq(2,-5,length=100)
)

set.seed(1234)
system.time({
  lasso_fit_1se <- train(
    Formula,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid = lasso_tune_grid,
    trControl = train_control_1se
  )
})

lasso_fit %>% ggplot() +xlim(c(0,1))

lasso_fit$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = Rsquared), color = "red") + 
  geom_line(aes(y = MAE), color = "green") + xlim(c(0,1))

### RIDGE ----
ridge_tune_grid <- expand.grid(
  "alpha" = c(0),
  "lambda" = 10^seq(2,-5,length=100)  
)

set.seed(1234)
system.time({
  ridge_fit_1se <- train(
    Formula,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid = ridge_tune_grid,
    trControl = train_control_1se
  )
})

ridge_fit %>% ggplot()

ridge_fit$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE)) +
  geom_line(aes(y = Rsquared)) + 
  geom_line(aes(y = MAE))


### Elastic Net ----
enet_tune_grid <- expand.grid(
  "alpha" = seq(0, 1, by = 0.1),
  "lambda" = union(lasso_tune_grid[["lambda"]], ridge_tune_grid[["lambda"]])
)

set.seed(1234)
system.time({
  enet_fit_1se <- train(
    Formula,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid = enet_tune_grid,
    trControl = train_control_1se
  )  
})

#### Summary ----

resample_profile <- resamples(
  list("OLS" = ols_model,
       "RIDGE" = ridge_fit,
       "LASSO" = lasso_fit,
       "Elastic Net" = enet_fit,
       "RIDGE_1se" = ridge_fit_1se,
       "LASSO_1se" = lasso_fit_1se,
       "Elastic_Net_1se" = enet_fit_1se)) 

Models <- list("OLS" = ols_model,"RIDGE" = ridge_fit,
               "LASSO" = lasso_fit,"Elastic Net" = enet_fit,
               "RIDGE_1se" = ridge_fit_1se,"LASSO_1se" = lasso_fit_1se,
               "Elastic_Net_1se" = enet_fit_1se)

ols_coeffs      <- as.matrix(coef(ols_model$finalModel))
lasso_coeffs    <- as.matrix(coef(lasso_fit$finalModel, lasso_fit$bestTune$lambda))
ridge_coeffs    <- as.matrix(coef(ridge_fit$finalModel, ridge_fit$bestTune$lambda))
enet_coeffs     <- as.matrix(coef(enet_fit$finalModel, enet_fit$bestTune$lambda))
lasso1se_coeffs <- as.matrix(coef(lasso_fit_1se$finalModel, lasso_fit_1se$bestTune$lambda))
ridge1se_coeffs <- as.matrix(coef(ridge_fit_1se$finalModel, ridge_fit_1se$bestTune$lambda))
enet1se_coeffs  <- as.matrix(coef(enet_fit_1se$finalModel, enet_fit_1se$bestTune$lambda))

Nr_Vars <- list(
  "OLS"   = sum(ols_coeffs != 0, na.rm = T),
  "RIDGE" = sum(ridge_coeffs != 0),
  "LASSO" = sum(lasso_coeffs != 0),
  "E_Net" = sum(enet_coeffs != 0),
  "RIDGE_1se" = sum(ridge1se_coeffs != 0),
  "LASSO_1se" = sum(lasso1se_coeffs != 0),
  "E_Net_1se" = sum(enet1se_coeffs != 0))


Penalized_SummTable <- lapply(names(Models), function(x) {
  tdf <-  resample_profile$values %>% dplyr::select(matches(x))
  tl <- list()
  
  tl[['Regr.Model']] <- x
  tl[['CV_MAE']] <- (tdf %>% dplyr::select(matches("MAE")))[[1]] %>% mean() %>% round(4)
  tl[['CV_RMSE']] <- (tdf %>% dplyr::select(matches("RMSE")))[[1]] %>% mean() %>% round(4)
  tl[['CV_R^2']] <- (tdf %>% dplyr::select(matches("Rsquared")))[[1]] %>% mean() %>% round(4)
  
  return(tl)
}) %>% rbindlist() %>% cbind(Nr_Vars)


Penalized_SummTable



# f) Try improve Linear Model w PCA 4 Dim Reduction. ----
    #   Center & Scale variables -> use pcr 2 find optimal number of PCs
    #   Does PCA Improve fit over simple linear models
    #     # Many factor variables -> Include 60-90 PCs in search as well

#    preProcess = c("center", "scale", "pca"),

#   1) Pre-Processing -> removing factors
      # Other factors -> dummy variable table -> dummyVars
        #   -> will probably result in smaller variances -> hence 60 - 90 PCs

# Using dummyVars to make factor variables -> make new dataframe basically
dummies <- dummyVars(Formula, data = df)
tdf <- cbind("logTotalValue" = df$logTotalValue,predict(dummies,newdata = df) %>% as.data.frame())

#### Sample vs Holdout sets
set.seed(1234)
train_indices <- as.integer(createDataPartition(tdf$logTotalValue, p = 0.3, list = FALSE))
data_train <- tdf[train_indices, ]
data_holdout2 <- tdf[-train_indices, ]

#### OLS w PCA ####
PCA_tdf <- prcomp(tdf)
Summary <- PCA_tdf %>% summary()
PCA_Summ_df <- Summary$importance %>% rbind() %>% as.data.frame() %>% transpose() %>% cbind(index = 1:300)
colnames(PCA_Summ_df) <- c("StDev","Prop%_Variance","CumSum_Prop%_Variance","index")

PCA_Summ_df %>% ggplot(aes(x = index)) +
  geom_line(aes(y = `Prop%_Variance`)) + xlim(c(0,30)) +
  geom_point(aes(y = `Prop%_Variance`))

PCA_Summ_df %>% ggplot(aes(x = index)) +
  geom_point(aes(y = `CumSum_Prop%_Variance`)) +
  geom_line(aes(y = `CumSum_Prop%_Variance`)) + xlim(c(0,30))

# PC35 -> 99.9% of total variance -> PC60 = 99.99%


trctrlWithPCA <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
tune_grid <- data.frame(ncomp = 35:120)

set.seed(1234)
  ols_model_pca <- train(
    logTotalValue ~ .,
    data = data_train,
    method = "pcr",
    preProcess = c("center", "scale"),
    tuneGrid = tune_grid,
    trControl = trctrlWithPCA)
summary(ols_model_pca)
#     ncomp = 117 is optimal


# g) Applying PCA to penalized models via perProcess -> achieves better fit?
    #   Include "nzv" to preProcess also -> Drops zero variance features
      # What's your intuition, why is this happening ? 




### LASSO PCA ----

lasso_tune_grid <- expand.grid(
  "alpha" = c(1),
  "lambda" = 10^seq(0,-5.5,length=100))

set.seed(1234)
system.time({
  lasso_fit_pca <- train(
    logTotalValue ~ .,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale","nzv","pca"),
    tuneGrid = lasso_tune_grid,
    trControl = trainControl(method = "cv",number = 10,preProcOptions = list(pcaComp = 117), verboseIter = T)
  )
})

lasso_fit_pca %>% ggplot() +xlim(c(0,1))

lasso_fit_pca$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = Rsquared), color = "red") + 
  geom_line(aes(y = MAE), color = "green") + xlim(c(0.005,1))

### RIDGE PCA ----
ridge_tune_grid <- expand.grid(
  "alpha" = c(0),
  "lambda" = 10^seq(4,-5.5,length=200)  
)

set.seed(1234)
system.time({
  ridge_fit_pca <- train(
    logTotalValue ~ .,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale","nzv","pca"),
    tuneGrid = ridge_tune_grid,
    trControl = trainControl(method = "cv",number = 10,preProcOptions = list(pcaComp = 117), verboseIter = T)
  )
})

ridge_fit_pca$bestTune

ridge_fit_pca$results %>% arrange(RMSE)

ridge_fit %>% ggplot()

ridge_fit$results %>% ggplot(aes(x = lambda)) +
  geom_line(aes(y = RMSE)) +
  geom_line(aes(y = Rsquared)) + 
  geom_line(aes(y = MAE))


### Elastic Net PCA ----
enet_tune_grid <- expand.grid(
  "alpha" = seq(0.7, 1, by = 0.03),
  "lambda" = union(lasso_tune_grid[["lambda"]], ridge_tune_grid[["lambda"]])
)

set.seed(1234)
system.time({
  enet_fit_pca <- train(
    logTotalValue ~ .,
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale","nzv","pca"),
    tuneGrid = enet_tune_grid,
    trControl = trainControl(method = "cv",number = 10,preProcOptions = list(pcaComp = 117), verboseIter = T)
  )  
})

enet_fit_pca$results %>% arrange(RMSE)
enet_fit_pca$bestTune




# h) Select best model trained -> Evaluate your preferred model on test set. ----

#### Summary ----

Resamples_w_pca <- resamples(
  list("OLS" = ols_model,
       "OLS_PCA" = ols_model_pca,
       "RIDGE" = ridge_fit,
       "RIDGE_PCA" = ridge_fit_pca,
       "LASSO" = lasso_fit,
       "LASSO_PCA" = lasso_fit_pca,
       "Elastic Net" = enet_fit,
       "Elastic_Net_PCA" = enet_fit_pca
       )) 

Models <- list("OLS" = ols_model       ,"OLS_PCA" = ols_model_pca,
               "RIDGE" = ridge_fit     ,"RIDGE_PCA" = ridge_fit_pca,
               "LASSO" = lasso_fit     ,"LASSO_PCA" = lasso_fit_pca,
               "Elastic Net" = enet_fit,"Elastic_Net_PCA" = enet_fit_pca)


PCA_SummTable <- lapply(names(Models), function(x) {
  tdf <-  Resamples_w_pca$values %>% dplyr::select(matches(x))
  tl <- list()
  
  tl[['Regr.Model']] <- x
  tl[['CV_MAE']] <- (tdf %>% dplyr::select(matches("MAE")))[[1]] %>% mean() %>% round(4)
  tl[['CV_RMSE']] <- (tdf %>% dplyr::select(matches("RMSE")))[[1]] %>% mean() %>% round(4)
  tl[['CV_R^2']] <- (tdf %>% dplyr::select(matches("Rsquared")))[[1]] %>% mean() %>% round(4)
  
  return(tl)
}) %>% rbindlist()


PCA_SummTable

#   PCA-d penalized & OLS models worse vs respective regular counterparts
# Why? we do have lot less variables w PCA 117 was the optimized PCs to use
# 35 PCs capture 99.9% of Var, 60 99.99%  - other 182-239 vars not really relevant
# Indeed it might be the case the purely best model is well-overfitted
# Reccommendation: Elastic_net 
#   Sheer curiosity: test also Elastic_Net_PCA

data_holdout$pred_E.Net <- predict(enet_fit,newdata = data_holdout)
Hd_RMSE <- RMSE(data_holdout$pred_E.Net,data_holdout$logTotalValue) %>% round(4)
Hd_MAE <-  MAE(data_holdout$pred_E.Net,data_holdout$logTotalValue) %>% round(4)

data_holdout2$pred_E.NetPca <- predict(enet_fit_pca,newdata = data_holdout2)
Hd_RMSE_Pc <- RMSE(data_holdout2$pred_E.NetPca,data_holdout2$logTotalValue) %>% round(4)
Hd_MAE_Pc <-  MAE(data_holdout2$pred_E.NetPca,data_holdout2$logTotalValue) %>% round(4)

HoldoutSumms <- rbind(cbind(Hd_RMSE,Hd_MAE),cbind(Hd_RMSE_Pc,Hd_MAE_Pc)) %>% as.data.frame()

rownames(HoldoutSumms) <- c("Best_Elastic_Net","Elastic_Net_PCA")
HoldoutSumms

### 2) Clustering on the USArrests dataset ----
  #   Task: Apply Clustering -> make sense of clusters with PCA
  #   Data used in Class

library(tidyverse)
library(caret)
library(skimr)
library(janitor)
library(factoextra) # provides nice functions for visualizing the output of PCA
library(NbClust) # for choosing the optimal number of clusters
library(knitr)
library(kableExtra)

theme_set(theme_tufte())

data <- USArrests
GGally::ggpairs(data, title = "USArrests data Scatters, Densities & Correlations")

# a) Think of data pre-processing steps you may / should / would do before Clustering ----
    #   Are there any ?
      #   Scale variables 

data_scaled <- scale(data) %>% as.data.frame()


# b) Determine optimal number of clusters as indicated by NbClust heuristics 
nb <- NbClust(data_scaled, method = "kmeans", min.nc = 2, max.nc = 10, index = "all")


Cluster <- nb$Best.nc["Number_clusters",] %>% cbind() %>% as.data.frame()
colnames(Cluster) <- "Optimal_Cluster_Nr"

NrClustsRec <- Cluster %>% #group_by(Optimal_Cluster_Nr) %>% summarize(count = n()) %>% 
    ggplot(aes(x = Optimal_Cluster_Nr)) + 
    geom_bar(color = "red", fill = "navy") +
    scale_x_continuous(labels = c(0:10), breaks = c(0:10)) +
    scale_y_continuous(labels = c(1:11), breaks = c(1:11)) + 
    labs(title = "Frequency of Recommended Nr. Of Clusters",
         y = "Nr. Times Reccommended",
         x = "Nr. of Clusters Recommended")

# c) Use K-means to cluster states using Nr. Clusters found in b)
km <- kmeans(data_scaled, centers = nb$Best.partition %>% unique() %>% length(),
             nstart = 30)

data_w_clusters <- mutate(data, cluster = factor(km$cluster))



# Plots -------
# Murder
Murder <- data_w_clusters %>% 
  ggplot(aes(x = UrbanPop, y = Murder,  color = cluster)) +
  geom_point( )

# Assault
Assault <- ggplot(data_w_clusters, aes(x = UrbanPop, y = Assault, color = cluster)) +geom_point()

# Rape
Rape <- ggplot(data_w_clusters, aes(x = UrbanPop, y = Rape, color = cluster)) +  geom_point()

    #   Plus anything else you think makes sense
    #   Plot observations colored by Clusters
      #   Urban population vs another Crime-related Variable
        #   See e.g. code from class - use factor(kn$cluster) 4 vector of class labels

# d) Do PCA & Get 1st 2 PC coordinates 4 all observations: ----
pca_result <- prcomp(data,scale = T)

pca_result$x[,1:2]
pca_result$rotation %>% colSums()
Cities <- rownames(pca_result$x)

first_two_pc <- pca_result$x[,1:2] %>% as.data.frame()
first_two_pc <- first_two_pc %>% mutate(cluster = factor(km$cluster),
                                        City = Cities)

rownames(first_two_pc) <- Cities

    #   Plot clusters of choice along co-ordinate system of 1st 2 PCs

PC12Plot <- first_two_pc %>% ggplot(aes(x = PC1,y = PC2, color = cluster)) +
  geom_point() +geom_text(label = Cities, size = 3, hjust = -0.1)

  # Very clear Seperation along PC1 

#   How do clusters relate to these?
ClusterTable <- data_w_clusters %>% group_by(cluster) %>%
  summarize(StateCount = n(),
            AvgMurder = mean(Murder)     ,AvgAssault = mean(Assault),
            AvgUrbanPop = mean(UrbanPop) ,AvgRape = mean(Rape))
  # Cluster 2 ca. 3x Murder / 2x Assault / 2x Rape
    # Safe vs Unsafe seems an intuitive cluster title 
      # Weights used to project observations along PCs helps this further:

PC12_Weights <- pca_result$rotation[,1:2] %>% round(4)

PC1_Cont <- fviz_contrib(pca_result, "var", axes = 1) # Crime-related
PC2_Cont <- fviz_contrib(pca_result, "var", axes = 2) # Population-related


### 3) PCA of high-dimensional data ----
  #   40 Obs.s. - 1000 var.s
    #   20 - 20 obs. healthy - diseased patiant obs.s

genes <- read_csv("https://www.statlearning.com/s/Ch10Ex11.csv", col_names = FALSE) %>%
  t() %>% as_tibble()  # the original dataset is of dimension 1000x40 so we transpose it
dim(data)

# a) Perform PCA on this data w scaling features

# b) Visualize data points in the space of 1st 2 PCs - fviz_pca_ind f(x)
    #   What can you see in the figure ?

# c) Which indy features can matter most 2 seperate diseased from healthy ? 
    #  Strategy 2 answer:

      #   We see PC1 matters A LOT
      #   Look at which features have HIGH LOADING 4 PC1 
          #   i.e. largest absolute value co-ordinates -> use $rotation
          # choose these 2 features -> plot observations across these on co-ordinates
            # What can you see ?






