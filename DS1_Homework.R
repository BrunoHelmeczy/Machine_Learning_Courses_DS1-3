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

### 1) Supervised Learning with Penalized Models & PCA ----
    # Property Values in Manhattan - R 4 Everyone
    #   Predict log-Property Value = logTotalValue

df <- readRDS(url('http://www.jaredlander.com/data/manhattan_Train.rds')) %>%
  mutate(logTotalValue = log(TotalValue)) %>%
  drop_na() %>% dplyr::select(-TotalValue)

# Function to check distinct values & class of all columns - anytime
ColsUniques <- function() {
  rbindlist(lapply(1:length(df), function(x) {
    tl <- list()
    tl[['name']] <- colnames(df)[x]
    tl[['distinct']] <- nrow(unique(df[,x]))
    tl[['class']] <- df[,x][[1]] %>% class()
    return(tl)
  }))  
}
ColsUni1 <- ColsUniques()

# Dummies : Area measures -> has, or not 
# logicals / binaries
ColsUni1$name[ColsUni1$distinct == 2]

lapply(ColsUni1$name[ColsUni1$distinct == 2],function(x) {
  df[,x] %>% table()
})

    # Zone Distr 2-4 -> 90+ %  Missing value
df$ZoneDist4 <- NULL
df$ZoneDist3 <- NULL
df$ZoneDist2 <- NULL

# Binary Factors -> Yes = 1 No = 0
df <- df %>% mutate(
  IrregularLot     = as.factor(ifelse(IrregularLot == "Yes",1,0)),
  Landmark         = as.factor(ifelse(Landmark == "Yes",1,0)),
  HistoricDistrict = as.factor(ifelse(HistoricDistrict == "Yes" ,1,0)),
  High             = as.factor(ifelse(High == T,1,0))
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
    if( ColsUni2$class[ColsUni2$name == x] %in% c("integer","numeric") ) {
    plot <- df %>% ggplot(aes_string(x = x)) + 
        geom_histogram( color = "red", fill = "blue") + theme_tufte() + 
        labs(title = paste0("NYC Properties ",x," Distribution")) 
    print(plot)
    } else if (ColsUni2$class[ColsUni2$name == x] %in% c("factor","logical") ) {
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
  lapply(df %>% dplyr::select(-matches("id|logTotalValue")) %>%  colnames(), function(x) {
      if (  ColsUni1$class[ColsUni1$name == x] %in% c("factor","logical") ) {
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
# Boxes_n_Scatters()

# By default with raw variables, we see Area-related variables to be highly skewed,
  #   while in all cases there are a number of zero values. When such areas values are zero,
  #   it implies there is no such aspect of the building, e.g. no garage.
  #   It is in and of itself a value driver if a garage exists 
      #   (reflected by all '_Zero' ending variables) 
  #   While also flags those observations where the raw values were zero before log-transforms
    # we thus can interpret coefficient parameters as 
      #   % higher area being associated with on average % higher total value  

df <- df %>% mutate(
  BuiltFAR_Zero   = as.factor(ifelse(BuiltFAR == 0, 1, 0)),
  BldgDepth_Zero  = as.factor(ifelse(BldgDepth == 0,1,0)),
  BldgFront_Zero  = as.factor(ifelse(BldgFront == 0 , 1,0)),
  LotDepth_Zero   = as.factor(ifelse(LotDepth == 0,1,0)),
  LotFront_Zero   = as.factor(ifelse(LotFront == 0,1,0)),
  UnitsTotal_Zero = as.factor(ifelse(UnitsTotal == 0,1,0)),
  UnitsRes_Zero   = as.factor(ifelse(UnitsRes == 0,1,0)),
  NumFloors_Zero  = as.factor(ifelse(NumFloors == 0,1,0)),
  NumBldgs_Zero   = as.factor(ifelse(NumBldgs == 0,1,0)),
  OtherArea_Zero  = as.factor(ifelse(OtherArea == 0,1,0)),
  FactryArea_Zero = as.factor(ifelse(FactryArea == 0,1,0)),
  StrgeArea_Zero  = as.factor(ifelse(StrgeArea == 0,1,0)),
  GarageArea_Zero = as.factor(ifelse(GarageArea == 0,1,0)),
  RetailArea_Zero = as.factor(ifelse(RetailArea == 0,1,0)),
  OfficeArea_Zero = as.factor(ifelse(OfficeArea == 0,1,0)),
  ResArea_Zero    = as.factor(ifelse(ResArea == 0,1,0)),
  ComArea_Zero    = as.factor(ifelse(ComArea == 0,1,0)),
  BldgArea_Zero   = as.factor(ifelse(BldgArea == 0,1,0)),
  LotArea_Zero    = as.factor(ifelse(LotArea == 0,1,0)),
  Easements       = as.factor(ifelse(Easements > 0,1,0))
)

# log-transform:
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


Y_Cors %>% ggplot(aes(x = reorder(Colname, Corr_w_Y_abs), y = Corr_w_Y_abs,
                      color = Corr_w_Y_abs, fill = Corr_w_Y_abs)) +
  geom_point(size = 8) + 
  geom_col(width = 0.1, position = "dodge") +
  geom_text(aes(label = Corr_w_Y_abs), size = 3, color = "black") + 
  theme_tufte() + 
  theme(legend.position = c(0.8,0.35),legend.title = element_text(size = 8)
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
set.seed(1)
train_indices <- as.integer(createDataPartition(df$logTotalValue, p = 0.3, list = FALSE))
data_train <- df[train_indices, ]
data_holdout <- df[-train_indices, ]

# train control is 10 fold cross validation
train_control <- trainControl(method = "cv",number = 10,verboseIter = FALSE)  


# c) Use Lin Reg 2 predict logTotalValue -> 10-fold CV to assess predictive power ----
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
  family = "gaussian", # for continuous response
  alpha = 0,  # the ridge model
  nfolds = 10
)
})

plot(ridge_model, xvar = "lambda", main = "Collapsing Coefs. in RIDGE")

best_lambda <- ridge_model$lambda.min
message(paste0("The optimally chosen penalty parameter: ", best_lambda))


ridge_model[[2]][c(98,100)]^2
#best_fit <- ridge_model$glmnet.fit

highest_good_enough_lambda <- ridge_model$lambda.1se
message(paste0("The highest good enough penalty parameter: ", highest_good_enough_lambda))








### CARET versions ----
### LASSO
# Again, let's look at individual coefficients. 
# We can see that some are set exactly to zero for higher values of the penalty term. 
# This is in contrast to what we saw with the Ridge model.

set.seed(1234)
lasso_model <- glmnet(
  x = x_train, y = data_train[["log_sale_price"]], 
  family = "gaussian",
  alpha = 1  # the lasso model
)

plot(lasso_model, xvar = "lambda")

lasso_coeffs <- get_glmnet_coeff_sequence(lasso_model)



selected_variables <- c("gr_liv_area", "tot_rms_abv_grd", "garage_area", "kitchen_abv_gr")
ggplot(
  data = lasso_coeffs %>% filter(variable %in% selected_variables),
  aes(x = log(lambda), y = value)) +
    geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 1)


#Again, we can apply cross-validation to determine the optimal value for the penalty term.
set.seed(1234)
lasso_model_cv <- cv.glmnet(
  x = x_train, y = data_train[["log_sale_price"]], 
  family = "gaussian",
  alpha = 1,
  nfolds = 10
)

best_lambda <- lasso_model_cv$lambda.min
message(paste0("The optimally chosen penalty parameter: ", best_lambda))

highest_good_enough_lambda <- lasso_model_cv$lambda.1se
message(paste0("The highest good enough penalty parameter: ", highest_good_enough_lambda))


### RIDGE


### Elastic Net

# e)  Which model is "simplest one still good enough" ?
    #   Explore adding: selectionFunction = "oneSE" to trainControl



# f) Try improve Linaer Model w PCA 4 Dim Reduction.
    #   Center & Scale variables -> use pcr 2 find optimal number of PCs
    #   Does PCA Improve fit over simple linear models
    #     # Many factor variables -> Include 60-90 PCs in search as well



# g) Applying PCA to penalized models via perProcess -> achieves better fit?
    #   Include "nzv" to preProcess also -> Dorps zero variance features
      # What's your intuition, why is this happening ? 



# h) Select best model trained -> Evaluate your preferred model on test set.




### 2) Clustering on the USArrests dataset ----
  #   Task: Apply Clustering -> make sense of clusters with PCA
  #   Data used in Class


# a) Think of data pre-processing steps you may / should / would do before Clustering 
    #   Are there any ?

# b) Determine optimal number of clusters as indicated by NbClust heuristics 

# c) Use K-means to cluster states using Nr. Clusters found in a)
    #   Plus anything else you think makes sense
    #   Plot observations colored by Clusters
      #   Urban population vs another Crime-related Variable
        #   See e.g. code from class - use factor(kn$cluster) 4 vector of class labels

# d) Do PCA & Get 1st 2 PC coordinates 4 all observations: 
#   pca_result <- prcomp(data,scale = T)
#   first_two_pc <- as_tibble(pca_results$x[,1:2])

    #   Plot clusters of choice along co-ordinate system of 1st 2 PCs
      #   How do clusters relate to these?




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






