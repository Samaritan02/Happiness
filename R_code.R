  # load packages
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(ggplot2)
  library(car)
  library(corrgram)
  library(MASS)
  library(reshape2)
  library(GGally)
  library(psych)
  library(cluster)
  # load data
  happiness <- read.csv("/Users/shuimuqinghua/Desktop/作业/23Spring/Multivariate Analysis/Project/processed_happiness.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  ### transform the string variables into factors("1.76" -> 1.76)
  happiness$Economy..GDP.per.Capita. <- as.numeric(happiness$Economy..GDP.per.Capita.)
  happiness$Family..Social.Support. <- as.numeric(happiness$Family..Social.Support.)
  happiness$Health..Life.Expectancy. <- as.numeric(happiness$Health..Life.Expectancy.)
  happiness$Freedom <- as.numeric(happiness$Freedom)
  happiness$Trust..Government.Corruption. <- as.numeric(happiness$Trust..Government.Corruption.)
  happiness$Generosity <- as.numeric(happiness$Generosity)
  happiness$Happiness.Score <- as.numeric(happiness$Happiness.Score)
  happiness$Happiness.Rank <- as.numeric(happiness$Happiness.Rank)
  happiness$Country <- as.factor(happiness$Country)
  happiness$Region <- as.factor(happiness$Region)
  happiness <- happiness[, -1]
  head(happiness)
  
  # EDA
  summary(happiness)
  
  ## draw correlation
  happiness_feature = read.csv("/Users/shuimuqinghua/Desktop/作业/23Spring/Multivariate Analysis/Project/happiness_feature.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  ggpairs(happiness_feature, lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)))
  
  # Data Reduction
  
  
  ## FA
  ### standardization
  mle_model_2 = factanal(happiness_feature, factors = 2, rotation ="varimax")
  mle_model_2
  mle_model_3 = factanal(happiness_feature, factors = 3, rotation ="varimax")
  mle_model_3
  
  ### standardization promax
  mle_model_2_pro = factanal(happiness_feature, factors = 2, rotation ="promax")
  mle_model_2_pro
  mle_model_3_pro = factanal(happiness_feature, factors = 3, rotation ="promax")
  mle_model_3_pro
  
  ### minres
  mle_model_2_minres = fa(happiness_feature, nfactors = 2, rotate ="varimax", fm = "minres")
  mle_model_2_minres
  mle_model_3_minres = fa(happiness_feature, nfactors = 3, rotate ="varimax", fm = "minres")
  mle_model_3_minres
  mle_model_2_minres_pro = fa(happiness_feature, nfactors = 2, rotate ="promax", fm = "minres")
  mle_model_2_minres_pro
  
  ### merge the scores and the happiness data
  mle_model_2_minres_pro_scores = as.data.frame(mle_model_2_minres_pro$scores)
  mle_model_2_minres_pro_scores$Happiness.Score = happiness$Happiness.Score
  
  ### draw the scatter plot, color by Happiness.Score(higher score, shallower color)
  ggplot(mle_model_2_minres_pro_scores, aes(x = MR1, y = MR2, color = Happiness.Score)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("Factor Scores")
  
  # PCA
  ## standardization 
  pca_model = prcomp(happiness_feature_std, center = TRUE, scale. = TRUE)
  
  ### draw the scree plot
  scree_df = data.frame(x = 1:6, y = pca_model$sdev^2)
  ggplot(scree_df, aes(x = x, y = y)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    xlab("Number of Components") +
    ylab("Eigenvalues") +
    ggtitle("Scree Plot")
  
  ### merge the scores and the happiness data
  pca_model_6_std_scores = as.data.frame(pca_model_6_std$scores)
  pca_model_6_std_scores$Happiness.Score = happiness$Happiness.Score
  
  ### draw the scatter plot, color by Happiness.Score(higher score, shallower color)
  ggplot(pca_model_6_std_scores, aes(x = PC1, y = PC2, color = Happiness.Score)) + geom_point() + xlab("Component1") + ylab("Component2") + ggtitle("Component Scores")
  
  ### draw to show how different variables affect the first two components, use segment to show the direction
  pca_model_6_std_loadings = as.data.frame(pca_model_6_std$weights)
  pca_model_6_std_loadings = pca_model_6_std_loadings[, 1:2]
  pca_model_6_std_loadings$var = rownames(pca_model_6_std_loadings)
  ggplot(pca_model_6_std_loadings, aes(x = PC1, y = PC2, color = var)) +
    geom_point() +
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, color = var), data = pca_model_6_std_loadings) +
    ggtitle("Component Loadings")
  
  # Cluster Analysis
  ## draw factor scores using ggplot
  mle_model_2_minres_pro_scores = as.data.frame(mle_model_2_minres_pro$scores)
  ggplot(mle_model_2_minres_pro_scores, aes(x = MR1, y = MR2)) + geom_point() + xlab("Factor1") + ylab("Factor2") + ggtitle("Factor Scores")
  
  ## draw pca scores using ggplot
  pca_model_6_std_scores = as.data.frame(pca_model_6_std$scores)
  ggplot(pca_model_6_std_scores, aes(x = PC1, y = PC2)) + geom_point() + xlab("Component1") + ylab("Component2") + ggtitle("Component Scores")
  
  ## use K-means to cluster
  set.seed(123)
  kmeans_model = kmeans(mle_model_2_minres_pro_scores, centers = 3, nstart = 20)
  
  ### draw the cluster plot
  kmeans_model$cluster = as.factor(kmeans_model$cluster)
  kmeans_model$cluster = factor(kmeans_model$cluster, levels = c(1, 2, 3))
  ggplot(mle_model_2_minres_pro_scores, aes(x = MR1, y = MR2, color = kmeans_model$cluster)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("K-means Cluster Plot")
  
  ## use K-medoids to cluster
  set.seed(123)
  kmedoids_model = pam(mle_model_2_minres_pro_scores, k = 3)
  
  ### draw the cluster plot
  kmedoids_model$cluster = as.factor(kmedoids_model$cluster)
  kmedoids_model$cluster = factor(kmedoids_model$cluster, levels = c(1, 2, 3))
  ggplot(mle_model_2_minres_pro_scores, aes(x = MR1, y = MR2, color = kmedoids_model$cluster)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("K-medoids Cluster Plot")
  
  ## use hierarchical clustering to cluster
  set.seed(123)
  hclust_model = hclust(dist(mle_model_2_minres_pro_scores), method = "ward.D2")
  
  ### draw the cluster plot
  hclust_model$cluster = as.factor(cutree(hclust_model, k = 3))
  hclust_model$cluster = factor(hclust_model$cluster, levels = c(1, 2, 3))
  ggplot(mle_model_2_minres_pro_scores, aes(x = MR1, y = MR2, color = hclust_model$cluster)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("Hierarchical Cluster Plot")
  
  # Discriminant Analysis
  ## label the data based on the rank(high, medium, low; 50 and 100 to be the boundary)
  mle_model_2_minres_pro_scores$Happiness.Rank = happiness$Happiness.Rank
  mle_model_2_minres_pro_scores$Happiness.Rank = as.factor(ifelse(mle_model_2_minres_pro_scores$Happiness.Rank <= 50, 3, ifelse(mle_model_2_minres_pro_scores$Happiness.Rank <= 100, 2, 1)))
  mle_model_2_minres_pro_scores$Happiness.Rank = factor(mle_model_2_minres_pro_scores$Happiness.Rank, levels = c(1, 2, 3))
  
  
  
  ## split the data into training and testing, 7:3
  set.seed(123)
  train_index = sample(1:nrow(mle_model_2_minres_pro_scores), size = 0.7 * nrow(mle_model_2_minres_pro_scores))
  train_data = mle_model_2_minres_pro_scores[train_index, ]
  test_data = mle_model_2_minres_pro_scores[-train_index, ]
  
  ## use lda to train the model
  lda_model = lda(Happiness.Rank ~ ., data = train_data)
  lda_model
  
  ## use the model to predict the training data
  lda_pred_train = predict(lda_model, train_data)
  train_data$pred_class = lda_pred_train$class
  train_data$pred_class = as.factor(train_data$pred_class)
  train_data$pred_class = factor(train_data$pred_class, levels = c(1, 2, 3))
  
  ## use the model to predict the testing data
  lda_pred_test = predict(lda_model, test_data)
  test_data$pred_class = lda_pred_test$class
  test_data$pred_class = as.factor(test_data$pred_class)
  test_data$pred_class = factor(test_data$pred_class, levels = c(1, 2, 3))
  
  ## use ggplot to draw the scatter plot and confusion matrix
  ### training data, the color is based on the predicted class and the shape is based on the real class
  ggplot(train_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("LDA Training Data")
  
  confusionMatrix(train_data$pred_class, train_data$Happiness.Rank)
  
  ### testing data, the color is based on the predicted class and the shape is based on the real class
  ggplot(test_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("LDA Testing Data")
  
  confusionMatrix(test_data$pred_class, test_data$Happiness.Rank)
  
  ### remove pred_class
  train_data$pred_class = NULL
  test_data$pred_class = NULL
  
  ## use qda to train the model
  qda_model = qda(Happiness.Rank ~ ., data = train_data)
  qda_model
  
  ## use the model to predict the training data
  qda_pred_train = predict(qda_model, train_data)
  train_data$pred_class = qda_pred_train$class
  train_data$pred_class = as.factor(train_data$pred_class)
  train_data$pred_class = factor(train_data$pred_class, levels = c(1, 2, 3))
  
  ## use the model to predict the testing data
  qda_pred_test = predict(qda_model, test_data)
  test_data$pred_class = qda_pred_test$class
  test_data$pred_class = as.factor(test_data$pred_class)
  test_data$pred_class = factor(test_data$pred_class, levels = c(1, 2, 3))
  
  ## use ggplot to draw the scatter plot and confusion matrix
  ### training data, the color is based on the predicted class and the shape is based on the real class, draw the boundary of the classes
  ggplot(train_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("QDA Training Data")
  
  confusionMatrix(train_data$pred_class, train_data$Happiness.Rank)
  
  ### testing data, the color is based on the predicted class and the shape is based on the real class
  ggplot(test_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("QDA Testing Data")
  
  confusionMatrix(test_data$pred_class, test_data$Happiness.Rank)
  
  ### remove pred_class
  train_data$pred_class = NULL
  test_data$pred_class = NULL
  
  ## use SVM to train the model
  svm_model = svm(Happiness.Rank ~ ., data = train_data)
  svm_model
  
  ## use the model to predict the training data
  svm_pred_train = predict(svm_model, train_data)
  train_data$pred_class = svm_pred_train
  
  ## use the model to predict the testing data
  svm_pred_test = predict(svm_model, test_data)
  test_data$pred_class = svm_pred_test
  
  ## use ggplot to draw the scatter plot and confusion matrix
  ### training data, the color is based on the predicted class and the shape is based on the real class, draw the boundary of the classes
  ggplot(train_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("SVM Training Data")
  
  confusionMatrix(train_data$pred_class, train_data$Happiness.Rank)
  
  ### testing data, the color is based on the predicted class and the shape is based on the real class
  ggplot(test_data, aes(x = MR1, y = MR2, color = pred_class, shape = Happiness.Rank)) +
    geom_point() +
    xlab("Material") +
    ylab("Spiritual") +
    ggtitle("SVM Testing Data")
  
  confusionMatrix(test_data$pred_class, test_data$Happiness.Rank)
  
  # Regression Analysis
  ## prepare the data
  ### remove the Happiness.Rank, Country, Region, Year
  regression_data = happiness[, -c(1, 2, 3, 11)]
  
  ### split the data into training and testing, 7:3
  set.seed(123)
  train_index = sample(1:nrow(regression_data), size = 0.7 * nrow(regression_data))
  train_data = regression_data[train_index, ]
  test_data = regression_data[-train_index, ]
  
  ## linear regression
  lm_model = lm(Happiness.Score ~ ., data = train_data)
  summary(lm_model)
  
  ### use the model to predict the training data
  lm_pred_train = predict(lm_model, train_data)
  
  ### use the model to predict the testing data
  lm_pred_test = predict(lm_model, test_data)
  
  ### print the MSE
  mean((lm_pred_test - test_data$Happiness.Score)^2)
  mean((lm_pred_train - train_data$Happiness.Score)^2)
  
  ### draw the 4 residual plots in one plot
  par(mfrow = c(2, 2))
  plot(lm_model)
  
  
  ## SVR regression
  ### use the e1071 package
  library(e1071)
  
  ### use the model to predict the training data
  svr_model = svm(Happiness.Score ~ ., data = train_data)
  svr_pred_train = predict(svr_model, train_data)
  
  ### use the model to predict the testing data
  svr_pred_test = predict(svr_model, test_data)
  
  ### print the MSE
  mean((svr_pred_test - test_data$Happiness.Score)^2)
  mean((svr_pred_train - train_data$Happiness.Score)^2)
  
  # Causual Inference
  ## prepare the data
  ### merge the happiness data and the factor scores
  mle_model_2_minres_pro_scores$Happiness.Rank = NULL
  happiness = cbind(happiness, mle_model_2_minres_pro_scores)
  
  ### split the data into different dataframes based on the year and the region
  happiness_developed_before_COVID = happiness[happiness$Year <= 2019 & happiness$Region %in% c("Western Europe", "North America", "Australia and New Zealand", "North America and ANZ"), ]
  happiness_developed_after_COVID = happiness[happiness$Year >= 2020 & happiness$Region %in% c("Western Europe", "North America", "Australia and New Zealand", "North America and ANZ"), ]
  happiness_developing_before_COVID = happiness[happiness$Year <= 2019 & !happiness$Region %in% c("Western Europe", "North America", "Australia and New Zealand", "North America and ANZ"), ]
  happiness_developing_after_COVID = happiness[happiness$Year >= 2020 & !happiness$Region %in% c("Western Europe", "North America", "Australia and New Zealand", "North America and ANZ"), ]
  
  ### set the treatment(use the factor1 score as the treatment, median as the threshold)
  treatment_threshold = median(happiness_developed_before_COVID$MR1)
  happiness_developed_before_COVID$treatment = ifelse(happiness_developed_before_COVID$MR1 >= treatment_threshold, 1, 0)
  happiness_developed_before_COVID$treatment = factor(happiness_developed_before_COVID$treatment, levels = c(0, 1))
  happiness_developed_after_COVID$treatment = ifelse(happiness_developed_after_COVID$MR1 >= treatment_threshold, 1, 0)
  happiness_developed_after_COVID$treatment = factor(happiness_developed_after_COVID$treatment, levels = c(0, 1))
  happiness_developing_before_COVID$treatment = ifelse(happiness_developing_before_COVID$MR1 >= treatment_threshold, 1, 0)
  happiness_developing_before_COVID$treatment = factor(happiness_developing_before_COVID$treatment, levels = c(0, 1))
  happiness_developing_after_COVID$treatment = ifelse(happiness_developing_after_COVID$MR1 >= treatment_threshold, 1, 0)
  happiness_developing_after_COVID$treatment = factor(happiness_developing_after_COVID$treatment, levels = c(0, 1))
  
  ## check the balance of the data
  ggplot(happiness_developed_before_COVID, aes(x = treatment, y = MR2)) +
    geom_boxplot() +
    ggtitle("Developed Countries Before COVID")
  
  ggplot(happiness_developed_after_COVID, aes(x = treatment, y = MR2)) +
    geom_boxplot() +
    ggtitle("Developed Countries After COVID")
  
  ggplot(happiness_developing_before_COVID, aes(x = treatment, y = MR2)) +
    geom_boxplot() +
    ggtitle("Developing Countries Before COVID")
  
  ggplot(happiness_developing_after_COVID, aes(x = treatment, y = MR2)) +
    geom_boxplot() +
    ggtitle("Developing Countries After COVID")
  
  ## SRE model
  ### Neyman's approach
  y_developed_before_COVID_t = happiness_developed_before_COVID[happiness_developed_before_COVID$treatment == 1, ]$Happiness.Score
  y_developed_before_COVID_c = happiness_developed_before_COVID[happiness_developed_before_COVID$treatment == 0, ]$Happiness.Score
  ate_developed_before_COVID_hat = mean(y_developed_before_COVID_t) - mean(y_developed_before_COVID_c)
  var_developed_before_COVID_hat = var(y_developed_before_COVID_t) / length(y_developed_before_COVID_t) + var(y_developed_before_COVID_c) / length(y_developed_before_COVID_c)
  
  print(paste("The ATE of developed countries before COVID is", ate_developed_before_COVID_hat))
  print(paste("The 95% confidence interval of developed countries before COVID is [", ate_developed_before_COVID_hat - 1.96 * sqrt(var_developed_before_COVID_hat), ",", ate_developed_before_COVID_hat + 1.96 * sqrt(var_developed_before_COVID_hat), "]"))
  
  y_developed_after_COVID_t = happiness_developed_after_COVID[happiness_developed_after_COVID$treatment == 1, ]$Happiness.Score
  y_developed_after_COVID_c = happiness_developed_after_COVID[happiness_developed_after_COVID$treatment == 0, ]$Happiness.Score
  ate_developed_after_COVID_hat = mean(y_developed_after_COVID_t) - mean(y_developed_after_COVID_c)
  var_developed_after_COVID_hat = var(y_developed_after_COVID_t) / length(y_developed_after_COVID_t) + var(y_developed_after_COVID_c) / length(y_developed_after_COVID_c)
  
  print(paste("The ATE of developed countries after COVID is", ate_developed_after_COVID_hat))
  print(paste("The 95% confidence interval of developed countries after COVID is [", ate_developed_after_COVID_hat - 1.96 * sqrt(var_developed_after_COVID_hat), ",", ate_developed_after_COVID_hat + 1.96 * sqrt(var_developed_after_COVID_hat), "]"))
  
  y_developing_before_COVID_t = happiness_developing_before_COVID[happiness_developing_before_COVID$treatment == 1, ]$Happiness.Score
  y_developing_before_COVID_c = happiness_developing_before_COVID[happiness_developing_before_COVID$treatment == 0, ]$Happiness.Score
  ate_developing_before_COVID_hat = mean(y_developing_before_COVID_t) - mean(y_developing_before_COVID_c)
  var_developing_before_COVID_hat = var(y_developing_before_COVID_t) / length(y_developing_before_COVID_t) + var(y_developing_before_COVID_c) / length(y_developing_before_COVID_c)
  
  print(paste("The ATE of developing countries before COVID is", ate_developing_before_COVID_hat))
  print(paste("The 95% confidence interval of developing countries before COVID is [", ate_developing_before_COVID_hat - 1.96 * sqrt(var_developing_before_COVID_hat), ",", ate_developing_before_COVID_hat + 1.96 * sqrt(var_developing_before_COVID_hat), "]"))
  
  y_developing_after_COVID_t = happiness_developing_after_COVID[happiness_developing_after_COVID$treatment == 1, ]$Happiness.Score
  y_developing_after_COVID_c = happiness_developing_after_COVID[happiness_developing_after_COVID$treatment == 0, ]$Happiness.Score
  ate_developing_after_COVID_hat = mean(y_developing_after_COVID_t) - mean(y_developing_after_COVID_c)
  var_developing_after_COVID_hat = var(y_developing_after_COVID_t) / length(y_developing_after_COVID_t) + var(y_developing_after_COVID_c) / length(y_developing_after_COVID_c)
  
  print(paste("The ATE of developing countries after COVID is", ate_developing_after_COVID_hat))
  print(paste("The 95% confidence interval of developing countries after COVID is [", ate_developing_after_COVID_hat - 1.96 * sqrt(var_developing_after_COVID_hat), ",", ate_developing_after_COVID_hat + 1.96 * sqrt(var_developing_after_COVID_hat), "]"))
  
  ### Regression approach
  regression_developed_before_COVID = lm(Happiness.Score ~ treatment + MR2, data = happiness_developed_before_COVID)
  summary(regression_developed_before_COVID)
  
  regression_developed_after_COVID = lm(Happiness.Score ~ treatment + MR2, data = happiness_developed_after_COVID)
  summary(regression_developed_after_COVID)
  
  regression_developing_before_COVID = lm(Happiness.Score ~ treatment + MR2, data = happiness_developing_before_COVID)
  summary(regression_developing_before_COVID)
  
  regression_developing_after_COVID = lm(Happiness.Score ~ treatment + MR2, data = happiness_developing_after_COVID)
  summary(regression_developing_after_COVID)