# Set R to always use binary packages when available
options(install.packages.check.source = "no")

# Proceed with installation
required_packages <- c(
  "ggplot2", "glmnet", "MASS", "stargazer", "broom", 
  "FactoMineR", "factoextra", "mgcv", "nls", 
  "randomForest", "caret", "data.table"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# Load the database
articles_all = read.csv("data/phpgBMvy4.csv")

# Pre-visualize the data
head(articles_all)

# Remove articles that are very recent (less than 3 weeks)
articles_all = articles_all[articles_all["timedelta"]>21,]

# Remove useless columns
articles = articles_all[,-which(colnames(articles_all) %in% c("url","timedelta","weekday_is_sunday","is_weekend"))]

# Define categorical and numerical variable names
categorials_names = c("data_channel_is_lifestyle","data_channel_is_entertainment","data_channel_is_bus","data_channel_is_socmed",
                      "data_channel_is_tech","data_channel_is_world","weekday_is_monday","weekday_is_monday","weekday_is_tuesday",
                      "weekday_is_wednesday","weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday",
                      "is_weekend")

numericals_names = c("n_tokens_title","n_tokens_content","n_unique_tokens","n_non_stop_words",
                     "n_non_stop_unique_tokens","num_hrefs","num_self_hrefs","num_imgs","num_videos","average_token_length",
                     "num_keywords","kw_min_min","kw_max_min","kw_avg_min","kw_min_max","kw_max_max","kw_avg_max","kw_min_avg",
                     "kw_max_avg","kw_avg_avg","self_reference_min_shares","self_reference_max_shares","self_reference_avg_sharess",
                     "LDA_00","LDA_01","LDA_02","LDA_03","LDA_04","global_subjectivity","global_sentiment_polarity","global_rate_positive_words",
                     "global_rate_negative_words","rate_positive_words","rate_negative_words","avg_positive_polarity","min_positive_polarity",
                     "max_positive_polarity","avg_negative_polarity","min_negative_polarity","max_negative_polarity","title_subjectivity",
                     "title_sentiment_polarity","abs_title_subjectivity","abs_title_sentiment_polarity","shares")

# Compute the variance of the variables
variances = diag(var(articles[,numericals_names]))

# Select those which have a great variance and log them
too_much_variation = names(variances[variances>1000])
for (name in too_much_variation){
  articles[articles[,name]>0,name]=log(articles[articles[,name]>0,name])
  articles[articles[,name]<0,name]=-log(abs(articles[articles[,name]<0,name]))
}

# Number of variables which were logged
print(length(too_much_variation))

# Split dataset into train_set, test_set
set.seed(1)
random_selection = runif(nrow(articles))
train_set = articles[random_selection<=0.7,]
test_set = articles[random_selection>0.7,]
Y_train = train_set[,"shares"]
X_train = train_set[,-which(colnames(train_set)=="shares")]

# Function to compute RMSE
get_rmse = function(model,newdata=test_set){
  pred = predict(model,newdata=newdata)
  mse = sqrt(mean((pred-newdata$shares)^2))
  mse
}

# Initialize vectors for storing results
rmse = 1:3
aic = 1:3
nb_var_selected = 1:3

# Linear Models
# Full model
reg.model =lm(formula = shares~. , data = train_set)

# Forward looking
reg.ols_fw = stepAIC(lm(shares~1 , data =train_set ),direction = "forward",
                     trace = FALSE,scope=formula(reg.model))
summary(reg.ols_fw)
rmse[1]=get_rmse(reg.ols_fw,test_set)
nb_var_selected[1]= length(reg.ols_fw$coefficients) -1

# Backward looking
reg.ols_bw = stepAIC(reg.model,direction = "backward",trace = FALSE)
summary(reg.ols_bw)
rmse[2]=get_rmse(reg.ols_bw)
nb_var_selected[2]= length(reg.ols_bw$coefficients) -1

# Autometrics (Import result from OxMetrics)
autometrics=fread("Variables Coefficient Std.Error t-value t-prob Part.R^2
Constant 6.34564 0.1165 54.5 0.0000 0.0980
num_hrefs 0.00576486 0.0005678 10.2 0.0000 0.0038
num_self_hrefs -0.00895584 0.002069 -4.33 0.0000 0.0007
num_imgs 0.00326124 0.0007059 4.62 0.0000 0.0008
average_token_length -0.0934452 0.009080 -10.3 0.0000 0.0039
data_channel_is_lifestyle -0.160020 0.02543 -6.29 0.0000 0.0014
data_channel_is_entertainment -0.293392 0.01950 -15.0 0.0000 0.0082
data_channel_is_bus -0.275748 0.02435 -11.3 0.0000 0.0047
data_channel_is_world -0.164046 0.02504 -6.55 0.0000 0.0016
kw_min_min -0.0398621 0.004612 -8.64 0.0000 0.0027
kw_avg_min 0.0189611 0.005823 3.26 0.0011 0.0004
kw_max_max -0.0937760 0.01736 -5.40 0.0000 0.0011
kw_avg_max -0.0900276 0.02021 -4.45 0.0000 0.0007
kw_min_avg -0.0113125 0.001788 -6.33 0.0000 0.0015
kw_max_avg -0.116565 0.02566 -4.54 0.0000 0.0008
kw_avg_avg 0.588645 0.03954 14.9 0.0000 0.0081
self_reference_min_shares -0.0527530 0.01199 -4.40 0.0000 0.0007
self_reference_max_shares -0.176154 0.02938 -6.00 0.0000 0.0013
self_reference_avg_sharess 0.257239 0.03920 6.56 0.0000 0.0016
weekday_is_monday -0.209836 0.01973 -10.6 0.0000 0.0041
weekday_is_tuesday -0.292862 0.01921 -15.2 0.0000 0.0084
weekday_is_wednesday -0.273204 0.01922 -14.2 0.0000 0.0073
weekday_is_thursday -0.275569 0.01929 -14.3 0.0000 0.0074
weekday_is_friday -0.219754 0.02037 -10.8 0.0000 0.0042
LDA_00 0.390004 0.04276 9.12 0.0000 0.0030
LDA_02 -0.118139 0.04403 -2.68 0.0073 0.0003
LDA_03 0.0664285 0.03254 2.04 0.0412 0.0002
LDA_04 0.117647 0.03534 3.33 0.0009 0.0004
global_subjectivity 0.375635 0.06422 5.85 0.0000 0.0013
min_positive_polarity -0.339871 0.08008 -4.24 0.0000 0.0007
min_negative_polarity -0.0544420 0.02093 -2.60 0.0093 0.0002
title_subjectivity 0.0918682 0.01926 4.77 0.0000 0.0008
title_sentiment_polarity 0.0700415 0.02085 3.36 0.0008 0.0004
abs_title_subjectivity 0.146428 0.03254 4.50 0.0000 0.0007")

formule_auto = "shares ~"
cols = c(autometrics$Variables)[-1]
for (col in cols){
  formule_auto = paste0(formule_auto,"+",trimws(col))
}

reg.auto = lm(as.formula(formule_auto),data = train_set)
summary(reg.auto)
rmse[3]=get_rmse(reg.auto)
nb_var_selected[3]= length(reg.auto$coefficients) - 1
aic=AIC(reg.ols_fw,reg.ols_bw,reg.auto)$AIC

# Export results
stargazer(reg.ols_fw,reg.ols_bw,reg.auto,
          column.labels = c("Forward","Backward","Autometrics"),
          dep.var.labels = rep("Popularity",3,3),
          add.lines = list(
            c("AIC",round(aic,3)),
            c("RMSE",round(rmse,3)),
            c("Nb var",nb_var_selected)
          ),
          out = "exports/regression_lineaire_autometrics.tex")

# Ridge Regression
reg.ridge = glmnet(X_train,Y_train,family = "gaussian",alpha = 0,standardize = FALSE)
print(reg.ridge$beta)
summary(reg.ridge)
plot(reg.ridge,xvar = "lambda")

# Choose lambda with k-fold validation
reg.ridge_cv = cv.glmnet(model.matrix(shares~., data = train_set),
                         Y_train,family = "gaussian",
                         nfolds = 10,alpha=0,keep = TRUE)
plot(reg.ridge_cv)

# Print various lambda values and MSE
print(min(reg.ridge_cv$cvm))
print(reg.ridge_cv$lambda.min)
print(log(reg.ridge_cv$lambda.min))
print(reg.ridge_cv$lambda.1se)
print(log(reg.ridge_cv$lambda.1se))

# Compare on test sample
mse_ridge = 1:2
lambdas_ridge = c(reg.ridge_cv$lambda.min,reg.ridge_cv$lambda.1se)

# Make predictions
pred.ridge = predict(reg.ridge_cv,newx = model.matrix(shares~.,data=test_set),
                     s=c(reg.ridge_cv$lambda.min,reg.ridge_cv$lambda.1se))

# Get MSE
mse_ridge[1] = sqrt(mean((pred.ridge[,1] - test_set[,"shares"])^2))
mse_ridge[2] = sqrt(mean((pred.ridge[,2] - test_set[,"shares"])^2))

ridge_comp_results = data.frame(lambda=lambdas_ridge,RMSE=mse_ridge)
row.names(ridge_comp_results)=c("Min rule","Std rule")
stargazer(ridge_comp_results,out="ridge_compare_results.tex",summary = FALSE,table.placement = "h",flip = TRUE)

# Train the model with the optimal value
reg.ridge_final = glmnet(X_train,Y_train,family = "gaussian",alpha = 0,lambda =reg.ridge_cv$lambda.min , standardize = FALSE)
summary(reg.ridge_final$beta)
stargazer(reg.ridge_final$beta[,1] ,out="ridge_coefs.tex",flip = TRUE,summary = FALSE,table.placement = "h")

# Lasso Regression
reg.lasso = glmnet(X_train,Y_train,family = "gaussian",alpha = 1,standardize = FALSE)
print(reg.lasso$beta)
summary(reg.lasso)
plot(reg.lasso,xvar = "lambda")

reg.lasso_cv = cv.glmnet(model.matrix(shares~., data = train_set),
                         Y_train,family = "gaussian",
                         nfolds = 10,alpha=1,keep = TRUE)
plot(reg.lasso_cv)

# Print various lambda values and MSE
print(min(reg.lasso_cv$cvm))
print(reg.lasso_cv$lambda.min)
print(log(reg.lasso_cv$lambda.min))
print(reg.lasso_cv$lambda.1se)
print(log(reg.lasso_cv$lambda.1se))

# Compare on test sample
mse_lasso=1:2
pred.lasso = predict(reg.lasso_cv,newx = model.matrix(shares~.,data=test_set),
                     s=c(reg.lasso_cv$lambda.min,reg.lasso_cv$lambda.1se))
mse_lasso[1] = sqrt(mean((pred.lasso[,1] - test_set[,"shares"])^2))
mse_lasso[2] = sqrt(mean((pred.lasso[,2] - test_set[,"shares"])^2))

lambdas_lasso = c(reg.lasso_cv$lambda.min,reg.lasso_cv$lambda.1se)
lasso_comp_results = data.frame(lambda=lambdas_lasso,RMSE=mse_lasso)
row.names(lasso_comp_results)=c("Min rule","Std rule")
stargazer(lasso_comp_results,out="lasso_compare_results.tex",summary = FALSE,table.placement = "h",flip = TRUE)

reg.lasso_final = glmnet(X_train,Y_train,family = "gaussian",
                         alpha = 1,lambda =reg.lasso_cv$lambda.min , standardize = FALSE)

lasso_results = data.frame(Lasso= round(reg.lasso_final$beta[,1],3))
lasso_results[,"var"]= rownames(lasso_results)
lasso_results = data.frame(lasso_results[lasso_results$Lasso!=0,])

ols_results = data.frame(Ols=round(reg.ols_bw$coefficients,3))
ols_results[,"var"] = rownames(ols_results)

results_ols_lasso=merge(ols_results,lasso_results,by="var",all=TRUE)
# Combine OLS and Lasso results
results_ols_lasso <- rbind(results_ols_lasso,
                           c("===========", "===========", "==========="),
                           c("Number of var", nrow(ols_results) - 1, nrow(lasso_results)))

# Export OLS and Lasso comparison
stargazer(results_ols_lasso, out = "ols_lasso_results.tex",
          summary = FALSE, table.placement = "h", rownames = FALSE)

# Principal Component Analysis (PCA)
pca <- PCA(articles[, numericals_names[-which(numericals_names == "shares")]], 
           ncp = length(numericals_names))

# Percentages explained by components
pca.percents <- get_eigenvalue(pca)
stargazer(pca.percents, out = "pca_percents.tex",
          summary = FALSE, table.placement = "h")

# PCA Visualizations
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 11))
fviz_pca_var(pca, col.var = "black")

# Prepare PCA regression data
pca.components <- data.frame(pca$ind$coord[, 1:24])
pca.reg_data <- cbind(shares = articles[, "shares"], pca.components)
pca.reg_full_data <- cbind(pca.reg_data, articles_all[, categorials_names])

# Split PCA data
train_set_pca <- pca.reg_full_data[random_selection <= 0.7, ]
test_set_pca <- pca.reg_full_data[random_selection > 0.7, ]

# PCA regression models
reg.pca_full_model <- lm(shares ~ ., data = train_set_pca)
formula_initial <- formula(lm(shares ~ ., data = pca.reg_data))

# Forward stepwise model for PCA
reg.pca <- stepAIC(lm(formula_initial, data = pca.reg_full_data),
                   direction = "forward",
                   trace = FALSE,
                   scope = formula(reg.pca_full_model))

summary(reg.pca)
pca_mse <- get_rmse(reg.pca, newdata = test_set_pca)

stargazer(reg.pca, out = "pca_regression.tex", table.placement = "h", 
          add.lines = list(c("RMSE", round(pca_mse, 3))))

# Generalized Additive Model (GAM)
formule <- "shares ~ "
for (col in categorials_names[-which(categorials_names %in% c("weekday_is_sunday", "is_weekend"))]) {
  formule <- paste0(formule, "+", col)
}
for (col in numericals_names[-which(numericals_names == "shares")]) {
  formule <- paste0(formule, "+s(", col, ",k=6)")
}

reg.gam <- gam(as.formula(formule), data = train_set)
reg.gam_sum <- summary(reg.gam)

get_rmse(reg.gam)

stargazer(reg.gam_sum$p.table, out = "exports/gam_lineaire.tex", 
          summary = FALSE, table.placement = "h")
stargazer(reg.gam_sum$s.table, out = "exports/gam_function.tex", 
          summary = FALSE, table.placement = "h")

# Plot GAM results
par(mfrow = c(5, 9))
plot(reg.gam, shade = TRUE, shade.col = "lightgreen")
par(mfrow = c(1, 1))

# Compare all models
compare_all <- data.frame(
  models = c("OLS", "Ridge", "Lasso", "PCA", "GAM"),
  RMSE = c(rmse[2], mse_ridge[1], mse_lasso[1], pca_mse, get_rmse(reg.gam)),
  var = c(length(reg.ols_bw$coefficients) - 1, nrow(reg.ridge$beta), 
          nrow(lasso_results), length(reg.pca$coefficients) - 1, 56)
)

# Random Forest
trainOptions <- trainControl(method = "cv", number = 5, search = "grid")
reg.rf <- randomForest(x = X_train, y = Y_train)

# Final comparison of all models
compare_all_final <- data.frame(
  models = c("OLS", "Ridge", "Lasso", "PCA", "GAM", "Random Forest"),
  RMSE = c(rmse[2], mse_ridge[1], mse_lasso[1], pca_mse, get_rmse(reg.gam), get_rmse(reg.rf)),
  var = c(length(reg.ols_bw$coefficients) - 1, nrow(reg.ridge$beta), 
          nrow(lasso_results), length(reg.pca$coefficients) - 1, 56, "-")
)

stargazer(compare_all_final, out = "exports/compare_all_final_models.tex",
          summary = FALSE, table.placement = "h", rownames = FALSE)