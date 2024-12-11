rm(list=ls())
# install.packages("sf")
# install.packages("tigris")
library(tidyverse) #includes ggplot2 and dplyer
library(ggthemes) #optional 
library(logistf)#firth's penalized
library(glmnet) # for fitting lasso, ridge regressions (GLMs)
library(haven) #for reading in SAS data exports
library(randomForest)
library(pROC)
library(sf)
library(tigris)

source("code/clean_acs.R")#don't clear workspace in clean_acs!
source("code/clean_cps.R")#don't clear workspace in clean_cps!

# Looking at FSFoods
summary(cps_data)

# Removing NA's from FSFoods
cps_data <- cps_data %>% 
  filter(!is.na(FSFOODS)) %>%
  select(FSFOODS, hhsize, female, hispanic, black, kids, elderly, 
         education, married, weight, p.female, p.hispanic, p.black, p.kids, 
         p.elderly)

# Split Data into Train and Test----
RNGkind(sample.kind = "default")
set.seed(11252024)
train.idx <- sample(x = 1:nrow(cps_data), size = 0.8*nrow(cps_data))
train.df <- cps_data[train.idx, ]
test.df <- cps_data[-train.idx, ]

# First forest
train.df$FSFOODS <- as.factor(train.df$FSFOODS)
test.df$FSFOODS <- as.factor(test.df$FSFOODS)
myforest <- randomForest(FSFOODS ~ hhsize + female + hispanic + black +
                           kids + elderly + education + married + p.female +
                           p.hispanic + p.black + p.kids + p.elderly,
                         data = train.df,
                         ntree = 1000, 
                         mtry = 3,
                         importance = TRUE)

myforest
# .12637 mean of squared residuals
# 27.68 % of Var Explained
# This is not very good, so let's not use a tree for this.

# Working on Bernoulli Regression ----

varImpPlot(myforest, type = 1)

# So let's first start looking at the importance of each variable
# It looks like Education is by far the most important variable
# then married, hispanic, black, kids, hhsize, female, and elderly.
# So let's start adding those in one by one and checking AIC to find
# our best model.

# For Regression Models (MLE, Lasso, Ridge)
train.df$FSFOODS <- as.numeric(as.character(train.df$FSFOODS))
test.df$FSFOODS <- as.numeric(as.character(test.df$FSFOODS))

# Start by fitting a general MLE
lr_mle <- glm(FSFOODS ~ hhsize + female + hispanic + black +
                kids + elderly + education + married + p.female +
                p.hispanic + p.black + p.kids + p.elderly,
              data = train.df,
              weights = weight,
              family = binomial(link="logit"))

# Let's look at the coefficients
beta <- coef(lr_mle)

# Now let's make our train and test data into matrices so that we 
# can use lasso and ridge regressions
x.train <- model.matrix(FSFOODS ~ hhsize + female + hispanic + black +
                          kids + elderly + education + married + p.female +
                          p.hispanic + p.black + p.kids + p.elderly, data = train.df)[,-1]
x.test <- model.matrix(FSFOODS ~ hhsize + female + hispanic + black +
                         kids + elderly + education + married + p.female +
                         p.hispanic + p.black + p.kids + p.elderly, data = test.df)[,-1]

#need y variables to be represented as vectors
#note that x.train and x.test does not have the y variable
y.train <- as.vector(train.df$FSFOODS)
y.test <- as.vector(test.df$FSFOODS)

# Ok now let's test out a bunch of lambda values for lasso and ridge

lr_lasso_cv <- cv.glmnet(x.train, #this is the x MATRIX (not data frame)
                         y.train, #this is the y VECTOR
                         family = binomial(link="logit"),
                         weights = as.integer(train.df$weight),
                         alpha =1) #1 for lasso

lr_ridge_cv <- cv.glmnet(x.train, #this is the x MATRIX (not data frame)
                         y.train, #this is the y VECTOR
                         family = binomial(link="logit"),
                         weights = as.integer(train.df$weight),
                         alpha =0) #0 for ridge

# Let's look at those lambda values and how it turned out
plot(lr_lasso_cv)
plot(lr_ridge_cv)

# Now let's pick out the optimal values for lambda
best_lasso_lambda <- lr_lasso_cv$lambda.min 
best_ridge_lambda <- lr_ridge_cv$lambda.min 

# So let's compare these models some
lr_lasso_coefs <- coef(lr_lasso_cv, s="lambda.min") %>% as.matrix
lr_ridge_coefs <- coef(lr_ridge_cv, s="lambda.min") %>% as.matrix

ggplot()+
  geom_point(aes(lr_ridge_coefs, lr_lasso_coefs))+
  geom_abline(aes(slope=1, intercept=0))

#Fit 'final' ridge and lasso regression models
final_lasso <- glmnet(x.train,
                      y.train,
                      family = binomial(link="logit"),
                      weights = as.integer(train.df$weight),
                      alpha = 1, #1 for lasso
                      lambda = best_lasso_lambda) #lambda with both a 'b' and also a 'd'

final_ridge <- glmnet(x.train,
                      y.train,
                      family = binomial(link="logit"),
                      weights = as.integer(train.df$weight),
                      alpha = 0, #0 for ridge
                      lambda = best_ridge_lambda)

# Create a new data frame with our updated predictions
test.df.preds <- test.df %>% 
  mutate(
    mle_pred = predict(lr_mle, test.df, type = "response"),
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
  )

# Now let's figure out which one is the BEST
mle_rocCurve <- roc(response = as.factor(test.df.preds$FSFOODS),
                    predictor = test.df.preds$mle_pred, #predicted probs
                    levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSFOODS),
                      predictor = test.df.preds$lasso_pred, #predicted probs
                      levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSFOODS),
                      predictor = test.df.preds$ridge_pred, #predicted probs
                      levels = c("0", "1"))

plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .758
# .264(.779, .613)
plot(mle_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .758
# .228(.711, .685) 
plot(ridge_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .757
# .259(.776, .616)

#make data frame of MLE ROC info
mle_data <- data.frame(
  Model = "MLE",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(mle_data, lasso_data, ridge_data)


# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()


# Fitting onto ACS data ----
acs_data <- acs_data %>% 
  select(PUMA, hhsize, female, hispanic, black, kids, 
         elderly, education, married, weight, p.female,
         p.hispanic, p.black, p.kids, p.elderly)


#Prepare data for lasso/ridge by making test/train matrices 

x.acs <- model.matrix(weight ~ hhsize + female + hispanic + black + kids + 
                        elderly + education + married + p.female +
                        p.hispanic + p.black + p.kids + p.elderly, data = acs_data)[,-1]

# Make predictions based on CPS models
acs_data_preds <- acs_data %>%
  mutate(
    mle_pred = predict(lr_mle, newdata = acs_data, type = "response"),
    lasso_pred = predict(final_lasso, x.acs, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.acs, type = "response")[,1]
  )


# Aggregate acs household predictions to PUMA level

acs_data_agg <- acs_data_preds %>% 
  filter(elderly >=1) %>% 
  group_by(PUMA) %>% 
  summarise(meanlasso = weighted.mean(x = lasso_pred, w = weight),
            meanridge = weighted.mean(x = ridge_pred, w = weight))

# Choropleth Map for PUMA level
# Used ChatGPT here for this code

options(tigris_use_cache = TRUE) # Cache files for faster processing
iowa_pumas <- tigris::pumas(state = "IA", year = 2022) # Adjust year as needed

acs_data_agg <- acs_data_agg %>% 
  mutate(PUMA = as.character(PUMA)) # Convert to character for joining

# Join shapefile with your data 
# MAY NEED TO CHANGE GEOID10 
iowa_map_data <- iowa_pumas %>%
  mutate(PUMA = as.character(GEOID20)) %>% # GEOID20 contains PUMA codes
  left_join(acs_data_agg, by = "PUMA")

ggplot(data = iowa_map_data) +
  geom_sf(aes(fill = meanlasso), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", 
                       name = "Proportion of Food \nInsecure Seniors", 
                       direction = -1) +
  labs(
    title = "Proportion of Seniors Predicted to Experience Food Insecurity Among Iowa Seniors",
    subtitle = "Aggregated by PUMA Level in Iowa",
    caption = "Data Source: ACS & CPS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#Counts of Food Insecurity
senior_counts <- read.csv("data/total_iowa_seniors_by_puma.csv")
senior_counts <- senior_counts  %>%
  mutate(PUMA = as.character(GEOID)) %>%
  select(-GEOID)


iowa_map_data <- iowa_map_data %>%
  left_join(senior_counts, by = "PUMA")  %>%
  mutate(number_insecure = meanlasso * senior_population)

ggplot(data = iowa_map_data) +
  geom_sf(aes(fill = number_insecure), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", name = "Number of \nSeniors", 
                       direction = -1) +
  labs(
    title = "Estimated Number of Food Insecure Seniors in Iowa",
    subtitle = "Aggregated Predictions by Region (PUMA)",
    caption = "Data Source: ACS & CPS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Interpretations ----

coefficients <- coef(final_lasso, s = best_lasso_lambda)
coefficients

#(Intercept) -1.01453309
#hhsize       0.13379372
#female       0.14363423
#hispanic    -0.09318695
#black        .         
#kids         0.00905490
#elderly      .         
#education   -0.48856899
#married     -0.38818573
#p.female     0.04787603
#p.hispanic   0.95596428
#p.black      0.48478304
#p.kids       .         
#p.elderly   -0.34404251

exp(-(-0.09318695*5 + 0.13379372*5) +(0.13379372*4))
# 1.393949 odds of a hispanic family of 5 with 2 women not having enough and kinds 
# of food compared to a non-hispanic household of 4 with 2 women
# odds increase by 39.3949%
# We would expect a hispanic family of 5 to be 39.3949% more food insecure then 
# a non-hispanic family of 4. 
# For example, if a white family of 4 has issues with food insecurity and 
# options half of the time (50%), then a hispanic family of 5 would have the
# same issues 69.7% of the time.

# Odds for hispanic and non-hispanic households
hispanic_df <- data.frame(
  hispanic = c(0, 1), # 0 for non-hispanic, 1 for hispanic
  odds = exp(-0.09318695 * 5 + 0.13379372 * 5 * c(0, 1)) # Apply hispanic effect
)

# Plot
ggplot(hispanic_df, aes(x = as.factor(hispanic), y = odds)) +
  geom_bar(stat = "identity", fill = "darkred", width = 0.5) +
  labs(
    x = "Hispanic (0 = Non-Hispanic, 1 = Hispanic)",
    y = "Odds of Food Insecurity",
    title = "Effect of Hispanic on Food Insecurity"
  ) +
  theme_minimal()


exp(0.13379372)
# 1.143157 odds of not having enough and variation in food
# increase for each additional person in the household
# We would expect any additional person in a household to increase the odds
# of that family being food insecure by a factor of 14.32%. 
# That is to say, if a family of 5 experiences food insecurity or lack of diet 
# diversity 70% of the time, we would expect a family of 6 to experience the
# same 80% of the time.

# Odds for varying household sizes (from 1 to 6)
hhsize_df <- data.frame(
  hhsize = 1:6, # Household size from 1 to 6
  odds = exp(0.13379372 * (1:6)) # Apply household size effect
)

# Plot
ggplot(hhsize_df, aes(x = hhsize, y = odds)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "blue", size = 3) +
  labs(
    x = "Household Size",
    y = "Odds of Food Insecurity",
    title = "Effect of Household Size on Food Insecurity"
  ) +
  theme_minimal()


exp(0.14363423)
# 1.154462 odds of not having enough and variation in food increase as the # of 
# females in the house increases by 1
# We would expect the odds of food insecurity or lack of food diversity to increase
# by 15.45% for each additional female in the household. 
# For example, if a family of 5 with no women experiences food insecurity 20% 
# of the time, we would expect a family of 5 with 1 women to experience
# food insecurity 23% of the time.

# Odds for varying number of females in the household
female_df <- data.frame(
  females = 0:6, # Number of females from 0 to 6
  odds = exp(0.14363423 * (0:6)) # Apply female effect
)

# Plot
ggplot(female_df, aes(x = females, y = odds)) +
  geom_line(color = "purple", size = 1.5) +
  geom_point(color = "purple", size = 3) +
  labs(
    x = "Number of Females in Household",
    y = "Odds of Food Insecurity",
    title = "Effect of Females in Household on Food Insecurity"
  ) +
  theme_minimal()


# Used ChatGPT to assist in making these plots for the interpretations

  






