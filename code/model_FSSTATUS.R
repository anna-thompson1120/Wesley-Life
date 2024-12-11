rm(list=ls())
library(tidyverse) #includes ggplot2 and dplyer
library(ggthemes) #optional 
library(logistf)#firth's penalized
library(glmnet) # for fitting lasso, ridge regressions (GLMs)
library(haven) #for reading in SAS data exports

source("code/clean_acs.R")
source("code/clean_cps.R")


# Y = FSSTATUS
# FSSTATUS = Household food security scale
# Households are classified as food secure, low food secure, or very low food secure.
# 01	Food secure
# 02	Low food secure
# 03	Very low food secure
# 98	No response
# 99	NIU

cps_data <- cps_data %>% 
  filter(!is.na(FSSTATUS)) %>%
  select(FSSTATUS, hhsize, female, hispanic, black, kids, elderly, 
         education, married, weight, p.female, p.hispanic, p.black, p.kids, 
         p.elderly)


# Split Data into Train and Test----
RNGkind(sample.kind = "default")
set.seed(11252024)
train.idx <- sample(x = 1:nrow(cps_data), size = 0.8*nrow(cps_data))
train.df <- cps_data[train.idx, ]
test.df <- cps_data[-train.idx, ]

# First forest
train.df$FSSTATUS <- as.factor(train.df$FSSTATUS)
test.df$FSSTATUS <- as.factor(test.df$FSSTATUS)
myforest <- randomForest(FSSTATUS ~ hhsize + female + hispanic + black +
                           kids + elderly + education + married + p.female +
                           p.hispanic + p.black + p.kids + p.elderly,
                         data = train.df,
                         ntree = 1000, 
                         mtry = 3,
                         importance = TRUE)

myforest
# OOB Estimate of error rate: 8.48%


# Working on Bernoulli Regression ----

varImpPlot(myforest, type = 1)

# So let's first start looking at the importance of each variable
# It looks like Education is by far the most important variable
# then married, hispanic, black, kids, hhsize, female, and elderly.
# So let's start adding those in one by one and checking AIC to find
# our best model.



# For Regression Models (MLE, Lasso, Ridge)
train.df$FSSTATUS <- as.numeric(as.character(train.df$FSSTATUS))
test.df$FSSTATUS <- as.numeric(as.character(test.df$FSSTATUS))


# Start by fitting a general MLE
lr_mle <- glm(FSSTATUS ~ hhsize + female + hispanic + black +
                kids + elderly + education + married + p.female +
                p.hispanic + p.black + p.kids + p.elderly,
              data = train.df,
              weights = weight,
              family = binomial(link="logit"))

# Let's look at the coefficients
beta <- coef(lr_mle)

# Now let's make our train and test data into matrices so that we 
# can use lasso and ridge regressions
x.train <- model.matrix(FSSTATUS ~ hhsize + female + hispanic + black +
                          kids + elderly + education + married + p.female +
                          p.hispanic + p.black + p.kids + p.elderly, data = train.df)[,-1]
x.test <- model.matrix(FSSTATUS ~ hhsize + female + hispanic + black +
                         kids + elderly + education + married + p.female +
                         p.hispanic + p.black + p.kids + p.elderly, data = test.df)[,-1]

#need y variables to be represented as vectors
#note that x.train and x.test does not have the y variable
y.train <- as.vector(train.df$FSSTATUS)
y.test <- as.vector(test.df$FSSTATUS)

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
mle_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUS),
                    predictor = test.df.preds$mle_pred, #predicted probs
                    levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUS),
                      predictor = test.df.preds$lasso_pred, #predicted probs
                      levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUS),
                      predictor = test.df.preds$ridge_pred, #predicted probs
                      levels = c("0", "1"))

plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .782
# .141(0.744, 0.690)
plot(mle_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .564
# .500(0.919, 0.209)
plot(ridge_rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = .781
# .148(0.781, 0.663)

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
         elderly, education, married, weight, p.female, p.hispanic, p.black, p.kids, 
         p.elderly)


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

#(Intercept) -1.79455726
#hhsize       0.11944880
#female       .         
#hispanic    -0.09263093
#black       -0.05230111
#kids         0.19354635
#elderly      0.06437901
#education   -0.76178987
#married     -0.48734668
#p.female     0.44188401
#p.hispanic   0.95057318
#p.black      0.97207107
#p.kids       0.18127073
#p.elderly   -0.64764494

exp(-0.64764494*.2)
# 0.8785091 odds of food insecurity increasing as the % of elderly in the house 
# increases by 20%. For example, in a household of 5 people if 1 person turned 61

0.8785091/(1+0.8785091)
# 46.76629% times less likely
# As the percentage of people over 60 in a household increases by 20 percent, 
# we would expect to see the odds of any level of food insecurity to decrease by
# 46.76629%.
# For example, if a household of 5 people had a 75% chance of being food insecure,
# as one member turned 61 they would have a 35% chance of being food insecure.

# Baseline odds (at p.elderly = 0)
baseline_odds <- exp(-1.79455726)

# Create a data frame for plotting `p.elderly`
elderly_df <- data.frame(
  p_elderly = seq(0, 1, by = 0.05), # Percentages from 0% to 100%
  odds = baseline_odds * exp(-0.64764494 * seq(0, 1, by = 0.05))
)

# Plot
ggplot(elderly_df, aes(x = p_elderly, y = odds)) +
  geom_line(colour = "darkblue", size = 1) +
  labs(
    x = "Percentage of Elderly in Household",
    y = "Odds of Food Insecurity",
    title = "Effect of Elderly Percentage on Food Insecurity"
  ) +
  theme_minimal()


exp(-0.76178987)
# 0.4668301 odds of food insecurity as the # of people in the household holding a degree 
# increases by 1
0.4668301/(1+0.4668301)
# 31.82578% times decrease by
# As one additional person in the household gains a post high school degree,
# we would expect the odds of the household experiencing any level of food 
# insecurity to decrease by a factor of 31.82578%.

# Create a data frame for plotting `education`
education_df <- data.frame(
  num_educated = 0:5, # Number of educated individuals
  odds = exp(-0.76178987 * (0:5))
)

# Plot
ggplot(education_df, aes(x = num_educated, y = odds)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    x = "Number of Educated Individuals in Household",
    y = "Odds of Food Insecurity",
    title = "Effect of Education on Food Insecurity"
  ) +
  theme_minimal()


# married
exp(-0.48734668)
# the odds of food insecurity for married households are 0.614 times 
# the odds of food insecurity for non-married households.
# that is the odds of food insecurity decrease by 38.6% for married households 
# compared to non-married households, assuming all other factors are held constant.

# Baseline odds (when married = 0, i.e., unmarried)
baseline_odds <- exp(-1.79455726) # Intercept

# Create a data frame for plotting `married` effect
married_df <- data.frame(
  married = c(0, 1), # 0 for unmarried, 1 for married
  odds = baseline_odds * exp(-0.48734668 * c(0, 1)) # Apply coefficient for married
)

# Plot
ggplot(married_df, aes(x = as.factor(married), y = odds)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  labs(
    x = "Married (0 = Unmarried, 1 = Married)",
    y = "Odds of Food Insecurity",
    title = "Effect of Being Married on Food Insecurity"
  ) +
  theme_minimal()

# Used ChatGPT to assist in making the plots for these interpretations


