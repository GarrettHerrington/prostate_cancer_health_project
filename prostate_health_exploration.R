library(tidyverse)
library(ggplot2)
library(nnet)
patients_df <- read.csv('/Users/rworkspace/Downloads/synthetic_prostate_cancer_risk.csv')

glimpse(patients_df)

head(patients_df)

?factor

patients_df$risk_level <- factor(patients_df$risk_level, 
                                 levels = c("Low", "Medium", "High"))




#First I want to examine how many are in each category to get a general feel for how much each level contains

ggplot(data = patients_df,
       mapping = aes(x = risk_level, fill = risk_level)
       ) + 
        geom_bar() + 
        geom_text(stat = 'count', aes(label = ..count..),
                  vjust = -0.3, size = 3) +
        labs(
          x = "Risk Category",
          y = "Number of Patients",
          fill = "Risk Level",
          title = "Amount of Patients Per Risk Level"
        )
        scale_fill_manual(values = c("Low" = 'blue',
                                     'Medium' = 'green',
                                     'High' = 'orange')) +
        theme_minimal()
        
levels(patients_df$risk_level)
# Now I would like to examine each risk level individually to get a sense of common factors of each

patients_df_low <- patients_df %>% filter(risk_level == 'Low')
glimpse(patients_df_low)

patients_df_medium <- patients_df %>% filter(risk_level == 'Medium')


glimpse(patients_df_medium)

patients_df_high <- patients_df %>% filter(risk_level == 'High')
glimpse(patients_df_high)


ggplot(patients_df_low, mapping = aes(x = age, y = bmi)) +
         geom_col()

ggplot(patients_df_medium, mapping = aes(x = age, y = bmi)) +
  geom_col()

ggplot(patients_df_high, mapping = aes(x = age, y = bmi)) +
  geom_col()

#There is no apparent coorrelation between bmi and age, probably because of synthetic data

patients_df_low$risk_level <- factor(patients_df_low$risk_level, 
                                 levels = c("Low", "Medium", "High"))



ggplot(patients_df_low, aes(x = mental_stress_level, y = bmi)) +
  geom_col() 
  
patients_df_low$risk_level <- factor(
  patients_df_low$mental_stress_level,
  levels = c("Low", "Medium", "High") 
)


ggplot(patients_df_low, aes(x = risk_level, y = mental_stress_level)) +
  geom_col()

#A cursory glance at the plot demonstrates a correlation between mental stress level and risk level

glimpse(patients_df)


ggplot(patients_df_low, aes(x = age, y = bmi)) +
  geom_col()

ggplot(patients_df_medium, aes(x = age, y = bmi)) +
  geom_col()

ggplot(patients_df_high, aes(x = age, y = bmi)) +
  geom_col()

#it appears there is some semblance of a bell curve in the low and high categories for
#correlation between bmi and age

glimpse(patients_df)



patients_df %>%
  group_by(diet_type) %>%
  summarize(avg_bmi = mean(bmi, na.rm = TRUE)) %>%
  ggplot(aes(x = diet_type, y = avg_bmi)) +
  geom_col()

#This graph shows that the number of people who are fatty, healthy, and mixed
#is the same across bmi, this would not make sense in a real world study



# Using a multinomial logistic regression model (Risk level is categorical)

#First going to factor all categorical data

patients_df$risk_level <- factor(patients_df$risk_level)

patients_df$smoker <- factor(patients_df$smoker)
patients_df$alcohol_consumption <- factor(patients_df$alcohol_consumption)
patients_df$diet_type <- factor(patients_df$diet_type)
patients_df$physical_activity_level <- factor(patients_df$physical_activity_level)
patients_df$family_history <- factor(patients_df$family_history)
patients_df$mental_stress_level <- factor(patients_df$mental_stress_level)
patients_df$regular_health_checkup <- factor(patients_df$regular_health_checkup)
patients_df$prostate_exam_done <- factor(patients_df$prostate_exam_done)

model <- multinom(
  risk_level ~ age + bmi + smoker + alcohol_consumption + 
    diet_type + physical_activity_level + family_history +
    mental_stress_level + sleep_hours + regular_health_checkup + 
    prostate_exam_done,
  data = patients_df
)

summary(model)

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

#This model shows P values that show that all variables show a signifcant correlation to medium and high risk prostate cancer


#visualizing the predicted probablities, beginning with adding predicted probabilites to the dataframe
pred_probs <- predict(model, type = "probs")  # matrix of probabilities
head(pred_probs)

patients_df$prob_low    <- pred_probs[, "Low"]
patients_df$prob_medium <- pred_probs[, "Medium"]
patients_df$prob_high   <- pred_probs[, "High"]


patients_long <- patients_df %>% 
  select(bmi, prob_low, prob_medium, prob_high) %>% 
  pivot_longer(cols = starts_with("prob_"),
               names_to = 'predicted_risk_level',
               values_to = 'probability')

patients_long$predicted_risk_level <- factor(
  patients_long$predicted_risk_level,
  levels = c('prob_low', 'prob_medium', 'prob_high'),
  labels = c('Low', 'Medium', 'High')
)

ggplot(patients_long, aes(x = bmi, y = probability, color = predicted_risk_level)) +
  geom_line(stat = "smooth", method = "loess", se = FALSE) +
  labs(
    title = "Predicted Probability of Risk Level by BMI",
    x = "BMI",
    y = "Predicted Probability",
    color = "Risk Level"
  ) +
  theme_minimal()

glimpse(patients_df)

#doing the same with age
patients_long <- patients_df %>% 
  select(age, prob_low, prob_medium, prob_high) %>% 
  pivot_longer(cols = starts_with("prob_"),
               names_to = 'predicted_risk_level',
               values_to = 'probability')

patients_long$predicted_risk_level <- factor(
  patients_long$predicted_risk_level,
  levels = c('prob_low', 'prob_medium', 'prob_high'),
  labels = c('Low', 'Medium', 'High')
)

ggplot(patients_long, aes(x = age, y = probability, color = predicted_risk_level)) +
  geom_line(stat = "smooth", method = "loess", se = FALSE) +
  labs(
    title = "Predicted Probability of Risk Level by BMI",
    x = "AGE",
    y = "Predicted Probability",
    color = "Risk Level"
  ) +
  theme_minimal()

# The graph for low risk is interesting, if this were real data
#it may be because of a genetic predisposition

glimpse(patients_df)

#For the smoker category I will use binary logistic regression


#
patients_df$smoker <- factor(patients_df$smoker, levels = c("No", "Yes"))


#Fit model
model <- glm(risk_flag ~ smoker, data = patients_df, family = binomial)








