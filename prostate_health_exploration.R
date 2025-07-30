library(tidyverse)
install.packages("ggplot2")

patients_df <- read.csv('/Users/rworkspace/Downloads/synthetic_prostate_cancer_risk.csv')

glimpse(patients_df)

head(patients_df)

patients_df$risk_level <- factor(patients_df$risk_level, 
                                 levels = c("Low", "Medium", "High"))



?factor

patient_smokers <- patient_smokers$risk_level <- factor(patient_smokers$risk_level, levels = c("Low", "Medium", "High"))

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


