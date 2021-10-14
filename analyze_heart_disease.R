library(tidyverse)
data <- read.csv("heart.csv")


# age: age in years
# sex: sex (1 = male; 0 = female) 
# cp: chest pain type
# -- Value 1: typical angina
# -- Value 2: atypical angina
# -- Value 3: non-anginal pain
# -- Value 4: asymptomatic 
# trestbps: resting blood pressure (in mm Hg on admission to the hospital) 
# chol: serum cholestoral in mg/dl 
# fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false) 
# restecg: resting electrocardiographic results
# -- Value 0: normal
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
# -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria 
# thalach: maximum heart rate achieved 
# exang: exercise induced angina (1 = yes; 0 = no) 
# oldpeak = ST depression induced by exercise relative to rest 
# slope: the slope of the peak exercise ST segment
# -- Value 1: upsloping
# -- Value 2: flat
# -- Value 3: downsloping 
# ca: number of major vessels (0-3) colored by flourosopy 
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect 
# num: diagnosis of heart disease (angiographic disease status)
# -- Value 0: < 50% diameter narrowing
# -- Value 1: > 50% diameter narrowing
# (in any major vessel: attributes 59 through 68 are vessels) 


# Data Transformation

data2 <- data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES", "NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO"),
         ) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())



# Data Visualization

# Bar Plot

ggplot(data2, aes(x=target,fill=target))+ 
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name = "Heart Disease", label = c("Absence", "Presence"))

prop.table(table(data$target))


# count the frequency of the values of age

data2 %>%
  group_by(ï..age) %>%
  count()%>%
  filter(n>10)%>%
  ggplot()+
  geom_col(aes(ï..age, n), fill = "green")+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Age Count")



# compare blood pressure across the chest pain

data2 %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill = "purple")+
  xlab("Gender")+
  ylab("Blood Pressure")+
  facet_grid(~cp)


# compare cholestrol across the chest pain

data2 %>%
  ggplot(aes(x=sex, y=chol))+
  geom_boxplot(fill = "orange")+
  xlab("Gender")+
  ylab("Cholestrol Level")+
  facet_grid(~cp)


# Correlation
library(corrplot)
library(ggplot2)

cor_heart <- cor(data2[, 10:14])
cor_heart

corrplot(cor_heart, method = "square", type="upper")
