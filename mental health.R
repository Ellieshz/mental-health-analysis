# This celle is for importing necessary libraries and file
library(ggplot2)
library(dplyr)
library(corrplot)
data <- read.csv("dev/mental-health-analysis/depression_data.csv")

# The following cells for confirming the dataset was properly imported 
head(data)
str(data)
# Check for missing values
colSums(is.na(data))
# Summary statistics of the dataset
summary(data)

#The following cells for cleaning the format of the dataset
# Replace all "." in column names with "_"
colnames(data) <- gsub("\\.", "_", colnames(data))
# Verify the new column names
colnames(data)
# Convert categorical variables to factors with new column names
data$Marital_Status <- factor(data$Marital_Status)
data$Education_Level <- factor(data$Education_Level)
data$Smoking_Status <- factor(data$Smoking_Status)
data$Physical_Activity_Level <- factor(data$Physical_Activity_Level)
data$Employment_Status <- factor(data$Employment_Status)
data$Alcohol_Consumption <- factor(data$Alcohol_Consumption)
data$Dietary_Habits <- factor(data$Dietary_Habits)
data$Sleep_Patterns <- factor(data$Sleep_Patterns)
data$History_of_Mental_Illness <- factor(data$History_of_Mental_Illness)
data$History_of_Substance_Abuse <- factor(data$History_of_Substance_Abuse)
data$Family_History_of_Depression <- factor(data$Family_History_of_Depression)
data$Chronic_Medical_Conditions <- factor(data$Chronic_Medical_Conditions)
# Handling missing values (e.g., fill missing Age with mean)
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
# Verify the revised colum names
str(data)

# Exploratory Data Analysis (EDA)
# I want to visualize relationships between key variables ad explore their influence on mental illness related factors
# Visualizing the distribution of variables helps us understand the characteristics of the dataset and the population we'are analyzing.
# Plot of Age Distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "light green", color = "grey") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")
# The age distribution of the participants in this data set is evenly spread between 20 and 80, making it a well-balanced and neutral survey sample.


# Plot of Marital Status Distribution
ggplot(data, aes(x = Marital_Status)) +
  geom_bar(fill = "purple") +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")
#The group with a marital status of 'married' is much more representative compared to those who are divorced, single, or widowed.

# Plot of Famaily History of Depression
ggplot(data, aes(x = Family_History_of_Depression)) +
  geom_bar(fill = "orange") +
  labs(title = "Family History of Depression", x = "Family History", y = "Count")


# Bivariate Analysis
# This sectionb analyzes the relationships between key lifestyle, socio-economic factors, and depression-related variables.
# Bar plot showing average income for individuals with and without a history of mental illness
ggplot(data, aes(x = History_of_Mental_Illness, y = Income)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black") +
  labs(title = "Average Income vs. History of Mental Illness", x = "History of Mental Illness", y = "Average Income")



# Bar plot for Smoking Status vs. History of Mental Illness
ggplot(data, aes(x = Smoking_Status, fill = History_of_Mental_Illness)) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking Status vs. History of Mental Illness", x = "Smoking Status", y = "Count")


# Bar plot for Number of Children vs. History of Mental Illness
ggplot(data, aes(x = History_of_Mental_Illness, fill = as.factor(Number_of_Children))) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Children vs. History of Mental Illness", x = "History of Mental Illness", y = "Count", fill = "Number of Children")



# Bar plot for Physical Activity Level vs. History of Mental Illness
ggplot(data, aes(x = Physical_Activity_Level, fill = History_of_Mental_Illness)) +
  geom_bar(position = "fill") +
  labs(title = "Proportional Bar Plot: Physical Activity Level vs. History of Mental Illness", 
       x = "Physical Activity Level", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)



# Calculate the counts and proportions for each combination
# To ensure the plot is working as expected
proportions_data <- data %>%
  group_by(Physical_Activity_Level, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# View the calculated proportions
print(proportions_data)

# Generate the bar plot with proportion labels
ggplot(proportions_data, aes(x = Physical_Activity_Level, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Physical Activity Level vs. History of Mental Illness", 
       x = "Physical Activity Level", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)


# Calculate the proportions for Smoking Status
proportions_smoking <- data %>%
  group_by(Smoking_Status, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_smoking, aes(x = Smoking_Status, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Smoking Status vs. History of Mental Illness", 
       x = "Smoking Status", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)


# Calculate the proportions for Alcohol Consumption
proportions_Alcohol <- data %>%
  group_by(Alcohol_Consumption, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_Alcohol, aes(x = Alcohol_Consumption, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Alcohol Consumption vs. History of Mental Illness", 
       x = "Alcohol Consumption", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)


# Calculate the proportions for Dietary Habits
proportions_Dietary <- data %>%
  group_by(Dietary_Habits, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_Dietary, aes(x = Dietary_Habits, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Dietary Habits vs. History of Mental Illness", 
       x = "Dietary Habits", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)


# Calculate the proportions for Sleep Patterns
proportions_Sleep <- data %>%
  group_by(Sleep_Patterns, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_Sleep, aes(x = Sleep_Patterns, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Sleep Patterns vs. History of Mental Illness", 
       x = "Sleep Patterns", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)


# Calculate the proportions for Education Level
proportions_Education <- data %>%
  group_by(Education_Level, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_Education, aes(x = Education_Level, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Education Level vs. History of Mental Illness", 
       x = "Education Level", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)

# Calculate the proportions for Employment_Status
proportions_Employment <- data %>%
  group_by(Employment_Status, History_of_Mental_Illness) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
# Generate the bar plot with proportion labels
ggplot(proportions_Employment, aes(x = Employment_Status, y = proportion, fill = History_of_Mental_Illness)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportional Bar Plot: Employment_Status vs. History of Mental Illness", 
       x = "Employment Status", y = "Proportion", fill = "History of Mental Illness") +
  scale_y_continuous(labels = scales::percent)




# Density plot for Income vs. History of Mental Illness
ggplot(data, aes(x = Income, fill = History_of_Mental_Illness)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot: Income vs. History of Mental Illness", 
       x = "Income", y = "Density", fill = "History of Mental Illness") +
  theme_minimal()



# Logistic regression model to predict mental illness based on lifestyle and socio-economic factors
model <- glm(History_of_Mental_Illness ~ Smoking_Status + Physical_Activity_Level + Alcohol_Consumption +
               Dietary_Habits + Sleep_Patterns + Education_Level + Employment_Status + Income, 
             data = data, family = binomial)

summary(model)

#Results
#The logistic regression model identified several significant predictors of mental illness:
#Unemployed individuals and those with lower education levels are more likely to have a history of mental illness.
#Lifestyle factors like smoking status and dietary habits also play a significant role.
#Income is negatively correlated with mental illness, meaning higher income slightly reduces the likelihood of mental illness.




