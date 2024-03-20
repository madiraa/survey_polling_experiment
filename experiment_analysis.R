library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Set options to display numbers without scientific notation
options(scipen = 999)

rm(list = ls())

setwd("/Users/madison/Desktop/survey_polling_experiment")

df <- read_csv("results_march11.csv")

print(colnames(df))
##################################
##################################
# Transform data into properly coded variables for regressions and subgrouping # 
##################################
##################################

# filter data 
df <- select(df, c("Q1", "Q2", "Q3", "Q21", "Q22", "Q23", "Q24", "Q4", "Q5_1","Q5_2","Q5_3","Q5_4","Q5_5","Q5_6","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15"))
# Replace NA values with 0 for all columns
df <- df %>%
  mutate_all(~ replace(., is.na(.), 0))
# Convert "I read the snippet" to 1 and NA to 0 for columns Q21, Q22, and Q23
df <- df %>%
  mutate_at(vars(Q21:Q23), ~ ifelse(. == "I read the snippet", 1, 0))
# Convert "ok" to 1 and NA to 0 for column Q24
df <- df %>%
  mutate(Q24 = ifelse(Q24 == "ok", 1, ifelse(is.na(Q24), 0, Q24)))
# rename columns to reflect treatments
df <- df %>%
  rename(adversary_message = Q21,
         aid_fact = Q22,
         ukr_need = Q23,
         control = Q24)

# transform support/oppose into binary variables 
columns_to_transform <- c("Q5_1", "Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6")
df <- df %>%
  mutate_at(vars(columns_to_transform), ~ ifelse(. == "Support", 1, ifelse(. == "Oppose", 0, .)))

# mutate Q4 column into 3 column with binary variable
df <- mutate(df,
             too_much = as.integer(Q4 == "Too much"),
             too_little = as.integer(Q4 == "Too little"),
             about_right = as.integer(Q4 == "About the right amount"))

# Partisanship 7 point scale transformation 
# Define a function to map the responses to the corresponding codes
map_to_code <- function(Q6, Q7, Q8, Q9) {
  if (Q7 == "Strong Democrat") {
    return(0)
  } else if (Q7 == "Not very strong Democrat") {
    return(1)
  } else if (Q6 == "Independent" || Q6 == "Something else") {
    if (Q9 == "Democratic party") {
      return(2)
    } else if (Q9 == "Neither") {
      return(3)
    } else if (Q9 == "Republican party") {
      return(4)
    }
  } else if (Q8 == "Not very strong Republican") {
    return(5)
  } else if (Q8 == "Strong Republican") {
    return(6)
  }
}
# Apply the function to create a new variable representing the 7-point scale
df$partisan_score <- mapply(map_to_code, df$Q6, df$Q7, df$Q8, df$Q9)

# Transform all partisan scores into binary
df <- mutate(df,
             strong_dem = ifelse(partisan_score == "0", 1, 0),
             not_strong_dem = ifelse(partisan_score == "1", 1, 0),
             ind_close_dem = ifelse(partisan_score == "2", 1, 0), 
             ind = ifelse(partisan_score == "3", 1, 0), 
             ind_close_rep = ifelse(partisan_score == "4", 1, 0), 
             not_strong_rep = ifelse(partisan_score == "5", 1, 0), 
             strong_rep = ifelse(partisan_score == "6", 1, 0), 
)

# transform various aid type binary responses 
df = df %>%
  rename("additional_arms" = Q5_1,
         "additional_econ" = Q5_2,
         "send_troops" = Q5_3,
         "sanction_rus" = Q5_4,
         "grad_decrease_mil_aid" = Q5_5,
         "grad_decrease_econ_aid" = Q5_6)            

# transform gender variable
df = mutate(df,
            is_man = ifelse(Q12 == "Man", 1, 0))

# transform citizenship variable
df = mutate(df, 
            is_citizen = ifelse(Q15 == "Yes",1,0))

# transform advantage 
df = mutate(df, 
            rus_advantage = ifelse(Q1 == "Russia has the advantage",1,0),
            ukr_advantage = ifelse(Q1 == "Ukraine has the advantage",1,0),
            neither_advantage = ifelse(Q1 == "Neither side has the advantage",1,0)
)

#################################
#################################
# Experiment Regressions and Models
#################################
#################################

# adversary message affect 
model <- lm(too_little ~ adversary_message , data = df)
summary(model)

# ukraine need message affect 
model <- lm(too_little ~ ukr_need, data = df)
summary(model)

# aid factoid message affect
model <- lm(too_little ~ aid_fact, data = df)
summary(model)

# Better Model adv message TODO
#model <- lm(too_little ~ adversary_message + is_man + )

# getting $ spent right and believing we are doing too X; results ppl who got it right on average think too much or abotu right 
df <- mutate(df,
             aid_knowledge = ifelse(Q3 == "75-100 billion USD", 1, 0))

model <- lm(too_little ~ aid_knowledge, data = df)
summary(model)

model <- lm(about_right ~ aid_knowledge, data = df)
summary(model)

model <- lm(too_much ~ aid_knowledge, data = df)
summary(model)

# advantage assessment regressions
model <- lm(too_little ~ rus_advantage + ukr_advantage + neither_advantage, data = df)
summary(model)

model <- lm(too_much ~ rus_advantage + ukr_advantage + neither_advantage, data = df)
summary(model) #significant

model <- lm(about_right ~ rus_advantage + ukr_advantage + neither_advantage, data = df)
summary(model)

################################
################################
# VISUALIZATIONS 
################################
################################
# Filter out NA values from the Q3 column
df_filtered <- df[!is.na(df$Q3), ]

# Define the desired order of levels
desired_order <- c("less than 100 million USD", 
                   "100 million - 500 million USD", 
                   "500 million - 1 billion USD", 
                   "1 billion - 5 billion USD", 
                   "5-10 billion USD", 
                   "10-25 billion USD", 
                   "25-50 billion USD", 
                   "50 - 75 billion USD", 
                   "75-100 billion USD")

# Convert Q3 to a factor with the desired order
df_filtered$Q3 <- factor(df_filtered$Q3, levels = desired_order)

# Create the bar chart with the reordered levels
ggplot(df_filtered, aes(x = Q3)) +
  geom_bar() +
  labs(title = "About how much do you think the United States has spent on aid to Ukraine since the beginning of the Russian operation in Ukraine (February 2022)",
       x = "Responses",
       y = "Frequency")

# Distribution of responses to Q5 as a bar chart
ggplot(df_filtered, aes(x = send_troops)) +
  geom_bar() +
  labs(title = "Do you support sending U.S. troops to Ukraine?",
       x = "Response",
       y = "Frequency")

# Distribution of responses to Q2 as a bar chart
# Define the desired order of levels
desired_order <- c("a few months", "six months to a year", "over a year", "two to four years", "five years or more")

# Convert Q2 to a factor with the desired order
df$Q2 <- factor(df$Q2, levels = desired_order)

# Create the bar chart with the reordered levels
ggplot(df, aes(x = Q2)) +
  geom_bar() +
  labs(title = "From this point forward, how long do you expect the war in Ukraine to last?",
       x = "Responses",
       y = "Frequency")

# Compute proportions for the values in the "Q2" column
proportions <- prop.table(table(df$Q2))

# Print the proportions
print(proportions)

# Distribution of responses to Q4 as a bar chart
ggplot(df, aes(x = Q4)) +
  geom_bar() +
  labs(title = "Do you think the United States is doing too much, too little, or about the right amount to help Ukraine?",
       x = "Responses",
       y = "Frequency")

# distribution of advantage assessment 
ggplot(df, aes(x = Q1)) +
  geom_bar() +
  labs(title = "Which side do you think has the advantage?",
       x = "Responses",
       y = "Frequency")

# Compute proportions for each response in the "Q1" column
proportions <- prop.table(table(df$Q1))

# Print the proportions
print(proportions)

# distribution of partisanship scores
# Define labels for the partisan scores
labels <- c(
  "0" = "STRONG DEMOCRAT",
  "1" = "NOT VERY STRONG DEMOCRAT",
  "2" = "INDEPENDENT CLOSER TO DEMOCRAT",
  "3" = "INDEPENDENT",
  "4" = "INDEPENDENT CLOSER TO REPUBLICANS",
  "5" = "NOT VERY STRONG REPUBLICAN",
  "6" = "STRONG REPUBLICAN"
)

# Convert partisan_score to a factor with custom labels
df$partisan_score <- factor(df$partisan_score, levels = 0:6, labels = labels)

# Plot the bar plot
ggplot(data = df, aes(x = partisan_score, fill = partisan_score)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +  # You can choose any color palette you prefer
  labs(x = "Partisan Score", y = "Frequency") +
  theme_minimal() +  # Optional: customize the plot theme
  theme(axis.text.x = element_blank())  # Remove x-axis labels

# distribution of education level 
ggplot(data = df, aes(x = Q13, fill = Q13)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +  # You can choose any color palette you prefer
  labs(x = "Education Level", y = "Frequency") +
  theme_minimal() +  # Optional: customize the plot theme
  theme(axis.text.x = element_blank())

# Distribution of treatment assignments
# Count the number of "1" values for columns "adversary_message", "aid_fact", "ukr_need", and "control"
count_adversary_message <- sum(df$adversary_message == 1)
count_aid_fact <- sum(df$aid_fact == 1)
count_ukr_need <- sum(df$ukr_need == 1)
count_control <- sum(df$control == 1)

# Print the counts
print(count_adversary_message)
print(count_aid_fact)
print(count_ukr_need)
print(count_control)


# policy specific after-treatment Questions 
print_proportions <- function(data, question) {
  # Count the frequency of responses
  response_counts <- table(data[[question]])
  
  # Calculate proportions
  response_proportions <- prop.table(response_counts)
  
  # Print proportions
  cat("Question:", question, "\n")
  print(response_proportions)
}

# Print proportions for each question
questions <- c("additional_arms", "additional_econ", "send_troops", 
               "sanction_rus", "grad_decrease_mil_aid", "grad_decrease_econ_aid")

for (q in questions) {
  print_proportions(df, q)
  cat("\n")
}

#  message affects on policy QS 
# Convert "additional_arms" to numeric
df$additional_arms <- as.numeric(df$additional_arms)

model <- glm(additional_arms ~ adversary_message, data = df)
summary(model)

model <- glm(additional_arms ~ ukr_need, data = df)
summary(model)

model <- glm(additional_arms ~ aid_fact, data = df)
summary(model)

#convert additional_econ to numeric 
df$additional_econ <- as.numeric(df$additional_econ)
model <- glm(additional_econ ~ adversary_message + ukr_need + aid_fact + control + strong_dem + not_strong_dem + ind_close_dem + ind + ind_close_rep + not_strong_rep + strong_rep, data = df, family = "binomial")
summary(model)
