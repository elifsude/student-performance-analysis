library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)
library(tidyr)


df <- student.mat
dim(df)
str(df)
summary(df)

# avg_grade 

df$avg_grade <- (df$G1 + df$G2 + df$G3) / 3


#2. EXPLORATORY DATA ANALYSIS

# 2.1 DESCRIPTIVE STATISTICS

summary(df)

# Descriptive statistics for numeric variables
numeric_vars <- df %>% select(age, Medu, Fedu, traveltime, studytime, failures,
                              famrel, freetime, goout, Dalc, Walc, health,
                              absences, G1, G2, G3, avg_grade)

desc_stats <- data.frame(
  Variable = names(numeric_vars),
  Min = sapply(numeric_vars, min),
  Max = sapply(numeric_vars, max),
  Mean = round(sapply(numeric_vars, mean), 2),
  Median = sapply(numeric_vars, median),
  SD = round(sapply(numeric_vars, sd), 2)
)
print(desc_stats)

#Descriptive statistics for categorical variables
cat_vars <- c("school", "sex", "address", "famsize", "Pstatus", 
              "Mjob", "Fjob", "reason", "guardian", "schoolsup", 
              "famsup", "paid", "activities", "nursery", 
              "higher", "internet", "romantic")

for (var in cat_vars) {
  cat("\n---", var, "---\n")
  tbl <- table(df[[var]])
  pct <- round(prop.table(tbl) * 100, 1)
  print(data.frame(Category = names(tbl), Count = as.integer(tbl), Percentage = as.numeric(pct)))
}




# School distribution
table(df$school)

# Avg grade summary by schools
df %>%
  group_by(school) %>%
  summarise(
    n = n(),
    mean_avg = round(mean(avg_grade), 2),
    median_avg = median(avg_grade),
    sd_avg = round(sd(avg_grade), 2),
    mean_G1 = round(mean(G1), 2),
    mean_G2 = round(mean(G2), 2),
    mean_G3 = round(mean(G3), 2)
  )

# Boxplot
ggplot(df, aes(x = school, y = avg_grade, fill = school)) +
  geom_boxplot() +
  labs(title = "Average Grade by School", x = "School", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

# T-test for school differences?
t.test(avg_grade ~ school, data = df)

# 2.3 DISTRIBUTION OF NUMERICAL VARIABLES

# avg_grade dist
ggplot(df, aes(x = avg_grade)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white") +
  geom_vline(xintercept = mean(df$avg_grade), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Average Grade", x = "Average Grade", y = "Count") +
  theme_minimal()

# G1, G2, G3 yan yana
df_long <- df %>% select(G1, G2, G3) %>% pivot_longer(everything(), names_to = "Period", values_to = "Grade")

ggplot(df_long, aes(x = Grade, fill = Period)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~Period) +
  labs(title = "Distribution of Grades by Period", x = "Grade", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Absences 
ggplot(df, aes(x = absences)) +
  geom_histogram(binwidth = 2, fill = "#E84855", color = "white") +
  labs(title = "Distribution of Absences", x = "Absences", y = "Count") +
  theme_minimal()

# Studytime, failures, age
p1 <- ggplot(df, aes(x = factor(studytime))) +
  geom_bar(fill = "#3BB273") +
  labs(title = "Study Time", x = "Study Time (1-4)", y = "Count") +
  theme_minimal()

p2 <- ggplot(df, aes(x = factor(failures))) +
  geom_bar(fill = "#E84855") +
  labs(title = "Past Failures", x = "Failures", y = "Count") +
  theme_minimal()

p3 <- ggplot(df, aes(x = factor(age))) +
  geom_bar(fill = "#F4A261") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)

# Alcohol consumption
df$avg_alc <- (df$Dalc + df$Walc) / 2

p4 <- ggplot(df, aes(x = factor(Dalc))) +
  geom_bar(fill = "#9B2335") +
  labs(title = "Workday Alcohol (Dalc)", x = "Level (1-5)", y = "Count") +
  theme_minimal()

p5 <- ggplot(df, aes(x = factor(Walc))) +
  geom_bar(fill = "#C0392B") +
  labs(title = "Weekend Alcohol (Walc)", x = "Level (1-5)", y = "Count") +
  theme_minimal()

p6 <- ggplot(df, aes(x = avg_alc)) +
  geom_histogram(binwidth = 0.5, fill = "#7B1421", color = "white") +
  geom_vline(xintercept = mean(df$avg_alc), color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "Overall Alcohol Consumption", x = "Average Alcohol Level", y = "Count") +
  theme_minimal()

grid.arrange(p4, p5, p6, ncol = 3)


# 2.4 DISTRIBUTION OF CATEGORICAL VARIABLES




# avg_grade by sex
ggplot(df, aes(x = sex, y = avg_grade, fill = sex)) +
  geom_boxplot() +
  labs(title = "Average Grade by Sex", x = "Sex", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

# avg_grade by address
ggplot(df, aes(x = address, y = avg_grade, fill = address)) +
  geom_boxplot() +
  labs(title = "Average Grade by Address", x = "Address", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

# Mjob ve Fjob
p7 <- ggplot(df, aes(x = Mjob, fill = Mjob)) +
  geom_bar() +
  labs(title = "Mother's Job", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

p8 <- ggplot(df, aes(x = Fjob, fill = Fjob)) +
  geom_bar() +
  labs(title = "Father's Job", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

grid.arrange(p7, p8, ncol = 2)

# Higher education aspiration vs avg_grade
ggplot(df, aes(x = higher, y = avg_grade, fill = higher)) +
  geom_boxplot() +
  labs(title = "Average Grade by Higher Education Aspiration", x = "Wants Higher Education", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = factor(failures), y = avg_grade, fill = factor(failures))) +
  geom_boxplot() +
  labs(title = "Average Grade by Past Failures", x = "Number of Failures", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")
# 2.4 CORRELATION ANALYSIS

# Numeric korelasyon matrisi
cor_vars <- df %>% select(age, Medu, Fedu, traveltime, studytime, failures,
                          famrel, freetime, goout, Dalc, Walc, health,
                          absences, avg_grade)

cor_matrix <- cor(cor_vars, use = "complete.obs")
print(round(cor_matrix, 2))

# Korelasyon plot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, tl.col = "black",
         col = colorRampPalette(c("#E84855", "white", "#2E86AB"))(200),
         addCoef.col = "black", number.cex = 0.6,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# avg_grade ile en yüksek korelasyonlar
cor_with_avg <- sort(cor_matrix["avg_grade", ], decreasing = TRUE)
print(cor_with_avg)

# Scatter: failures vs avg_grade
ggplot(df, aes(x = failures, y = avg_grade)) +
  geom_jitter(alpha = 0.4, color = "#2E86AB") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Failures vs Average Grade", x = "Past Failures", y = "Average Grade") +
  theme_minimal()

# Scatter: studytime vs avg_grade
ggplot(df, aes(x = studytime, y = avg_grade)) +
  geom_jitter(alpha = 0.4, color = "#3BB273") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Study Time vs Average Grade", x = "Study Time", y = "Average Grade") +
  theme_minimal()

# Scatter: absences vs avg_grade
ggplot(df, aes(x = absences, y = avg_grade)) +
  geom_jitter(alpha = 0.4, color = "#F4A261") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Absences vs Average Grade", x = "Absences", y = "Average Grade") +
  theme_minimal()

