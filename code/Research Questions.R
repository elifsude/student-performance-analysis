data <- df
model <- lm(avg_grade ~ studytime + failures + absences + Medu + goout + Walc, data = df)
summary(model)


# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(model, which = 1)

# 2. Normal Q-Q (normality of residuals)
plot(model, which = 2)

# 3. Scale-Location (homoscedasticity)
plot(model, which = 3)

# 4. Cook's Distance (influential points)
plot(model, which = 4)

# 5. Multicollinearity
library(car)
vif(model)

library(lmtest)
library(nortest)

# Normality of residuals
shapiro.test(residuals(model))

# Homoscedasticity (Breusch-Pagan)
bptest(model)

# Autocorrelation (Durbin-Watson)
dwtest(model)

library(sandwich)


coeftest(model, vcov = vcovHC(model, type = "HC3"))

model2 <- lm(avg_grade ~ failures + Medu + goout, data = df)
vif(model2)

shapiro.test(residuals(model2))
bptest(model2)
dwtest(model2)

summary(model2)




# Sex
t.test(avg_grade ~ sex, data = df)

# Address
t.test(avg_grade ~ address, data = df)

# Age
cor.test(df$age, df$avg_grade)

# Sex normality
shapiro.test(df$avg_grade[df$sex == "F"])
shapiro.test(df$avg_grade[df$sex == "M"])

# Address normality
shapiro.test(df$avg_grade[df$address == "R"])
shapiro.test(df$avg_grade[df$address == "U"])

# Normality for correlation
shapiro.test(df$age)

cor.test(df$age, df$avg_grade, method = "spearman")

library(ggpubr)

p_sex <- ggplot(df, aes(x = sex, y = avg_grade, fill = sex)) +
  geom_boxplot() +
  labs(title = "Average Grade by Sex", x = "Sex", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

p_address <- ggplot(df, aes(x = address, y = avg_grade, fill = address)) +
  geom_boxplot() +
  labs(title = "Average Grade by Address", x = "Address", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

p_age <- ggplot(df, aes(x = age, y = avg_grade)) +
  geom_jitter(alpha = 0.4, color = "#2E86AB") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Age vs Average Grade", x = "Age", y = "Average Grade") +
  theme_minimal()

grid.arrange(p_sex, p_address, p_age, ncol = 3)

# Normality check
shapiro.test(df$avg_grade[df$higher == "yes"])
shapiro.test(df$avg_grade[df$higher == "no"])

# T-test
t.test(avg_grade ~ higher, data = df)



library(gridExtra)
# 1. bOXPLOTS
p_medu <- ggplot(df, aes(x = factor(Medu), y = avg_grade, fill = factor(Medu))) +
  geom_boxplot() +
  labs(title = "Average Grade by Mother's Education", x = "Mother's Education Level", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

p_fedu <- ggplot(df, aes(x = factor(Fedu), y = avg_grade, fill = factor(Fedu))) +
  geom_boxplot() +
  labs(title = "Average Grade by Father's Education", x = "Father's Education Level", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p_medu, p_fedu, ncol = 2)

table(df$Medu)
table(df$Fedu)

df_edu <- subset(df, Medu > 0 & Fedu > 0)
p_medu2 <- ggplot(df_edu, aes(x = factor(Medu), y = avg_grade, fill = factor(Medu))) +
  geom_boxplot() +
  labs(title = "Average Grade by Mother's Education", x = "Mother's Education Level", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

p_fedu2 <- ggplot(df_edu, aes(x = factor(Fedu), y = avg_grade, fill = factor(Fedu))) +
  geom_boxplot() +
  labs(title = "Average Grade by Father's Education", x = "Father's Education Level", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p_medu2, p_fedu2, ncol = 2)

# 2. Spearman correlation
shapiro.test(df$Medu)
shapiro.test(df$Fedu)
cor.test(df$Medu, df$avg_grade, method = "spearman")
cor.test(df$Fedu, df$avg_grade, method = "spearman")

# 3. Combined parental education
df_edu$combined_edu <- df_edu$Medu + df_edu$Fedu

# Low (0-3) vs hİGH (6-8) grupları
df_edu$edu_group <- ifelse(df_edu$combined_edu <= 3, "Low",
                           ifelse(df_edu$combined_edu >= 6, "High", "Medium"))
table(df_edu$edu_group)

df_edu$edu_group <- factor(df_edu$edu_group, levels = c("Low", "Medium", "High"))

ggplot(df_edu, aes(x = edu_group, y = avg_grade, fill = edu_group)) +
  geom_boxplot() +
  labs(title = "Average Grade by Combined Parental Education", 
       x = "Parental Education Group", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")
# ANOVA
anova_edu2 <- aov(avg_grade ~ edu_group, data = df_edu)
summary(anova_edu2)
TukeyHSD(anova_edu2)

# Normality of residuals
shapiro.test(residuals(anova_edu2))

# Homoscedasticity (Levene test)
library(car)
leveneTest(avg_grade ~ edu_group, data = df_edu)

# Medu or Fedu?
cor.test(df_edu$Medu, df_edu$avg_grade, method = "spearman")
cor.test(df_edu$Fedu, df_edu$avg_grade, method = "spearman")

tukey_results <- TukeyHSD(anova_edu2)
tukey_df <- as.data.frame(tukey_results$edu_group)
tukey_df$Comparison <- rownames(tukey_df)
tukey_df <- tukey_df[, c("Comparison", "diff", "lwr", "upr", "p adj")]
colnames(tukey_df) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "p-value")
tukey_df$Difference <- round(tukey_df$Difference, 3)
tukey_df$`Lower CI` <- round(tukey_df$`Lower CI`, 3)
tukey_df$`Upper CI` <- round(tukey_df$`Upper CI`, 3)
tukey_df$`p-value` <- round(tukey_df$`p-value`, 4)
print(tukey_df)


# Normality check
shapiro.test(df$avg_grade[df$famsize == "GT3"])
shapiro.test(df$avg_grade[df$famsize == "LE3"])
shapiro.test(df$avg_grade[df$Pstatus == "T"])
shapiro.test(df$avg_grade[df$Pstatus == "A"])
shapiro.test(df$famrel)

# T-tests
t.test(avg_grade ~ famsize, data = df)
t.test(avg_grade ~ Pstatus, data = df)

# Spearman
cor.test(df$famrel, df$avg_grade, method = "spearman")



# famrel için sadece 1-2 (low) vs 4-5 (high)
df_famrel <- subset(df, famrel %in% c(1, 2, 4, 5))
df_famrel$famrel_group <- ifelse(df_famrel$famrel <= 2, "Low", "High")

table(df_famrel$famrel_group)

ggplot(df_famrel, aes(x = famrel_group, y = avg_grade, fill = famrel_group)) +
  geom_boxplot() +
  labs(title = "Average Grade by Family Relationship Quality", 
       x = "Family Relationship Quality", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

t.test(avg_grade ~ famrel_group, data = df_famrel)

ggplot(df, aes(x = Pstatus, y = avg_grade, fill = Pstatus)) +
  geom_boxplot() +
  labs(title = "Average Grade by Parent Cohabitation Status", 
       x = "Parent Status", y = "Average Grade") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = factor(famrel), y = avg_grade, fill = Pstatus)) +
  geom_boxplot() +
  labs(title = "Average Grade by Family Relationship Quality and Parent Status", 
       x = "Family Relationship Quality", y = "Average Grade") +
  theme_minimal()


#
ggplot(df, aes(x = Pstatus, y = famrel, fill = Pstatus)) +
  geom_boxplot() +
  labs(title = "Family Relationship Quality by Parent Status",
       x = "Parent Status", y = "Family Relationship Quality") +
  theme_minimal() +
  theme(legend.position = "none")

t.test(famrel ~ Pstatus, data = df)



df$famrel_group <- ifelse(df$famrel <= 2, "Low", 
                          ifelse(df$famrel >= 4, "High", "Medium"))

table(df$famrel_group, df$Pstatus)
prop.table(table(df$famrel_group, df$Pstatus), margin = 1)

chisq.test(table(df$famrel_group, df$Pstatus))


t.test(famrel ~ sex, data = df)

ggplot(df, aes(x = sex, y = famrel, fill = sex)) +
  geom_boxplot() +
  labs(title = "Family Relationship Quality by Sex",
       x = "Sex", y = "Family Relationship Quality") +
  theme_minimal() +
  theme(legend.position = "none")


chisq.test(table(df$famrel_group, df$sex))
prop.table(table(df$famrel_group, df$sex), margin = 1)



# famrel by address
t.test(famrel ~ address, data = df)

ggplot(df, aes(x = address, y = famrel, fill = address)) +
  geom_boxplot() +
  labs(title = "Family Relationship Quality by Address", 
       x = "Address", y = "Family Relationship Quality") +
  theme_minimal() +
  theme(legend.position = "none")

# alkol by address
t.test(Walc ~ address, data = df)
t.test(Dalc ~ address, data = df)

ggplot(df, aes(x = address, y = Walc, fill = address)) +
  geom_boxplot() +
  labs(title = "Weekend Alcohol Consumption by Address", 
       x = "Address", y = "Walc") +
  theme_minimal() +
  theme(legend.position = "none")