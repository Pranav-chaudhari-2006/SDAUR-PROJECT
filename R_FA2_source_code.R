# ============================================================
# Load required libraries
# ============================================================
# install.packages(c("dplyr","ggplot2","grid","broom","ggpubr","openxlsx","rio",
#                    "modeest","pastecs","moments","corrplot")) # uncomment if needed

library(dplyr)
library(ggplot2)
library(grid)
library(broom)
library(ggpubr)
library(openxlsx)
library(rio)
library(modeest)      # mfv for mode if needed
library(pastecs)      # stat.desc
library(moments)      # skewness, kurtosis
library(corrplot)     # correlation visualization

# ============================================================
# Read dataset
# ============================================================
Data <- read.csv("diabetes.csv")

# Basic overview
View(Data)
summary(Data)
colSums(is.na(Data))

# ============================================================
# DATA CLEANING & PRE-PROCESSING
# ============================================================
# Treat medically-impossible zeros as NA for specific columns
zero_as_na_cols <- c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")
for(col in zero_as_na_cols){
  if(col %in% names(Data)){
    Data[[col]][Data[[col]] == 0] <- NA
  }
}

# Check NAs after zero->NA
colSums(is.na(Data))

# Impute numeric columns with median (loop to avoid repetition)
numeric_cols <- names(select_if(Data, is.numeric))
for(col in numeric_cols){
  med <- median(Data[[col]], na.rm = TRUE)
  Data[[col]][is.na(Data[[col]])] <- med
}

# Final check
colSums(is.na(Data))

# Create cleaned copy for downstream analysis
Data_cleaned <- Data
View(Data_cleaned)

# Save cleaned dataset (optional)
write.csv(Data_cleaned, "diabetes_cleaned.csv", row.names = FALSE)

# ============================================================
# DESCRIPTIVE STATISTICS (use cleaned data)
# ============================================================
# Variance & SD (cleaned)
variance_Glucose <- var(Data_cleaned$Glucose)
variance_BMI <- var(Data_cleaned$BMI)
variance_Age <- var(Data_cleaned$Age)

SD_Glucose <- sd(Data_cleaned$Glucose)
SD_BMI <- sd(Data_cleaned$BMI)
SD_Age <- sd(Data_cleaned$Age)

# Comprehensive stats
stat.desc(Data_cleaned)

# ============================================================
# OUTLIER DETECTION USING BOXPLOTS (cleaned)
# ============================================================
ggplot(Data_cleaned, aes(Glucose)) + geom_boxplot() + geom_vline(aes(xintercept = mean(Glucose)), color = "red") + labs(title = "Boxplot: Glucose")
ggplot(Data_cleaned, aes(BMI)) + geom_boxplot() + geom_vline(aes(xintercept = mean(BMI)), color = "red") + labs(title = "Boxplot: BMI")
ggplot(Data_cleaned, aes(Age)) + geom_boxplot() + geom_vline(aes(xintercept = mean(Age)), color = "red") + labs(title = "Boxplot: Age")
ggplot(Data_cleaned, aes(Insulin)) + geom_boxplot() + geom_vline(aes(xintercept = mean(Insulin)), color = "red") + labs(title = "Boxplot: Insulin")
ggplot(Data_cleaned, aes(BloodPressure)) + geom_boxplot() + geom_vline(aes(xintercept = mean(BloodPressure)), color = "red") + labs(title = "Boxplot: Blood Pressure")

# Boxplots by Outcome (PS1)
plot1 <- filter(Data_cleaned, Outcome == 1) %>% ggplot(aes(BMI)) + geom_boxplot(color = "red") + labs(title = "Diabetic (Outcome = 1)", x = "BMI")
plot2 <- filter(Data_cleaned, Outcome == 0) %>% ggplot(aes(BMI)) + geom_boxplot(color = "blue") + labs(title = "Non-diabetic (Outcome = 0)", x = "BMI")
print(plot1); print(plot2)

# ============================================================
# HISTOGRAMS (Mean & Median lines) - cleaned
# ============================================================
ggplot(Data_cleaned, aes(Glucose)) + geom_histogram(bins = 30) + geom_vline(aes(xintercept = mean(Glucose)), color = "red") + geom_vline(aes(xintercept = median(Glucose)), color = "purple") + labs(title = "Histogram: Glucose")
ggplot(Data_cleaned, aes(BMI)) + geom_histogram(bins = 30) + geom_vline(aes(xintercept = mean(BMI)), color = "red") + geom_vline(aes(xintercept = median(BMI)), color = "purple") + labs(title = "Histogram: BMI")
ggplot(Data_cleaned, aes(Age)) + geom_histogram(bins = 30) + geom_vline(aes(xintercept = mean(Age)), color = "red") + geom_vline(aes(xintercept = median(Age)), color = "purple") + labs(title = "Histogram: Age")

# ============================================================
# SKEWNESS, KURTOSIS, DENSITY (cleaned)
# ============================================================
skewness(Data_cleaned$Glucose); kurtosis(Data_cleaned$Glucose)
ggplot(Data_cleaned, aes(Glucose)) + geom_density(alpha = 0.5) + labs(title = "Density: Glucose")

skewness(Data_cleaned$BMI); kurtosis(Data_cleaned$BMI)
ggplot(Data_cleaned, aes(BMI)) + geom_density(alpha = 0.5) + labs(title = "Density: BMI")

skewness(Data_cleaned$Age); kurtosis(Data_cleaned$Age)
ggplot(Data_cleaned, aes(Age)) + geom_density(alpha = 0.5) + labs(title = "Density: Age")

# ============================================================
# CORRELATION MATRIX (use complete.obs)
# ============================================================
corr_data <- select(Data_cleaned, Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age, Outcome)
corr_matrix <- cor(corr_data, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black", number.cex = 0.7, tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue","white","red"))(200))

# ============================================================
# PS1: Analysis of glucose by outcome
# ============================================================
PS1_table <- Data_cleaned %>% group_by(Outcome) %>% summarise(Mean_Glucose = mean(Glucose, na.rm = TRUE), SD_Glucose = sd(Glucose, na.rm = TRUE), N=n())
PS1_table
ggplot(Data_cleaned, aes(factor(Outcome), Glucose)) + geom_boxplot() + labs(x = "Outcome", y = "Glucose")

t.test(Glucose ~ Outcome, data = Data_cleaned)

# ============================================================
# PS4: Pregnancies grouped summary (used also elsewhere)
# ============================================================
preg_summary <- Data_cleaned %>% group_by(Pregnancies) %>% summarise(Mean_Glucose = mean(Glucose, na.rm = TRUE), Mean_Insulin = mean(Insulin, na.rm = TRUE), Mean_Age = mean(Age, na.rm = TRUE), Count = n())
View(preg_summary)
ggplot(preg_summary, aes(x = Pregnancies, y = Mean_Glucose)) + geom_col() + labs(title = "Mean Glucose by Pregnancies")

# ============================================================
# PS2: Linear regression (Glucose ~ BMI)
# ============================================================
ggplot(Data_cleaned, aes(BMI, Glucose)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + labs(title = "Glucose vs BMI")
lm_bmi <- lm(Glucose ~ BMI, data = Data_cleaned)
summary(lm_bmi)
tidy(lm_bmi); glance(lm_bmi)
cor(Data_cleaned$Glucose, Data_cleaned$BMI, use = "complete.obs")

# regression plot with equation & R
ggplot(Data_cleaned, aes(BMI, Glucose)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation(aes(label = ..eq.label..)) +
  stat_cor(aes(label = ..r.label..)) +
  labs(title = "Glucose vs BMI with linear fit")

# Residual diagnostics
resid_df <- augment(lm_bmi)
ggplot(resid_df, aes(.fitted, .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + labs(title = "Residuals vs Fitted")

# ============================================================
# PS3: Age distribution by Outcome
# ============================================================
Data_cleaned %>% group_by(Outcome) %>% summarise(Mean_Age = mean(Age, na.rm = TRUE), SD_Age = sd(Age, na.rm = TRUE))
ggplot(Data_cleaned, aes(Age, fill = factor(Outcome))) + geom_density(alpha = 0.4) + labs(fill = "Outcome")

# t-test for Age by Outcome
t.test(Age ~ Outcome, data = Data_cleaned)

# ============================================================
# PS5: Logistic regression predicting Outcome
# ============================================================
glm_outcome <- glm(Outcome ~ Glucose + BMI + Age, family = binomial, data = Data_cleaned)
summary(glm_outcome)
odds_ratios <- exp(coef(glm_outcome))
ci_or <- exp(confint(glm_outcome))
odds_ratios; ci_or

# Predictions and plot
Data_cleaned$pred_prob <- predict(glm_outcome, type = "response")
ggplot(Data_cleaned, aes(Glucose, pred_prob)) + geom_point(alpha = 0.5) + geom_smooth(method = "loess") + labs(title = "Predicted probability of Outcome vs Glucose", y = "Predicted probability")

# Optional: compute AUC if pROC is available
# install.packages("pROC") # uncomment to install
# library(pROC)
# roc_obj <- roc(Data_cleaned$Outcome, Data_cleaned$pred_prob)
# auc(roc_obj)

# ============================================================
# Export: grouped summary and cleaned data
# ============================================================
export(preg_summary, "Stmt_3.xlsx")
write.csv(Data_cleaned, "diabetes_cleaned.csv", row.names = FALSE)
