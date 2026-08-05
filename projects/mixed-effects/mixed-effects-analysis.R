set.seed(123)

library(doBy)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(dplyr)
library(gt)
library(car)

### Data setup

data("dietox")
dietox$Evit <- as.factor(dietox$Evit)
dietox$Cu <- as.factor(dietox$Cu)
dietox$Time <- as.factor(dietox$Time)
dietox$Litter <- as.factor(dietox$Litter)
dietox$Pig <- as.factor(dietox$Pig)

### Basic summaries

cat("Dimensions of dataset:\n")
print(dim(dietox))

cat("\nStructure:\n")
str(dietox)

cat("\nMissing values:\n")
print(colSums(is.na(dietox)))

cat("\nTreatment counts:\n")
print(with(dietox, table(Evit, Cu)))


cat("\nTreatment counts (number of pigs per treatment):\n")

print(
  with(
    dietox[!duplicated(dietox$Pig), ],
    table(Evit, Cu)
  )
)

cat("\nNumber of pigs:\n")
print(length(unique(dietox$Pig)))

cat("\nNumber of litters:\n")
print(length(unique(dietox$Litter)))

cat("\nSummary of Weight, Feed, Start:\n")
print(summary(dietox[, c("Weight", "Feed", "Start")]))

desc_table <- dietox %>%
  group_by(Evit, Cu) %>%
  summarise(
    n_obs=n(),
    n_pigs=n_distinct(Pig),
    mean_weight=mean(Weight),
    sd_weight=sd(Weight),
    mean_feed=mean(Feed),
    mean_start=mean(Start),
    .groups="drop"
  )
gt_desc <- gt(desc_table)

cat("\nDescriptive statistics by treatment:\n")
print(desc_table)

### Full mixed model

nested_model <- lmer(Weight ~ Evit * Cu * Time + (1 | Litter/Pig), data=dietox)

cat("\nFull model summary:\n")
print(summary(nested_model))

cat("\nANOVA for full model:\n")
print(anova(nested_model))
gt_anova_full <- gt(as.data.frame(anova(nested_model)))

cat("\nRandom effects:\n")
print(VarCorr(nested_model))

### Backward step selection

backward_selection <- step(nested_model)

cat("\nBackward step selection results:\n")
print(backward_selection)

final_model <- get_model(backward_selection)

cat("\nFinal selected model summary:\n")
print(summary(final_model))

cat("\nANOVA for final selected model:\n")
print(anova(final_model))
gt_anova_final <- gt(as.data.frame(anova(final_model)))

cat("\nRandom effects for final selected model:\n")
print(VarCorr(final_model))

### Estimated marginal means

cat("\nEstimated marginal means for Evit * Cu:\n")
emm_treat <- emmeans(nested_model, ~ Evit * Cu)
print(emm_treat)
gt_emm_treat <- gt(as.data.frame(emm_treat))

cat("\nPairwise comparisons for Evit * Cu:\n")
print(pairs(emm_treat, adjust="tukey"))
gt_pairs <- gt(as.data.frame(pairs(emm_treat, adjust="tukey")))

cat("\nEstimated marginal means by Time:\n")
emm_time <- emmeans(final_model, ~ Time)
print(emm_time)
gt_emm_time <- gt(as.data.frame(emm_time))

cat("\nEstimated marginal means for treatment within Time:\n")
emm_treat_time <- emmeans(nested_model, ~ Evit * Cu | Time)
print(emm_treat_time)
gt_emm_treat_time <- gt(as.data.frame(emm_treat_time))

### Predicted values

dietox$fitted_final <- fitted(final_model)
dietox$fitted_full <- fitted(nested_model)
dietox$resid_final <- resid(final_model)

plot_data <- summarise(
  group_by(dietox, Evit, Cu, Time),
  mean_weight=mean(Weight),
  mean_fitted=mean(fitted_full),
  .groups="drop"
)

plot_data$Time_num <- as.numeric(as.character(plot_data$Time))
dietox$Time_num <- as.numeric(as.character(dietox$Time))

### Plot 1: Raw growth curves + mean trend

p1 <- ggplot(dietox, aes(x=Time_num, y=Weight, group=Pig, color=interaction(Evit, Cu))) +
  geom_line(alpha=0.20) +
  stat_summary(aes(group=interaction(Evit, Cu)), fun=mean, geom="line", linewidth=1.2) +
  labs(
    title="Pig Weight Over Time by Treatment Group",
    x="Week",
    y="Weight (kg)",
    color="Evit.Cu"
  ) +
  theme_minimal()

print(p1)

### Plot 2: Observed vs fitted mean curves

p2 <- ggplot(plot_data, aes(x=Time_num, group=interaction(Evit, Cu), color=interaction(Evit, Cu))) +
  geom_line(aes(y=mean_weight), linetype="dashed", linewidth=0.9) +
  geom_line(aes(y=mean_fitted), linewidth=1.1) +
  labs(
    title="Observed (dashed) vs Fitted (solid) Mean Weight",
    x="Week",
    y="Weight (kg)",
    color="Evit.Cu"
  ) +
  theme_minimal()

print(p2)

### Plot 3: Residuals vs fitted

p3 <- ggplot(dietox, aes(x=fitted_final, y=resid_final)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(
    title="Residuals vs Fitted",
    x="Fitted values",
    y="Residuals"
  ) +
  theme_minimal()

print(p3)

### Brown Forsthe Test
cat("Brown Forsythe Test")

# Create grouping variable
group_var <- interaction(dietox$Evit, dietox$Cu, dietox$Time)

# Compute absolute deviations from group medians
median_resid <- ave(dietox$resid_final, group_var, FUN = median)
abs_dev <- abs(dietox$resid_final - median_resid)

# ANOVA on absolute deviations (Brown-Forsythe)
bf <- aov(abs_dev ~ group_var)

cat("\nBrown-Forsythe Test:\n")
summary(bf)

# Extract ANOVA table from summary
bf_summary <- summary(bf)[[1]]

# Convert to data frame
bf_df <- as.data.frame(bf_summary)

### Plot 4: Normal Q-Q plot

p4 <- ggplot(dietox, aes(sample=resid_final)) +
  stat_qq() +
  stat_qq_line() +
  labs(title="Normal Q-Q Plot of Residuals") +
  theme_minimal()

print(p4)

### Shapiro-Wilk Test
shapiro <- shapiro.test(dietox$resid_final)

cat("\nShapiro-Wilk Test:\n")
print(shapiro)

# Convert to data frame
shapiro_df <- data.frame(
  W = as.numeric(shapiro$statistic),
  p_value = shapiro$p.value
)

### Optional: Week 12 boxplot

week12 <- subset(dietox, Time == "12")

p5 <- ggplot(week12, aes(x=interaction(Evit, Cu), y=Weight)) +
  geom_boxplot() +
  labs(
    title="Week 12 Weight by Treatment Group",
    x="Treatment Group (Evit.Cu)",
    y="Weight (kg)"
  ) +
  theme_minimal()

print(p5)

library(MASS)

# Fit a simple linear model 
lm_temp <- lm(Weight ~ Evit * Cu * Time, data = dietox)

# Run Box-Cox
bc <- boxcox(lm_temp, lambda = seq(-2, 2, by = 0.01))

# Extract optimal lambda (max log-likelihood)
lambda_opt <- bc$x[which.max(bc$y)]

cat("\nEstimated lambda:\n")
print(lambda_opt)

cat("\n================ BOX-COX DIAGNOSTICS ================\n")



# Apply Box-Cox transformation
lambda_opt <- 0.545
dietox$bcWeight <- (dietox$Weight^lambda_opt - 1) / lambda_opt

# Fit model (same structure as original)
model_bc <- lmer(bcWeight ~ Evit * Cu * Time + (1 | Litter/Pig), data = dietox)

# Residuals
dietox$resid_bc <- resid(model_bc)

### -------------------------
### Brown-Forsythe (Box-Cox)
### -------------------------

# Grouping variable
group_var <- interaction(dietox$Evit, dietox$Cu, dietox$Time)

# Absolute deviations from group medians
median_resid_bc <- ave(dietox$resid_bc, group_var, FUN = median)
abs_dev_bc <- abs(dietox$resid_bc - median_resid_bc)

# ANOVA
bf_boxcox <- aov(abs_dev_bc ~ group_var)

cat("\nBrown-Forsythe Test (Box-Cox Model):\n")
print(summary(bf_boxcox))

# Convert to dataframe
bf_boxcox_df <- as.data.frame(summary(bf_boxcox)[[1]])

### Plot 5: Residuals vs fitted
dietox$fitted_bc <- fitted(model_bc)

p_resid_bc <- ggplot(dietox, aes(x = fitted_bc, y = resid_bc)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted (Box-Cox Model)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

print(p_resid_bc)



### -------------------------
### Shapiro-Wilk (Box-Cox)
### -------------------------

shapiro_boxcox <- shapiro.test(dietox$resid_bc)

cat("\nShapiro-Wilk Test (Box-Cox Model):\n")
print(shapiro_boxcox)

# Convert to dataframe + save
shapiro_boxcox_df <- data.frame(
  W = as.numeric(shapiro_boxcox$statistic),
  p_value = shapiro_boxcox$p.value
)
print(shapiro_boxcox_df)

### Plot 6: Normal Q-Q plot (Box-Cox Model)
p_qq_bc <- ggplot(dietox, aes(sample = resid_bc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot (Box-Cox Model)") +
  theme_minimal()

print(p_qq_bc)

### Export tables to CSV

write.csv(desc_table, "desc_table.csv", row.names=FALSE)
write.csv(as.data.frame(anova(nested_model)), "anova_full_model.csv", row.names=TRUE)
write.csv(as.data.frame(anova(final_model)), "anova_final_model.csv", row.names=TRUE)
write.csv(as.data.frame(emm_treat), "emm_treatment.csv", row.names=FALSE)
write.csv(as.data.frame(pairs(emm_treat, adjust="tukey")), "pairwise_treatment.csv", row.names=FALSE)
write.csv(as.data.frame(emm_time), "emm_time.csv", row.names=FALSE)
write.csv(as.data.frame(emm_treat_time), "emm_treatment_time.csv", row.names=FALSE)
write.csv(shapiro_df, "shapiro_original.csv", row.names = FALSE)
write.csv(bf_boxcox_df, "brown_forsythe_boxcox.csv", row.names = TRUE)
write.csv(shapiro_boxcox_df, "shapiro_boxcox.csv", row.names = FALSE)
### Export tables to png

gtsave(data=gt_desc, filename="desc_table.png")
gtsave(data=gt_anova_full, filename="full_model_anova.png")
gtsave(data=gt_anova_final, filename="final_model_anova.png")
gtsave(data=gt_emm_treat, filename="emm_treatment.png")
gtsave(data=gt_pairs, filename="emm_pairwise_treatment.png")
gtsave(data=gt_emm_time, filename="emm_time.png")
gtsave(data=gt_emm_treat_time, filename="emm_treatment_time.png")

### Export plots

ggsave("growth_curves_by_treatment.png", p1, width=9, height=6, dpi=300)
ggsave("observed_vs_fitted_curves.png", p2, width=9, height=6, dpi=300)
ggsave("residuals_vs_fitted.png", p3, width=7, height=5, dpi=300)
ggsave("qq_plot_residuals.png", p4, width=7, height=5, dpi=300)
ggsave("week12_boxplot.png", p5, width=8, height=5, dpi=300)
ggsave("residuals_vs_fitted_boxcox.png", p_resid_bc, width = 7, height = 5, dpi = 300)
ggsave("qq_plot_boxcox.png", p_qq_bc, width = 7, height = 5, dpi = 300)
