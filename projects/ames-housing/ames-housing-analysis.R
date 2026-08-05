# Ames Housing Statistical Consulting Analysis
# Student: Spencer Andries
# Course: STAT 692-601/701 - Summer 2026
# Purpose: Complete EDA, regression, inference, diagnostics, and exports

# -----------------------------------------------------------------------------
# 0. Package setup
# -----------------------------------------------------------------------------


library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(sandwich)
library(performance)
library(scales)
library(patchwork)
library(see)

options(scipen = 999)

# -----------------------------------------------------------------------------
# 1. File paths and data import
# -----------------------------------------------------------------------------

data_file <- "AMES Housing train Practice 2.csv"

ames_full <- read.csv(data_file, stringsAsFactors = FALSE)

#pulling out the needed column and and turn neighborhood values from text to categorical variables
#Also makes Oldtown the reference group
ames <- ames_full %>%
  select(SalePrice, GrLivArea, OverallQual, Neighborhood) %>%
  mutate(
    Neighborhood = factor(Neighborhood),
    Neighborhood = relevel(Neighborhood, ref = "OldTown")
  )

#printing number of rows and number of neighborhoods
cat("Rows:", nrow(ames), "\n")
cat("Neighborhoods:", nlevels(ames$Neighborhood), "\n")
str(ames)

# -----------------------------------------------------------------------------
# 2. Missing data and descriptive statistics
# -----------------------------------------------------------------------------
missing_summary <- tibble(
  variable = names(ames),
  missing_n = colSums(is.na(ames)),
  missing_percent = 100 * missing_n / nrow(ames)
)
# no missing values
print(missing_summary)


numeric_summary <- ames %>%
  summarise(
    across(
      c(SalePrice, GrLivArea, OverallQual),
      list(
        n = ~sum(!is.na(.x)),
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        q3 = ~quantile(.x, 0.75, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(numeric_summary)

#grouping summary statistics by neighborhood
neighborhood_summary <- ames %>%
  group_by(Neighborhood) %>%
  summarise(
    n = n(),
    mean_sale_price = mean(SalePrice),
    median_sale_price = median(SalePrice),
    sd_sale_price = sd(SalePrice),
    mean_living_area = mean(GrLivArea),
    mean_overall_quality = mean(OverallQual),
    .groups = "drop"
  ) %>%
  arrange(median_sale_price)

print(neighborhood_summary)

# comparing oldtown to nridght directly
oldtown_raw <- ames %>% filter(Neighborhood == "OldTown")
nridght_raw <- ames %>% filter(Neighborhood == "NridgHt")

raw_comparison <- tibble(
  Neighborhood = c("OldTown", "NridgHt"),
  n = c(nrow(oldtown_raw), nrow(nridght_raw)),
  mean_sale_price = c(mean(oldtown_raw$SalePrice), mean(nridght_raw$SalePrice)),
  median_sale_price = c(median(oldtown_raw$SalePrice), median(nridght_raw$SalePrice)),
  mean_living_area = c(mean(oldtown_raw$GrLivArea), mean(nridght_raw$GrLivArea)),
  mean_overall_quality = c(mean(oldtown_raw$OverallQual), mean(nridght_raw$OverallQual))
)

print(raw_comparison)


# Numeric correlations, output seems like multicollinearity is not super worrisome
correlation_matrix <- ames %>%
  select(SalePrice, GrLivArea, OverallQual) %>%
  cor(use = "complete.obs")

print(correlation_matrix)


# -----------------------------------------------------------------------------
# 3. Exploratory visualizations
# -----------------------------------------------------------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_sale_distribution <- ggplot(ames, aes(x = SalePrice)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Distribution of Sale Prices",
    x = "Sale price",
    y = "Number of homes"
  ) +
  plot_theme

print(p_sale_distribution)

p_living_distribution <- ggplot(ames, aes(x = GrLivArea)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Above-Ground Living Area",
    x = "Living area (square feet)",
    y = "Number of homes"
  ) +
  plot_theme

print(p_living_distribution)

p_scatter <- ggplot(ames, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Sale Price and Above-Ground Living Area",
    x = "Living area (square feet)",
    y = "Sale price"
  ) +
  plot_theme

print(p_scatter)

p_quality <- ggplot(ames, aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Sale Price by Overall Quality",
    x = "Overall quality rating",
    y = "Sale price"
  ) +
  plot_theme

print(p_quality)

p_neighborhood <- neighborhood_summary %>%
  mutate(Neighborhood = fct_reorder(Neighborhood, median_sale_price)) %>%
  ggplot(aes(x = median_sale_price, y = Neighborhood)) +
  geom_col() +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Median Sale Price by Neighborhood",
    x = "Median sale price",
    y = "Neighborhood"
  ) +
  plot_theme

print(p_neighborhood)

# Focused comparison showing why adjustment is needed
p_raw_comparison <- ames %>%
  filter(Neighborhood %in% c("OldTown", "NridgHt")) %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_jitter(width = 0.12, alpha = 0.35) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Unadjusted Sale Prices: OldTown vs NridgHt",
    subtitle = "Regression is needed because the homes also differ in size and quality",
    x = NULL,
    y = "Sale price"
  ) +
  plot_theme

print(p_raw_comparison)

# -----------------------------------------------------------------------------
# 4. Multiple linear regression model
# -----------------------------------------------------------------------------
model <- lm(
  SalePrice ~ GrLivArea + OverallQual + Neighborhood,
  data = ames
)

print(summary(model))

model_glance <- glance(model)
model_tidy <- tidy(model, conf.int = TRUE, conf.level = 0.95)

print(model_glance)
print(model_tidy)

# Key coefficients requested by Brenda
key_results <- model_tidy %>%
  filter(term %in% c("GrLivArea", "OverallQual", "NeighborhoodNridgHt")) %>%
  mutate(
    interpretation_unit = case_when(
      term == "GrLivArea" ~ "Per 1 square foot",
      term == "OverallQual" ~ "Per 1 quality point",
      term == "NeighborhoodNridgHt" ~ "NridgHt relative to OldTown"
    )
  )

# Add a 100-square-foot version of the living-area result
living_100 <- model_tidy %>%
  filter(term == "GrLivArea") %>%
  transmute(
    term = "GrLivArea_100sqft",
    estimate = estimate * 100,
    std.error = std.error * 100,
    statistic = statistic,
    p.value = p.value,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100,
    interpretation_unit = "Per 100 square feet"
  )

client_results <- bind_rows(key_results, living_100)
print(client_results)


# Plain-language values for console output
sqft_result <- client_results %>% filter(term == "GrLivArea_100sqft")
nridght_result <- client_results %>% filter(term == "NeighborhoodNridgHt")
quality_result <- client_results %>% filter(term == "OverallQual")

cat("\nCLIENT ANSWER 1\n")
cat(
  "Each additional 100 square feet is associated with an estimated $",
  format(round(sqft_result$estimate), big.mark = ","),
  " increase in expected sale price, holding quality and neighborhood constant.\n",
  sep = ""
)
cat(
  "95% CI: $", format(round(sqft_result$conf.low), big.mark = ","),
  " to $", format(round(sqft_result$conf.high), big.mark = ","), "\n",
  sep = ""
)

cat("\nCLIENT ANSWER 2\n")
cat(
  "NridgHt is associated with an estimated $",
  format(round(nridght_result$estimate), big.mark = ","),
  " premium relative to OldTown for homes with the same living area and quality.\n",
  sep = ""
)
cat(
  "95% CI: $", format(round(nridght_result$conf.low), big.mark = ","),
  " to $", format(round(nridght_result$conf.high), big.mark = ","), "\n",
  sep = ""
)

# -----------------------------------------------------------------------------
# 5. Heteroscedasticity-robust inference (HC3 sensitivity analysis)
# -----------------------------------------------------------------------------
hc3_vcov <- vcovHC(model, type = "HC3")
hc3_test <- coeftest(model, vcov. = hc3_vcov)
hc3_ci <- coefci(model, vcov. = hc3_vcov, level = 0.95)

hc3_results <- tibble(
  term = rownames(hc3_test),
  estimate = hc3_test[, 1],
  robust_std_error = hc3_test[, 2],
  robust_statistic = hc3_test[, 3],
  robust_p_value = hc3_test[, 4],
  robust_conf_low = hc3_ci[, 1],
  robust_conf_high = hc3_ci[, 2]
)

hc3_client_results <- hc3_results %>%
  filter(term %in% c("GrLivArea", "OverallQual", "NeighborhoodNridgHt"))

hc3_living_100 <- hc3_results %>%
  filter(term == "GrLivArea") %>%
  transmute(
    term = "GrLivArea_100sqft",
    estimate = estimate * 100,
    robust_std_error = robust_std_error * 100,
    robust_statistic = robust_statistic,
    robust_p_value = robust_p_value,
    robust_conf_low = robust_conf_low * 100,
    robust_conf_high = robust_conf_high * 100
  )

hc3_client_results <- bind_rows(hc3_client_results, hc3_living_100)
print(hc3_client_results)


# -----------------------------------------------------------------------------
# 6. Regression diagnostics
# -----------------------------------------------------------------------------
# Base R four-panel diagnostic plot
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

# Tidy diagnostic data
model_augmented <- augment(model) %>%
  mutate(
    observation = row_number(),
    cooks_threshold = 4 / nrow(ames),
    influential_4_over_n = .cooksd > cooks_threshold
  )

print(model_augmented)

p_residuals <- ggplot(model_augmented, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted sale price",
    y = "Residual"
  ) +
  plot_theme

print(p_residuals)

p_qq <- ggplot(model_augmented, aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot",
    x = "Theoretical quantiles",
    y = "Standardized residuals"
  ) +
  plot_theme

print(p_qq)

p_scale_location <- ggplot(
  model_augmented,
  aes(x = .fitted, y = sqrt(abs(.std.resid)))
) +
  geom_point(alpha = 0.45) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Scale-Location Plot",
    x = "Fitted sale price",
    y = "Square root of |standardized residual|"
  ) +
  plot_theme

print(p_scale_location)

cook_threshold <- 4 / nrow(ames)

p_cooks <- ggplot(model_augmented, aes(x = observation, y = .cooksd)) +
  geom_segment(aes(xend = observation, yend = 0)) +
  geom_hline(yintercept = cook_threshold, linetype = "dashed") +
  labs(
    title = "Cook's Distance",
    subtitle = paste0("Dashed line = 4/n = ", round(cook_threshold, 4)),
    x = "Observation",
    y = "Cook's distance"
  ) +
  plot_theme

print(p_cooks)

# Formal diagnostic tests
shapiro_result <- shapiro.test(residuals(model))
breusch_pagan_result <- bptest(model)
durbin_watson_result <- dwtest(model)

cat("\nShapiro-Wilk test\n")
print(shapiro_result)
cat("\nBreusch-Pagan test\n")
print(breusch_pagan_result)
cat("\nDurbin-Watson test\n")
print(durbin_watson_result)

formal_tests <- tibble(
  test = c("Shapiro-Wilk normality", "Breusch-Pagan heteroscedasticity", "Durbin-Watson autocorrelation"),
  statistic = c(
    unname(shapiro_result$statistic),
    unname(breusch_pagan_result$statistic),
    unname(durbin_watson_result$statistic)
  ),
  p_value = c(
    shapiro_result$p.value,
    breusch_pagan_result$p.value,
    durbin_watson_result$p.value
  )
)

print(formal_tests)

# Multicollinearity: car::vif returns GVIF for multi-df factors
vif_results <- car::vif(model)
print(vif_results)


# Additional check using performance package
collinearity_check <- performance::check_collinearity(model)
print(collinearity_check)


# Influential observations
influential_cases <- model_augmented %>%
  filter(influential_4_over_n) %>%
  arrange(desc(.cooksd)) %>%
  select(observation, .fitted, .resid, .std.resid, .hat, .cooksd)

cat("\nNumber above Cook's distance 4/n threshold:",
    nrow(influential_cases), "\n")
print(head(influential_cases, 20))


# Comprehensive visual assumption check
check_model(model)

#Overall, the diagnostic plots suggest that the regression model provides a reasonable fit to the data and that
#the assumptions of linearity and low multicollinearity are largely satisfied. The main violations are non-constant
#error variance (heteroscedasticity) and departures from normality, particularly among higher-priced homes.
#These findings support your decision to report HC3 robust confidence intervals, which provide more reliable inference despite these assumption violations.
#Influential observations are present but do not appear to unduly drive the model results.
# -----------------------------------------------------------------------------
# 8. Compact one-page cheat-sheet values
# -----------------------------------------------------------------------------
cheat_sheet <- tibble(
  pricing_rule = c(
    "Additional 100 square feet",
    "NridgHt compared with OldTown",
    "One-point increase in OverallQual"
  ),
  estimated_adjustment = c(
    sqft_result$estimate,
    nridght_result$estimate,
    quality_result$estimate
  ),
  conventional_95_CI_low = c(
    sqft_result$conf.low,
    nridght_result$conf.low,
    quality_result$conf.low
  ),
  conventional_95_CI_high = c(
    sqft_result$conf.high,
    nridght_result$conf.high,
    quality_result$conf.high
  )
)

print(cheat_sheet)


