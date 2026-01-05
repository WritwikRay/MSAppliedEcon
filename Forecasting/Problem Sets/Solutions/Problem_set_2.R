library("readr")
library("modelsummary")

expo_impo_de <- read_csv("Documents/R-Practice/Forecasting Problem Sets/Data_Sets/expo-impo-germany.csv")
glimpse(expo_impo_de)

expo_impo_de <- expo_impo_de %>%
  mutate(
    # Extract year and month from stringtime 
    year = as.numeric(substr(time, 1, 4)),
    month = as.numeric(substr(time, 6, nchar(time))),
    
    # Create proper date object (first day of each month)
    date = make_date(year, month, 1),
  )

# Plot exports and imports over time
ggplot(expo_impo_de, aes(x = date)) +
  geom_line(aes(y = exports, color = "Exports")) +
  geom_line(aes(y = imports, color = "Imports")) +
  labs(title = "German Exports and Imports Over Time",
       x = "Date", y = "Value", color = "Series") +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red"))

eid <- expo_impo_de

ts_exports <- ts(eid$exports, 
                 start = c(eid$year[1], eid$month[1]), 
                 frequency = 12)

ts_imports <- ts(eid$imports, 
                 start = c(eid$year[1], eid$month[1]), 
                 frequency = 12)

ts_p_exports <- ts(eid$p_exports, 
                   start = c(eid$year[1], eid$month[1]), 
                   frequency = 12)

ts_p_imports <- ts(eid$p_imports, 
                   start = c(eid$year[1], eid$month[1]), 
                   frequency = 12)

# Alternative: Combined plot using ggplot2
library(tidyr)

# Create long format data for plotting
eid_long <- eid %>%
  select(date, exports, imports, p_exports, p_imports) %>%
  pivot_longer(cols = c(exports, imports, p_exports, p_imports),
               names_to = "variable",
               values_to = "value")

# Create faceted plot
ggplot(eid_long, aes(x = date, y = value)) +
  geom_line(linewidth = 0.8, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "German Export-Import Over Time",
       x = "Date",
       y = "Value")

# Exports Decomposition
plot(ts_exports)
decomp_exports <- decompose(ts_exports, type = 'additive')
# I chose additive method since the peaks more or less follow
# the same variance with time, .i.e. the seasonal variation of the data stays
# relatively constant over time.
plot(decomp_exports)

# Comments about Exports decomposition ------------------------------------
# The raw exports data shows values ranging around 5.0-5.5×10^7 (50-55 million units) 
# with clear fluctuations over the 2000-2010 period.
# There's a strong upward trend from 2000 to around 2008
# A sharp decline occurs around 2008-2009 (likely the Global Financial Crisis)
# Recovery begins after 2009, with the trend turning upward again by 2010
# This reflects Germany's export-driven economy being heavily impacted by the global recession.
# There is a  regular, stable seasonal pattern with values oscillating around 
# 0.94-0.95, The consistency of the pattern suggests predictable seasonal fluctuations in exports 
# (possibly related to holidays, production cycles, or vacation periods)
# However the amplitude appears relatively small, indicating seasonality is present but not the dominant factor.
# The random component has some irregular movements visible around 2008-2009 (crisis period) but generally small.
# Overall, the German exports were on a strong growth trajectory until the 2008 financial crisis
# The crisis caused a significant but temporary disruption and it is now recovering.
# Seasonal patterns are consistent and predictable

#Imports Decomposition
plot(ts_imports)
decomp_imports <- decompose(ts_imports, type = 'additive')
plot(decomp_imports)
# Comments about Imports
# Very similar explanation.

plot(ts_p_exports)
decomp_p_exports <- decompose(ts_p_exports, type = 'additive')

plot(ts_p_imports)
decomp_ts_p_imports <- decompose(ts_p_imports, type = 'additive')

#Real Export and Import prices
eid <- eid %>% 
  mutate(real_exports = (exports - p_exports) * 100) %>% 
  mutate(real_imports = (imports - p_imports) * 100)

ts_real_exports <- ts(eid$real_exports, 
                 start = c(eid$year[1], eid$month[1]), 
                 frequency = 12)

ts_real_imports <- ts(eid$real_imports, 
                      start = c(eid$year[1], eid$month[1]), 
                      frequency = 12)

plot(ts_real_exports)
decomp_real_exports <- decompose(ts_real_exports, type = 'additive')
plot(decomp_real_exports)
# Similar explanation

plot(ts_real_imports)
decomp_real_imports <- decompose(ts_real_imports, type = 'additive')
plot(decomp_real_imports)
# Similar explanation

eid <- eid %>%
  mutate(time_index = row_number())

model_real_exports <- lm(real_exports ~ time_index, data = eid)
model_real_imports <- lm(real_imports ~ time_index, data = eid)

eid <- eid %>%
  mutate(
    real_exports_fitted = predict(model_real_exports),
    real_imports_fitted = predict(model_real_imports)
  )

model_real_exports
summary(model_real_exports)
model_real_imports
summary(model_real_imports)

ggplot(eid, aes(x = date)) +
  geom_line(aes(y = real_exports, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = real_exports_fitted, color = "Linear Trend"), 
            linewidth = 1.2, linetype = "dashed") +
  labs(title = "Real Exports with Linear Trend",
       x = "Date", y = "Real Exports", color = "Series") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "darkblue", 
                                "Linear Trend" = "red"))

ggplot(eid, aes(x = date)) +
  geom_line(aes(y = real_imports, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = real_imports_fitted, color = "Linear Trend"), 
            linewidth = 1.2, linetype = "dashed") +
  labs(title = "Real Imports with Linear Trend",
       x = "Date", y = "Real Imports", color = "Series") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "darkred", 
                                "Linear Trend" = "red"))

msummary(list(model_real_exports, model_real_imports),
         stars = c("*" = .1,
                   "**" = .05,
                   "***" = .01),
         fmt = "%.4f"
)

# positive (and signiificant) intercept with an upward trend, but is a 
# bad fit as can be seen visually as well as the exponentially high RMSE
# But why does R and Adjusted R squared seem fine?

# How to predict variables and generate residuals?
eid <- eid %>%
  mutate(
    # Fitted/predicted values
    pred_real_exports = predict(model_real_exports),
    pred_real_imports = predict(model_real_imports),
    
    # Residuals (actual - predicted)
    resid_real_exports = residuals(model_real_exports),
    resid_real_imports = residuals(model_real_imports)
  )

#Polynomial reg

# Fit polynomial models for real exports
model_real_exports_poly2 <- lm(real_exports ~ poly(time_index, 2), data = eid)
model_real_exports_poly5 <- lm(real_exports ~ poly(time_index, 5), data = eid)

# Fit polynomial models for real imports
model_real_imports_poly2 <- lm(real_imports ~ poly(time_index, 2), data = eid)
model_real_imports_poly5 <- lm(real_imports ~ poly(time_index, 5), data = eid)

eid <- eid %>%
  mutate(
    # Polynomial order 2 predictions
    pred_real_exports_poly2 = predict(model_real_exports_poly2),
    pred_real_imports_poly2 = predict(model_real_imports_poly2),
    # Polynomial order 5 predictions
    pred_real_exports_poly5 = predict(model_real_exports_poly5),
    pred_real_imports_poly5 = predict(model_real_imports_poly5)

#Compare all models for real exports
ggplot(eid, aes(x = date)) +
  geom_line(aes(y = real_exports, color = "Actual"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_real_exports, color = "Linear"), 
            linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = pred_real_exports_poly2, color = "Polynomial 2"), 
            linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = pred_real_exports_poly5, color = "Polynomial 5"), 
            linewidth = 1, linetype = "dashed") +
  labs(title = "Real Exports: Model Comparison",
       x = "Date", y = "Real Exports", color = "Model") +
  scale_color_manual(values = c("Actual" = "black",
                                "Linear" = "red",
                                "Polynomial 2" = "blue",
                                "Polynomial 5" = "green"))

    

# Compare all models for real imports
ggplot(eid, aes(x = date)) +
  geom_line(aes(y = real_imports, color = "Actual"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_real_imports, color = "Linear"), 
            linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = pred_real_imports_poly2, color = "Polynomial 2"), 
            linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = pred_real_imports_poly5, color = "Polynomial 5"), 
            linewidth = 1, linetype = "dashed") +
  labs(title = "Real Imports: Model Comparison",
       x = "Date", y = "Real Imports", color = "Model") +
  scale_color_manual(values = c("Actual" = "black",
                                "Linear" = "red",
                                "Polynomial 2" = "blue",
                                "Polynomial 5" = "green"))

# How to Standardize residuals? Do we use the command: rstandard or using formula?
