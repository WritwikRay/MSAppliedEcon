library("readr")
library("haven")
library("dplyr")


#stan <- read_csv("Documents/R-Practice/Forecasting Problem Sets/Data_Sets/STAN_ALL.csv")
stan <- read_dta("Documents/R-Practice/Forecasting Problem Sets/Data_Sets/STAN_ALL.dta")
glimpse(stan)


stan_reduced <- stan %>%
  select(COUNTRY, YEAR, VALU, WAGE, INDUSTRY, ISICREV3)

stan_reduced <- stan_reduced %>%
  arrange(COUNTRY, INDUSTRY, YEAR) %>%
  group_by(COUNTRY, INDUSTRY) %>%
  mutate(
    grw_VALU = 100 * (VALU / lag(VALU) - 1)
  ) %>%
  ungroup()

stan_reduced <- stan_reduced %>%
  arrange(COUNTRY, INDUSTRY, YEAR) %>%
  group_by(COUNTRY, INDUSTRY) %>%
  mutate(
    grw_WAGE = 100 * (WAGE / lag(WAGE) - 1)
  ) %>%
  ungroup()

stan_reduced <- stan_reduced %>%
  filter(!(is.na(VALU) & is.na(WAGE)))

is.na(stan_reduced$VALU) # all returns are FALSE, meaning no missing values
is.na(stan$VALU)         # old dataset has TRUE and FALSE mixed, so some missing values
is.na(stan_reduced$WAGE)


# Which industry is the Motor Veicles, Trailers and Semi-Trailers industry?
stan_reduced %>% 
  group_by(INDUSTRY) %>% 
  summarise() #ISICREV3 == 50?

stan_plot <- stan_reduced %>%
  filter(
    COUNTRY == "FRA",
    ISICREV3 == "50"
  )

ggplot(stan_plot, aes(x = grw_VALU, y = grw_WAGE)) +
  geom_point() +
  labs(
    title = "Growth Rates of Value Added and Wages",
    subtitle = "Motor Vehicles, Trailers and Semi-Trailers – France",
    x = "Growth rate of Value Added (%)",
    y = "Growth rate of Wages (%)")

ggplot(stan_plot, aes(x = YEAR, y = grw_WAGE)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Growth Rate of Wages Over Time",
    subtitle = "Motor Vehicles, Trailers and Semi-Trailers – France",
    x = "Year",
    y = "Wage Growth Rate (%)"
  )

stan_plot %>% 
  summarise(mean(grw_VALU, na.rm = TRUE),
            mean(grw_WAGE, na.rm = TRUE))
