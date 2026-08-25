library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(readr)

# ---- Load & clean revenue data ----
concession_rev <- read_excel("J:/JoshPomerantz/2026/Concessions/braves_concessions_sales_25_26.xlsx") %>%
  filter(event_general == "regular season") %>%
  mutate(across(c(revenue_fandb, revenue_food, revenue_bar, revenue_other,
                   revenue_retail, revenue_total), ~ parse_number(as.character(.)))) %>%
  mutate(date = if (is.character(date)) mdy(date) else as.Date(date))


concession_rev_2024 <- read_excel("J:/JoshPomerantz/2026/Concessions/braves_concessions_sales_24.xlsx") %>%
  mutate(across(c(revenue_fandb, revenue_food, revenue_bar, revenue_other,
                   revenue_retail, revenue_total), ~ parse_number(as.character(.)))) %>%
  mutate(date = if (is.character(date)) mdy(date) else as.Date(date))


concession_rev_all <- bind_rows(concession_rev, concession_rev_2024)

rev_2026 <- concession_rev %>% filter(season == 2026)

# ---- Load & clean KPI data ----
concession_kpis <- read_excel("J:/JoshPomerantz/2026/Concessions/braves_concessions_kpis_25_26.xlsx") %>%
  mutate(
    across(c(transactions, total, quantity, turnstile, trans_per_turnstile,
             items_per_trans, spend_per_trans, percap), ~ parse_number(as.character(.))),
    day = wday(saleAttribution_date, label = TRUE, abbr = TRUE),
    items_per_turnstile = quantity / turnstile
  )


concession_kpis_2024 <- read_excel("J:/JoshPomerantz/2026/Concessions/braves_concessions_kpis_24.xlsx")  %>%
  mutate(
    across(c(transactions, total, quantity, turnstile, trans_per_turnstile,
             items_per_trans, spend_per_trans, percap), ~ parse_number(as.character(.))),
    day = wday(saleAttribution_date, label = TRUE, abbr = TRUE),
    items_per_turnstile = quantity / turnstile
  )


concession_kpis_all <- bind_rows(concession_kpis, concession_kpis_2024)

concession_kpis_2026 <- concession_kpis %>% filter(season == 2026)

# ---- Combine: keep all KPI games, attach revenue where it matches ----
combined_2026 <- concession_kpis_2026 %>%
  left_join(
    rev_2026 %>% rename(saleAttribution_date = date) %>%
      select(saleAttribution_date, revenue_total, revenue_fandb),
    by = "saleAttribution_date"
  )

# ---- THE KEY COMPARISON: promo vs non-promo, by day of week ----
day_matched <- combined_2026 %>%
  group_by(day, concession_promotion) %>%
  summarise(
    n_games = n(),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_items_per_trans = mean(items_per_trans, na.rm = TRUE),
    avg_items_per_turnstile = mean(items_per_turnstile, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(day, concession_promotion)
print(day_matched)

day_matched_wide <- day_matched %>%
  pivot_wider(
    names_from = concession_promotion,
    values_from = c(n_games, avg_revenue_total, avg_percap, avg_items_per_trans, avg_items_per_turnstile, avg_trans_per_turnstile),
    names_prefix = "promo_"
  ) %>%
  mutate(
    revenue_diff_pct = (avg_revenue_total_promo_1 - avg_revenue_total_promo_0) / avg_revenue_total_promo_0 * 100,
    percap_diff_pct = (avg_percap_promo_1 - avg_percap_promo_0) / avg_percap_promo_0 * 100
  )
print(day_matched_wide)

# ---- By specific 2026 promotion (spring_value_games, may_in_the_a) ----
promo_cols_2026 <- c("spring_value_games", "may_in_the_a")

promo_type_summary <- combined_2026 %>%
  pivot_longer(cols = all_of(promo_cols_2026), names_to = "promo_type", values_to = "active") %>%
  filter(active == 1) %>%
  group_by(promo_type) %>%
  summarise(
    n_games = n(),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_items_per_trans = mean(items_per_trans, na.rm = TRUE),
    avg_items_per_turnstile = mean(items_per_turnstile, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE)
  )

no_promo_row <- combined_2026 %>%
  filter(concession_promotion == 0) %>%
  summarise(
    promo_type = "no_promotion",
    n_games = n(),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_items_per_trans = mean(items_per_trans, na.rm = TRUE),
    avg_items_per_turnstile = mean(items_per_turnstile, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE)
  )

promo_type_summary <- bind_rows(promo_type_summary, no_promo_row) %>%
  arrange(desc(avg_percap))
print(promo_type_summary)

day_promo_crosstab <- combined_2026 %>%
  mutate(promo_type = case_when(
    spring_value_games == 1 ~ "spring_value_games",
    may_in_the_a == 1 ~ "may_in_the_a",
    TRUE ~ "no_promotion"
  )) %>%
  count(day, promo_type) %>%
  pivot_wider(names_from = promo_type, values_from = n, values_fill = 0)

print(day_promo_crosstab)

day_promo_summary_26 <- combined_2026 %>%
  mutate(promo_type = case_when(
    spring_value_games == 1 ~ "spring_value_games",
    may_in_the_a == 1 ~ "may_in_the_a",
    TRUE ~ "no_promotion"
  )) %>%
  group_by(day, promo_type) %>%
  summarise(
    n_games = n(),
    avg_turnstile = mean(turnstile, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE),
    avg_items_per_turnstile = mean(items_per_turnstile, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE),
    .groups = "drop"
  )

turnstile_plot <- ggplot(day_promo_summary_26, aes(x = day, y = avg_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#979797ff",
    "spring_value_games" = "#4682B4",
    "may_in_the_a" = "#3CB371"
  )) +
  labs(x = "Day of week", y = "Avg turnstile",
       fill = "Promotion",
       title = "2026: Turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggsave(
  filename = "J:/JoshPomerantz/2026/Concessions/turnstile_by_day_promo.png",
  plot = turnstile_plot,
  width = 8,
  height = 5,
  dpi = 300
)


percap_plot <- ggplot(day_promo_summary_26, aes(x = day, y = avg_percap, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#979797ff",
    "spring_value_games" = "#4682B4",
    "may_in_the_a" = "#3CB371"
  )) +
  labs(x = "Day of week", y = "Avg per-cap spend",
       fill = "Promotion",
       title = "2026: Per-cap spend by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggsave(
  filename = "J:/JoshPomerantz/2026/Concessions/percap_by_day_promo.png",
  plot = percap_plot,
  width = 8,
  height = 5,
  dpi = 300
)


items_per_turnstile_plot <- ggplot(day_promo_summary_26, aes(x = day, y = avg_items_per_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#979797ff",
    "spring_value_games" = "#4682B4",
    "may_in_the_a" = "#3CB371"
  )) +
  labs(x = "Day of week", y = "Avg items per turnstile",
       fill = "Promotion",
       title = "2026: Items per turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggsave(
  filename = "J:/JoshPomerantz/2026/Concessions/items_per_turnstile_by_day_promo.png",
  plot = items_per_turnstile_plot,
  width = 8,
  height = 5,
  dpi = 300
)


trans_per_turnstile_plot <- ggplot(day_promo_summary_26, aes(x = day, y = avg_trans_per_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#979797ff",
    "spring_value_games" = "#4682B4",
    "may_in_the_a" = "#3CB371"
  )) +
  labs(x = "Day of week", y = "Avg transactions per turnstile",
       fill = "Promotion",
       title = "2026: Transactions per turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggsave(
  filename = "J:/JoshPomerantz/2026/Concessions/trans_per_turnstile_by_day_promo.png",
  plot = trans_per_turnstile_plot,
  width = 8,
  height = 5,
  dpi = 300
)


revenue_plot <- ggplot(day_promo_summary_26, aes(x = day, y = avg_revenue_total, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#979797ff",
    "spring_value_games" = "#4682B4",
    "may_in_the_a" = "#3CB371"
  )) +
  labs(x = "Day of week", y = "Avg revenue per game",
       fill = "Promotion",
       title = "2026: Revenue per game by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggsave(
  filename = "J:/JoshPomerantz/2026/Concessions/revenue_by_day_promo.png",
  plot = revenue_plot,
  width = 8,
  height = 5,
  dpi = 300
)

pct_diff_by_day <- day_promo_summary_26 %>%
  filter(promo_type %in% c("may_in_the_a", "no_promotion")) %>%
  select(day, promo_type, n_games, avg_turnstile, avg_percap,
         avg_items_per_turnstile, avg_trans_per_turnstile, avg_revenue_total) %>%
  pivot_wider(
    names_from = promo_type,
    values_from = c(n_games, avg_turnstile, avg_percap,
                     avg_items_per_turnstile, avg_trans_per_turnstile, avg_revenue_total)
  ) %>%
  mutate(
    pct_diff_turnstile = (avg_turnstile_may_in_the_a - avg_turnstile_no_promotion) / avg_turnstile_no_promotion * 100,
    pct_diff_percap = (avg_percap_may_in_the_a - avg_percap_no_promotion) / avg_percap_no_promotion * 100,
    pct_diff_items_per_turnstile = (avg_items_per_turnstile_may_in_the_a - avg_items_per_turnstile_no_promotion) / avg_items_per_turnstile_no_promotion * 100,
    pct_diff_trans_per_turnstile = (avg_trans_per_turnstile_may_in_the_a - avg_trans_per_turnstile_no_promotion) / avg_trans_per_turnstile_no_promotion * 100,
    pct_diff_revenue_total = (avg_revenue_total_may_in_the_a - avg_revenue_total_no_promotion) / avg_revenue_total_no_promotion * 100
  ) %>%
  select(day, n_games_may_in_the_a, n_games_no_promotion, starts_with("pct_diff"))

print(pct_diff_by_day, width = Inf)

combined_2026 <- combined_2026 %>%
  mutate(promo_type = case_when(
    spring_value_games == 1 ~ "spring_value_games",
    may_in_the_a == 1 ~ "may_in_the_a",
    TRUE ~ "no_promotion"
  ))

overall_pct_diff <- combined_2026 %>%
  filter(promo_type %in% c("may_in_the_a", "no_promotion")) %>%
  group_by(promo_type) %>%
  summarise(
    n_games = n(),
    avg_turnstile = mean(turnstile, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_items_per_trans = mean(items_per_trans, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = promo_type,
    values_from = c(n_games, avg_turnstile, avg_percap,
                     avg_items_per_trans, avg_trans_per_turnstile, avg_revenue_total)
  ) %>%
  mutate(
    pct_diff_turnstile = (avg_turnstile_may_in_the_a - avg_turnstile_no_promotion) / avg_turnstile_no_promotion * 100,
    pct_diff_percap = (avg_percap_may_in_the_a - avg_percap_no_promotion) / avg_percap_no_promotion * 100,
    pct_diff_items_per_trans = (avg_items_per_trans_may_in_the_a - avg_items_per_trans_no_promotion) / avg_items_per_trans_no_promotion * 100,
    pct_diff_trans_per_turnstile = (avg_trans_per_turnstile_may_in_the_a - avg_trans_per_turnstile_no_promotion) / avg_trans_per_turnstile_no_promotion * 100,
    pct_diff_revenue_total = (avg_revenue_total_may_in_the_a - avg_revenue_total_no_promotion) / avg_revenue_total_no_promotion * 100
  )

print(overall_pct_diff, width = Inf)


combined_all <- concession_kpis %>%
  left_join(
    concession_rev %>% rename(saleAttribution_date = date) %>%
      select(saleAttribution_date, revenue_total, revenue_fandb),
    by = "saleAttribution_date"
  ) %>%
  mutate(promo_type = case_when(
    spring_value_games == 1 ~ "spring_value_games",
    may_in_the_a == 1 ~ "may_in_the_a",
    oktoberfest == 1 ~ "oktoberfest",
    summer_steal == 1 ~ "summer_steal",
    slide_into_summer == 1 ~ "slide_into_summer",
    TRUE ~ "no_promotion"
  ))

day_promo_crosstab_all <- combined_all %>%
  count(season, day, promo_type) %>%
  pivot_wider(names_from = promo_type, values_from = n, values_fill = 0)

print(day_promo_crosstab_all)

day_promo_summary_all <- combined_all %>%
  group_by(season, day, promo_type) %>%
  summarise(
    n_games = n(),
    avg_turnstile = mean(turnstile, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE),
    avg_revenue_total = mean(revenue_total, na.rm = TRUE),
    avg_items_per_turnstile = mean(items_per_turnstile, na.rm = TRUE),
    avg_trans_per_turnstile = mean(trans_per_turnstile, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(day_promo_summary_all, aes(x = day, y = avg_percap, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 2.8) +
  facet_wrap(~ season, ncol = 1) +
  scale_fill_manual(values = c(
    "no_promotion" = "#888780",
    "spring_value_games" = "#378ADD",
    "may_in_the_a" = "#CE1141",
    "oktoberfest" = "#EDA100",
    "summer_steal" = "#639922",
    "slide_into_summer" = "#4A3AA7"
  )) +
  labs(x = "Day of week", y = "Avg per-cap spend",
       fill = "Promotion",
       title = "Per-cap spend by day: promotion vs. no promotion, 2025 vs. 2026",
       subtitle = "Numbers above bars = game count") +
  theme_minimal() +
  theme(legend.position = "bottom")

day_promo_summary_25 <- day_promo_summary_all %>% filter(season == 2025)

ggplot(day_promo_summary_25, aes(x = day, y = avg_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#b4b3a6ff",
    "summer_steal" = "#378ADD",
    "slide_into_summer" = "#CE1141",
    "oktoberfest" = "#067e10ff"
  )) +
  labs(x = "Day of week", y = "Avg turnstile",
       fill = "Promotion",
       title = "2025: Turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggplot(day_promo_summary_25, aes(x = day, y = avg_percap, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#b4b3a6ff",
    "summer_steal" = "#378ADD",
    "slide_into_summer" = "#CE1141",
    "oktoberfest" = "#067e10ff"
  )) +
  labs(x = "Day of week", y = "Avg per-cap spend",
       fill = "Promotion",
       title = "2025: Per-cap spend by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggplot(day_promo_summary_25, aes(x = day, y = avg_items_per_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#b4b3a6ff",
    "summer_steal" = "#378ADD",
    "slide_into_summer" = "#CE1141",
    "oktoberfest" = "#067e10ff"
  )) +
  labs(x = "Day of week", y = "Avg items per turnstile",
       fill = "Promotion",
       title = "2025: Items per turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggplot(day_promo_summary_25, aes(x = day, y = avg_trans_per_turnstile, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#b4b3a6ff",
    "summer_steal" = "#378ADD",
    "slide_into_summer" = "#CE1141",
    "oktoberfest" = "#067e10ff"
  )) +
  labs(x = "Day of week", y = "Avg transactions per turnstile",
       fill = "Promotion",
       title = "2025: Transactions per turnstile by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()

ggplot(day_promo_summary_25, aes(x = day, y = avg_revenue_total, fill = promo_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n_games), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "no_promotion" = "#b4b3a6ff",
    "summer_steal" = "#378ADD",
    "slide_into_summer" = "#CE1141",
    "oktoberfest" = "#067e10ff"
  )) +
  labs(x = "Day of week", y = "Avg revenue per game",
       fill = "Promotion",
       title = "2025: Revenue per game by day, promotion vs. no promotion",
       subtitle = "Numbers above bars = game count") +
  theme_minimal()


library(lme4)
library(lmerTest)  # gives p-values for fixed effects, which base lme4 omits

mixed_model_revenue <- lmer(
  revenue_total ~ concession_promotion + (1 | day),
  data = combined_all
)

schedule_2024 <- read.csv("J:/JoshPomerantz/schedule_info/2024_Schedule_info.csv") %>% mutate(Date = mdy(Date))
schedule_2025 <- read.csv("J:/JoshPomerantz/schedule_info/2025_Schedule_info.csv") %>% mutate(Date = mdy(Date))
schedule_2026 <- read.csv("J:/JoshPomerantz/schedule_info/2026_Schedule_info.csv") %>% mutate(Date = mdy(Date))

schedule_all <- bind_rows(schedule_2024, schedule_2025, schedule_2026) %>%
  rename(saleAttribution_date = Date)

combined_all <- combined_all %>%
  left_join(
    schedule_all %>%
      select(saleAttribution_date, Time_of_year, effective_day, O_Team, Time_Type, Month),
    by = "saleAttribution_date"
  ) 

combined_all <- combined_all %>%
  mutate(season = factor(season), Time_Type = factor(Time_Type))

library(lme4)
library(lmerTest)



# Adding Ticket Sales
library(bigrquery)

ticket_sales_query <- "
SELECT
  t.add_date,
  t.season_id,
  t.event_name,
  e.event_date,
  t.section_name,
  t.num_seats,
  t.block_purchase_price,
  t.pc_ticket,
  t.ticket_class
FROM `braves-data-prod.Ticketing.ticket` t
LEFT JOIN `braves-data-prod.Archtics_Views.v_event` e
  ON t.event_name = e.event_name
  AND t.season_id = e.season_id
WHERE t.season_id IN (569)
  AND e.season_id IN (569)

UNION ALL

SELECT
  t.add_date,
  t.season_id,
  t.event_name,
  e.event_date,
  t.section_name,
  t.num_seats,
  t.block_purchase_price,
  t.pc_ticket,
  t.ticket_class
FROM `braves-data-prod.Ticketing_Archive.ticket` t
LEFT JOIN `braves-data-prod.Ticketing_Archive.event` e
  ON t.event_name = e.event_name
  AND t.season_id = e.season_id
WHERE t.season_id IN (534, 510)
  AND e.season_id IN (534, 510)
"

ticket_sales_raw <- bq_project_query("braves-data-prod", ticket_sales_query) %>%
  bq_table_download()

ticket_sales_by_game <- ticket_sales_raw %>%
  mutate(event_date = as.Date(event_date)) %>%
  group_by(event_date) %>%
  summarise(
    total_tickets_sold = sum(num_seats, na.rm = TRUE),
    total_ticket_revenue = sum(block_purchase_price, na.rm = TRUE),
  ) %>%
  rename(saleAttribution_date = event_date)


# ---- Combine schedule + ticket sales directly ----


ticket_model_data <- schedule_all %>%
  left_join(ticket_sales_by_game, by = "saleAttribution_date") %>%
  mutate(
    season = factor(Season),
    Time_Type = factor(Time_Type)
  ) %>%
  filter(saleAttribution_date < Sys.Date(), !is.na(total_tickets_sold))


mixed_model_ticket_sales <- lmer(
  total_tickets_sold ~ effective_day + Month + O_Team + Time_Type + (1 | Season),
  data = ticket_model_data
)

summary(mixed_model_ticket_sales)

library(performance)

r2(mixed_model_ticket_sales)

ranef(mixed_model_ticket_sales)

vc_season <- as.data.frame(lme4::VarCorr(mixed_model_ticket_sales))
season_var <- vc_season$vcov[vc_season$grp == "season"]
resid_var_season <- vc_season$vcov[vc_season$grp == "Residual"]
icc_season <- season_var / (season_var + resid_var_season)
icc_season

combined_all <- combined_all %>%
  left_join(
    ticket_sales_by_game,
    by = "saleAttribution_date"
  )

options(scipen = 999)

# Turnstile mixed effects model with promotion as random effect

mixed_model_turnstile <- lmer(
  turnstile ~ season +  Time_Type + Time_of_year +
    (1 | concession_promotion),
  data = combined_all
)

r2(mixed_model_turnstile)

summary(mixed_model_turnstile)

ranef(mixed_model_turnstile)


vc <- as.data.frame(lme4::VarCorr(mixed_model_turnstile))
promotion_var <- vc$vcov[vc$grp == "concession_promotion"]
resid_var <- vc$vcov[vc$grp == "Residual"]
icc <- promotion_var / (promotion_var + resid_var)
icc

# Ticket sales mixed effects model with promotion as random effect

mixed_model_ticket_sales <- lmer(
  total_tickets_sold ~ season + Time_Type + Time_of_year +
    (1 | concession_promotion),
  data = combined_all
)

summary(mixed_model_ticket_sales)

r2(mixed_model_ticket_sales)

vc_tickets <- as.data.frame(lme4::VarCorr(mixed_model_ticket_sales))
promotion_var_tickets <- vc_tickets$vcov[vc_tickets$grp == "concession_promotion"]
resid_var_tickets <- vc_tickets$vcov[vc_tickets$grp == "Residual"]
icc_tickets <- promotion_var_tickets / (promotion_var_tickets + resid_var_tickets)
icc_tickets

ticket_sales_regression <- lm(
  total_tickets_sold ~ season + effective_day + Month + Time_Type + Time_of_year,
  data = combined_all
)

summary(ticket_sales_regression)


mixed_model_ticket_sales <- lmer(
  total_tickets_sold ~ season + Time_Type + Time_of_year + concession_promotion +
    (1 | Time_Type),
  data = combined_all
)
summary(mixed_model_ticket_sales)


r2(mixed_model_ticket_sales)

# Simple regression models

trans_regression <- lm(transactions ~ turnstile + effective_day + Month + season + O_Team + Time_Type + Time_of_year, data = combined_all)

summary(trans_regression)

rev_total_regression <- lm(total ~ transactions + effective_day + Month + season + O_Team + Time_Type + Time_of_year, data = combined_all)

summary(rev_total_regression)

# Reading in concession items and stands

items_2025 <- read.csv("J:/JoshPomerantz/2026/Concessions/concessions_2025_item_level.csv")
items_2026 <- read.csv("J:/JoshPomerantz/2026/Concessions/concessions_2026_item_level.csv")
stands_2025 <- read.csv("J:/JoshPomerantz/2026/Concessions/concessions_2025_stand_level.csv")
stands_2026 <- read.csv("J:/JoshPomerantz/2026/Concessions/concessions_2026_stand_level.csv")

names(items_2026)
names(stands_2026)

items_2025 <- items_2025 %>%
  mutate(saleAttribution_date = ymd(saleAttribution_date))


items_2026 <- items_2026 %>%
  mutate(saleAttribution_date = ymd(saleAttribution_date))


tiers_2025 <- read.csv("J:/JoshPomerantz/schedule_info/2025_Schedule_info.csv") %>%
  select(Date, Tier) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

tiers_2026 <- read.csv("J:/JoshPomerantz/schedule_info/2026_Schedule_info.csv") %>%
  select(Date, Tier) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

items_2025 <- items_2025 %>%
  left_join(tiers_2025, by = c("saleAttribution_date" = "Date"))

items_2026 <- items_2026 %>%
  left_join(tiers_2026, by = c("saleAttribution_date" = "Date"))

# Check everything matched
items_2025 %>% summarise(unmatched = sum(is.na(Tier)))
items_2026 %>% summarise(unmatched = sum(is.na(Tier)))

# ---- Combine years ----
items_all <- bind_rows(items_2025, items_2026)



stands_all <- bind_rows(stands_2025, stands_2026)


# ---- Price per item ----
items_all <- items_all %>%
  mutate(price_per_item = amount / quantity)

# ---- Pull promo flags from combined_all (one row per date) ----
promo_flags <- combined_all %>%
  select(saleAttribution_date, concession_promotion, promo_type, spring_value_games, may_in_the_a, spring_value_games, oktoberfest, summer_steal, slide_into_summer) %>%
  distinct()

# ---- Join onto items and stands ----
items_all <- items_all %>%
  left_join(promo_flags, by = "saleAttribution_date")

stands_all <- stands_all %>%
  mutate(saleAttribution_date = as.Date(saleAttribution_date)) %>%
  left_join(promo_flags, by = "saleAttribution_date")

items_all <- items_all %>%
  mutate(saleAttribution_date = as.Date(saleAttribution_date)) %>%
  filter(!is.na(concession_promotion))

stands_all <- stands_all %>%
  mutate(saleAttribution_date = as.Date(saleAttribution_date)) %>%
  filter(!is.na(concession_promotion))

# Confirm

items_all %>% summarise(unmatched = sum(is.na(concession_promotion)))
stands_all %>% summarise(unmatched = sum(is.na(concession_promotion)))

promo_cols_all <- c("spring_value_games", "may_in_the_a", "oktoberfest", "summer_steal", "slide_into_summer")

baseline_price <- items_all %>%
  filter(concession_promotion == 0) %>%
  group_by(name) %>%
  summarise(
    baseline_avg_price = mean(price_per_item, na.rm = TRUE),
    baseline_n = n(),
    .groups = "drop"
  )

item_price_by_promo_type <- items_all %>%
  pivot_longer(cols = all_of(promo_cols_all), names_to = "promo_flag_type", values_to = "active") %>%
  filter(active == 1) %>%
  group_by(name, promo_flag_type) %>%
  summarise(
    n_games = n(),
    avg_price = mean(price_per_item, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

discounted_items <- item_price_by_promo_type %>%
  left_join(baseline_price, by = "name") %>%
  mutate(
    price_diff = avg_price - baseline_avg_price,
    pct_diff = price_diff / baseline_avg_price * 100
  ) %>%
  filter(!is.na(baseline_avg_price), baseline_n >= 3) %>%
  arrange(promo_flag_type, pct_diff)

print(discounted_items, n = 50)

items_by_game <- items_all %>%
  group_by(name, saleAttribution_date, concession_promotion,
           spring_value_games, may_in_the_a, oktoberfest, summer_steal, slide_into_summer) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")

promo_items <- tibble::tribble(
  ~promo_flag_type,      ~name,
  "may_in_the_a",        "Ice Cream Sandwich",
  "may_in_the_a",        "Beer Miller High Life 12oz",
  "spring_value_games",  "Beer Miller High Life 12oz",
  "spring_value_games",  "Hot Dog All Beef 6-1",
  "oktoberfest",         "Beer Cruz Blanca Los Bravos Cleat",
  "oktoberfest",         "Beer Coors Light Cleat",
  "oktoberfest",         "Beer Blue Moon Cleat",
  "oktoberfest",         "Beer Blue Moon Blood Orange IPA Cleat",
  "slide_into_summer",   "Hot Dog All Beef 6-1",
  "slide_into_summer",   "Beer Miller High Life 12oz"
)


compare_item_promo <- function(data, item_name, promo_col) {
  d <- data %>% filter(name == item_name)
  promo_vals <- d %>% filter(.data[[promo_col]] == 1) %>% pull(quantity)
  base_vals  <- d %>% filter(concession_promotion == 0) %>% pull(quantity)

  if (length(promo_vals) < 2 || length(base_vals) < 2) {
    return(tibble(item = item_name, promo = promo_col,
                   n_promo = length(promo_vals), n_base = length(base_vals),
                   note = "too few games for a reliable CI/test"))
  }

  base_test <- t.test(base_vals)
  promo_test <- t.test(promo_vals)
  diff_test <- t.test(promo_vals, base_vals)

  tibble(
    item = item_name, promo = promo_col,
    n_base = length(base_vals), mean_base = mean(base_vals),
    ci_low_base = base_test$conf.int[1], ci_high_base = base_test$conf.int[2],
    n_promo = length(promo_vals), mean_promo = mean(promo_vals),
    ci_low_promo = promo_test$conf.int[1], ci_high_promo = promo_test$conf.int[2],
    pct_diff = (mean(promo_vals) - mean(base_vals)) / mean(base_vals) * 100,
    p_value = diff_test$p.value
  )
}

promo_item_results <- purrr::map2_dfr(
  promo_items$name, promo_items$promo_flag_type,
  ~ compare_item_promo(items_by_game, .x, .y)
)
print(promo_item_results, n = 20)

hot_dog_promos <- c("spring_value_games", "slide_into_summer")
food_comparison_items <- c("Chicken Tender Basket", "Burger Basket")

food_cannibalization <- purrr::map2_dfr(
  rep(food_comparison_items, each = length(hot_dog_promos)),
  rep(hot_dog_promos, times = length(food_comparison_items)),
  ~ compare_item_promo(items_by_game, .x, .y)
)
print(food_cannibalization, n = 20)

beer_promos <- c("may_in_the_a", "spring_value_games", "oktoberfest", "slide_into_summer")

drink_comparison_items <- c("Beer Coors Light 16oz", "Beer Miller Lite 16oz", "Beer Leinenkugel Summer Shandy 16oz", "Beer Blue Moon 16oz", "Water Bottled 20oz", "Coke Regular", "Coke Souvenir")

drink_comparison <- purrr::map2_dfr(
  rep(drink_comparison_items, each = length(beer_promos)),
  rep(beer_promos, times = length(drink_comparison_items)),
  ~ compare_item_promo(items_by_game, .x, .y)
)
print(drink_comparison, n = 40)

# Baseline: one CI per item for both quantity and revenue, from non-promo days


items_by_game_rev <- items_all %>%
  group_by(name, saleAttribution_date, concession_promotion,
           spring_value_games, may_in_the_a, oktoberfest, summer_steal, slide_into_summer) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    revenue = sum(amount, na.rm = TRUE),
    .groups = "drop"
  )

baseline_summary <- items_by_game_rev %>%
  filter(concession_promotion == 0, name != "", !is.na(name)) %>%
  group_by(name) %>%
  filter(n() >= 2) %>%   # t.test needs at least 2 observations
  summarise(
    n_base = n(),
    avg_quant_base = mean(quantity, na.rm = TRUE),
    qty_ci_low = tryCatch(t.test(quantity)$conf.int[1], error = function(e) NA_real_),
    qty_ci_high = tryCatch(t.test(quantity)$conf.int[2], error = function(e) NA_real_),
    avg_rev_base = mean(revenue, na.rm = TRUE),
    rev_ci_low = tryCatch(t.test(revenue)$conf.int[1], error = function(e) NA_real_),
    rev_ci_high = tryCatch(t.test(revenue)$conf.int[2], error = function(e) NA_real_),
    .groups = "drop"
  )

# Promo days: simple average quantity and revenue per item per promo
promo_summary <- items_by_game_rev %>%
  pivot_longer(cols = all_of(promo_cols_all), names_to = "promo_flag_type", values_to = "active") %>%
  filter(active == 1) %>%
  group_by(name, promo_flag_type) %>%
  summarise(
    n_promo = n(),
    avg_quant_promo = mean(quantity, na.rm = TRUE),
    avg_rev_promo = mean(revenue, na.rm = TRUE),
    .groups = "drop"
  )

# Combine
simple_promo_comparison <- promo_summary %>%
  left_join(baseline_summary, by = "name") %>%
  select(name, promo_flag_type, n_base, avg_quant_base, qty_ci_low, qty_ci_high,
         avg_rev_base, rev_ci_low, rev_ci_high, n_promo, avg_quant_promo, avg_rev_promo) %>%
  arrange(name, promo_flag_type)

print(simple_promo_comparison, n = 40)

# Find the top items by total quantity sold (across all non-promo days, as the "normal" baseline)
top_items_by_volume <- items_by_game_rev %>%
  filter(name != "", !is.na(name)) %>%
  group_by(name) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity))

print(top_items_by_volume, n = 25)

# Your named promo items, so they're never excluded even if lower volume
promo_item_names <- unique(promo_items$name)

# Top 20 by volume, plus the promo items, de-duplicated
items_to_keep <- top_items_by_volume %>%
  slice_head(n = 5) %>%
  pull(name) %>%
  union(promo_item_names) %>%
  union(c("Chicken Tender Basket", "Burger Basket"))

simple_promo_comparison_top <- simple_promo_comparison %>%
  filter(name %in% items_to_keep) %>%
  arrange(desc(avg_quant_base))

print(simple_promo_comparison_top, n = 40)

# ---- Promo category flags ----
promo_categories <- tibble::tribble(
  ~promo_flag_type,      ~is_food, ~is_bev,
  "spring_value_games",   1,        1,
  "may_in_the_a",         0,        1,   # beer + ice cream (treat ice cream as its own category below)
  "oktoberfest",          0,        1,
  "slide_into_summer",    1,        1
)

# ---- Comparison item sets by category ----
food_comparison_items <- c("Chicken Tender Basket", "Burger Basket")
bev_comparison_items <- c("Water Bottled 20oz", "Coke Regular", "Coke Souvenir",
                            "Beer Coors Light 16oz", "Beer Miller Lite 16oz",
                            "Beer Blue Moon 16oz", "Beer Leinenkugel Summer Shandy 16oz")
dessert_comparison_items <- c("Ice Cream Dippin Dot Cup", "Ice Cream Dippin Dot Helmet")

# ---- Build the comparison item list per promo ----
comparison_items_by_promo <- promo_categories %>%
  rowwise() %>%
  mutate(
    comparison_items = list(c(
      if (is_food == 1) food_comparison_items else NULL,
      if (is_bev == 1) bev_comparison_items else NULL,
      if (promo_flag_type == "may_in_the_a") dessert_comparison_items else NULL
    ))
  ) %>%
  ungroup()

# ---- Assemble final item list per promo: promo's own target items + relevant comparison items ----
promo_full_item_list <- promo_items %>%
  group_by(promo_flag_type) %>%
  summarise(target_items = list(unique(name)), .groups = "drop") %>%
  left_join(comparison_items_by_promo %>% select(promo_flag_type, comparison_items), by = "promo_flag_type") %>%
  rowwise() %>%
  mutate(all_items = list(union(target_items, comparison_items))) %>%
  ungroup()

# ---- Build the final table, promo by promo ----
promo_report <- purrr::map_dfr(1:nrow(promo_full_item_list), function(i) {
  promo <- promo_full_item_list$promo_flag_type[i]
  item_list <- promo_full_item_list$all_items[[i]]

  simple_promo_comparison %>%
    filter(promo_flag_type == promo, name %in% item_list)
})

print(promo_report, n = 60)

promo_report <- promo_report %>%
  left_join(promo_items %>% mutate(is_target = 1), by = c("name", "promo_flag_type")) %>%
  mutate(is_target = coalesce(is_target, 0))

promo_report_final <- promo_report %>%
  mutate(
    is_promo_item = is_target,  # rename for clarity: 1 = promo item, 0 = compared item
    qty_out_ci_high = if_else(avg_quant_promo > qty_ci_high, 1, 0),
    qty_out_ci_low  = if_else(avg_quant_promo < qty_ci_low, 1, 0),
    rev_out_ci_high = if_else(avg_rev_promo > rev_ci_high, 1, 0),
    rev_out_ci_low  = if_else(avg_rev_promo < rev_ci_low, 1, 0)
  ) %>%
  select(
    promo_flag_type, name, is_promo_item,
    n_base, avg_quant_base, qty_ci_low, qty_ci_high,
    n_promo, avg_quant_promo, qty_out_ci_high, qty_out_ci_low,
    avg_rev_base, rev_ci_low, rev_ci_high,
    avg_rev_promo, rev_out_ci_high, rev_out_ci_low
  ) %>%
  arrange(promo_flag_type, desc(is_promo_item), desc(avg_quant_base))

print(promo_report_final, n = 60)

#Ridgeline plots

library(ggridges)
library(ggplot2)
library(patchwork)

promo_cols_all <- c("spring_value_games", "may_in_the_a", "slide_into_summer")

promo_stand_names_to_plot <- c("C343 MARKET", "C313 MARKET")
comparison_stand_names <- c("C135 BALLPARK CLASSICS", "FOH CHOPHOUSE", "C113 1871 GRILLE",
                              "C143 CHICK-FIL-A", "C149 CENTERFIELD MARKET")
stands_to_plot <- unique(c(promo_stand_names_to_plot, comparison_stand_names))

relevant_promo_items <- promo_items %>%
  filter(promo_flag_type %in% promo_cols_all) %>%
  pull(name) %>%
  unique()

items_to_plot <- unique(c(relevant_promo_items, "Chicken Tender Basket", "Burger Basket",
                            "Water Bottled 20oz", "Beer Coors Light 16oz",
                            "Beer Miller Lite 16oz", "Coke Regular", "Coke Souvenir",
                            "Ice Cream Dippin Dot Cup", "Beer Blue Moon 16oz",
                            "Beer Leinenkugel Summer Shandy 16oz", "Hot Dog All Beef 4-1","Hot Dog All Beef 2-1"))

# ---- Join turnstile counts ----
turnstile_by_date <- combined_all %>%
  select(saleAttribution_date, turnstile) %>%
  distinct()

items_by_game_norm <- items_by_game_rev %>%
  left_join(turnstile_by_date, by = "saleAttribution_date") %>%
  mutate(
    quantity_per_turnstile = quantity / turnstile,
    revenue_per_turnstile = revenue / turnstile
  )


stands_by_game <- stands_all %>%
  group_by(cost_center_origin_name, saleAttribution_date, concession_promotion,
           spring_value_games, may_in_the_a, oktoberfest, summer_steal, slide_into_summer) %>%
  summarise(
    transactions = sum(transactions, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  )


stands_by_game_norm <- stands_by_game %>%
  left_join(turnstile_by_date, by = "saleAttribution_date") %>%
  mutate(
    transactions_per_turnstile = transactions / turnstile,
    revenue_per_turnstile = total / turnstile
  )

# Check everything matched
items_by_game_norm %>% summarise(unmatched = sum(is.na(turnstile)))
stands_by_game_norm %>% summarise(unmatched = sum(is.na(turnstile)))

# ============================================================
# ITEMS
# ============================================================

ridge_data <- items_by_game_norm %>%
  filter(name %in% items_to_plot) %>%
  pivot_longer(cols = all_of(promo_cols_all), names_to = "promo_flag_type", values_to = "active") %>%
  mutate(
    promo_status = case_when(
      active == 1 ~ promo_flag_type,
      concession_promotion == 0 ~ "no_promotion",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(promo_status)) %>%
  distinct(name, saleAttribution_date, promo_status, quantity_per_turnstile, revenue_per_turnstile)

ridge_data <- ridge_data %>%
  mutate(promo_status = factor(promo_status, levels = c(
    "no_promotion",
    setdiff(unique(promo_status), "no_promotion")
  )))


# ---- Ridgeline: quantity per turnstile ----
ggplot(ridge_data, aes(x = quantity_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ name, scales = "free_x") +
  labs(x = "Quantity sold per turnstile", y = NULL,
       title = "Distribution of quantity sold per turnstile: promo vs. non-promo") +
  theme_ridges() +
  theme(legend.position = "none")

# ---- Ridgeline: revenue per turnstile ----
ggplot(ridge_data, aes(x = revenue_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ name, scales = "free_x") +
  labs(x = "Revenue per turnstile", y = NULL,
       title = "Distribution of revenue per turnstile: promo vs. non-promo") +
  theme_ridges() +
  theme(legend.position = "none")

# ---- Kernel density: quantity per turnstile ----
ggplot(ridge_data, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Quantity sold per turnstile", y = "Density",
       title = "Density of quantity sold per turnstile: promo vs. non-promo") +
  theme_minimal()

# ---- Kernel density: revenue per turnstile ----
ggplot(ridge_data, aes(x = revenue_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Revenue per turnstile", y = "Density",
       title = "Density of revenue per turnstile: promo vs. non-promo") +
  theme_minimal()

# ============================================================
# STANDS
# ============================================================

ridge_data_stands <- stands_by_game_norm %>%
  filter(cost_center_origin_name %in% stands_to_plot) %>%
  pivot_longer(cols = all_of(promo_cols_all), names_to = "promo_flag_type", values_to = "active") %>%
  mutate(
    promo_status = case_when(
      active == 1 ~ promo_flag_type,
      concession_promotion == 0 ~ "no_promotion",
      TRUE ~ NA_character_
    ),
    stand_role = if_else(cost_center_origin_name %in% promo_stand_names_to_plot, "Promo Stand", "Comparison Stand"),
    cost_center_origin_name = factor(cost_center_origin_name, levels = c(
      sort(promo_stand_names_to_plot),
      sort(comparison_stand_names)
    ))
  ) %>%
  filter(!is.na(promo_status)) %>%
  distinct(cost_center_origin_name, saleAttribution_date, promo_status,
           transactions_per_turnstile, revenue_per_turnstile, stand_role)

ridge_data_stands <- ridge_data_stands %>%
  mutate(promo_status = factor(promo_status, levels = c(
    "no_promotion",
    setdiff(unique(promo_status), "no_promotion")
  )))


promo_ridge <- ggplot(ridge_data_stands %>% filter(stand_role == "Promo Stand"),
                       aes(x = transactions_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ cost_center_origin_name, scales = "free_x", ncol = 2) +
  labs(title = "Promo Stands", x = "Transactions per turnstile", y = NULL) +
  theme_ridges() +
  theme(legend.position = "none")

comparison_ridge <- ggplot(ridge_data_stands %>% filter(stand_role == "Comparison Stand"),
                            aes(x = transactions_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ cost_center_origin_name, scales = "free_x", ncol = 3) +
  labs(title = "Comparison Stands", x = "Transactions per turnstile", y = NULL) +
  theme_ridges() +
  theme(legend.position = "none")

promo_ridge / comparison_ridge

promo_ridge_rev <- ggplot(ridge_data_stands %>% filter(stand_role == "Promo Stand"),
                           aes(x = revenue_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ cost_center_origin_name, scales = "free_x", ncol = 2) +
  labs(title = "Promo Stands", x = "Revenue per turnstile", y = NULL) +
  theme_ridges() +
  theme(legend.position = "none")

comparison_ridge_rev <- ggplot(ridge_data_stands %>% filter(stand_role == "Comparison Stand"),
                                aes(x = revenue_per_turnstile, y = promo_status, fill = promo_status)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~ cost_center_origin_name, scales = "free_x", ncol = 3) +
  labs(title = "Comparison Stands", x = "Revenue per turnstile", y = NULL) +
  theme_ridges() +
  theme(legend.position = "none")


promo_ridge_rev / comparison_ridge_rev



# ---- Patchwork: transactions per turnstile, promo vs. comparison stands ----
promo_plot <- ggplot(ridge_data_stands %>% filter(stand_role == "Promo Stand"),
                      aes(x = transactions_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ cost_center_origin_name, scales = "free", ncol = 2) +
  labs(title = "Promo Stands", x = "Transactions per turnstile", y = "Density") +
  theme_minimal()

comparison_plot <- ggplot(ridge_data_stands %>% filter(stand_role == "Comparison Stand"),
                           aes(x = transactions_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ cost_center_origin_name, scales = "free", ncol = 3) +
  labs(title = "Comparison Stands", x = "Transactions per turnstile", y = "Density") +
  theme_minimal()

promo_plot / comparison_plot + plot_layout(heights = c(1, 1.5))

# ---- Patchwork: revenue per turnstile, promo vs. comparison stands ----
promo_plot_rev <- ggplot(ridge_data_stands %>% filter(stand_role == "Promo Stand"),
                          aes(x = revenue_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ cost_center_origin_name, scales = "free", ncol = 2) +
  labs(title = "Promo Stands", x = "Revenue per turnstile", y = "Density") +
  theme_minimal()

comparison_plot_rev <- ggplot(ridge_data_stands %>% filter(stand_role == "Comparison Stand"),
                               aes(x = revenue_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ cost_center_origin_name, scales = "free", ncol = 3) +
  labs(title = "Comparison Stands", x = "Revenue per turnstile", y = "Density") +
  theme_minimal()

promo_plot_rev / comparison_plot_rev + plot_layout(heights = c(1, 1.5))

#write_xlsx(promo_report_final, "J:/JoshPomerantz/2026/Concessions/promo_report_final.xlsx")

# ---- Baseline: CI for transactions AND revenue per stand on non-promo days ----



stand_baseline <- stands_by_game %>%
  filter(concession_promotion == 0, cost_center_origin_name != "", !is.na(cost_center_origin_name)) %>%
  group_by(cost_center_origin_name) %>%
  filter(n() >= 2) %>%
  summarise(
    n_base = n(),
    avg_transactions_base = mean(transactions, na.rm = TRUE),
    trans_ci_low = tryCatch(t.test(transactions)$conf.int[1], error = function(e) NA_real_),
    trans_ci_high = tryCatch(t.test(transactions)$conf.int[2], error = function(e) NA_real_),
    avg_revenue_base = mean(total, na.rm = TRUE),
    rev_ci_low = tryCatch(t.test(total)$conf.int[1], error = function(e) NA_real_),
    rev_ci_high = tryCatch(t.test(total)$conf.int[2], error = function(e) NA_real_),
    .groups = "drop"
  )



# ---- Promo days: avg transactions AND revenue per stand, per promo type ----
promo_cols_main <- c("spring_value_games", "may_in_the_a", "slide_into_summer")

stand_promo_summary <- stands_by_game %>%
  filter(cost_center_origin_name != "", !is.na(cost_center_origin_name)) %>%
  pivot_longer(cols = all_of(promo_cols_main), names_to = "promo_flag_type", values_to = "active") %>%
  filter(active == 1) %>%
  group_by(cost_center_origin_name, promo_flag_type) %>%
  summarise(
    n_promo = n(),
    avg_transactions_promo = mean(transactions, na.rm = TRUE),
    avg_revenue_promo = mean(total, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Promo stand flag ----
promo_stand_names <- c("MARKET_343", "1871 GRILLE / THE SLICE / TACO FACTORY_313")

# ---- Combine and flag ----
stand_promo_report <- stand_promo_summary %>%
  left_join(stand_baseline, by = "cost_center_origin_name") %>%
  mutate(
    is_promo_stand = if_else(cost_center_origin_name %in% promo_stand_names, 1, 0),
    trans_pct_diff = (avg_transactions_promo - avg_transactions_base) / avg_transactions_base,
    trans_outside_ci_higher = if_else(avg_transactions_promo > trans_ci_high, 1, 0),
    trans_outside_ci_lower  = if_else(avg_transactions_promo < trans_ci_low, 1, 0),
    rev_pct_diff = (avg_revenue_promo - avg_revenue_base) / avg_revenue_base,
    rev_outside_ci_higher = if_else(avg_revenue_promo > rev_ci_high, 1, 0),
    rev_outside_ci_lower  = if_else(avg_revenue_promo < rev_ci_low, 1, 0)
  ) %>%
  select(promo_flag_type, cost_center_origin_name, is_promo_stand,
         n_base, avg_transactions_base, trans_ci_low, trans_ci_high,
         n_promo, avg_transactions_promo, trans_pct_diff, trans_outside_ci_higher, trans_outside_ci_lower,
         avg_revenue_base, rev_ci_low, rev_ci_high,
         avg_revenue_promo, rev_pct_diff, rev_outside_ci_higher, rev_outside_ci_lower) %>%
  arrange(promo_flag_type, desc(is_promo_stand), desc(trans_pct_diff))

# ---- Major stands: promo stands + top stands by baseline transaction volume ----
top_stands_by_volume <- stand_baseline %>%
  arrange(desc(avg_transactions_base)) %>%
  slice_head(n = 10) %>%
  pull(cost_center_origin_name) %>%
  union(promo_stand_names)

stand_promo_report_major <- stand_promo_report %>%
  filter(cost_center_origin_name %in% top_stands_by_volume) %>%
  arrange(promo_flag_type, desc(is_promo_stand), desc(avg_transactions_base)) %>%
  rename(loc_name = cost_center_origin_name, avg_trans_base = avg_transactions_base, avg_trans_promo = avg_transactions_promo, trans_out_ci_high = trans_outside_ci_higher, trans_out_ci_low = trans_outside_ci_lower,
  avg_rev_base = avg_revenue_base, avg_rev_promo = avg_revenue_promo, rev_out_ci_high = rev_outside_ci_higher, rev_out_ci_low = rev_outside_ci_lower)

print(stand_promo_report_major, n = 100)

# ---- Write to Excel with percent formatting ----
#library(openxlsx)

#wb <- createWorkbook()
#addWorksheet(wb, "Report")
#writeData(wb, "Report", stand_promo_report_major)

#pct_style <- createStyle(numFmt = "0.0%")
#pct_cols <- which(names(stand_promo_report_major) %in% c("trans_pct_diff", "rev_pct_diff"))
#addStyle(wb, "Report", style = pct_style,
#         cols = pct_cols, rows = 2:(nrow(stand_promo_report_major) + 1), gridExpand = TRUE)

#saveWorkbook(wb, "J:/JoshPomerantz/2026/Concessions/stands_promo_report.xlsx", overwrite = TRUE)

# Show rate work

library(readxl)


showrate_overall <- read_excel(
  "J:/JoshPomerantz/2026/Showrate Overall/showrates_overall.xlsx",
  sheet = "Past Games Show Rate"
) %>%
  mutate(event_date = if (is.character(event_date)) mdy(event_date) else as.Date(event_date)) %>%
  rename(saleAttribution_date = event_date)

# Sanity check date parsing worked
class(showrate_overall$saleAttribution_date)
head(showrate_overall$saleAttribution_date)

# ---- Join onto combined_2026 ----
combined_2026 <- combined_2026 %>%
  left_join(
    showrate_overall %>%
      select(saleAttribution_date, weekday, is_weekday, is_summer, sold_seats, attended_seats, show_rate),
    by = "saleAttribution_date"
  )

# Confirm match rate
combined_2026 %>% summarise(unmatched = sum(is.na(show_rate)))

combined_2026 <- combined_2026 %>%
  left_join(
    ticket_sales_by_game %>% select(saleAttribution_date, total_tickets_sold),
    by = "saleAttribution_date"
  )

summer_weekday_summary <- combined_2026 %>%
  filter(is_summer == 1, is_weekday == 1) %>%
  summarise(
    n_games = n(),
    avg_tickets_sold = mean(total_tickets_sold, na.rm = TRUE),
    avg_turnstile = mean(turnstile, na.rm = TRUE),
    avg_showrate = mean(show_rate, na.rm = TRUE),
    avg_concession_revenue = mean(revenue_total, na.rm = TRUE),
    avg_percap = mean(percap, na.rm = TRUE)
  )

print(summer_weekday_summary)


# New Item Analysis and T-tests by Game Tiers

library(stringr)
library(purrr)

combined_all <- combined_all %>%
  filter(saleAttribution_date != as.Date("2025-08-09"))

turnstile_by_date <- combined_all %>%
  select(saleAttribution_date, turnstile) %>%
  distinct()


items_all_v2 <- bind_rows(items_2025, items_2026) %>%
  mutate(price_per_item = amount / quantity) %>%
  left_join(promo_flags, by = "saleAttribution_date") %>%   # promo_flags built earlier from combined_all
  left_join(turnstile_by_date, by = "saleAttribution_date") %>%
  filter(!is.na(concession_promotion))  # drops the postponed/makeup dates, same as before

# Check category labels so beer/food filtering below is accurate
items_all_v2 %>% distinct(mlb_category, category) %>% head(50)

items_by_game_v2 <- items_all_v2 %>%
  group_by(name, saleAttribution_date, Tier, concession_promotion,
           spring_value_games, may_in_the_a, oktoberfest, summer_steal, slide_into_summer,
           turnstile) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE), revenue = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(quantity_per_turnstile = quantity / turnstile, revenue_per_turnstile = revenue / turnstile)

# Top beers by total quantity (adjust filter if category-based works better)
top_beers <- items_by_game_v2 %>%
  filter(str_detect(name, "^Beer")) %>%
  group_by(name) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 7)
print(top_beers)

# Top food items by total quantity, excluding all hot dog variants (those are the promo item itself)
top_food <- items_by_game_v2 %>%
  filter(!str_detect(name, "^Beer|Hot Dog|^Water|^Coke|^Sprite|^Topo Chico|Ice Cream|^Vodka|^Wine")) %>%
  group_by(name) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 10)
print(top_food)

food_items_to_plot <- unique(c(
  "Hot Dog All Beef 6-1",
  "Burger Basket",
  "Chicken Tender Basket",
  "CFA Chicken Sandwich",
  "Pizza Pepperoni Slice 18",
  "Snack Nachos Classic"
))


# ---- Step 3: comparison item sets + the missing function ----
beer_items_to_plot <- unique(c("Beer Miller High Life 12oz", top_beers$name))

build_ridge_data <- function(item_list, promo_cols) {
  items_by_game_v2 %>%
    filter(name %in% item_list) %>%
    pivot_longer(cols = all_of(promo_cols), names_to = "promo_flag_type", values_to = "active") %>%
    mutate(promo_status = case_when(
      active == 1 ~ promo_flag_type,
      concession_promotion == 0 ~ "no_promotion",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(promo_status)) %>%
    distinct(name, saleAttribution_date, Tier, promo_status, quantity_per_turnstile, revenue_per_turnstile) %>%
    mutate(promo_status = factor(promo_status, levels = c("no_promotion", setdiff(unique(promo_status), "no_promotion"))))
}

# ---- Rebuild beer_ridge_data with all three beer promos ----
beer_ridge_data <- build_ridge_data(beer_items_to_plot, c("may_in_the_a", "spring_value_games", "slide_into_summer"))

food_ridge_data <- build_ridge_data(food_items_to_plot, c("spring_value_games", "slide_into_summer"))

# ---- Beer: restrict to tiers 2 and 3 (May in the A) ----
beer_ridge_data_filtered <- beer_ridge_data %>%
  filter(
    (promo_status == "may_in_the_a" & Tier %in% c(2, 3)) |
    (promo_status == "spring_value_games" & Tier == 5) |
    (promo_status == "slide_into_summer" & Tier %in% c(1, 3)) |
    (promo_status == "no_promotion" & Tier %in% c(1, 2, 3, 5))
  )

ggplot(beer_ridge_data_filtered, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_grid(Tier ~ name, scales = "free") +
  labs(x = "Quantity per turnstile", y = "Density",
       title = "Beer performance by tier: Miller High Life vs. other top beers") +
  theme_minimal()

ggplot(beer_ridge_data_filtered, aes(x = revenue_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_grid(Tier ~ name, scales = "free") +
  labs(x = "Revenue per turnstile", y = "Density",
       title = "Beer revenue by tier: Miller High Life vs. other top beers") +
  theme_minimal()

# ---- Food: restrict to tiers 1, 3, and 5 (Slide into Summer: 1,3; Spring Value Games: 5) ----

food_ridge_data_filtered <- food_ridge_data %>%
  filter(Tier %in% c(1, 3, 5))

ggplot(food_ridge_data_filtered, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_grid(Tier ~ name, scales = "free") +
  labs(x = "Quantity per turnstile", y = "Density",
       title = "Food performance by tier: Hot Dog vs. other top food items") +
  theme_minimal()

ggplot(food_ridge_data_filtered, aes(x = revenue_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  facet_grid(Tier ~ name, scales = "free") +
  labs(x = "Revenue per turnstile", y = "Density",
       title = "Food revenue by tier: Hot Dog vs. other top food items") +
  theme_minimal()


# ---- Step 5: t-tests on category revenue/quantity per turnstile, promo vs. non-promo, overall + by tier ----
# ---- Rebuild base data with promo-specific flags instead of the generic concession_promotion ----
category_revenue_by_game_norm <- items_all_v2 %>%
  mutate(item_category = case_when(
    mlb_category == "Beer" ~ "beer",
    str_detect(name, "Hot Dog|Chicken Tender|Burger|Fries|Nachos|Pretzel|Popcorn|Peanut|CFA|Pizza") ~ "food",
    TRUE ~ "other"
  )) %>%
  filter(item_category %in% c("beer", "food")) %>%
  mutate(
    beer_promo = if_else(may_in_the_a == 1 | spring_value_games == 1 | slide_into_summer == 1, 1, 0),
    food_promo = if_else(spring_value_games == 1 | slide_into_summer == 1, 1, 0)
  ) %>%
  group_by(saleAttribution_date, Tier, concession_promotion, beer_promo, food_promo, item_category) %>%
  summarise(category_revenue = sum(amount, na.rm = TRUE), category_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  left_join(turnstile_by_date, by = "saleAttribution_date") %>%
  mutate(
    revenue_per_turnstile = category_revenue / turnstile,
    quantity_per_turnstile = category_quantity / turnstile
  )

# Beer promos ran at tiers 1, 2, 3, 5 (may_in_the_a: 2,3 | spring_value_games: 5 | slide_into_summer: 1,3)
beer_relevant_tiers <- c(1, 2, 3, 5)
# Food promos ran at tiers 1, 3, 5 (spring_value_games: 5 | slide_into_summer: 1,3)
food_relevant_tiers <- c(1, 3, 5)
run_promo_ttest <- function(data, category, metric_col, promo_col, relevant_tiers, tier_filter = NULL) {
  if (is.null(tier_filter)) {
    # Overall: promo games restricted to relevant tiers, baseline uses ALL non-promo games
    promo_vals <- data %>%
      filter(item_category == category, Tier %in% relevant_tiers, .data[[promo_col]] == 1) %>%
      pull(.data[[metric_col]])
    base_vals <- data %>%
      filter(item_category == category, concession_promotion == 0) %>%
      pull(.data[[metric_col]])
  } else {
    # By tier: compare within the same tier only
    d <- data %>% filter(item_category == category, Tier == tier_filter)
    promo_vals <- d %>% filter(.data[[promo_col]] == 1) %>% pull(.data[[metric_col]])
    base_vals <- d %>% filter(concession_promotion == 0) %>% pull(.data[[metric_col]])
  }

  if (length(promo_vals) < 2 || length(base_vals) < 2) return(NULL)

  test <- t.test(promo_vals, base_vals)

  label_suffix <- if (is.null(tier_filter)) {
    paste0("(promo tiers: ", paste(relevant_tiers, collapse = ","), "; baseline: all non-promo games)")
  } else {
    paste0("— Tier ", tier_filter)
  }

  cat("\n====================\n")
  cat(str_to_title(category), metric_col, "—", promo_col, label_suffix, "\n")
  cat("n promo games:", length(promo_vals), "| n baseline games:", length(base_vals), "\n")
  cat("====================\n")
  print(test)

  invisible(test)
}

# ---- Beer: overall (tiers 1,2,3,5 combined) ----
run_promo_ttest(category_revenue_by_game_norm, "beer", "revenue_per_turnstile", "beer_promo", beer_relevant_tiers)
run_promo_ttest(category_revenue_by_game_norm, "beer", "quantity_per_turnstile", "beer_promo", beer_relevant_tiers)

# ---- Food: overall (tiers 1,3,5 combined) ----
run_promo_ttest(category_revenue_by_game_norm, "food", "revenue_per_turnstile", "food_promo", food_relevant_tiers)
run_promo_ttest(category_revenue_by_game_norm, "food", "quantity_per_turnstile", "food_promo", food_relevant_tiers)

# ---- Beer: by individual tier ----
walk(beer_relevant_tiers, ~ run_promo_ttest(category_revenue_by_game_norm, "beer", "revenue_per_turnstile", "beer_promo", beer_relevant_tiers, .x))
walk(beer_relevant_tiers, ~ run_promo_ttest(category_revenue_by_game_norm, "beer", "quantity_per_turnstile", "beer_promo", beer_relevant_tiers, .x))

# ---- Food: by individual tier ----
walk(food_relevant_tiers, ~ run_promo_ttest(category_revenue_by_game_norm, "food", "revenue_per_turnstile", "food_promo", food_relevant_tiers, .x))
walk(food_relevant_tiers, ~ run_promo_ttest(category_revenue_by_game_norm, "food", "quantity_per_turnstile", "food_promo", food_relevant_tiers, .x))



# Individual item plots and T-tests

coors_light_data <- items_by_game_v2 %>%
  filter(name == "Beer Coors Light 16oz", Tier %in% beer_relevant_tiers) %>%
  mutate(
    beer_promo = if_else(may_in_the_a == 1 | spring_value_games == 1 | slide_into_summer == 1, 1, 0),
    promo_status = if_else(beer_promo == 1, "Beer Promo", "No Promotion")
  )

# ---- Density: quantity sold ----
ggplot(coors_light_data, aes(x = quantity, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  labs(x = "Quantity sold", y = "Density",
       title = "Beer Coors Light 16oz: Quantity sold, beer promo vs. no promotion",
       subtitle = "Tiers 1, 2, 3, 5") +
  theme_minimal()

# ---- Density: quantity per turnstile ----
ggplot(coors_light_data, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  labs(x = "Quantity per turnstile", y = "Density",
       title = "Beer Coors Light 16oz: Quantity per turnstile, beer promo vs. no promotion",
       subtitle = "Tiers 1, 2, 3, 5") +
  theme_minimal()

# ---- T-test: overall quantity sold ----
coors_promo_qty <- coors_light_data %>% filter(beer_promo == 1) %>% pull(quantity)
coors_base_qty <- coors_light_data %>% filter(beer_promo == 0) %>% pull(quantity)

cat("\n====================\n")
cat("Beer Coors Light 16oz — quantity — beer_promo (tiers: 1,2,3,5)\n")
cat("n promo games:", length(coors_promo_qty), "| n baseline games:", length(coors_base_qty), "\n")
cat("====================\n")
t.test(coors_promo_qty, coors_base_qty)


wilcox.test(coors_promo_qty, coors_base_qty)


# Overall Quantity 

# Virtually identical units, 2,209 vs. 2,207 average units, promo vs. non-promo


# ---- T-test: quantity per turnstile ----
coors_promo_pt <- coors_light_data %>% filter(beer_promo == 1) %>% pull(quantity_per_turnstile)
coors_base_pt <- coors_light_data %>% filter(beer_promo == 0) %>% pull(quantity_per_turnstile)

cat("\n====================\n")
cat("Beer Coors Light 16oz — quantity_per_turnstile — beer_promo (tiers: 1,2,3,5)\n")
cat("n promo games:", length(coors_promo_pt), "| n baseline games:", length(coors_base_pt), "\n")
cat("====================\n")
t.test(coors_promo_pt, coors_base_pt)


# Quantity per turnstile 

# Nearly identical: 0.079 vs. 0.077, promo vs. non-promo

chicken_tender_data <- items_by_game_v2 %>%
  filter(name == "Chicken Tender Basket", Tier %in% food_relevant_tiers) %>%
  mutate(
    food_promo = if_else(spring_value_games == 1 | slide_into_summer == 1, 1, 0),
    promo_status = if_else(food_promo == 1, "Food Promo", "No Promotion")
  )

# ---- Density: quantity sold ----
ggplot(chicken_tender_data, aes(x = quantity, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  labs(x = "Quantity sold", y = "Density",
       title = "Chicken Tender Basket: Quantity sold, food promo vs. no promotion",
       subtitle = "Tiers 1, 3, 5") +
  theme_minimal()

# ---- Density: quantity per turnstile ----
ggplot(chicken_tender_data, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
  geom_density(alpha = 0.3) +
  labs(x = "Quantity per turnstile", y = "Density",
       title = "Chicken Tender Basket: Quantity per turnstile, food promo vs. no promotion",
       subtitle = "Tiers 1, 3, 5") +
  theme_minimal()

# ---- T-test: overall quantity ----
tender_promo_qty <- chicken_tender_data %>% filter(food_promo == 1) %>% pull(quantity)
tender_base_qty <- chicken_tender_data %>% filter(food_promo == 0) %>% pull(quantity)

cat("\n====================\n")
cat("Chicken Tender Basket — quantity — food_promo (tiers: 1,3,5)\n")
cat("n promo games:", length(tender_promo_qty), "| n baseline games:", length(tender_base_qty), "\n")
cat("====================\n")
t.test(tender_promo_qty, tender_base_qty)

# Overall Quantity

# Lower on promo days: 888 vs. 1,067 units, but not a significant result (p = 0.26)

# ---- T-test: quantity per turnstile ----
tender_promo_pt <- chicken_tender_data %>% filter(food_promo == 1) %>% pull(quantity_per_turnstile)
tender_base_pt <- chicken_tender_data %>% filter(food_promo == 0) %>% pull(quantity_per_turnstile)

cat("\n====================\n")
cat("Chicken Tender Basket — quantity_per_turnstile — food_promo (tiers: 1,3,5)\n")
cat("n promo games:", length(tender_promo_pt), "| n baseline games:", length(tender_base_pt), "\n")
cat("====================\n")
t.test(tender_promo_pt, tender_base_pt)


# Quantity per turnstile

# Slightly lower per person: 0.035 vs. 0.039, promo vs. non-promo
# Not significant(p =0.31) - a hint of possible cannibalization from the hot dog promo, but not strong enough to confirm

# Script for producing every item



library(ggplot2)
library(stringr)
library(purrr)

# ---- Top 10 beers, using mlb_category instead of name pattern ----
top_10_beers <- items_by_game_v2 %>%
  inner_join(items_all_v2 %>% distinct(name, mlb_category), by = "name") %>%
  filter(mlb_category == "Beer") %>%
  group_by(name) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 10) %>%
  pull(name)

print(top_10_beers)

# ---- Top 10 food, excluding beer via mlb_category, plus the usual non-food exclusions ----
top_8_food <- items_by_game_v2 %>%
  inner_join(items_all_v2 %>% distinct(name, mlb_category), by = "name") %>%
  filter(mlb_category != "Beer" | is.na(mlb_category)) %>%
  filter(!str_detect(name, "Hot Dog|^Water|^Coke|^Sprite|^Topo Chico|Ice Cream|^Vodka|^Wine|Waffle Fries|Ginger Ale")) %>%
  group_by(name) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 8) %>%
  pull(name)

other_hot_dogs <- c("Hot Dog All Beef 4-1", "Hot Dog All Beef 2-1", "Hot Dog All Beef 6-1")
top_10_food <- unique(c(top_8_food, other_hot_dogs))

print(top_10_food)

# ---- Output folder ----
output_dir <- "J:/JoshPomerantz/2026/Concessions/Item Performance Promo vs Nonpromo"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- Reusable function: builds data, plots, saves images, runs tests ----
analyze_and_save_item <- function(item_name, promo_type) {

  if (promo_type == "beer") {
    tiers <- beer_relevant_tiers
    promo_label <- "Beer Promo"
  } else {
    tiers <- food_relevant_tiers
    promo_label <- "Food Promo"
  }

  d <- items_by_game_v2 %>%
    filter(name == item_name, Tier %in% tiers) %>%
    mutate(
      is_promo = if (promo_type == "beer") {
        if_else(may_in_the_a == 1 | spring_value_games == 1 | slide_into_summer == 1, 1, 0)
      } else {
        if_else(spring_value_games == 1 | slide_into_summer == 1, 1, 0)
      },
      promo_status = if_else(is_promo == 1, promo_label, "No Promotion")
    )

  if (nrow(d) == 0) {
    message("No data for: ", item_name)
    return(NULL)
  }

  safe_name <- str_replace_all(item_name, "[^A-Za-z0-9]+", "_")

  # ---- Plot 1: quantity sold ----
  p_qty <- ggplot(d, aes(x = quantity, fill = promo_status, color = promo_status)) +
    geom_density(alpha = 0.3) +
    labs(x = "Quantity sold", y = "Density",
         title = paste0(item_name, ": Quantity sold, ", promo_label, " vs. no promotion"),
         subtitle = paste0("Tiers ", paste(tiers, collapse = ", "))) +
    theme_minimal()

  ggsave(file.path(output_dir, paste0(safe_name, "_quantity.png")),
         plot = p_qty, width = 8, height = 5, dpi = 300)

  # ---- Plot 2: quantity per turnstile ----
  p_pt <- ggplot(d, aes(x = quantity_per_turnstile, fill = promo_status, color = promo_status)) +
    geom_density(alpha = 0.3) +
    labs(x = "Quantity per turnstile", y = "Density",
         title = paste0(item_name, ": Quantity per turnstile, ", promo_label, " vs. no promotion"),
         subtitle = paste0("Tiers ", paste(tiers, collapse = ", "))) +
    theme_minimal()

  ggsave(file.path(output_dir, paste0(safe_name, "_quantity_per_turnstile.png")),
         plot = p_pt, width = 8, height = 5, dpi = 300)

  # ---- Tests: overall quantity ----
  promo_qty <- d %>% filter(is_promo == 1) %>% pull(quantity)
  base_qty <- d %>% filter(is_promo == 0) %>% pull(quantity)

  ttest_qty <- if (length(promo_qty) >= 2 & length(base_qty) >= 2) t.test(promo_qty, base_qty) else NULL
  wilcox_qty <- if (length(promo_qty) >= 2 & length(base_qty) >= 2) wilcox.test(promo_qty, base_qty) else NULL

  # ---- Tests: quantity per turnstile ----
  promo_pt <- d %>% filter(is_promo == 1) %>% pull(quantity_per_turnstile)
  base_pt <- d %>% filter(is_promo == 0) %>% pull(quantity_per_turnstile)

  ttest_pt <- if (length(promo_pt) >= 2 & length(base_pt) >= 2) t.test(promo_pt, base_pt) else NULL
  wilcox_pt <- if (length(promo_pt) >= 2 & length(base_pt) >= 2) wilcox.test(promo_pt, base_pt) else NULL

  list(
    item_name = item_name,
    promo_type = promo_type,
    safe_name = safe_name,
    n_promo = length(promo_qty),
    n_base = length(base_qty),
    mean_promo_qty = mean(promo_qty, na.rm = TRUE),
    mean_base_qty = mean(base_qty, na.rm = TRUE),
    mean_promo_pt = mean(promo_pt, na.rm = TRUE),
    mean_base_pt = mean(base_pt, na.rm = TRUE),
    ttest_qty = ttest_qty,
    wilcox_qty = wilcox_qty,
    ttest_pt = ttest_pt,
    wilcox_pt = wilcox_pt,
    plot_qty_path = file.path(output_dir, paste0(safe_name, "_quantity.png")),
    plot_pt_path = file.path(output_dir, paste0(safe_name, "_quantity_per_turnstile.png"))
  )
}

# ---- Run for all beers and all food items ----
beer_results <- purrr::map(top_10_beers, ~ analyze_and_save_item(.x, "beer"))
food_results <- purrr::map(top_10_food, ~ analyze_and_save_item(.x, "food"))

all_item_results <- c(beer_results, food_results)

# ---- Save results for use in the Rmd ----
saveRDS(all_item_results, file.path(output_dir, "all_item_results.rds"))

# ---- Quick printout to review results before building the Rmd ----
purrr::walk(all_item_results, function(res) {
  if (is.null(res)) return(invisible(NULL))
  cat("\n====================\n")
  cat(res$item_name, "(", res$promo_type, ")\n")
  cat("n promo:", res$n_promo, "| n baseline:", res$n_base, "\n")
  cat("Qty — promo:", round(res$mean_promo_qty, 2), "| base:", round(res$mean_base_qty, 2),
      "| t-test p:", if (!is.null(res$ttest_qty)) round(res$ttest_qty$p.value, 4) else NA,
      "| wilcox p:", if (!is.null(res$wilcox_qty)) round(res$wilcox_qty$p.value, 4) else NA, "\n")
  cat("Per turnstile — promo:", round(res$mean_promo_pt, 4), "| base:", round(res$mean_base_pt, 4),
      "| t-test p:", if (!is.null(res$ttest_pt)) round(res$ttest_pt$p.value, 4) else NA,
      "| wilcox p:", if (!is.null(res$wilcox_pt)) round(res$wilcox_pt$p.value, 4) else NA, "\n")
})


