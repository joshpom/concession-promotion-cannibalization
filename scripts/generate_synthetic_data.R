# =============================================================================
# Generate Synthetic Data for Concessions Promotions Portfolio
# =============================================================================
# Run this script FIRST to create all synthetic datasets. The schedule files
# in data/raw/ are real (public MLB schedule info); everything else is synthetic
# but structured to produce directionally similar analytical findings.
#
# Usage: Rscript scripts/generate_synthetic_data.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
})

set.seed(2026)

proj_dir <- tryCatch({
  script_dir <- dirname(sys.frame(1)$ofile)
  normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
}, error = function(e) {
  if (dir.exists("data")) normalizePath(".") else normalizePath("..")
})
raw_dir <- file.path(proj_dir, "data", "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

cat("Project dir:", proj_dir, "\n")
cat("Raw data dir:", raw_dir, "\n\n")

# =============================================================================
# 1. READ SCHEDULE FILES
# =============================================================================
read_sched <- function(f) {
  read.csv(file.path(raw_dir, f), check.names = FALSE) %>%
    mutate(saleAttribution_date = as.Date(Date, "%m/%d/%Y"))
}

sched_2024 <- read_sched("2024_Schedule_info.csv")
sched_2025 <- read_sched("2025_Schedule_info.csv")
sched_2026 <- read_sched("2026_Schedule_info.csv")

schedule_all <- bind_rows(sched_2024, sched_2025, sched_2026)

cat("Games loaded:", nrow(sched_2024), "(2024),",
    nrow(sched_2025), "(2025),", nrow(sched_2026), "(2026)\n\n")

# =============================================================================
# 2. DEFINE PROMOTION FLAGS
# =============================================================================
# Slide into Summer (2025): 5/30-6/05, all tiers in that window
slide_dates_2025 <- as.Date(c("2025-05-30","2025-05-31","2025-06-01",
                               "2025-06-03","2025-06-04","2025-06-05"))

# Summer Steal (2025): early July
summer_steal_dates <- as.Date(c("2025-07-01","2025-07-02","2025-07-03",
                                 "2025-07-04","2025-07-05","2025-07-06"))

# Oktoberfest (2025): late September
oktoberfest_dates <- as.Date(c("2025-09-22","2025-09-23","2025-09-24","2025-09-26"))

# Spring Value Games (2026): tier 5 games in late March/April
spring_value_dates <- as.Date(c("2026-03-30","2026-03-31","2026-04-01",
                                 "2026-04-13","2026-04-14","2026-04-15","2026-04-28"))

# May in the A (2026): tiers 2,3 games in May
may_in_the_a_dates <- as.Date(c("2026-05-12","2026-05-13","2026-05-14",
                                  "2026-05-17","2026-05-22","2026-05-24"))

assign_promo_flags <- function(sched) {
  sched %>%
    mutate(
      slide_into_summer    = as.integer(saleAttribution_date %in% slide_dates_2025),
      summer_steal         = as.integer(saleAttribution_date %in% summer_steal_dates),
      oktoberfest          = as.integer(saleAttribution_date %in% oktoberfest_dates),
      spring_value_games   = as.integer(saleAttribution_date %in% spring_value_dates),
      may_in_the_a         = as.integer(saleAttribution_date %in% may_in_the_a_dates),
      concession_promotion = as.integer(slide_into_summer | summer_steal | oktoberfest |
                                          spring_value_games | may_in_the_a)
    )
}

games_25_26 <- bind_rows(sched_2025, sched_2026) %>% assign_promo_flags()
games_2024  <- sched_2024 %>%
  mutate(slide_into_summer = 0L, summer_steal = 0L, oktoberfest = 0L,
         spring_value_games = 0L, may_in_the_a = 0L, concession_promotion = 0L)

promo_check <- games_25_26 %>% filter(concession_promotion == 1) %>%
  count(Season, spring_value_games, may_in_the_a, slide_into_summer,
        summer_steal, oktoberfest)
cat("Promotion game counts:\n"); print(promo_check); cat("\n")

# =============================================================================
# 3. GENERATE TURNSTILE / ATTENDANCE
# =============================================================================
tier_base <- c(`0` = 32000, `1` = 35000, `2` = 31000, `3` = 29000,
               `4` = 27000, `5` = 24000, `6` = 33000)

month_mult <- c(Mar = 1.05, Apr = 0.95, May = 0.97, Jun = 1.00,
                Jul = 1.08, Aug = 1.05, Sep = 0.92)

gen_turnstile <- function(game_df) {
  n <- nrow(game_df)
  base <- sapply(as.character(game_df$Tier), function(t) {
    if (t %in% names(tier_base)) tier_base[[t]] else 28000
  })
  wknd <- ifelse(game_df$Weekend == 1, 1.07, 1.0)
  mon  <- sapply(game_df$Month, function(m) {
    if (m %in% names(month_mult)) month_mult[[m]] else 1.0
  })
  mu <- base * wknd * mon
  pmax(rnbinom(n, size = 30, mu = mu), 15000)
}

games_25_26$turnstile <- gen_turnstile(games_25_26)
games_2024$turnstile  <- gen_turnstile(games_2024)

# =============================================================================
# 4. GENERATE KPI DATA
# =============================================================================
gen_kpis <- function(game_df) {
  n <- nrow(game_df)
  promo_trans_mult <- ifelse(game_df$concession_promotion == 1, 1.04, 1.0)
  promo_items_mult <- ifelse(game_df$concession_promotion == 1, 1.03, 1.0)

  trans_rate <- rnorm(n, mean = 0.42, sd = 0.03) * promo_trans_mult
  transactions <- round(game_df$turnstile * trans_rate)

  items_rate <- rnorm(n, mean = 2.1, sd = 0.15) * promo_items_mult
  quantity <- round(transactions * items_rate)

  spend_per <- rnorm(n, mean = 9.80, sd = 0.60)
  total <- round(transactions * spend_per, 2)

  game_df %>% mutate(
    transactions         = transactions,
    total                = total,
    quantity             = quantity,
    trans_per_turnstile  = round(transactions / turnstile, 4),
    items_per_trans      = round(quantity / transactions, 4),
    spend_per_trans      = round(total / transactions, 2),
    percap               = round(total / turnstile, 2)
  )
}

kpi_25_26 <- gen_kpis(games_25_26)
kpi_2024  <- gen_kpis(games_2024)

# Write KPI files
kpi_cols <- c("saleAttribution_date", "season", "transactions", "total", "quantity",
              "turnstile", "trans_per_turnstile", "items_per_trans", "spend_per_trans",
              "percap", "concession_promotion", "spring_value_games", "may_in_the_a",
              "oktoberfest", "summer_steal", "slide_into_summer")

write_kpi <- function(df, fname) {
  out <- df %>% rename(season = Season) %>% select(all_of(kpi_cols))
  write.csv(out, file.path(raw_dir, fname), row.names = FALSE)
  cat("Wrote", fname, ":", nrow(out), "rows\n")
}

write_kpi(kpi_25_26, "braves_concessions_kpis_25_26.csv")
write_kpi(kpi_2024,  "braves_concessions_kpis_24.csv")

# =============================================================================
# 5. GENERATE REVENUE DATA
# =============================================================================
gen_revenue <- function(kpi_df) {
  n <- nrow(kpi_df)
  kpi_df %>% mutate(
    date            = saleAttribution_date,
    event_general   = "regular season",
    season          = Season,
    revenue_total   = total,
    revenue_food    = round(total * runif(n, 0.42, 0.48), 2),
    revenue_bar     = round(total * runif(n, 0.33, 0.39), 2),
    revenue_retail  = round(total * runif(n, 0.04, 0.07), 2),
    revenue_other   = round(pmax(0, total - revenue_food - revenue_bar - revenue_retail), 2),
    revenue_fandb   = round(revenue_food + revenue_bar, 2)
  ) %>%
    select(date, event_general, season, revenue_fandb, revenue_food, revenue_bar,
           revenue_other, revenue_retail, revenue_total)
}

rev_25_26 <- gen_revenue(kpi_25_26)
rev_2024  <- gen_revenue(kpi_2024)

write.csv(rev_25_26, file.path(raw_dir, "braves_concessions_sales_25_26.csv"), row.names = FALSE)
write.csv(rev_2024,  file.path(raw_dir, "braves_concessions_sales_24.csv"), row.names = FALSE)
cat("Wrote revenue files\n")

# =============================================================================
# 6. DEFINE ITEM UNIVERSE
# =============================================================================
items_def <- tribble(
  ~name,                                ~mlb_category,                   ~category,    ~base_rate, ~base_price, ~promo_price, ~beer_mult, ~food_mult, ~season_beer_boost,
  "Beer Miller High Life 12oz",         "Beer",                          "Beer",       2.8,        9.50,        4.00,         1.70,       1.0,        0.0,
  "Beer Coors Light 16oz",              "Beer",                          "Beer",       5.5,        14.00,       NA,           0.90,       1.0,        0.15,
  "Coors Light 16oz",                   "Beer",                          "Beer",       1.2,        14.00,       NA,           0.90,       1.0,        0.15,
  "Beer Coors Light 24oz",              "Beer",                          "Beer",       1.8,        18.50,       NA,           1.02,       1.0,        0.20,
  "Coors Light Large",                  "Beer",                          "Beer",       0.5,        18.50,       NA,           1.02,       1.0,        0.20,
  "Beer Miller Lite 16oz",              "Beer",                          "Beer",       3.2,        14.00,       NA,           0.94,       1.0,        0.12,
  "Beer Miller Lite 24oz",              "Beer",                          "Beer",       1.2,        18.50,       NA,           1.00,       1.0,        0.18,
  "Beer Coors Banquet 16oz",            "Beer",                          "Beer",       0.8,        14.00,       NA,           0.97,       1.0,        0.10,
  "Beer Blue Moon 16oz",                "Beer",                          "Beer",       1.5,        15.00,       NA,           1.00,       1.0,        0.08,
  "Beer Leinenkugel Summer Shandy 16oz","Beer",                          "Beer",       1.0,        15.00,       NA,           1.00,       1.0,        0.25,
  "Beer Sweetwater Atlanta OG 16oz",    "Beer",                          "Beer",       0.6,        15.00,       NA,           1.00,       1.0,        0.05,
  "Beer Cruz Blanca Los Bravos Cleat",  "Beer",                          "Beer",       0.3,        16.00,       NA,           1.00,       1.0,        0.00,
  "Beer Coors Light Cleat",             "Beer",                          "Beer",       0.3,        16.00,       NA,           1.00,       1.0,        0.00,
  "Beer Blue Moon Cleat",               "Beer",                          "Beer",       0.2,        16.00,       NA,           1.00,       1.0,        0.00,
  "Beer Blue Moon Blood Orange IPA Cleat","Beer",                        "Beer",       0.2,        16.00,       NA,           1.00,       1.0,        0.00,
  "Hot Dog All Beef 6-1",               "Hot Dogs/Sausages",             "Food",       4.5,        7.50,        2.50,         1.0,        1.11,       0.00,
  "Hot Dog All Beef 4-1",               "Hot Dogs/Sausages",             "Food",       2.0,        9.00,        NA,           1.0,        0.85,       0.00,
  "Hot Dog All Beef 2-1",               "Hot Dogs/Sausages",             "Food",       0.8,        11.00,       NA,           1.0,        0.85,       0.00,
  "Burger Basket",                      "Burgers/Sandwiches/Wraps/Bowls","Food",       2.8,        14.50,       NA,           1.0,        0.98,       0.00,
  "Chicken Tender Basket",              "Chicken",                       "Food",       3.0,        13.50,       NA,           1.0,        0.97,       0.00,
  "CFA Chicken Sandwich",               "Chicken",                       "Food",       3.5,        10.50,       NA,           1.0,        1.00,       0.00,
  "Pizza Pepperoni Slice 18",           "Pizza/Pasta/Italian",           "Food",       4.0,        8.50,        NA,           1.0,        1.00,       0.00,
  "Snack Nachos Classic",               "Nachos/Snacks",                 "Food",       3.5,        8.00,        NA,           1.0,        1.00,       0.00,
  "Pretzel Jumbo",                      "Pretzels",                      "Food",       2.5,        7.00,        NA,           1.0,        1.00,       0.00,
  "Colossal Bavarian Pretzel",          "Pretzels",                      "Food",       1.0,        12.00,       NA,           1.0,        1.00,       0.00,
  "Waffle Fries",                       "French Fries/Onion Rings",      "Food",       3.8,        7.50,        NA,           1.0,        1.00,       0.00,
  "Water Bottled 20oz",                 "Water",                         "Non-Alc",    6.0,        6.00,        NA,           1.0,        1.00,       0.00,
  "Coke Regular",                       "Soda/Energy",                   "Non-Alc",    5.0,        6.50,        NA,           1.0,        1.00,       0.00,
  "Coke Souvenir",                      "Soda/Energy",                   "Non-Alc",    2.0,        12.00,       NA,           1.0,        1.00,       0.00,
  "Ice Cream Sandwich",                 "Ice cream/desserts/sweets",     "Dessert",    1.5,        6.00,        2.00,         1.0,        1.00,       0.00,
  "Ice Cream Dippin Dot Cup",           "Ice cream/desserts/sweets",     "Dessert",    1.0,        7.00,        NA,           1.0,        1.00,       0.10,
  "Ice Cream Dippin Dot Helmet",        "Ice cream/desserts/sweets",     "Dessert",    0.5,        14.00,       NA,           1.0,        1.00,       0.10,
  "Popcorn Regular",                    "Popcorn/Peanuts/Cracker Jack",  "Food",       2.0,        7.00,        NA,           1.0,        1.00,       0.00,
  "Peanuts",                            "Popcorn/Peanuts/Cracker Jack",  "Food",       1.5,        6.50,        NA,           1.0,        1.00,       0.00,
)

# =============================================================================
# 7. GENERATE ITEM-LEVEL SALES
# =============================================================================
gen_items_for_season <- function(game_df, season_year, items_def) {
  games <- game_df %>% filter(Season == season_year)

  # beer_promo and food_promo flags
  games <- games %>% mutate(
    beer_promo = as.integer(may_in_the_a == 1 | spring_value_games == 1 |
                              slide_into_summer == 1 | oktoberfest == 1),
    food_promo = as.integer(spring_value_games == 1 | slide_into_summer == 1)
  )

  month_num <- month(games$saleAttribution_date)

  rows <- list()
  for (i in seq_len(nrow(items_def))) {
    item <- items_def[i, ]

    # Seasonal multiplier: beer items sell more in summer
    season_mult <- 1.0 + item$season_beer_boost *
      ifelse(month_num >= 6 & month_num <= 8, 1.0, -0.3)

    # Promo multiplier
    promo_mult <- rep(1.0, nrow(games))
    if (item$beer_mult != 1.0) {
      promo_mult <- ifelse(games$beer_promo == 1, item$beer_mult, 1.0)
    }
    if (item$food_mult != 1.0) {
      promo_mult <- ifelse(games$food_promo == 1, item$food_mult, promo_mult)
    }

    # Ice cream sandwich gets a boost on may_in_the_a (it was a promo item)
    if (item$name == "Ice Cream Sandwich") {
      promo_mult <- ifelse(games$may_in_the_a == 1, 1.80, promo_mult)
    }

    mu <- item$base_rate * (games$turnstile / 1000) * promo_mult * season_mult
    mu <- pmax(mu, 0.5)

    qty <- rnbinom(nrow(games), size = 4, mu = mu)

    # Price: use promo price when applicable, else base price with small noise
    use_promo_price <- !is.na(item$promo_price) & (
      (item$mlb_category == "Beer" & games$beer_promo == 1) |
      (item$name == "Hot Dog All Beef 6-1" & games$food_promo == 1) |
      (item$name == "Ice Cream Sandwich" & games$may_in_the_a == 1)
    )
    unit_price <- ifelse(use_promo_price,
                         item$promo_price,
                         item$base_price * runif(nrow(games), 0.97, 1.03))
    amt <- round(qty * unit_price, 2)

    rows[[i]] <- tibble(
      saleAttribution_date = games$saleAttribution_date,
      name                 = item$name,
      quantity             = qty,
      amount               = amt,
      season               = season_year,
      mlb_category         = item$mlb_category,
      category             = item$category
    )
  }

  bind_rows(rows) %>% filter(quantity > 0)
}

cat("Generating item-level sales...\n")
items_2025 <- gen_items_for_season(games_25_26, 2025, items_def)
items_2026 <- gen_items_for_season(games_25_26, 2026, items_def)
items_2024 <- gen_items_for_season(games_2024,  2024, items_def)

write.csv(items_2025, file.path(raw_dir, "concessions_2025_item_level.csv"), row.names = FALSE)
write.csv(items_2026, file.path(raw_dir, "concessions_2026_item_level.csv"), row.names = FALSE)

cat("  2025 items:", nrow(items_2025), "rows\n")
cat("  2026 items:", nrow(items_2026), "rows\n")

# =============================================================================
# 8. GENERATE STAND-LEVEL SALES
# =============================================================================
stand_defs <- tribble(
  ~cost_center_origin_name,                              ~floor, ~is_promo, ~base_trans_rate,
  "C343 MARKET",                                         3,      TRUE,      0.015,
  "C313 MARKET",                                         3,      TRUE,      0.012,
  "1871 GRILLE / THE SLICE / TACO FACTORY_313",          3,      TRUE,      0.010,
  "C135 BALLPARK CLASSICS",                              1,      FALSE,     0.018,
  "FOH CHOPHOUSE",                                       1,      FALSE,     0.014,
  "C113 1871 GRILLE",                                    1,      FALSE,     0.016,
  "C143 CHICK-FIL-A",                                    1,      FALSE,     0.020,
  "C149 CENTERFIELD MARKET",                             1,      FALSE,     0.017,
  "C105 DRAFT HOUSE",                                    1,      FALSE,     0.013,
  "C127 THE BRAVOS CANTINA",                             1,      FALSE,     0.011,
  "C155 BLOOPER'S DOGS",                                 1,      FALSE,     0.010,
  "C301 CHOP HOUSE UPPER",                               3,      FALSE,     0.008,
  "C307 TERRAPIN TAPROOM",                               3,      FALSE,     0.009,
  "C325 FIRE PIT",                                       3,      FALSE,     0.007,
  "C331 THE LANDING",                                    3,      FALSE,     0.008,
  "C337 BRAVES ALL STAR GRILL",                          3,      FALSE,     0.009,
  "C345 LIME FRESH",                                     3,      FALSE,     0.006,
  "C355 FROZEN TREATS",                                  3,      FALSE,     0.005,
  "C201 DELTA SKY360 BAR",                               2,      FALSE,     0.004,
  "C209 SUITE LEVEL PANTRY",                             2,      FALSE,     0.003,
  "MARKET_343",                                          3,      TRUE,      0.014,
)

gen_stands_for_season <- function(game_df, season_year) {
  games <- game_df %>% filter(Season == season_year) %>%
    mutate(beer_promo = as.integer(may_in_the_a == 1 | spring_value_games == 1 |
                                     slide_into_summer == 1))

  rows <- list()
  for (i in seq_len(nrow(stand_defs))) {
    s <- stand_defs[i, ]

    # Promo stands get a transaction lift on beer promo days
    promo_mult <- ifelse(s$is_promo & games$beer_promo == 1, 1.10, 1.0)

    mu_trans <- s$base_trans_rate * games$turnstile * promo_mult
    transactions <- rnbinom(nrow(games), size = 8, mu = mu_trans)

    # Revenue per transaction: slightly lower at promo stands on promo days
    rev_per_trans <- rnorm(nrow(games), mean = 12.50, sd = 1.5)
    if (s$is_promo) {
      rev_per_trans <- ifelse(games$beer_promo == 1,
                              rev_per_trans * 0.92,
                              rev_per_trans)
    }
    total <- round(transactions * rev_per_trans, 2)

    rows[[i]] <- tibble(
      saleAttribution_date     = games$saleAttribution_date,
      cost_center_origin_name  = s$cost_center_origin_name,
      transactions             = transactions,
      total                    = total
    )
  }

  bind_rows(rows) %>% filter(transactions > 0)
}

cat("Generating stand-level sales...\n")
stands_2025 <- gen_stands_for_season(games_25_26, 2025)
stands_2026 <- gen_stands_for_season(games_25_26, 2026)

write.csv(stands_2025, file.path(raw_dir, "concessions_2025_stand_level.csv"), row.names = FALSE)
write.csv(stands_2026, file.path(raw_dir, "concessions_2026_stand_level.csv"), row.names = FALSE)

cat("  2025 stands:", nrow(stands_2025), "rows\n")
cat("  2026 stands:", nrow(stands_2026), "rows\n")

# =============================================================================
# 9. STAND LABELS MASTER
# =============================================================================
stand_labels <- stand_defs %>%
  transmute(
    cost_center_origin_name,
    floor,
    location      = paste0("Level ", floor, "00"),
    location_name = cost_center_origin_name
  )

write.csv(stand_labels, file.path(raw_dir, "stand_labels_MASTER.csv"), row.names = FALSE)
cat("Wrote stand_labels_MASTER.csv\n")

# =============================================================================
# 10. SHOW RATE DATA
# =============================================================================
all_games <- bind_rows(
  games_25_26 %>% select(saleAttribution_date, Season, Weekend, Month, turnstile),
  games_2024  %>% select(saleAttribution_date, Season, Weekend, Month, turnstile)
)

showrate_data <- all_games %>%
  mutate(
    event_date     = saleAttribution_date,
    weekday        = wday(saleAttribution_date, label = TRUE, abbr = TRUE),
    is_weekday     = as.integer(Weekend == 0),
    is_summer      = as.integer(Month %in% c("Jun", "Jul", "Aug")),
    show_rate      = round(runif(n(), 0.76, 0.92), 4),
    attended_seats = turnstile,
    sold_seats     = round(turnstile / show_rate)
  ) %>%
  select(event_date, weekday, is_weekday, is_summer, sold_seats, attended_seats, show_rate)

write.csv(showrate_data, file.path(raw_dir, "showrates_overall.csv"), row.names = FALSE)
cat("Wrote showrates_overall.csv:", nrow(showrate_data), "rows\n")

# =============================================================================
# 11. BIGQUERY REPLACEMENT CSVs
# =============================================================================

# 11a. Turnstile by date (used by bogo/ice cream scripts, also useful as reference)
turnstile_by_date <- bind_rows(
  games_25_26 %>% select(event_date = saleAttribution_date, turnstile),
  games_2024  %>% select(event_date = saleAttribution_date, turnstile)
)
write.csv(turnstile_by_date, file.path(raw_dir, "turnstile_by_date.csv"), row.names = FALSE)

# 11b. Tickets sold by date
tickets_sold <- showrate_data %>%
  select(event_date, sold_seats)
write.csv(tickets_sold, file.path(raw_dir, "tickets_sold_by_date.csv"), row.names = FALSE)

# 11c. Ticket sales by game (for concessions_promotions.R)
ticket_sales <- showrate_data %>%
  mutate(
    saleAttribution_date   = event_date,
    total_tickets_sold     = sold_seats,
    total_ticket_revenue   = round(sold_seats * runif(n(), 24, 38), 2)
  ) %>%
  select(saleAttribution_date, total_tickets_sold, total_ticket_revenue)
write.csv(ticket_sales, file.path(raw_dir, "ticket_sales_by_game.csv"), row.names = FALSE)

# 11d. Retail sales (Xenial Lake replacement)
retail_dates <- bind_rows(
  games_25_26 %>% select(saleAttribution_date, turnstile),
  games_2024  %>% select(saleAttribution_date, turnstile)
)
retail_sales <- retail_dates %>%
  mutate(
    business_date = saleAttribution_date,
    num_orders    = round(turnstile * runif(n(), 0.08, 0.12)),
    gross_sales   = round(num_orders * runif(n(), 30, 40), 2),
    net_sales     = round(gross_sales * 0.93, 2)
  ) %>%
  select(business_date, num_orders, net_sales, gross_sales)
write.csv(retail_sales, file.path(raw_dir, "retail_sales.csv"), row.names = FALSE)

# 11e. 300-level item sales (for stand_level_promotion_analysis.R)
# Generate item-level data tied to 300-level stands
cat("Generating 300-level item data...\n")

stands_300 <- stand_defs %>% filter(floor == 3)

gen_300_items <- function(game_df, season_year) {
  games <- game_df %>% filter(Season == season_year) %>%
    mutate(
      beer_promo = as.integer(may_in_the_a == 1 | spring_value_games == 1 |
                                slide_into_summer == 1),
      food_promo = as.integer(spring_value_games == 1 | slide_into_summer == 1)
    )

  trans_id_start <- (season_year - 2024) * 500000
  rows <- list()
  tid <- trans_id_start

  for (g in seq_len(nrow(games))) {
    game <- games[g, ]
    month_num <- month(game$saleAttribution_date)

    for (s in seq_len(nrow(stands_300))) {
      stand <- stands_300[s, ]
      n_trans <- round(stand$base_trans_rate * game$turnstile * 0.3 *
                         ifelse(stand$is_promo & game$beer_promo == 1, 1.12, 1.0))
      if (n_trans < 1) next

      for (t in seq_len(min(n_trans, 50))) {
        tid <- tid + 1
        item_idx <- sample(nrow(items_def), 1, prob = items_def$base_rate)
        item <- items_def[item_idx, ]

        promo_mult <- 1.0
        if (item$beer_mult != 1.0 & game$beer_promo == 1) promo_mult <- item$beer_mult
        if (item$food_mult != 1.0 & game$food_promo == 1) promo_mult <- item$food_mult

        qty <- max(1, rnbinom(1, size = 2, mu = 1.5 * promo_mult))
        use_pp <- !is.na(item$promo_price) & (
          (item$mlb_category == "Beer" & game$beer_promo == 1) |
          (item$name == "Hot Dog All Beef 6-1" & game$food_promo == 1)
        )
        price <- ifelse(use_pp, item$promo_price, item$base_price)
        amt <- round(qty * price * runif(1, 0.97, 1.03), 2)

        rows[[length(rows) + 1]] <- tibble(
          trans_id                 = tid,
          saleAttribution_date     = game$saleAttribution_date,
          name                     = item$name,
          quantity                 = qty,
          amount                   = amt,
          cost_center_origin_name  = stand$cost_center_origin_name
        )
      }
    }
  }

  bind_rows(rows)
}

items_300_2025 <- gen_300_items(games_25_26, 2025)
items_300_2026 <- gen_300_items(games_25_26, 2026)
items_300 <- bind_rows(items_300_2025, items_300_2026)

write.csv(items_300, file.path(raw_dir, "items_300level.csv"), row.names = FALSE)
cat("  300-level items:", nrow(items_300), "rows\n")

# =============================================================================
# 12. ICE CREAM DATA (separate file for compatibility)
# =============================================================================
icecream_items <- c("Ice Cream Sandwich", "Ice Cream Dippin Dot Cup",
                    "Ice Cream Dippin Dot Helmet")

icecream_data <- bind_rows(items_2025, items_2026) %>%
  filter(name %in% icecream_items) %>%
  mutate(mlb_category = "Ice cream/desserts/sweets")

write.csv(icecream_data, file.path(raw_dir, "braves_concessions_icecream2.csv"), row.names = FALSE)
cat("Wrote braves_concessions_icecream2.csv:", nrow(icecream_data), "rows\n")

# =============================================================================
# DONE
# =============================================================================
cat("\n========================================\n")
cat("All synthetic data generated successfully.\n")
cat("Files in:", raw_dir, "\n")
cat("========================================\n")
