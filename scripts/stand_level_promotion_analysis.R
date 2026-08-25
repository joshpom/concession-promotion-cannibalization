# =============================================================================
# Concession Promotion Cannibalization Analysis — 300 LEVEL ONLY
# -----------------------------------------------------------------------------
# Same methods as the main cannibalization_analysis.R script, restricted to
# item sales at 300-level stands only, pulled from a local CSV (replacing the
# original BigQuery transaction/item tables used in the main script).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(purrr); library(ggplot2); library(MASS); library(fixest); library(broom)
})
select <- dplyr::select
options(scipen = 999)

proj_dir <- if (dir.exists("../output")) normalizePath("..") else normalizePath(".")
raw_dir    <- file.path(proj_dir, "data", "raw")
tables_dir <- file.path(proj_dir, "output", "tables")
fig_dir    <- file.path(proj_dir, "output", "figures", "cannibalization")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,    showWarnings = FALSE, recursive = TRUE)

output_tag <- "_300level_v2"

# ---- Small helpers ----------------------------------------------------------
to_date <- function(x) {
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  x <- as.character(x); d <- as.Date(x, "%Y-%m-%d")
  if (any(is.na(d) & !is.na(x))) d[is.na(d)] <- as.Date(x[is.na(d)], "%m/%d/%Y")
  d
}
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# =============================================================================
# 1. LOAD SUPPORTING DATA (games / schedule / promo flags)
# =============================================================================
promo_flag_cols <- c("concession_promotion", "spring_value_games", "may_in_the_a",
                     "oktoberfest", "summer_steal", "slide_into_summer")

kpis <- read.csv(file.path(raw_dir, "braves_concessions_kpis_25_26.csv"), check.names = FALSE) %>%
  mutate(saleAttribution_date = to_date(saleAttribution_date),
         across(all_of(promo_flag_cols), num),
         turnstile = num(turnstile), season = num(season))

read_sched <- function(f) read.csv(file.path(raw_dir, f), check.names = FALSE) %>%
  mutate(saleAttribution_date = as.Date(Date, "%m/%d/%Y"))
schedule <- bind_rows(read_sched("2025_Schedule_info.csv"),
                      read_sched("2026_Schedule_info.csv")) %>%
  transmute(saleAttribution_date, Tier = num(Tier),
            Time_Type = as.character(Time_Type), Time_of_year = as.character(Time_of_year),
            O_Team = as.character(O_Team), Month = as.character(Month),
            Weekend = as.character(Weekend), effective_day = as.character(effective_day)) %>%
  distinct(saleAttribution_date, .keep_all = TRUE)

game_info <- kpis %>%
  select(saleAttribution_date, season, turnstile, all_of(promo_flag_cols)) %>%
  distinct(saleAttribution_date, .keep_all = TRUE) %>%
  left_join(schedule, by = "saleAttribution_date") %>%
  mutate(beer_promo = as.integer(may_in_the_a == 1 | spring_value_games == 1 | slide_into_summer == 1),
         food_promo = as.integer(spring_value_games == 1 | slide_into_summer == 1)) %>%
  filter(!is.na(turnstile), turnstile > 0, !is.na(concession_promotion))

games <- game_info %>%
  select(saleAttribution_date, season, turnstile, Tier, Month, Time_of_year, Weekend, effective_day,
         concession_promotion, beer_promo, food_promo)

beer_relevant_tiers <- c(0, 1, 2, 3, 4, 5)
food_relevant_tiers <- c(0, 1, 3, 4, 5)

# =============================================================================
# 2. LOAD 300-LEVEL ITEM SALES FROM CSV (replaces BigQuery pull)
# =============================================================================
floor_lookup <- read.csv(file.path(raw_dir, "stand_labels_MASTER.csv"), check.names = FALSE)

item_stand_raw <- read.csv(file.path(raw_dir, "items_300level.csv"), check.names = FALSE) %>%
  mutate(saleAttribution_date = to_date(saleAttribution_date))

n_rows_before <- nrow(item_stand_raw)
n_distinct_trans_item <- item_stand_raw %>% distinct(trans_id, name, quantity, amount) %>% nrow()
cat("Rows before dedup check:", n_rows_before, "| distinct trans/item combos:", n_distinct_trans_item, "\n")

item_stand_300 <- item_stand_raw %>%
  inner_join(
    floor_lookup %>% dplyr::select(cost_center_origin_name, floor, location, location_name),
    by = "cost_center_origin_name"
  ) %>%
  dplyr::filter(floor == 3) %>%
  inner_join(games %>% dplyr::select(saleAttribution_date), by = "saleAttribution_date")

item_stand_300 %>% summarise(
  n_games = n_distinct(saleAttribution_date),
  n_stands = n_distinct(location_name),
  total_qty = sum(quantity, na.rm = TRUE)
)

item_stand_300 <- item_stand_300 %>%
  left_join(games %>% select(saleAttribution_date, season), by = "saleAttribution_date")

# =============================================================================
# 3. CONFIG: promoted items + curated substitute families
# =============================================================================
norm_name <- function(x) {
  x <- str_squish(str_remove(x, "^Beer "))
  x <- str_replace(x, "^Coors Light Regular$", "Coors Light 16oz")
  x <- str_replace(x, "^Coors Light Large$", "Coors Light 24oz")
  x <- str_replace(x, "^Miller Lite Large$", "Miller Lite 24oz")
  x
}

promoted_beer <- "Miller High Life 12oz"
promoted_food <- "Hot Dog All Beef 6-1"

rx_domestic <- regex("coors light|miller lite|coors banquet|miller high life", ignore_case = TRUE)
rx_largepour<- regex("large|24 ?oz", ignore_case = TRUE)
rx_hotdog   <- regex("hot dog all beef", ignore_case = TRUE)
rx_adjacent_snack  <- regex("nachos classic|pretzel jumbo|colossal bavarian pretzel", ignore_case = TRUE)
rx_adjacent_entree <- regex("burger basket|chicken tender basket", ignore_case = TRUE)
rx_exclude  <- regex("suites?|draft|souvenir|cleat|\\bcan\\b|\\bbat\\b|activation|employee|\\bkids?\\b|vegetarian|vegan|pitcher|toppings|4ct|\\btable\\b|fred'?s|^miller lite$", ignore_case = TRUE)
contrast_items <- c("Blue Moon 16oz", "Leinenkugel Summer Shandy 16oz",
                    "Sweetwater Atlanta OG 16oz", "Blue Moon Regular",
                    "Pizza Pepperoni Slice 18")

rx_is_beer <- regex(
  "^Beer |^Blue Moon$|^Coors Light 24oz$|^Leinenkugel Hazy IPA$|^Los Bravos$|^Miller Lite 24oz$|^Summer Shandy$|^Sweetwater IPA$",
  ignore_case = TRUE
)
rx_is_other_alcohol <- regex(
  "^Cocktail|^Cutwater|^Gin |^Rum |^Tequila|^Vodka|^Whiskey|^Wine |^WINE |^Suncruiser|^Nutrl|^Seltzer|^RTD |^Simply Spiked|^Specialty Dirty Soda|Crown & Cola",
  ignore_case = TRUE
)
rx_is_nonalc_drink <- regex(
  "^Coke|^Diet Coke|^Sprite|^Fanta|^Pibb|^Minute Maid|^Body Armor|^Water |^Energy Red Bull|^Mixer Red Bull|^Powerade|^Frozen Lemonade|^Frozen Strawberry|^Topo Chico|^Coffee|^Hot Chocolate|^Frozen Series Special Drink",
  ignore_case = TRUE
)

assign_family <- function(name, item_category) {
  dplyr::case_when(
    str_detect(name, regex("^miller high life", ignore_case = TRUE)) ~ "beer_promoted",
    item_category == "beer" & str_detect(name, rx_domestic) & str_detect(name, rx_largepour) ~ "beer_largepour_sub",
    item_category == "beer" & str_detect(name, rx_domestic) ~ "beer_domestic_sub",
    name == promoted_food ~ "food_promoted",
    item_category == "food" & str_detect(name, rx_hotdog)          ~ "food_hotdog_sub",
    item_category == "food" & str_detect(name, rx_adjacent_snack)  ~ "food_adjacent_snack_sub",
    item_category == "food" & str_detect(name, rx_adjacent_entree) ~ "food_adjacent_entree_sub",
    TRUE ~ "contrast"
  )
}
beer_sub_fams <- c("beer_domestic_sub", "beer_largepour_sub")
food_sub_fams <- c("food_hotdog_sub", "food_adjacent_snack_sub", "food_adjacent_entree_sub")
sub_fams      <- c(beer_sub_fams, food_sub_fams)

# =============================================================================
# 4. PANELS — sales_ng built from the 300-level data
# =============================================================================
sales_ng <- item_stand_300 %>%
  mutate(
    item_category = case_when(
      str_detect(name, regex("^Beer ", ignore_case = TRUE)) ~ "beer",
      str_detect(name, regex("^Blue Moon$|^Coors Light 24oz$|^Leinenkugel Hazy IPA$|^Los Bravos$|^Miller Lite 24oz$|^Summer Shandy$|^Sweetwater IPA$", ignore_case = TRUE)) ~ "beer",
      str_detect(name, rx_is_other_alcohol) ~ "other_alcohol",
      str_detect(name, rx_is_nonalc_drink) ~ "nonalc_drink",
      TRUE ~ "food"
    ),
    name = norm_name(name)
  ) %>%
  filter(!name %in% c("", "NULL"), !is.na(name), !is.na(quantity), name != "Open Amount Refund") %>%
  group_by(name, item_category, season, saleAttribution_date) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE), revenue = sum(amount, na.rm = TRUE), .groups = "drop")

category_by_game <- sales_ng %>%
  inner_join(games, by = c("season", "saleAttribution_date")) %>%
  group_by(item_category, saleAttribution_date, season, turnstile, Tier, Month, Time_of_year, Weekend,
           concession_promotion, beer_promo, food_promo) %>%
  summarise(cat_quantity = sum(quantity), cat_revenue = sum(revenue), .groups = "drop") %>%
  mutate(cat_qty_per_turnstile = cat_quantity / turnstile)

MIN_ITEM_VOL <- 1000
item_totals <- sales_ng %>% group_by(name, item_category) %>%
  summarise(totq = sum(quantity), .groups = "drop")
universe <- item_totals %>%
  filter(name %in% contrast_items |
         (totq >= MIN_ITEM_VOL & !str_detect(name, rx_exclude) &
          ((item_category == "beer" & str_detect(name, rx_domestic)) |
           (item_category == "food" & (str_detect(name, rx_hotdog) |
                                        str_detect(name, rx_adjacent_snack) |
                                        str_detect(name, rx_adjacent_entree)))))) %>%
  mutate(family = assign_family(name, item_category)) %>%
  select(name, item_category, family)

avail <- sales_ng %>% filter(name %in% universe$name) %>% distinct(name, item_category, season)
panel_zf <- avail %>%
  inner_join(games, by = "season", relationship = "many-to-many") %>%
  left_join(sales_ng, by = c("name", "item_category", "season", "saleAttribution_date")) %>%
  mutate(zero_filled = is.na(quantity),
         quantity = coalesce(quantity, 0), revenue = coalesce(revenue, 0),
         qty_per_turnstile = quantity / turnstile) %>%
  left_join(universe %>% select(name, family), by = "name")

zero_fill_report <- panel_zf %>%
  group_by(item_category) %>%
  summarise(rows = n(), n_zero_filled = sum(zero_filled),
            pct_zero_filled = round(100 * mean(zero_filled), 1), .groups = "drop")

# =============================================================================
# 5. (A) CATEGORY & FAMILY GROWTH
# =============================================================================
WINDOW_DAYS <- 21

win_base_dates <- function(promo_dates, promo_col) {
  nd <- games$saleAttribution_date[games[[promo_col]] == 0]
  nd_doy <- as.numeric(format(nd, "%j"))
  promo_doy <- as.numeric(format(promo_dates, "%j"))

  nd[vapply(nd_doy, function(d_doy) {
    any(abs(d_doy - promo_doy) <= WINDOW_DAYS)
  }, logical(1))]
}
beer_base_win <- win_base_dates(games$saleAttribution_date[games$beer_promo == 1], "beer_promo")
food_base_win <- win_base_dates(games$saleAttribution_date[games$food_promo == 1], "food_promo")

totals_all <- function(cat) category_by_game %>% filter(item_category == cat) %>%
  transmute(saleAttribution_date, quantity = cat_quantity, turnstile, Tier, season,
            Month, Weekend, concession_promotion, beer_promo, food_promo)
totals_fam <- function(fams) panel_zf %>% filter(family %in% fams) %>%
  group_by(saleAttribution_date, turnstile, Tier, season, Month, Weekend,
           concession_promotion, beer_promo, food_promo) %>%
  summarise(quantity = sum(quantity), .groups = "drop")

group_specs <- list(
  list(group = "total_beer",            label = "Total beer (category)",                 kind = "category",   promo_col = "beer_promo", tiers = beer_relevant_tiers, df = totals_all("beer")),
  list(group = "beer_domestic_sub_all", label = "Domestic-light beer substitutes",       kind = "substitute", promo_col = "beer_promo", tiers = beer_relevant_tiers, df = totals_fam(beer_sub_fams)),
  list(group = "beer_largepour_sub",    label = "Large-pour domestic beer", kind = "substitute", promo_col = "beer_promo", tiers = beer_relevant_tiers, df = totals_fam("beer_largepour_sub")),
  list(group = "beer_promoted",         label = "Miller High Life (promoted)",           kind = "promoted",   promo_col = "beer_promo", tiers = beer_relevant_tiers, df = totals_fam("beer_promoted")),
  list(group = "total_food",            label = "Total food (category, secondary)",      kind = "category",   promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_all("food")),
  list(group = "hotdog_family_all",     label = "Hot dog family (all all-beef dogs)",    kind = "category",   promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_fam(c("food_promoted", "food_hotdog_sub"))),
  list(group = "food_hotdog_sub",       label = "Other all-beef dogs (substitutes)",     kind = "substitute", promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_fam("food_hotdog_sub")),
  list(group = "food_adjacent_snack_sub",     label = "Adjacent staples (nachos/pretzel)",     kind = "substitute", promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_fam("food_adjacent_snack_sub")),
  list(group = "food_adjacent_entree_sub", label = "Adjacent entrees (burger/chicken tender)", kind = "substitute", promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_fam("food_adjacent_entree_sub")),
  list(group = "food_promoted",         label = "Hot Dog 6-1 (promoted)",                kind = "promoted",   promo_col = "food_promo", tiers = food_relevant_tiers, df = totals_fam("food_promoted"))
)

extract_promo <- function(m) {
  if (is.null(m)) return(c(rr = NA, lo = NA, hi = NA, p = NA))
  ct <- tryCatch(summary(m)$coefficients["promo", ], error = function(e) NULL)
  if (is.null(ct)) return(c(rr = NA, lo = NA, hi = NA, p = NA))
  ci <- suppressMessages(tryCatch(confint(m)["promo", ], error = function(e) c(NA, NA)))
  c(rr = exp(unname(ct[1])), lo = exp(unname(ci[1])), hi = exp(unname(ci[2])), p = unname(ct[4]))
}
build_form <- function(d, adjusted) {
  covs <- "factor(Tier)"
  if (adjusted) for (v in c("season", "Month", "Weekend"))
    if (n_distinct(d[[v]]) > 1) covs <- c(covs, sprintf("factor(%s)", v))
  reformulate(c("promo", covs, "offset(log(turnstile))"), "quantity")
}
fit_growth <- function(dat, promo_col, tiers, base_win) {
  promo <- dat %>% filter(.data[[promo_col]] == 1, Tier %in% tiers)
  base_all <- dat %>% filter(.data[[promo_col]] == 0, Tier %in% tiers)
  base_w   <- base_all %>% filter(saleAttribution_date %in% base_win)
  one <- function(base_df, adjusted, lens) {
    d <- bind_rows(promo, base_df) %>% mutate(promo = as.integer(.data[[promo_col]] == 1))
    m <- tryCatch(suppressWarnings(glm.nb(build_form(d, adjusted), data = d)), error = function(e) NULL)
    v <- extract_promo(m)
    tibble(model = lens, n_promo = sum(d$promo == 1), n_base = sum(d$promo == 0),
           rate_ratio = v["rr"], ci_low = v["lo"], ci_high = v["hi"],
           pct_change = (v["rr"] - 1) * 100, p_value = v["p"])
  }
  bind_rows(one(base_all, FALSE, "tier-matched"),
            one(base_w,   TRUE,  "window-matched"))
}
growth_results <- map_dfr(group_specs, function(g)
  fit_growth(g$df, g$promo_col, g$tiers,
             if (g$promo_col == "beer_promo") beer_base_win else food_base_win) %>%
    mutate(group = g$group, label = g$label, kind = g$kind, .before = 1))

share_of <- function(members, promoted_fam, promoted_label, promo_col, tiers) {
  d <- panel_zf %>% filter(family %in% members, Tier %in% tiers)
  b <- d %>% filter(.data[[promo_col]] == 0); p <- d %>% filter(.data[[promo_col]] == 1)
  sh <- function(x) sum(x$quantity[x$family == promoted_fam]) / sum(x$quantity)
  tibble(promoted_item = promoted_label, share_base = sh(b), share_promo = sh(p),
         share_pt_change = (sh(p) - sh(b)) * 100)
}
family_shares <- bind_rows(
  share_of(c("beer_promoted", beer_sub_fams), "beer_promoted", promoted_beer, "beer_promo", beer_relevant_tiers) %>%
    mutate(within = "domestic-light beer family"),
  share_of(c("food_promoted", "food_hotdog_sub"), "food_promoted", promoted_food, "food_promo", food_relevant_tiers) %>%
    mutate(within = "hot dog family")
)

# =============================================================================
# 6. (B) WITHIN-GAME FIXED-EFFECTS SUBSTITUTION
# =============================================================================
fit_within_game <- function(promoted_fam, keep_fams, promo_col, tiers) {
  d <- panel_zf %>%
    filter(family %in% keep_fams, Tier %in% tiers,
           (.data[[promo_col]] == 1) | (.data[[promo_col]] == 0)) %>%
    mutate(promo_active = as.integer(.data[[promo_col]] == 1),
           is_substitute = as.integer(family != promoted_fam),
           sub_x_promo = is_substitute * promo_active)
  if (n_distinct(d$promo_active) < 2 || n_distinct(d$name) < 2) return(NULL)
  m <- tryCatch(fepois(quantity ~ sub_x_promo | saleAttribution_date + name,
                       offset = ~log(turnstile), data = d, cluster = ~saleAttribution_date),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.frame(summary(m)$coeftable)["sub_x_promo", ]; est <- ct[[1]]; se <- ct[[2]]
  tibble(n_obs = nrow(d), n_games = n_distinct(d$saleAttribution_date),
         n_substitutes = n_distinct(d$name) - 1,
         rel_rate_ratio = exp(est), ci_low = exp(est - 1.96 * se), ci_high = exp(est + 1.96 * se),
         rel_pct = (exp(est) - 1) * 100, p_value = ct[[4]])
}
within_game <- bind_rows(
  fit_within_game("beer_promoted", c("beer_promoted", beer_sub_fams), "beer_promo", beer_relevant_tiers) %>%
    { if (is.null(.)) NULL else mutate(., category = "beer", promoted_item = promoted_beer, .before = 1) },
  fit_within_game("food_promoted", c("food_promoted", "food_hotdog_sub"), "food_promo", food_relevant_tiers) %>%
    { if (is.null(.)) NULL else mutate(., category = "food (other hot dogs)", promoted_item = promoted_food, .before = 1) },
  fit_within_game("food_promoted", c("food_promoted", food_sub_fams), "food_promo", food_relevant_tiers) %>%
    { if (is.null(.)) NULL else mutate(., category = "food (all substitutes)", promoted_item = promoted_food, .before = 1) }
)

# =============================================================================
# 7. (C) PER-ITEM RATE RATIOS
# =============================================================================
fit_item <- function(item, item_cat, promo_col, tiers, base_win) {
  fam <- universe$family[universe$name == item][1]
  di <- panel_zf %>% filter(name == item, Tier %in% tiers) %>%
    mutate(promo = as.integer(.data[[promo_col]] == 1))
  d_all <- di %>% filter(promo == 1 | .data[[promo_col]] == 0)
  d_win <- di %>% filter(promo == 1 | (.data[[promo_col]] == 0 & saleAttribution_date %in% base_win))
  base_row <- tibble(category = item_cat, name = item, family = fam,
                     is_substitute = as.integer(fam %in% sub_fams),
                     is_promoted = as.integer(fam %in% c("beer_promoted", "food_promoted")),
                     n_promo = sum(d_all$promo == 1), n_base = sum(d_all$promo == 0),
                     rate_ratio = NA, ci_low = NA, ci_high = NA, pct_change = NA, p_value = NA,
                     rr_adj = NA, pct_change_adj = NA, p_value_adj = NA,
                     mde_pct_80_approx = NA, note = "")
  if (base_row$n_promo < 3 || base_row$n_base < 3) return(mutate(base_row, note = "too few games"))
  fit_extract <- function(d, adjusted) {
    m <- tryCatch(suppressWarnings(glm.nb(build_form(d, adjusted), data = d)), error = function(e) NULL)
    if (is.null(m)) m <- tryCatch(glm(build_form(d, adjusted), family = poisson, data = d), error = function(e) NULL)
    if (is.null(m)) return(c(rr = NA, lo = NA, hi = NA, p = NA))
    ct <- tryCatch(summary(m)$coefficients["promo", ], error = function(e) NULL)
    if (is.null(ct)) return(c(rr = NA, lo = NA, hi = NA, p = NA))
    est <- unname(ct[1]); se <- unname(ct[2])
    c(rr = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se), p = unname(ct[4]))
  }
  t <- fit_extract(d_all, FALSE)
  a <- fit_extract(d_win, TRUE)
  pc <- d_all$qty_per_turnstile; mb <- mean(pc[d_all$promo == 0], na.rm = TRUE)
  mde <- tryCatch(power.t.test(n = min(base_row$n_promo, base_row$n_base),
                               sd = sd(pc, na.rm = TRUE), power = 0.8, sig.level = 0.05)$delta / mb * 100,
                  error = function(e) NA_real_)
  mutate(base_row,
         rate_ratio = unname(t["rr"]), ci_low = unname(t["lo"]), ci_high = unname(t["hi"]),
         pct_change = (unname(t["rr"]) - 1) * 100, p_value = unname(t["p"]),
         rr_adj = unname(a["rr"]), pct_change_adj = (unname(a["rr"]) - 1) * 100, p_value_adj = unname(a["p"]),
         mde_pct_80_approx = mde)
}
beer_universe <- universe %>% filter(item_category == "beer") %>% pull(name)
food_universe <- universe %>% filter(item_category == "food") %>% pull(name)
item_results <- bind_rows(
  map_dfr(beer_universe, ~ fit_item(.x, "beer", "beer_promo", beer_relevant_tiers, beer_base_win)),
  map_dfr(food_universe, ~ fit_item(.x, "food", "food_promo", food_relevant_tiers, food_base_win))
) %>% arrange(category, desc(is_promoted), p_value)

item_results$p_adj_within_family <- NA_real_
for (f in sub_fams) {
  ix <- which(item_results$family == f & !is.na(item_results$p_value_adj))
  if (length(ix)) item_results$p_adj_within_family[ix] <- p.adjust(item_results$p_value_adj[ix], "BH")
}
all_ix <- which(item_results$is_substitute == 1 & !is.na(item_results$p_value_adj))
item_results$p_adj_all_subs <- NA_real_
item_results$p_adj_all_subs[all_ix] <- p.adjust(item_results$p_value_adj[all_ix], "BH")

# =============================================================================
# 8. OUTPUTS
# =============================================================================
write.csv(growth_results, file.path(tables_dir, paste0("cannib_A_growth", output_tag, ".csv")), row.names = FALSE)
write.csv(family_shares,  file.path(tables_dir, paste0("cannib_A_family_shares", output_tag, ".csv")), row.names = FALSE)
write.csv(within_game,    file.path(tables_dir, paste0("cannib_B_within_game_fe", output_tag, ".csv")), row.names = FALSE)
write.csv(item_results,   file.path(tables_dir, paste0("cannib_C_item_rate_ratios", output_tag, ".csv")), row.names = FALSE)
write.csv(zero_fill_report, file.path(tables_dir, paste0("cannib_zero_fill_report", output_tag, ".csv")), row.names = FALSE)

saveRDS(list(
  growth_results = growth_results, family_shares = family_shares,
  within_game = within_game, item_results = item_results,
  zero_fill_report = zero_fill_report,
  item_panel = panel_zf, category_by_game = category_by_game,
  universe = universe,
  beer_base_win = beer_base_win, food_base_win = food_base_win,
  meta = list(promoted_beer = promoted_beer, promoted_food = promoted_food,
              beer_relevant_tiers = beer_relevant_tiers, food_relevant_tiers = food_relevant_tiers,
              beer_sub_fams = beer_sub_fams, food_sub_fams = food_sub_fams,
              window_days = WINDOW_DAYS,
              n_beer_base_win = length(beer_base_win), n_food_base_win = length(food_base_win),
              n_games = n_distinct(panel_zf$saleAttribution_date))),
  file.path(tables_dir, paste0("cannibalization_results", output_tag, ".rds")))

# =============================================================================
# 9. CONSOLE SUMMARY
# =============================================================================
cat("\n\n############ CANNIBALIZATION ANALYSIS - 300 LEVEL - SUMMARY ############\n")
cat("\n-- Zero-fill / availability report --\n"); print(as.data.frame(zero_fill_report))
cat("\n-- (A) Category & family growth (tier-matched vs calendar-adjusted) --\n")
print(as.data.frame(growth_results %>%
  transmute(label, kind, model, n_promo, pct_change = round(pct_change, 1),
            CI = sprintf("%.2f-%.2f", ci_low, ci_high), p = round(p_value, 4))), row.names = FALSE)
cat("\n-- (B) Within-game mix shift (curated substitutes only) --\n"); print(as.data.frame(within_game), digits = 3)
cat("\n-- (C) Per-item rate ratios --\n")
print(as.data.frame(item_results %>% transmute(category, name,
  role = ifelse(is_promoted == 1, "promoted", ifelse(is_substitute == 1, "sub", "contrast")),
  n_promo, pct_tier = round(pct_change, 1), p_tier = round(p_value, 4),
  pct_adj = round(pct_change_adj, 1), p_adj = round(p_value_adj, 4),
  p_fam = round(p_adj_within_family, 4), p_all = round(p_adj_all_subs, 4))), row.names = FALSE)
cat("\nOutputs written to:", tables_dir, "\n")
