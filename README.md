# Concession Promotion Cannibalization Analysis

This project measures whether the Atlanta Braves concession promotions on **Miller High Life 12 oz**
and the **6-1 all-beef hot dog** grow sales or simply shift them from comparable full-price items
(cannibalization). It also tests whether the Market stands that host the promotions draw extra traffic.

**This is a portfolio version using synthetic data.** The game schedule is real (public MLB schedule
information); all sales figures, attendance counts, and item volumes are synthetic but structured to
preserve the statistical patterns and directional findings of the original analysis.

The headline deliverable is the rendered report at
`output/reports/cannibalization_analysis.html`.

---

## The question

Discounting an item reliably sells more of that item. The decision-relevant question is where that
volume comes from:

- **Incremental:** the discount brings in spending that would not otherwise happen. The category grows.
- **Cannibalization:** fans switch from a full-price item to the discounted one. Revenue moves around
  the menu without growing.

The analysis separates these two stories for beer and for hot dogs.

---

## Folder layout

```
concessions_portfolio/
  data/raw/               Synthetic CSVs + real schedule files
  scripts/
    generate_synthetic_data.R              Run first -- creates all data files
    cannibalization_analysis.R             Main cannibalization models (Methods A-D)
    cannibalization_analysis.qmd           Renders the report from saved results
    stand_level_promotion_analysis.R       Same methods, restricted to 300-level stands
    cannibalization_analysis_300_level.qmd 300-level report
    concessions_promotions.R               Exploratory KPI/plot script
    concession_promo_summary.qmd           Static narrative summary
  output/
    tables/       cannib_*.csv result tables + cannibalization_results*.rds
    figures/      Generated plots
    reports/      Rendered reports (HTML)
  README.md       This file
```

## How to run

### 0. Generate synthetic data

```bash
Rscript scripts/generate_synthetic_data.R
```

This creates all CSV files in `data/raw/`. The schedule files should already be present (they are
real public data copied into the project). Run this once before any analysis.

### 1. Build the results

```bash
Rscript scripts/cannibalization_analysis.R
Rscript scripts/stand_level_promotion_analysis.R
Rscript scripts/concessions_promotions.R
```

Each script resolves paths relative to the project root and reads only from `data/raw/`.

Required R packages: `dplyr`, `tidyr`, `stringr`, `purrr`, `ggplot2`, `MASS`, `fixest`, `broom`,
`lubridate`, `scales`, `ggridges`, `patchwork`, `lme4`, `lmerTest`, `performance`.

### 2. Render the reports

```bash
quarto render scripts/cannibalization_analysis.qmd
quarto render scripts/cannibalization_analysis_300_level.qmd
```

Move the resulting HTML files into `output/reports/`.

Re-run step 1 only when the data changes; re-render step 2 to refresh the documents.

---

## Data sources

All data is synthetic except the game schedule files. The synthetic data generator
(`generate_synthetic_data.R`) uses negative-binomial draws with promotion multipliers to produce
overdispersed count data that yields directionally similar analytical findings.

| Source | Level of detail |
|--------|-----------------|
| Item-level sales, 2025-26 | Every item, every game, ballpark-wide (no stand breakdown) |
| Stand-level sales, 2025-26 | Every stand, every game: transactions and revenue only (no item detail) |
| 300-level item sales | Transaction-level item data tied to 300-level stands |
| Concession KPIs | Attendance (turnstile) and promotion flags per game |
| Schedule (real) | Game tier, month, day, weekend, opponent, time of year |

Two constraints shape what is answerable:

- **Item sales are not broken out by stand.** Item-level cannibalization can only be measured
  ballpark-wide, not for the Market specifically.
- **Stand sales carry no item detail.** The Market-specific test is limited to transactions and revenue.

Promotions covered (all fall in the first half of the season):

| Promotion | Window | Discounts | Tiers |
|-----------|--------|-----------|-------|
| Slide into Summer (2025) | 5/30 to 6/05 | $2 6-1 hot dogs, $4 Miller High Life 12oz | 1, 3 |
| Spring Value Games (2026) | 3/30-4/01, 4/13-4/15, 4/28 | $2.50 6-1 hot dogs, $4 Miller High Life 12oz | 5 |
| May in the A (2026) | 5/12-5/14, 5/17, 5/22, 5/24 | $3 Miller High Life 12oz, $2 ice cream sandwiches | 2, 3 |

---

## Methodology

### Building blocks

- **Everything is per fan.** Sales are modeled as counts with an attendance offset, `log(turnstile)`,
  so a busier or quieter night does not by itself look like a promotion effect. Rate ratios are the
  natural output: 1.15 means 15% more units per fan; 1.0 means no change.
- **Negative-binomial models.** Concession counts are over-dispersed, so negative-binomial regression
  is used rather than a t-test on divided rates.
- **Curated substitute families, not the whole menu.** Items are grouped into close substitutes:
  - Beer: domestic and light beers (Coors Light, Miller Lite, Coors Banquet), split out into a
    large-pour subgroup.
  - Hot dogs: the other all-beef hot dog sizes (4-1, 2-1), plus adjacent staples (nachos, jumbo pretzel).
  - POS name duplicates are merged, novelty and very low-volume items are excluded (minimum 5,000
    units over the two seasons), and a set of unrelated "contrast" items is shown for reference.
- **Complete, zero-filled item-by-game table.** An item is treated as available in a season if it
  sold at least once that season; every game in an available season then gets a row, with unsold games
  filled as a genuine zero rather than dropped.

### The two lenses on across-game comparisons

Every across-game result is estimated two ways:

1. **Simple comparison (tier-matched):** promotion games versus all non-promotion games in the same
   game tiers.
2. **Season-matched:** promotion games versus only the non-promotion games within 21 days of a
   promotion date, plus a coarse adjustment for month, weekend, and season.

The second lens exists because **all promotion games fall in the first half of the season**. Beer
sells very differently in cool spring than in summer heat for reasons unrelated to the promotion.
**When the two lenses disagree, the simple number reflected timing, and we rely on the season-matched
result.**

### The four methods

| Method | Question | Design |
|--------|----------|--------|
| **A. Category and family growth** | Did total beer / total hot dogs go up or down per fan? | Negative-binomial with attendance offset, both lenses |
| **B. Within-game comparison** | Within the same game, did the featured item pull ahead of its close substitutes? | Poisson model with game and item fixed effects (`fixest`), clustered SEs |
| **C. Item-by-item** | Which specific items moved, and is it real? | Negative-binomial per item, both lenses, Benjamini-Hochberg FDR correction |
| **D. Market-stand traffic** | Did the Market stands draw more traffic? | Difference-in-differences with stand and game fixed effects |

Methods B and D absorb game-level confounders through fixed effects and are the strongest designs.
Methods A and C are associations.

---

## Outputs

| File | Contents |
|------|----------|
| `output/reports/cannibalization_analysis.html` | The report (written for decision-makers) |
| `output/tables/cannib_A_growth*.csv` | Category and family growth, both lenses |
| `output/tables/cannib_A_family_shares*.csv` | Featured item share of its family |
| `output/tables/cannib_B_within_game_fe*.csv` | Within-game mix shift |
| `output/tables/cannib_C_item_rate_ratios*.csv` | Item-by-item results with FDR |
| `output/tables/cannib_D_stand_did*.csv` | Market-stand difference-in-differences |
| `output/tables/cannibalization_results*.rds` | Full results bundle the reports read |
| `output/figures/cannibalization/` | Generated plots |

## How to read the results

- **Category grows (Method A, category rows above 1):** evidence of incremental sales.
- **Substitute family shrinks (Method A, substitute rows below 1) or specific substitutes fall
  (Method C):** evidence of cannibalization.
- **Featured item up, category flat, a substitute down:** the promotion is shifting volume, not
  growing it.
- **"Not significant" is not "no effect."** With few promotion games, several comparisons cannot
  detect a moderate effect; see the power figure in the report.

## Known limitations

- Item-level cannibalization is ballpark-wide, not Market-specific (item data has no stand field).
- Few promotion games, all in the first half of the season.
- Across-game comparisons are associations; they do not control opponent draw or weather directly.
- Item availability is inferred by season, not confirmed.
- Spring Value Games discounted beer and hot dogs on the same dates, so those effects cannot be fully
  separated.
- No margin data, so volume shifts cannot yet be translated into profit.
- **Data is synthetic.** The numbers are not real; the methodology and analytical framework are.
