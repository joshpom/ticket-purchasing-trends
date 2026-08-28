# ============================================================================
# generate_synthetic_data.R
#
# Generates synthetic datasets that mirror the structure and patterns of a
# real Atlanta Braves ticket-purchasing analysis (2023–2026), without
# exposing any proprietary data.
#
# Outputs (written to ../data/):
#   1. primary_site_traffic.csv      – Ticketmaster event-detail page hits
#   2. secondary_site_traffic_YYYY.csv – Secondary-market event page views
#   3. single_game_sales.csv         – Single-game ticket transactions
#   4. game_tiers.csv                – Game tier classifications
#
# Only base R is required — no additional packages.
# ============================================================================

set.seed(42)

# Detect whether we're running from scripts/ or the repo root
if (basename(getwd()) == "scripts") {
  out_dir <- "../data"
} else {
  out_dir <- "data"
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Output directory:", normalizePath(out_dir, mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Helper: generate ~30 realistic home-game dates for one season (late Mar–mid May)
# Games are spaced 1–3 days apart with occasional off-days, mimicking an
# MLB home schedule through the YTD cutoff used in the analysis.
# ---------------------------------------------------------------------------
generate_game_dates <- function(year, n_games = 30) {
  # Season starts late March
  start <- as.Date(paste0(year, "-03-25"))
  dates <- start
  current <- start
  while (length(dates) < n_games) {
    gap <- sample(1:3, 1, prob = c(0.55, 0.30, 0.15))
    current <- current + gap
    dates <- c(dates, current)
  }
  dates[1:n_games]
}

# ---------------------------------------------------------------------------
# Build game schedules for all four seasons
# ---------------------------------------------------------------------------
seasons <- c(2023, 2024, 2025, 2026)

all_games <- data.frame()
for (yr in seasons) {
  gd <- generate_game_dates(yr)
  df <- data.frame(
    season_year = yr,
    game_number = seq_along(gd),
    event_name  = sprintf("GAME_%d_%02d", yr, seq_along(gd)),
    event_date  = gd,
    stringsAsFactors = FALSE
  )
  all_games <- rbind(all_games, df)
}

cat("Generated", nrow(all_games), "total games across", length(seasons), "seasons\n")

# ============================================================================
# 1. PRIMARY SITE TRAFFIC
#
# Pattern: traffic ramps up exponentially as game day approaches.
# 2023 & 2024 are high, 2025 dips, 2026 rebounds.
# The % distribution across day-buckets is stable year-over-year.
# ============================================================================

# Target average hits per day in each bucket, by season
primary_targets <- list(
  "2023" = c("21+"   = 140, "8-21" = 700,  "3-7" = 1200, "1-2" = 2200, "0" = 4200),
  "2024" = c("21+"   = 130, "8-21" = 600,  "3-7" = 1100, "1-2" = 2100, "0" = 3800),
  "2025" = c("21+"   = 100, "8-21" = 400,  "3-7" =  800, "1-2" = 1600, "0" = 3100),
  "2026" = c("21+"   = 110, "8-21" = 530,  "3-7" = 1000, "1-2" = 2200, "0" = 4100)
)

# For each game, generate daily traffic going back up to 120 days
primary_rows <- list()
idx <- 1

for (i in seq_len(nrow(all_games))) {
  game   <- all_games[i, ]
  yr_key <- as.character(game$season_year)
  targets <- primary_targets[[yr_key]]

  for (days_before in 0:120) {
    # Determine which bucket this day falls in
    if (days_before == 0) {
      lambda <- targets["0"]
    } else if (days_before <= 2) {
      lambda <- targets["1-2"]
    } else if (days_before <= 7) {
      lambda <- targets["3-7"]
    } else if (days_before <= 21) {
      lambda <- targets["8-21"]
    } else {
      lambda <- targets["21+"]
    }

    # Add within-bucket variation: smooth gradient within each bucket
    # so that day 1 is slightly higher than day 2, day 3 higher than day 7, etc.
    if (days_before > 0 && days_before <= 2) {
      # Slight gradient: day 1 gets ~55% of bucket avg boost, day 2 ~45%
      gradient <- 1 + 0.10 * (2 - days_before)
      lambda <- lambda * gradient
    } else if (days_before > 2 && days_before <= 7) {
      gradient <- 1 + 0.08 * (7 - days_before) / 4
      lambda <- lambda * gradient
    } else if (days_before > 7 && days_before <= 21) {
      gradient <- 1 + 0.05 * (21 - days_before) / 13
      lambda <- lambda * gradient
    }

    hits     <- rpois(1, lambda)
    visits   <- rpois(1, hits * 0.90)
    visitors <- rpois(1, visits * 0.80)

    batch_date <- game$event_date - days_before

    primary_rows[[idx]] <- data.frame(
      batch_date  = format(batch_date, "%m/%d/%Y"),
      season_year = game$season_year,
      game_number = game$game_number,
      event_name  = game$event_name,
      event_date  = format(game$event_date, "%m/%d/%Y"),
      visitors    = max(visitors, 0),
      visits      = max(visits, 0),
      hits        = max(hits, 0),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}

primary_traffic <- do.call(rbind, primary_rows)
write.csv(primary_traffic,
          file.path(out_dir, "primary_site_traffic.csv"),
          row.names = FALSE)
cat("Wrote primary_site_traffic.csv:", nrow(primary_traffic), "rows\n")

# ============================================================================
# 2. SECONDARY SITE TRAFFIC (one file per year, 2024–2026)
#
# Pattern: secondary-market page views decline year-over-year.
# Same game dates, same day-bucket structure.
# ============================================================================

secondary_targets <- list(
  "2024" = c("21+"   = 180, "8-21" = 880,  "3-7" = 2600, "1-2" = 5800, "0" = 21000),
  "2025" = c("21+"   = 130, "8-21" = 700,  "3-7" = 2000, "1-2" = 4800, "0" = 19000),
  "2026" = c("21+"   =  90, "8-21" = 520,  "3-7" = 1200, "1-2" = 3300, "0" = 12000)
)

for (yr in c(2024, 2025, 2026)) {
  yr_key  <- as.character(yr)
  targets <- secondary_targets[[yr_key]]
  games   <- all_games[all_games$season_year == yr, ]
  sec_rows <- list()
  sidx <- 1

  for (i in seq_len(nrow(games))) {
    game <- games[i, ]
    for (days_before in 0:120) {
      if (days_before == 0) {
        lambda <- targets["0"]
      } else if (days_before <= 2) {
        lambda <- targets["1-2"]
      } else if (days_before <= 7) {
        lambda <- targets["3-7"]
      } else if (days_before <= 21) {
        lambda <- targets["8-21"]
      } else {
        lambda <- targets["21+"]
      }

      # Add gradient within buckets
      if (days_before > 0 && days_before <= 2) {
        lambda <- lambda * (1 + 0.10 * (2 - days_before))
      } else if (days_before > 2 && days_before <= 7) {
        lambda <- lambda * (1 + 0.08 * (7 - days_before) / 4)
      } else if (days_before > 7 && days_before <= 21) {
        lambda <- lambda * (1 + 0.05 * (21 - days_before) / 13)
      }

      views <- rpois(1, lambda)
      action_date <- game$event_date - days_before

      sec_rows[[sidx]] <- data.frame(
        action_date            = format(action_date, "%m/%d/%Y"),
        event_date             = format(game$event_date, "%m/%d/%Y"),
        season_year            = yr,
        event_page_view_count  = max(views, 0),
        stringsAsFactors       = FALSE
      )
      sidx <- sidx + 1
    }
  }

  sec_df <- do.call(rbind, sec_rows)
  fname  <- paste0("secondary_site_traffic_", yr, ".csv")
  write.csv(sec_df, file.path(out_dir, fname), row.names = FALSE)
  cat("Wrote", fname, ":", nrow(sec_df), "rows\n")
}

# ============================================================================
# 3. SINGLE GAME SALES
#
# Key finding to preserve: the % distribution of sales across day-buckets
# is remarkably STABLE across all four seasons (~55-57% day-of, ~24-27%
# 1-2 days before, ~12-14% 3-7 days, ~5-7% 8-21 days, ~1-2% 21+).
#
# While total volume varies (2026 highest, 2025 lowest), the timing mix
# stays consistent.
#
# For the density plot: 2026 has a lower weighted-median lead time (~8 days)
# vs 2025 (~18 days). This comes from the continuous distribution within
# buckets, not the bucket-level percentages. We achieve this by skewing
# 2026 sales toward the lower end within each bucket (e.g., more day 1
# vs day 2 within the 1-2 bucket), and spreading 2025 more evenly.
# ============================================================================

# Target average seats sold per day per game, by bucket.
# These produce roughly stable % splits:
#   day_of ~55%, 1-2 ~25%, 3-7 ~13%, 8-21 ~6%, 21+ ~1%
sales_targets <- list(
  "2023" = c("21+"  = 28,  "8-21" = 78,  "3-7" = 170, "1-2" = 330, "0" = 760),
  "2024" = c("21+"  = 22,  "8-21" = 95,  "3-7" = 180, "1-2" = 360, "0" = 640),
  "2025" = c("21+"  = 12,  "8-21" = 58,  "3-7" = 135, "1-2" = 245, "0" = 580),
  "2026" = c("21+"  = 18,  "8-21" = 88,  "3-7" = 215, "1-2" = 460, "0" = 860)
)

sales_rows <- list()
sidx <- 1

for (i in seq_len(nrow(all_games))) {
  game   <- all_games[i, ]
  yr_key <- as.character(game$season_year)
  targets <- sales_targets[[yr_key]]

  # Not every day has a sale — generate sales for a subset of days.
  # Days closer to the game are more likely to have sales.
  for (days_before in 0:120) {
    # Determine bucket and target seats
    if (days_before == 0) {
      bucket_seats <- targets["0"]
      # For day-of, always have sales
      has_sale <- TRUE
    } else if (days_before <= 2) {
      bucket_seats <- targets["1-2"]
      has_sale <- TRUE
    } else if (days_before <= 7) {
      bucket_seats <- targets["3-7"]
      has_sale <- runif(1) < 0.92
    } else if (days_before <= 21) {
      bucket_seats <- targets["8-21"]
      has_sale <- runif(1) < 0.80
    } else {
      bucket_seats <- targets["21+"]
      has_sale <- runif(1) < 0.40  # Many far-out days have no sales
    }

    if (!has_sale) next

    # Within-bucket variation: for 2026, concentrate more toward game day;
    # for 2025, spread more evenly — this produces the density-plot shift
    if (game$season_year == 2026) {
      # 2026: skew toward the closer end of each bucket
      if (days_before > 0 && days_before <= 2) {
        skew <- ifelse(days_before == 1, 1.25, 0.75)
      } else if (days_before > 2 && days_before <= 7) {
        skew <- 1 + 0.15 * (7 - days_before) / 4
      } else if (days_before > 7 && days_before <= 21) {
        skew <- 1 + 0.12 * (21 - days_before) / 13
      } else {
        skew <- 1
      }
    } else if (game$season_year == 2025) {
      # 2025: skew toward the farther end (higher lead time)
      if (days_before > 0 && days_before <= 2) {
        skew <- ifelse(days_before == 2, 1.15, 0.85)
      } else if (days_before > 2 && days_before <= 7) {
        skew <- 1 - 0.08 * (7 - days_before) / 4
      } else if (days_before > 7 && days_before <= 21) {
        skew <- 1 + 0.08 * (days_before - 7) / 13
      } else {
        skew <- 1.10
      }
    } else {
      skew <- 1
    }

    seats <- rpois(1, bucket_seats * skew)
    seats <- max(seats, 1)

    # Derive other fields
    transactions      <- max(round(seats * runif(1, 0.35, 0.45)), 1)
    unique_buyers     <- max(round(transactions * runif(1, 0.80, 0.90)), 1)
    price_per_seat    <- runif(1, 45, 75)
    block_price       <- round(seats * price_per_seat, 2)

    add_date <- game$event_date - days_before

    sales_rows[[sidx]] <- data.frame(
      add_date             = format(add_date, "%Y-%m-%d"),
      event_name           = game$event_name,
      event_date           = format(game$event_date, "%Y-%m-%d"),
      unique_buyers        = unique_buyers,
      transactions         = transactions,
      num_seats            = seats,
      block_purchase_price = block_price,
      season_year          = game$season_year,
      stringsAsFactors     = FALSE
    )
    sidx <- sidx + 1
  }
}

single_sales <- do.call(rbind, sales_rows)
write.csv(single_sales,
          file.path(out_dir, "single_game_sales.csv"),
          row.names = FALSE)
cat("Wrote single_game_sales.csv:", nrow(single_sales), "rows\n")

# ============================================================================
# 4. GAME TIERS
#
# Each season's ~30 games are classified into pricing tiers.
# Higher tiers (Diamond, Marquee, Premier) are more premium games.
# Lower tiers (Select, Standard, Value) are lower-demand games.
# ============================================================================

tier_distribution <- c(
  rep("Diamond",  3),
  rep("Marquee",  5),
  rep("Premier",  5),
  rep("Select",   7),
  rep("Standard", 6),
  rep("Value",    4)
)

game_tiers <- data.frame()
for (yr in seasons) {
  games <- all_games[all_games$season_year == yr, ]
  n <- nrow(games)
  tiers <- sample(tier_distribution, n, replace = FALSE)
  df <- data.frame(
    event_name  = games$event_name,
    season_year = yr,
    Tier        = tiers,
    stringsAsFactors = FALSE
  )
  game_tiers <- rbind(game_tiers, df)
}

write.csv(game_tiers,
          file.path(out_dir, "game_tiers.csv"),
          row.names = FALSE)
cat("Wrote game_tiers.csv:", nrow(game_tiers), "rows\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n--- Synthetic data generation complete ---\n")
cat("Files written to:", normalizePath(out_dir, mustWork = FALSE), "\n\n")

# Quick sanity check: show % distribution of sales by bucket
cat("Sanity check — % of avg daily seats by bucket per season:\n")
sales_check <- single_sales
sales_check$days_before <- as.numeric(
  as.Date(sales_check$event_date) - as.Date(sales_check$add_date)
)
sales_check$bucket <- cut(
  sales_check$days_before,
  breaks = c(-1, 0, 2, 7, 21, Inf),
  labels = c("Day of", "1-2 days", "3-7 days", "8-21 days", "21+ days")
)

for (yr in seasons) {
  sub <- sales_check[sales_check$season_year == yr, ]
  agg <- tapply(sub$num_seats, sub$bucket, sum)
  pcts <- round(agg / sum(agg) * 100, 1)
  cat(sprintf("  %d: %s\n", yr,
      paste(names(pcts), "=", paste0(pcts, "%"), collapse = ", ")))
}
cat("\nDone.\n")
