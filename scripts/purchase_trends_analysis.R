# =============================================================================
# Purchase Trends Analysis — Atlanta Braves Single Game Tickets
# =============================================================================
# Portfolio version using synthetic data.
# Original analysis compared primary & secondary site traffic and single-game
# ticket sales across 2023–2026 seasons to answer whether fans are buying
# tickets closer to game day over time.
#
# To run: setwd() to the scripts/ folder, then source this file.
# Data lives in ../data/  |  Plots are saved to ../graphs/
# =============================================================================

library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# YTD cutoff — restrict each season to games through mid-May so comparisons
# are apples-to-apples across years with different amounts of season played.
ytd_cutoff_md <- "05-15"

# Detect whether we're running from scripts/ or the repo root
if (basename(getwd()) == "scripts") {
  data_dir   <- "../data"
  graphs_dir <- "../graphs"
} else {
  data_dir   <- "data"
  graphs_dir <- "graphs"
}
if (!dir.exists(graphs_dir)) dir.create(graphs_dir, recursive = TRUE)


# =============================================================================
# Read Data
# =============================================================================

primary_site_traffic <- read.csv(file.path(data_dir, "primary_site_traffic.csv"))
secondary_site_traffic_2026 <- read.csv(file.path(data_dir, "secondary_site_traffic_2026.csv"))
secondary_site_traffic_2025 <- read.csv(file.path(data_dir, "secondary_site_traffic_2025.csv"))
secondary_site_traffic_2024 <- read.csv(file.path(data_dir, "secondary_site_traffic_2024.csv"))
game_tiers <- read.csv(file.path(data_dir, "game_tiers.csv"))
single_sales <- read.csv(file.path(data_dir, "single_game_sales.csv"))


# =============================================================================
# Helper: Day-Bucket Classification
# =============================================================================
# Groups days-before-game into purchase-timing windows used across all plots.

add_day_bucket <- function(df) {
  df %>%
    mutate(day_bucket = case_when(
      days_before_game == 0 ~ "Day of",
      days_before_game <= 2 ~ "1-2 days before",
      days_before_game <= 7 ~ "3-7 days before",
      days_before_game <= 21 ~ "8-21 days before",
      TRUE ~ "21+ days before"
    ),
    day_bucket = factor(day_bucket, levels = c(
      "21+ days before", "8-21 days before", "3-7 days before",
      "1-2 days before", "Day of"
    ))
  )
}


# =============================================================================
# Primary Site Traffic
# =============================================================================
# Clickstream hits on the Ticketmaster event-detail page for each game,
# measured daily. Joined with game tiers for tier-level analysis.

primary_site_traffic <- primary_site_traffic %>%
  mutate(days_before_game = as.numeric(
    as.Date(event_date, format = "%m/%d/%Y") -
    as.Date(batch_date, format = "%m/%d/%Y")
  )) %>%
  left_join(game_tiers, by = c("event_name", "season_year")) %>%
  mutate(tier_group = case_when(
    Tier %in% c("Diamond", "Marquee", "Premier") ~ "Higher (Diamond/Marquee/Premier)",
    Tier %in% c("Select", "Standard", "Value") ~ "Lower (Select/Standard/Value)"
  ))

# --- Aggregate: avg hits per day by day bucket & season ---
primary_bar_summary <- primary_site_traffic %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date, format = "%m/%d/%Y"), "%m-%d") <= ytd_cutoff_md) %>%
  add_day_bucket() %>%
  group_by(season_year, day_bucket) %>%
  summarise(avg_hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
  mutate(season_year = as.factor(season_year))

# --- Same, split by tier group ---
primary_bar_summary_tier <- primary_site_traffic %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date, format = "%m/%d/%Y"), "%m-%d") <= ytd_cutoff_md,
         !is.na(tier_group)) %>%
  add_day_bucket() %>%
  group_by(season_year, tier_group, day_bucket) %>%
  summarise(avg_hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
  mutate(season_year = as.factor(season_year))


# --- Plot: Primary Traffic Magnitude ---
primary_st_ytd_magnitude <- ggplot(primary_bar_summary,
                                    aes(x = day_bucket, y = avg_hits, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Average Primary Site Traffic per Day by Day Bucket (YTD)",
    subtitle = "Hits, 2023–2026",
    x = NULL, y = "Avg Hits", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "primary_traffic_magnitude.png"),
       plot = primary_st_ytd_magnitude,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Secondary Site Traffic (Resale Marketplace)
# =============================================================================

secondary_site_traffic <- bind_rows(
  secondary_site_traffic_2024,
  secondary_site_traffic_2025,
  secondary_site_traffic_2026
) %>%
  mutate(days_before_game = as.numeric(
    as.Date(event_date, format = "%m/%d/%Y") -
    as.Date(action_date, format = "%m/%d/%Y")
  ))

secondary_bar_summary <- secondary_site_traffic %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date, format = "%m/%d/%Y"), "%m-%d") <= ytd_cutoff_md) %>%
  add_day_bucket() %>%
  group_by(season_year, day_bucket) %>%
  summarise(avg_views = mean(event_page_view_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(season_year = as.factor(season_year))

# --- Plot: Secondary Traffic Magnitude ---
secondary_traffic_magnitude <- ggplot(secondary_bar_summary,
                                       aes(x = day_bucket, y = avg_views, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Average Secondary Site Traffic per Day by Day Bucket (YTD)",
    subtitle = "Event page views, 2024–2026",
    x = NULL, y = "Avg Event Page Views", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "secondary_traffic_magnitude.png"),
       plot = secondary_traffic_magnitude,
       width = 10, height = 6, dpi = 300)

# --- Secondary traffic by month group (March/April vs May) ---
secondary_bar_summary_month <- secondary_site_traffic %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date, format = "%m/%d/%Y"), "%m-%d") <= ytd_cutoff_md) %>%
  mutate(month_group = case_when(
    as.numeric(format(as.Date(event_date, format = "%m/%d/%Y"), "%m")) %in% c(3, 4) ~ "March/April",
    as.numeric(format(as.Date(event_date, format = "%m/%d/%Y"), "%m")) == 5 ~ "May"
  )) %>%
  filter(!is.na(month_group)) %>%
  add_day_bucket() %>%
  group_by(season_year, month_group, day_bucket) %>%
  summarise(avg_views = mean(event_page_view_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(season_year = as.factor(season_year))

secondary_traffic_magnitude_month <- ggplot(secondary_bar_summary_month,
                                             aes(x = day_bucket, y = avg_views, fill = season_year)) +
  geom_col(position = "dodge") +
  facet_wrap(~ month_group) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Average Secondary Site Traffic per Day by Day Bucket (YTD)",
    subtitle = "Event page views by month group, 2024–2026",
    x = NULL, y = "Avg Event Page Views", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(graphs_dir, "secondary_traffic_magnitude_month.png"),
       plot = secondary_traffic_magnitude_month,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Single Game Ticket Sales
# =============================================================================

single_sales <- single_sales %>%
  mutate(
    days_before_game = as.numeric(as.Date(event_date) - as.Date(add_date))
  )

single_bar_summary <- single_sales %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date), "%m-%d") <= ytd_cutoff_md) %>%
  add_day_bucket() %>%
  group_by(season_year, day_bucket) %>%
  summarise(
    avg_seats = sum(num_seats, na.rm = TRUE) / n_distinct(event_date) / n_distinct(days_before_game),
    avg_transactions = sum(transactions, na.rm = TRUE) / n_distinct(event_date) / n_distinct(days_before_game),
    .groups = "drop"
  ) %>%
  mutate(season_year = as.factor(season_year))

# --- Plot: Seats Sold Magnitude ---
seats_sold_ytd_magnitude <- ggplot(single_bar_summary,
                                    aes(x = day_bucket, y = avg_seats, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Average Single Game Seats Sold per Day by Day Bucket (YTD)",
    subtitle = "2023–2026",
    x = NULL, y = "Avg Seats Sold", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "seats_sold_magnitude.png"),
       plot = seats_sold_ytd_magnitude,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Year-to-Date Percentage Plots
# =============================================================================
# The key insight: the % distribution across day buckets is remarkably stable
# across all four seasons, even as total volume changes.

# --- Primary Traffic % ---
primary_bar_ytd <- primary_site_traffic %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date, format = "%m/%d/%Y"), "%m-%d") <= ytd_cutoff_md) %>%
  add_day_bucket() %>%
  group_by(season_year, day_bucket) %>%
  summarise(avg_hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
  group_by(season_year) %>%
  mutate(pct = avg_hits / sum(avg_hits)) %>%
  ungroup() %>%
  mutate(season_year = as.factor(season_year))

primary_st_ytd_percent <- ggplot(primary_bar_ytd,
                                  aes(x = day_bucket, y = pct, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Primary Site Traffic per Day by Day Bucket (YTD)",
    subtitle = "% share of avg hits, 2023–2026",
    x = NULL, y = "% of Avg Hits", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "primary_traffic_percent.png"),
       plot = primary_st_ytd_percent,
       width = 10, height = 6, dpi = 300)

# --- Seats Sold % ---
single_bar_ytd <- single_sales %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date), "%m-%d") <= ytd_cutoff_md) %>%
  add_day_bucket() %>%
  group_by(season_year, day_bucket) %>%
  summarise(avg_seats = sum(num_seats, na.rm = TRUE) / n_distinct(event_date) / n_distinct(days_before_game),
            .groups = "drop") %>%
  group_by(season_year) %>%
  mutate(pct = avg_seats / sum(avg_seats)) %>%
  ungroup() %>%
  mutate(season_year = as.factor(season_year))

seats_sold_ytd_percent <- ggplot(single_bar_ytd,
                                  aes(x = day_bucket, y = pct, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Single Game Seats Sold per Day by Day Bucket (YTD)",
    subtitle = "% share of avg seats sold, 2023–2026",
    x = NULL, y = "% of Avg Seats Sold", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "seats_sold_ytd_percent.png"),
       plot = seats_sold_ytd_percent,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Sales by Tier Group
# =============================================================================

single_sales <- single_sales %>%
  left_join(game_tiers, by = c("event_name", "season_year")) %>%
  mutate(tier_group = case_when(
    Tier %in% c("Diamond", "Marquee", "Premier") ~ "Higher (Diamond/Marquee/Premier)",
    Tier %in% c("Select", "Standard", "Value") ~ "Lower (Select/Standard/Value)"
  ))

single_bar_ytd_tier <- single_sales %>%
  filter(days_before_game >= 0,
         format(as.Date(event_date), "%m-%d") <= ytd_cutoff_md,
         season_year %in% c(2023, 2024, 2025, 2026),
         !is.na(tier_group)) %>%
  add_day_bucket() %>%
  group_by(season_year, tier_group, day_bucket) %>%
  summarise(
    avg_seats = sum(num_seats, na.rm = TRUE) / n_distinct(event_date) / n_distinct(days_before_game),
    avg_transactions = sum(transactions, na.rm = TRUE) / n_distinct(event_date) / n_distinct(days_before_game),
    .groups = "drop"
  ) %>%
  group_by(season_year, tier_group) %>%
  mutate(pct = avg_seats / sum(avg_seats)) %>%
  ungroup() %>%
  mutate(season_year = as.factor(season_year))

# --- Plot: Seats Sold by Tier Magnitude ---
seats_sold_by_tier_magnitude <- ggplot(single_bar_ytd_tier,
                                        aes(x = day_bucket, y = avg_seats, fill = season_year)) +
  geom_col(position = "dodge") +
  facet_wrap(~ tier_group) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Single Game Seats Sold per Day by Day Bucket (YTD)",
    subtitle = "Avg seats sold by tier group, 2023–2026",
    x = NULL, y = "Avg Seats Sold", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(graphs_dir, "seats_sold_by_tier_magnitude.png"),
       plot = seats_sold_by_tier_magnitude,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Conversion Rates — Transactions per Primary Site Hit
# =============================================================================
# Measures how efficiently site visits turn into purchases.

conversion_primary <- primary_bar_summary %>%
  inner_join(single_bar_summary, by = c("season_year", "day_bucket")) %>%
  mutate(transactions_per_hit = avg_transactions / avg_hits)

conversion_rates_primary <- ggplot(conversion_primary,
                                    aes(x = day_bucket, y = transactions_per_hit, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Single Game Transactions per Primary Site Hit (YTD)",
    subtitle = "Conversion efficiency by day bucket, 2023–2026",
    x = NULL, y = "Avg Transactions per Hit", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(graphs_dir, "conversion_rates_primary.png"),
       plot = conversion_rates_primary,
       width = 10, height = 6, dpi = 300)

# --- Conversion rates by tier ---
conversion_primary_tier <- primary_bar_summary_tier %>%
  inner_join(single_bar_ytd_tier, by = c("season_year", "tier_group", "day_bucket")) %>%
  mutate(transactions_per_hit = avg_transactions / avg_hits)

conversion_rates_by_tier <- ggplot(conversion_primary_tier,
                                    aes(x = day_bucket, y = transactions_per_hit, fill = season_year)) +
  geom_col(position = "dodge") +
  facet_wrap(~ tier_group) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2023" = "#378ADD", "2024" = "#1D9E75",
                               "2025" = "#D85A30", "2026" = "#7F77DD")) +
  labs(
    title = "Single Game Transactions per Primary Site Hit by Tier Group (YTD)",
    subtitle = "Conversion efficiency by day bucket, 2023–2026",
    x = NULL, y = "Transactions per Hit", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(graphs_dir, "conversion_rates_by_tier.png"),
       plot = conversion_rates_by_tier,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Cumulative % of Tickets Sold — 2025 vs 2026 (First ~21 Games)
# =============================================================================
# Shows what fraction of single-game seats were sold within shrinking time
# windows before game day. Nearly identical curves = stable timing behavior.

first_28_buckets <- single_sales %>%
  filter(
    (season_year == 2025 & as.Date(event_date) <= as.Date("2025-05-13")) |
    (season_year == 2026 & as.Date(event_date) <= as.Date("2026-05-14")),
    days_before_game >= 0
  ) %>%
  group_by(season_year) %>%
  summarise(
    total_seats   = sum(num_seats[days_before_game <= 27], na.rm = TRUE),
    last_28_days  = sum(num_seats[days_before_game <= 27], na.rm = TRUE),
    last_21_days  = sum(num_seats[days_before_game <= 20], na.rm = TRUE),
    last_14_days  = sum(num_seats[days_before_game <= 13], na.rm = TRUE),
    last_7_days   = sum(num_seats[days_before_game <= 6],  na.rm = TRUE),
    last_3_days   = sum(num_seats[days_before_game <= 2],  na.rm = TRUE),
    day_of        = sum(num_seats[days_before_game == 0],  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_last_28 = last_28_days / total_seats,
    pct_last_21 = last_21_days / total_seats,
    pct_last_14 = last_14_days / total_seats,
    pct_last_7  = last_7_days  / total_seats,
    pct_last_3  = last_3_days  / total_seats,
    pct_day_of  = day_of       / total_seats
  ) %>%
  select(season_year, pct_last_28, pct_last_21, pct_last_14,
         pct_last_7, pct_last_3, pct_day_of) %>%
  pivot_longer(cols = starts_with("pct"), names_to = "bucket", values_to = "pct") %>%
  mutate(
    bucket = case_when(
      bucket == "pct_last_28" ~ "Within 28 days",
      bucket == "pct_last_21" ~ "Within 21 days",
      bucket == "pct_last_14" ~ "Within 14 days",
      bucket == "pct_last_7"  ~ "Within 7 days",
      bucket == "pct_last_3"  ~ "Within 3 days",
      bucket == "pct_day_of"  ~ "Day of"
    ),
    bucket = factor(bucket, levels = c(
      "Within 28 days", "Within 21 days", "Within 14 days",
      "Within 7 days", "Within 3 days", "Day of"
    )),
    season_year = as.factor(season_year)
  )

ticket_pct_last_28 <- ggplot(first_28_buckets,
                               aes(x = bucket, y = pct, fill = season_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("2026" = "#203cc6ff", "2025" = "#7F77DD")) +
  labs(
    title = "Cumulative % of Tickets Sold by Time Window — First 21 Games",
    subtitle = "2025 (through 5/13) vs 2026 (through 5/14)",
    x = NULL, y = "% of Total Seats Sold", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "ticket_pct_last_28.png"),
       plot = ticket_pct_last_28,
       width = 10, height = 6, dpi = 300)


# =============================================================================
# Density Plot — Purchase Lead-Time Distribution (2025 vs 2026)
# =============================================================================
# Weighted by seats sold. Dashed lines show the day by which 50% of all seats
# had been purchased.

weighted_median <- function(x, w) {
  ord <- order(x)
  x_sorted <- x[ord]
  w_sorted <- w[ord]
  cumw <- cumsum(w_sorted) / sum(w_sorted)
  x_sorted[which(cumw >= 0.5)[1]]
}

lead_time_sales <- single_sales %>%
  filter(
    (season_year == 2025 & as.Date(event_date) <= as.Date("2025-05-13")) |
    (season_year == 2026 & as.Date(event_date) <= as.Date("2026-05-14")),
    days_before_game >= 0
  ) %>%
  mutate(season_year = as.factor(season_year))

medians_sales <- lead_time_sales %>%
  group_by(season_year) %>%
  summarise(median_days = weighted_median(days_before_game, num_seats),
            .groups = "drop")

sales_dens_plot <- ggplot(lead_time_sales,
                           aes(x = days_before_game, weight = num_seats,
                               color = season_year, fill = season_year)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = medians_sales,
             aes(xintercept = median_days, color = season_year),
             linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = c("2025" = "#7B2D8B", "2026" = "#00AFBB")) +
  scale_fill_manual(values  = c("2025" = "#7B2D8B", "2026" = "#00AFBB")) +
  labs(
    title = "Distribution of Purchase Lead Time — Single Game Sales",
    subtitle = "Weighted by seats sold. Dashed lines = day by which 50% of seats sold. 2025 vs 2026.",
    x = "Days Before Game", y = "Density", color = "Season", fill = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(graphs_dir, "sales_dens_plot.png"),
       plot = sales_dens_plot,
       width = 10, height = 6, dpi = 300)


cat("\nAll plots saved to", graphs_dir, "\n")
