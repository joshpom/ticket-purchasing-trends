-- =============================================================================
-- Sample BigQuery Queries — Ticket Purchasing Trends Analysis
-- =============================================================================
-- These queries were originally run against Google BigQuery to source the data
-- for this analysis. Project and table names have been generalized.
-- The synthetic data in data/ was generated to match the patterns these queries
-- produced.
-- =============================================================================


-- -----------------------------------------------------------------------------
-- Query 1: Primary Site Traffic Joined with Single-Game Ticket Sales
-- -----------------------------------------------------------------------------
-- Pulls daily clickstream traffic on Ticketmaster event detail pages and joins
-- it with same-day ticket sales for each game. This powers the site traffic
-- magnitude/percent charts and the conversion rate analysis.

WITH events_multiyear AS (
    -- Current season events
    SELECT
        season_id, event_date, event_name, season_year, game_number
    FROM `team-data-prod.Ticketing_Views.v_event`
    WHERE season_id = <current_season_id>
      AND LEFT(event_name, 4) IN (<current_season_event_prefixes>)

    UNION ALL

    -- Prior season events from archive
    SELECT
        season_id, event_date, event_name, season_year, game_number
    FROM `team-data-prod.Ticketing_Archive.event`
    WHERE season_id IN (<prior_season_ids>)  -- 2023, 2024, 2025
      AND LEFT(event_name, 4) IN (<prior_season_event_prefixes>)
),

traffic AS (
    -- Aggregate daily clickstream hits per game
    -- Joins clickstream data to events by parsing the event date from the
    -- product_list field on Ticketmaster event detail page views
    SELECT
        w.batch_date,
        e.season_year,
        e.game_number,
        e.season_id,
        e.event_name,
        e.event_date,
        COUNT(DISTINCT visitor_id)  AS visitors,
        COUNT(DISTINCT visit_id)    AS visits,
        COUNT(DISTINCT hit_id)      AS hits
    FROM `team-data-prod.clickstream.event_detail_pages` w
    LEFT JOIN events_multiyear e
        ON SAFE.PARSE_DATE(
               '%m/%d/%Y',
               REGEXP_EXTRACT(w.product_list, r'(\d{1,2}/\d{1,2}/\d{4})')
           ) = e.event_date
    WHERE w.pagename = 'Ticketmaster: Event Detail'
      AND w.product_list NOT LIKE '%Parking%'
      AND e.season_year IS NOT NULL
    GROUP BY w.batch_date, e.season_year, e.season_id,
             e.event_name, e.event_date, e.game_number
),

sales AS (
    -- Daily ticket sales aggregated by game
    -- Current season from live tables
    SELECT
        t.add_date,
        e.event_date,
        COUNT(DISTINCT acct_id)      AS unique_buyers,
        COUNT(*)                     AS transactions,
        SUM(num_seats)               AS num_seats,
        SUM(block_purchase_price)    AS block_purchase_price
    FROM `team-data-prod.Ticketing.ticket` t
    LEFT JOIN `team-data-prod.Ticketing_Views.v_event` e
        ON t.event_name = e.event_name
       AND t.season_id  = e.season_id
    WHERE t.season_id IN (<current_season_id>)
      AND e.season_id IN (<current_season_id>)
      AND t.add_usr LIKE 'Z%'          -- Filter to online/self-service sales
      AND LEFT(t.event_name, 4) IN (<current_season_event_prefixes>)
    GROUP BY t.add_date, e.event_date

    UNION ALL

    -- Prior seasons from archive
    SELECT
        t.add_date,
        e.event_date,
        COUNT(DISTINCT acct_id)      AS unique_buyers,
        COUNT(*)                     AS transactions,
        SUM(t.num_seats)             AS num_seats,
        SUM(t.block_purchase_price)  AS block_purchase_price
    FROM `team-data-prod.Ticketing_Archive.ticket` t
    LEFT JOIN `team-data-prod.Ticketing_Archive.event` e
        ON t.event_name = e.event_name
       AND t.season_id  = e.season_id
    WHERE t.season_id IN (<prior_season_ids>)
      AND e.season_id IN (<prior_season_ids>)
      AND t.add_usr LIKE 'Z%'
      AND LEFT(t.event_name, 4) IN (<prior_season_event_prefixes>)
    GROUP BY t.add_date, e.event_date
)

-- Final join: traffic LEFT JOIN sales on matching date + game
-- Gives one row per (batch_date, game) with both traffic and sales metrics
SELECT
    t.batch_date,
    t.season_year,
    t.game_number,
    t.season_id,
    t.event_name,
    t.event_date,
    t.visitors,
    t.visits,
    t.hits,
    IFNULL(s.unique_buyers, 0)       AS unique_buyers,
    IFNULL(s.transactions, 0)        AS transactions,
    IFNULL(s.num_seats, 0)           AS num_seats,
    IFNULL(s.block_purchase_price, 0) AS block_purchase_price
FROM traffic t
LEFT JOIN sales s
    ON t.batch_date = s.add_date
   AND t.event_date = s.event_date;


-- -----------------------------------------------------------------------------
-- Query 2: Single-Game Ticket Sales (Current Season)
-- -----------------------------------------------------------------------------
-- Pulls transaction-level ticket sales for the current season, grouped by
-- purchase date and game. Used for the seats-sold analysis and density plots.

SELECT
    t.add_date,
    t.season_id,
    t.event_name,
    e.event_date,
    COUNT(DISTINCT acct_id)      AS unique_buyers,
    COUNT(*)                     AS transactions,
    SUM(num_seats)               AS num_seats,
    SUM(block_purchase_price)    AS block_purchase_price
FROM `team-data-prod.Ticketing.ticket` t
LEFT JOIN `team-data-prod.Ticketing_Views.v_event` e
    ON t.event_name = e.event_name
   AND t.season_id  = e.season_id
WHERE t.season_id IN (<current_season_ids>)         -- Includes sub-seasons
  AND e.season_id IN (<current_season_ids>)
  AND t.add_date > '<first_presale_date>'            -- First presale for the season
  AND t.add_usr LIKE 'Z%'                            -- Online/self-service sales only
GROUP BY t.add_date, t.season_id, t.event_name, e.event_date;
