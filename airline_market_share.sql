-- airline_market_share.sql
SELECT
  carrier,
  EXTRACT(YEAR FROM quarter) AS year,
  SUM(revenue_million) AS total_revenue,
  ROUND(SUM(revenue_million) * 100.0 / SUM(SUM(revenue_million)) OVER (PARTITION BY EXTRACT(YEAR FROM quarter)), 2) AS market_share_pct
FROM airline_revenue
GROUP BY carrier, year
ORDER BY year, market_share_pct DESC;