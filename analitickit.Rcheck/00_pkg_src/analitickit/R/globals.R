# Suppress R CMD check NOTES for dplyr/tidyeval non-standard evaluation.
utils::globalVariables(c(
  ".data",
  ".env",
  ":=",
  "absolute_growth",
  "average_sales",
  "first_sales",
  "last_sales",
  "promoted_sales",
  "sales_sd",
  "total_sales"
))
