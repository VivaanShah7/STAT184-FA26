library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)
# PROBLEM 1
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# PART A
sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

sales_clean |>
  select(sale_id, item_clean, amount,
         discount_rate, net_amount, size) |>
  print(n = Inf)

# str_squish removes extra spaces.
# str_to_lower makes item names lowercase.
# parse_number converts text into numeric values.
# net_amount is the amount after the discount.
# Missing net amounts are unrecorded.
# Net amounts >= 20 are large; others are small.

# PART B

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(
      code_clean, "^(ST|ON)-[0-9]{3}$"
    )
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)

print(invalid_codes)

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )

valid_code_parts |>
  select(sale_id, code_raw, channel, number) |>
  print(n = Inf)

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")

print(sale_tags, n = Inf)
# separate_wider_delim splits one column into
# multiple columns, while separate_longer_delim
# splits values into multiple rows.
# Invalid codes are sale IDs 5 and 7.
# PART C

customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(
      known_n > 0,
      large_n / known_n,
      NA_real_
    ),
    .by = customer_id
  ) |>
  arrange(customer_id)

print(customer_summary, n = Inf)

# For C1, the numerator is 0 large sales.
# The denominator is 1 known sale.
# large_share 0/1 = 0.

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)
levels(sales_factors$item_group)

sales_factors |> count(item_factor) |> print(n = Inf)
sales_factors |> count(item_group) |> print(n = Inf)
# fct_infreq changes the order of factor levels
# based on frequency without combining categories.
# fct_lump_n combines less frequent categories into Other.
# PROBLEM 2

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# PART A

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round,
         primary_amount, backup_amount, amount) |>
  print(n = Inf)

# Missing customer IDs are filled within each form.
# -999 is treated as a missing primary amount.
# coalesce uses the backup only if primary is NA.

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )

print(reports_complete, n = Inf)

original_rows <- nrow(reports_clean)
added_rows <- sum(!reports_complete$recorded_row)
missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))

print(original_rows)
print(added_rows)
print(missing_before)
print(missing_after)

# Explicit missing: C3, round 1.
# Implicit missing: C1, round 2.


# PART B

# Check for missing customer IDs.

missing_ids <- customers |>
  filter(is.na(customer_id))

print(missing_ids)

# Check for duplicate customer IDs.

duplicate_ids <- customers |>
  count(customer_id) |>
  filter(n > 1)

print(duplicate_ids)

# Check repeated customer-round combinations.

duplicate_rounds <- reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)

print(duplicate_rounds)

# Results:
# No missing IDs in customers.
# No duplicate IDs in customers.
# No repeated customer-round combinations.

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )

reports_joined |>
  select(customer_id, round, amount,
         region, customer_found) |>
  print(n = Inf)

# IDs only in reports.

only_reports <- reports_clean |>
  distinct(customer_id) |>
  anti_join(
    customers,
    by = join_by(customer_id),
    na_matches = "never"
  )

print(only_reports)

# IDs only in customers.

only_customers <- customers |>
  distinct(customer_id) |>
  anti_join(
    reports_clean,
    by = join_by(customer_id),
    na_matches = "never"
  )

print(only_customers)

# C3 has customer_found TRUE because its ID exists
# in customers, even though its region is NA.
# C4 has customer_found FALSE because its ID
# does not exist in customers.


# PART C

# NOTE:
# customers_bad is mentioned  but not defined  so i created an example data

customers_bad <- bind_rows(
  customers,
  tibble(customer_id = "C2", region = "East")
)

reports_duplicated <- reports_clean |>
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )

# Compare number of rows.

nrow(reports_clean)
nrow(reports_duplicated)

# Compare total amounts.

sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)

# Find repeated reports.

reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1) |>
  print(n = Inf)

# Results for this example:
# Before join: 6 rows, total = 75
# After join: 7 rows, total = 95
# C2, round 1 appears twice.
#
# The duplicated C2 report adds another 20
# to the total.
# A many-to-one relationship would produce an error.
# Allowing many-to-many does not fix conflicting data.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

print(reports_wide, n = Inf)

reports_long <- reports_wide |>
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    values_to = "amount",
    values_drop_na = TRUE
  )

print(reports_long, n = Inf)
nrow(reports_long)

# Dropping NA removes rows with missing amounts.
# It does not recover information that was
# never recorded.

#Problem 3

# Part A

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {

  nx <- length(x)
  ny <- length(y)

  mean_x <- mean(x)
  mean_y <- mean(y)

  var_x <- var(x)
  var_y <- var(y)

  if (equal_var == TRUE) {

    pooled_var <- ((nx - 1) * var_x +
                   (ny - 1) * var_y) / (nx + ny - 2)

    SE <- sqrt(pooled_var * (1/nx + 1/ny))

    df <- nx + ny - 2

    method <- "Pooled"

  } else {

    SE <- sqrt(var_x/nx + var_y/ny)

    df <- (var_x/nx + var_y/ny)^2 /
      ((var_x/nx)^2 / (nx - 1) +
       (var_y/ny)^2 / (ny - 1))

    method <- "Welch"
  }

  t_stat <- (mean_x - mean_y) / SE

  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )

  reject <- p_value < alpha

  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}

# Test the function using the provided data.

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)

pooled_result <- two_sample_t(
  x_check, y_check, equal_var = TRUE
)

welch_result
pooled_result



welch_reference <- t.test(
  x_check, y_check, var.equal = FALSE
)

pooled_reference <- t.test(
  x_check, y_check, var.equal = TRUE
)

welch_reference
pooled_reference

# Check whether differences are less than 1e-8.

welch_check <- c(
  t_stat = abs(welch_result$t_stat -
                 unname(welch_reference$statistic)) < 1e-8,
  df = abs(welch_result$df -
             unname(welch_reference$parameter)) < 1e-8,
  p_value = abs(welch_result$p_value -
                  welch_reference$p.value) < 1e-8
)

pooled_check <- c(
  t_stat = abs(pooled_result$t_stat -
                 unname(pooled_reference$statistic)) < 1e-8,
  df = abs(pooled_result$df -
             unname(pooled_reference$parameter)) < 1e-8,
  p_value = abs(pooled_result$p_value -
                  pooled_reference$p.value) < 1e-8
)

welch_check
pooled_check

# The function calculates either Welch's or the
# pooled two-sample t-test using the given formulas.
# The verification checks should all return TRUE,  showing that the differences are below 1e-8.


# Part B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Set the seed once before the simulation.

set.seed(2026)

# Allocate storage in advance.

results_list <- vector(
  "list",
  nrow(scenarios) * B * 2
)

index <- 1

# Run simulations using nested for loops.

for (i in 1:nrow(scenarios)) {

  for (j in 1:B) {

    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )

    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )

    welch <- two_sample_t(
      x, y, equal_var = FALSE, alpha = alpha
    )

    pooled <- two_sample_t(
      x, y, equal_var = TRUE, alpha = alpha
    )

    results_list[[index]] <- data.frame(
      scenario = scenarios$scenario[i],
      replicate = j,
      method = welch$method,
      t_stat = welch$t_stat,
      df = welch$df,
      p_value = welch$p_value,
      reject = welch$reject
    )

    index <- index + 1

    results_list[[index]] <- data.frame(
      scenario = scenarios$scenario[i],
      replicate = j,
      method = pooled$method,
      t_stat = pooled$t_stat,
      df = pooled$df,
      p_value = pooled$p_value,
      reject = pooled$reject
    )

    index <- index + 1
  }
}

# Combine the results into one data frame.

simulation_results <- bind_rows(results_list)

# Display first six rows and total row count.

head(simulation_results, 6)

nrow(simulation_results)

# Expected row count: 8000


# Summarize simulation results.

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(
    across(
      c(mean_t, sd_t, rejection_rate),
      ~ round(.x, 3)
    )
  )

simulation_summary

# The summary contains 8 rows:
# 4 scenarios multiplied by 2 methods.


# Short Report:
# When the means are equal, the rejection rate
# should be close to 0.05 because there is no real difference.
# When the means are different, the tests are more likely
# to reject the null hypothesis.
# Welch's test works better when the variances are unequal.
# A higher rejection rate is not always better because
# it could also mean the test makes more false positives.