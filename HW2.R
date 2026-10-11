# Install Packages
library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# Problem 1: Clean, Validate, and Summarize
# Problem 1 SETUP
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ", "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00", "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104", "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member", "online;member", "gift", "member", "gift;member")
)

# Problem 1: Part A
# Create sales_clean from sales_row, keeping original cols & rows
# Create a new col, item_clean, from item.
sales_clean <- sales_raw

# Remove spaces at end/begin, lowercase all letters, reduce extra spaces between words to single.
sales_clean$items_clean<- 
  str_to_lower(str_squish(sales_clean$item_raw))

# Create amount and discount_rate using parse_number().
# Store discount as a proportion.
# net_amount = amount * (1 - discount_rate).
# Preserve missing amounts.
sales_clean <- sales_clean |> mutate(
  amount = parse_number(amount_raw),
  discount = parse_number(discount_raw) / 100,
  net_amount = amount * (1 - discount_rate)
)

# Use case_when() to create size
# unrecorded = NA, large = >=20, else small
sales_clean <- sales_clean |> mutate(size = case_when(
  is.na(net_amount) ~ "unrecorded",
  net_amount >= 20 ~ "large",
  .default = "small"
))

# Display sale_id, items_clean, amount,
# discount, net_amount, size
sales_clean[c
  ("sale_id", "items_clean", "amount", "discount", "net_amount", "size")
  ]

# Problem 1: Part B
# Create sales_codes from sales_clean
# Remove end space and capitalize code_raw
# Create valid_code: TRUE for codes with only ST & ON followed by -
# and exactly three digits after it; FALSE otherwise
sales_codes <- sales_clean |> mutate(
  code_clean = str_to_upper(str_squish(code_raw))
)

code_pattern <- "^(ST|ON)-[0-9]{3}$"
sales_codes <- sales_codes |> mutate(
  valid_code = str_detect(code_clean, code_pattern))

# Save and display invalid_rows as invalid_rows
# Show only sale_id, code_raw, code_clean
invalid_codes <- sales_codes |> filter(
  valid_code == FALSE
)

invalid_codes <- invalid_codes |> select(
  sale_id, code_raw, code_clean
)

# Create valid_code_parts from valid rows.
# Use separate_wider_delim() to split code_clean into channel and number.
# Do not delete invalid rows from code_clean
valid_code_parts <- sales_codes |> separate_wider_delim(
  code_clean, delim = "-", names = c("channel", "number"), too_few = "align_start"
)

valid_code_parts <- valid_code_parts |> select(
  sale_id, code_raw, channel, number
)

# Create sale_tags from sale_id and tags in sales_clean.
# Use separate_longer_delim() with ";"
sale_tags <- sales_clean |> 
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")

# Explain the difference between separate_longer_delim() and separate_wider_delim().
# Separate_longer_delim() creates new rows and does not require names.
# Separate_wider_delim() creates new columns and need names.

# Problem 1: Part C (Use sales_clean)
# Create customer_summary using summarise() and .by = customer_id.
# Report sales_n, known_n, large_n (>=20), large_share
# For no known amounts: large_n = 0, large_share = NA_real_.
# Display and sort by customer ID
customer_summary <- sales_clean |> summarise(
  sales_n = n(),
  known_n = sum(!is.na(net_amount)), 
  large_n = sum(net_amount >=20, na.rm = TRUE),
  large_share = large_n / known_n,
  .by = customer_id
  
)

#ifelse(condition, if True return, if False return)
customer_summary <- customer_summary |> mutate(
  large_n = ifelse(known_n == 0 | is.na(known_n), 0, large_n),
  large_share = ifelse(known_n == 0 | is.na(known_n), NA_real_, large_share)
)

customer_summary

# State and clarify the numerator and denomator for
# C1's large_share
numerator <- customer_summary[1, 4] # large_n for C1
denominator <- customer_summary[1, 3] # known_n for C1

# Create sales_factors with
# item_factor = fct_infreq(factor(items_clean))
# item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
# Display and explain the difference.]
item_factor <- fct_infreq(factor(sales_clean$items_clean))
item_group <- fct_lump_n(factor(sales_clean$items_clean), n = 2, other_level = "Other")
sales_factors <- sales_clean |> mutate(
  item_factor = item_factor,
  item_group = item_group
)

levels(item_factor)
levels(item_group)
count(sales_factors, item_factor)
count(sales_factors, item_group)
# changing level order only reorders the levels.
# Combining categories, merges/lumps things together and renames 
# them like "Other" consists of eraser and notebook (objects with less than 2 count).

# Problem 2: Setup
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10,-999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Problem 2: Part A
# Create reports_labeled by sorting form and line.
# Use fill() and ungroup(). 
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

# Create reports_clean
# Use na_if() to create primary_clean, -999 -> NA
# Use coalesce() to create primary_account with primary_clean and backup_amount
# Display form, line, customer_id, round, primary_amount, backup_amount, and amount
# Explain why a primary value is kept (include 0) instead of backing up.
reports_clean <- reports_labeled |> mutate(
  primary_clean = na_if(primary_amount, -999),
  amount = coalesce(primary_clean, backup_amount)
)

select(reports_clean, form, line, customer_id, round,
       primary_amount, backup_amount, amount)
# The primary value is kept including zero because it only replaces NA, so if 
# it is not NA, it does not need to be changed to its backup.

# reports_clean Keep customer_id, round, and amount.
# Use complete() to create reports_complete row, rounds 1:3
# Add recorded row, True for original rows, False for new rows
# Keep unknown and newly added amount as NA
reports_clean <- reports_clean[, c("customer_id", "round", "amount")]
reports_complete <- reports_clean |> 
  mutate(recorded_rows = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_rows = FALSE))

# count rows
reports_complete |> summarise(
  og_rows = sum(recorded_rows == TRUE),
  before_missing = sum(is.na(amount) & recorded_rows == TRUE),
  added_rows = sum(recorded_rows == FALSE),
  after_missing = sum(is.na(amount))
)
# Identify one explicit and one implicit missing amount
explicit <- reports_complete[7, ]
implicit <- reports_complete[2, ]

# Problem 2: Part B (Use reports_clean)
# Check customers$customer_id for duplicate ID or missing.
# Check if there are dulplicate customer_id-rounds in reports_clean
sum(is.na(customers$customer_id))
customers |> count(customer_id) |> filter(n > 1)
reports_clean |> count(customer_id, round) |> filter(n > 1)

# Add customer_found = TRUE to customers. Save as customers_marked
# reports_joined = left_join above table to reports_clean
# Use join_by(), relationship = "many-to-one", na_matches = "never"
# Display customer_id, round, amount, region, and customer_found.
customers_marked <- customers |> mutate(
  customer_found = TRUE
)
reports_joined <- reports_clean |> 
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = if_else(
    !is.na(customer_found), TRUE, FALSE
  ))
reports_joined |> select(customer_id, round, amount, region, customer_found)

# Use anti_join() in both directions with na_matches = "never"
# in reports_clean and customers
anti_report <- reports_clean |> anti_join(customers,
  by = join_by(customer_id),
  na_matches = "never")

anti_customer <- customers |> anti_join(reports_clean,
  by = join_by(customer_id),
  na_matches = "never")
# customers_found points out whether a customer is in the customer table
# For C3, though the region is NA, customers_found is TRUE meaning it is in customer.
# For C4, the region is NA, and customers_found is False meaning it is not in customer.

# Problem 2: Part C
# reports_dulplicated <- left_join reports_clean to customers_bad on customer_id
# relationship = "many-to-many"

# Couldn't find customer_bad, so created customer_bad from module 6 example
customers_bad <- bind_rows(customers, tibble(customer_id = "C2", region = "East"))
reports_dulplicated <- reports_clean |> left_join(
  customers_bad, # couldn't find customer_bad
  by = join_by(customer_id),
  relationship = "many-to-many"
)
# Compare rows 6 | 7
nrow(reports_clean)
nrow(reports_dulplicated)

# Compare total amount 75 | 95
sum(reports_clean$amount, na.rm = TRUE) 
sum(reports_dulplicated$amount, na.rm = TRUE)

# Identify dulplicated customer_id and round
reports_dulplicated |> count(customer_id, round) |> filter(n > 1)

# Relationship = "many-to-one" causes an error.
# Using many-to-many does not fix the conflicting customers problem, but
# it allows it to pass without error.

# Create reports_wide, use pivot_wider
# from reports_complete keep only customer_id, round, and amount
reports_wide <- reports_complete |> select(
  customer_id, round, amount) |>
  pivot_wider(names_from = round,
              values_from = amount,
              names_prefix = "round_")
# Prediction: 5 rows will remain because there is 5 non NA column values
reports_wide |> pivot_longer(
  starts_with("round_"),
  names_to = "round",
  values_to = "amount",
  values_drop_na = TRUE
)
# Dropping the missing values do not recover the unknown amounts because those
# amounts were only created by pivot_wider to fill the columns. 
# Pivoting back to longer, will drop those NA amounts.

# Problem 3: Two-sample t-tests and simulation
# Problem 3: Part A setup
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  # Your function body here.
  n_x = length(x) # x sample size
  n_y = length(y) # y sample size
  x_ = mean(x) # mean of x
  y_ = mean(y) # mean of y
  s_x = var(x) # sample variation of x
  s_y = var(y) # sample variation of y
  
  if (equal_var == TRUE)
  {
    s_2p <- ((n_x - 1)*s_x + (n_y - 1)*s_y) / (n_x + n_y - 2)
    se <- sqrt( s_2p*( (1/n_x) + (1/n_y) ) )
    v <- n_x + n_y - 2
    t <- (x_ - y_) / se
    p2 <- 2 * pt(abs(t), df = v, lower.tail = FALSE)
    method <- "Pooled"
  }
  else
  { sn_x <- s_x / n_x
    sn_y <- s_y / n_y
    se <- sqrt(sn_x + sn_y)
    v <-  (sn_x + sn_y)**2 / ( ((sn_x**2)/(n_x - 1)) + ((sn_y**2) / (n_y - 1)) )
    t <- (x_ - y_) / se
    p2 <- 2 * pt(abs(t), df = v, lower.tail = FALSE)
    method <- "Welch"
  }
  reject <- p2 < alpha
  out <- list(method = method,
              t_stat = t,
              df = v,
              p_value = p2,
              reject = reject)
  return(out)
}
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)
# Reference calculations; compare with your own function.
t.test(x_check, y_check, var.equal = FALSE)
# t = 3.4031, df = 7.6786, p = 0.009907
t.test(x_check, y_check, var.equal = TRUE)
# t = 3.5264, df = 11, p = 0.004744

two_sample_t(x_check, y_check, equal_var = FALSE, alpha = 0.05)
# t = 3.403072, df = 7.678614, p = 0.009907123
two_sample_t(x_check, y_check, equal_var = TRUE, alpha = 0.05)
# t = 3.526436, df = 11, p = 0.00474447

# the difference is smaller than 1e-8

# Problem 3: Part 3
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
# Run once immediately before the simulation, not inside a loop.
set.seed(2026)

n_rows <- nrow(scenarios) * B * 2

simulation_results <- data.frame(
  scenario = character(n_rows),
  replicate = integer(n_rows),
  method = character(n_rows),
  t_stat = numeric(n_rows),
  df = numeric(n_rows),
  p_value = numeric(n_rows),
  reject = logical(n_rows),
  stringsAsFactors = FALSE
  
)

row_index <- 1

for (i in seq_len(nrow(scenarios))) {
  
  for (b in 1:B) {
    x <- rnorm(n_x, mean = scenarios$mean_x[i],sd = scenarios$sd_x[i])
    y <- rnorm(n_y,mean = scenarios$mean_y[i], sd = scenarios$sd_y[i])
    welched <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      welched$method,
      welched$t_stat,
      welched$df,
      welched$p_value,
      welched$reject
    )
    
    row_index <- row_index + 1
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      pooled$method,
      pooled$t_stat,
      pooled$df,
      pooled$p_value,
      pooled$reject
    )
    
    row_index <- row_index + 1
  }
}

head(simulation_results)
nrow(simulation_results)

# Summarize results
simulation_summary <- simulation_results |> group_by(scenario, method) |>
  summarise(
    mean_t = round(mean(t_stat), 3),
    sd_t = round(sd(t_stat), 3),
    rejection_rate = round(sum(reject == "TRUE") / n(), 3),
    .groups = "drop"
) 

# Report
# The two with null rejection rate of about 0.05 are equal_null-Pooled and equal_null-Welched.
# They have close mean and sd.
# Type I error is a false positive. Power is the probability of correctly rejecting a false null hypothesis.
# Power alone is not enough because you need both to achieve a balanced and reliable test.