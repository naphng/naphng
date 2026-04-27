# ================================================================
# PART 1: LIBRARIES & HELPER FUNCTIONS
# ================================================================
library(readxl)
library(MASS)
library(dplyr)
# Explicitly assign select and filter to dplyr to prevent masking:
select <- dplyr::select
filter <- dplyr::filter
library(lubridate)
library(zoo)
library(tidyr)
library(writexl)
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(moments)   
library(tseries)   
library(ggplot2)   
library(corrplot)  

options(scipen = 999)

# 1. Winsorize Function
winsorize_cross_section <- function(x, lower = 0.01, upper = 0.99) {
  if(all(is.na(x))) return(x)
  qnt <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x <- ifelse(x < qnt[1], qnt[1], x)
  x <- ifelse(x > qnt[2], qnt[2], x)
  return(x)
}

# 2. Condition Number (Kappa) Calculation Function
compute_kappa <- function(fit, model_name) {
  X <- model.matrix(fit)
  X <- X[, -1]  # Remove intercept
  sv <- svd(scale(X))$d
  kappa <- max(sv) / min(sv)
  cat(sprintf("%-20s : κ = %.2f\n", model_name, kappa))
  invisible(kappa)
}

# 3. GRS Test Function (Added to execute Part 5)
grs_test_safe <- function(ret_matrix, factor_matrix) {
  T <- nrow(ret_matrix)
  N <- ncol(ret_matrix)
  K <- ncol(factor_matrix)
  
  fit <- lm(ret_matrix ~ factor_matrix)
  alpha <- coef(fit)[1, ]
  res <- residuals(fit)
  
  Sigma <- cov(res) * (T - 1) / T
  Omega <- cov(factor_matrix) * (T - 1) / T
  mu_f <- colMeans(factor_matrix)
  
  inv_Sigma <- solve(Sigma)
  inv_Omega <- solve(Omega)
  
  grs_stat <- (T / N) * ((T - N - K) / (T - K - 1)) * as.numeric(t(alpha) %*% inv_Sigma %*% alpha) / 
    as.numeric(1 + t(mu_f) %*% inv_Omega %*% mu_f)
  
  p_val <- 1 - pf(grs_stat, N, T - N - K)
  return(c(grs_stat, p_val))
}

# ================================================================
# PART 2: DATA LOADING AND PREPROCESSING
# ================================================================

# ---- 2.1 Market Data (Rm, Rf, MKT) ----
vnindex <- read_excel("vnindex_2018_2026_Fixed.xlsx", sheet = "Sheet1", col_names = TRUE) %>%
  setNames(c("date", "close")) %>%
  mutate(date = floor_date(as.Date(date), "month"), close = as.numeric(close)) %>%
  arrange(date) %>% filter(!is.na(close))

bond1y <- read.csv("TPCP_1Y_12.17_1.26.csv", fileEncoding = "UTF-8-BOM", 
                   stringsAsFactors = FALSE, check.names = FALSE) %>%
  setNames(c("date_str", "yield_close", "yield_open", "yield_high", "yield_low", "pct_chg")) %>%
  mutate(date = floor_date(dmy(date_str), "month"), yield_1y = as.numeric(yield_close), Rf = yield_1y / 100 / 12) %>%
  arrange(date) %>% dplyr::select(date, yield_1y, Rf) %>% filter(!is.na(Rf))

mkt_df <- inner_join(vnindex, bond1y, by = "date") %>% arrange(date) %>%
  mutate(Rm = (close / lag(close)) - 1, MKT = Rm - Rf) %>%
  filter(!is.na(MKT)) %>% select(date, Rm, Rf, yield_1y, MKT)

# ---- 2.2 Stock & Financial Data ----
price_raw <- read_excel("R_nonfin_CLEANED_FILTERED.xlsx", sheet = "Price_Clean") %>%
  setNames(c("ticker", "date", "price", "me")) %>%
  mutate(date = floor_date(as.Date(date), "month"), price = as.numeric(price), me = as.numeric(me)) %>%
  filter(!is.na(price), !is.na(me), me > 0) %>% arrange(ticker, date)

fin_raw <- read_excel("R_nonfin_CLEANED_FILTERED.xlsx", sheet = "Finance_Clean") %>%
  setNames(c("ticker", "date", "equity", "total_assets", "net_income")) %>%
  mutate(date = floor_date(as.Date(date), "month"), equity = as.numeric(equity)) %>%
  filter(!is.na(equity)) %>% arrange(ticker, date)

all_dates   <- sort(unique(price_raw$date))
all_tickers <- unique(price_raw$ticker)
full_grid <- expand.grid(ticker = all_tickers, date = all_dates, stringsAsFactors = FALSE) %>% as_tibble()

fin_monthly <- fin_raw %>% select(ticker, date, equity) %>%
  mutate(date = date + months(3)) %>%            
  right_join(full_grid, by = c("ticker", "date")) %>%
  arrange(ticker, date) %>% group_by(ticker) %>%
  fill(equity, .direction = "down") %>% ungroup()

# ---- 2.3 Calculate Returns, Weights, and Momentum Signal ----
stocks <- price_raw %>%
  left_join(fin_monthly, by = c("ticker", "date")) %>%
  filter(!is.na(equity), equity > 0) %>%
  mutate(bm = equity / me) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(
    ret = (price / lag(price, 1)) - 1,
    lag_me = lag(me, 1), 
    mom_signal = lag(rollapply(1 + ret, width = 11, FUN = function(x) {
      valid <- x[!is.na(x)]
      if (length(valid) < 8) return(NA)   
      prod(valid) - 1
    }, fill = NA, align = "right"), n = 2) # n = 2 to skip month t-1
  ) %>%
  ungroup() %>%
  filter(!is.na(ret), !is.na(lag_me))

# ---- 2.4 Filter Micro-caps & Winsorization ----
stocks <- stocks %>%
  group_by(date) %>%
  filter(lag_me > quantile(lag_me, 0.20, na.rm = TRUE)) %>% # Exclude the bottom 20% by market cap
  mutate(
    ret        = winsorize_cross_section(ret, 0.01, 0.99),
    bm         = winsorize_cross_section(bm, 0.01, 0.99),
    mom_signal = winsorize_cross_section(mom_signal, 0.01, 0.99)
  ) %>%
  ungroup() %>% arrange(ticker, date)


# ================================================================
# PART 3: CONSTRUCT STANDARD FACTORS (MKT, SMB, HML, MOM, iMOM)
# ================================================================

# ---- 3.1 SMB & HML Factors (Fama-French 1993) ----
ff_factors <- stocks %>%
  filter(!is.na(bm), bm > 0, !is.na(me)) %>%
  group_by(date) %>%
  mutate(
    size_grp = if_else(me >= median(me, na.rm = TRUE), "B", "S"),
    bm_grp   = case_when(
      bm <= quantile(bm, 0.30, na.rm = TRUE) ~ "L",
      bm >= quantile(bm, 0.70, na.rm = TRUE) ~ "H",
      TRUE                                   ~ "M"
    ),
    port = paste0(size_grp, "/", bm_grp)
  ) %>%
  group_by(date, port) %>%
  summarise(R = weighted.mean(ret, w = lag_me, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = port, values_from = R) %>%
  transmute(
    date,
    SMB = rowMeans(cbind(`S/L`, `S/M`, `S/H`), na.rm = TRUE) - rowMeans(cbind(`B/L`, `B/M`, `B/H`), na.rm = TRUE),
    HML = rowMeans(cbind(`S/H`, `B/H`), na.rm = TRUE) - rowMeans(cbind(`S/L`, `B/L`), na.rm = TRUE)
  )

# ---- 3.2 MOM Factor (Carhart 1997) ----
mom_factor <- stocks %>%
  filter(!is.na(mom_signal)) %>%
  group_by(date) %>%
  mutate(grp = case_when(
    mom_signal >= quantile(mom_signal, 0.70, na.rm = TRUE) ~ "W",
    mom_signal <= quantile(mom_signal, 0.30, na.rm = TRUE) ~ "L",
    TRUE                                                   ~ "M"
  )) %>%
  filter(grp %in% c("W", "L")) %>% group_by(date, grp) %>%
  summarise(R = weighted.mean(ret, w = lag_me, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = grp, values_from = R) %>%
  transmute(date, MOM = W - L)

# ---- Merge the 4 factors in preparation for iMOM calculation ----
factors_df <- mkt_df %>%
  left_join(ff_factors, by = "date") %>%
  left_join(mom_factor, by = "date") %>%
  arrange(date)

# ---- 3.3 iMOM Factor (11-month Rolling Regression) ----
cat("\n>> Running Rolling Regression for iMOM (may take 1-2 minutes)...\n")
stocks_imom <- stocks %>%
  left_join(factors_df %>% select(date, Rf, MKT, SMB, HML), by = "date") %>%
  mutate(ret_excess = ret - Rf) %>%
  arrange(ticker, date) %>% group_by(ticker) %>%
  mutate(
    imom_alpha = rollapply(cbind(ret_excess, MKT, SMB, HML), width = 11, FUN = function(x) {
      df <- as.data.frame(x); df <- df[complete.cases(df), ]
      if (nrow(df) < 8) return(NA) 
      coef(lm(ret_excess ~ MKT + SMB + HML, data = df))[1] 
    }, by.column = FALSE, fill = NA, align = "right"),
    imom_signal = lag(imom_alpha, 2)
  ) %>% ungroup()

imom_factor_df <- stocks_imom %>%
  filter(!is.na(imom_signal), !is.na(lag_me)) %>%
  group_by(date) %>%
  mutate(grp_imom = case_when(
    imom_signal >= quantile(imom_signal, 0.70, na.rm = TRUE) ~ "W_iMOM",
    imom_signal <= quantile(imom_signal, 0.30, na.rm = TRUE) ~ "L_iMOM",
    TRUE                                                     ~ "M_iMOM"
  )) %>%
  filter(grp_imom %in% c("W_iMOM", "L_iMOM")) %>% group_by(date, grp_imom) %>%
  summarise(R = weighted.mean(ret, w = lag_me, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = grp_imom, values_from = R) %>%
  transmute(date, iMOM = W_iMOM - L_iMOM)

# ---- 3.4 FINAL CONSOLIDATED FACTOR TABLE ----
final_factors_df <- factors_df %>%
  inner_join(imom_factor_df, by = "date") %>%
  filter(!is.na(iMOM))


# ================================================================
# PART 4: DESCRIPTIVE STATISTICS & VISUALIZATION (EDA)
# ================================================================

# 4.1 Descriptive Statistics
desc_stats <- final_factors_df %>%
  select(MKT, SMB, HML, MOM, iMOM) %>% na.omit() %>%
  summarise(across(everything(), list(
    Mean    = ~ mean(.x) * 100, Std = ~ sd(.x) * 100,
    t_stat  = ~ (mean(.x) / sd(.x)) * sqrt(n()), Sharpe = ~ (mean(.x) / sd(.x)) * sqrt(12), 
    Min     = ~ min(.x) * 100, Max = ~ max(.x) * 100,
    Skew    = ~ skewness(.x), Kurt = ~ kurtosis(.x) - 3, 
    JB_p    = ~ jarque.bera.test(.x)$p.value
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("Factor", "Stat"), names_sep = "__") %>%
  pivot_wider(names_from = Factor, values_from = value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n===== TABLE 1: DESCRIPTIVE STATISTICS OF 5 FACTORS =====\n")
print(desc_stats)

# 4.2 Correlation Matrix & Heatmap
cor_matrix <- cor(final_factors_df %>% select(MKT, SMB, HML, MOM, iMOM), use = "pairwise.complete.obs")
cat("\n===== TABLE 2: CORRELATION MATRIX =====\n")
print(round(cor_matrix, 3))
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, title = "Correlation matrix between factors", mar = c(0,0,1,0))

# 4.3 Cumulative Return Chart
df_long <- final_factors_df %>% select(date, MKT, SMB, HML, MOM, iMOM) %>% na.omit() %>%
  pivot_longer(cols = -date, names_to = "Factor", values_to = "Return") %>%
  group_by(Factor) %>% arrange(date) %>% mutate(CumReturn = cumprod(1 + Return) - 1) %>% ungroup()

ggplot(df_long, aes(x = date, y = CumReturn, color = Factor)) + 
  geom_line(linewidth = 0.8) +  theme_minimal() +
  labs(title = "Cumulative Returns of Factors", x = "Time", y = "Cumulative Return", color = "Factor") +
  theme(legend.position = "bottom")


# ================================================================
# PART 5: MODEL TESTING AND EVALUATION
# ================================================================

# ---- 5.1 Multicollinearity (VIF & Condition Number) ----
cat("\n--- Checking Multicollinearity (5 factors) ---\n")
set.seed(123) 
dummy_y <- rnorm(nrow(final_factors_df))
vif_5f_fit_fixed <- lm(dummy_y ~ MKT + SMB + HML + MOM + iMOM, data = final_factors_df)
print(round(vif(vif_5f_fit_fixed), 4))
compute_kappa(vif_5f_fit_fixed, "Full 5 factors")

# ---- 5.2 iMOM Strategy Testing ----
cat("\n===== iMOM STRATEGY TESTING RESULTS =====\n")
imom_test_ff3 <- lm(iMOM ~ MKT + SMB + HML, data = final_factors_df)
cat("\n1. Explaining iMOM with Fama-French 3 factors:\n")
print(coeftest(imom_test_ff3, vcov = NeweyWest(imom_test_ff3, lag = 4)))

imom_test_c4 <- lm(iMOM ~ MKT + SMB + HML + MOM, data = final_factors_df)
cat("\n2. Explaining iMOM with Carhart 4 factors:\n")
print(coeftest(imom_test_c4, vcov = NeweyWest(imom_test_c4, lag = 4)))

# ---- 5.3 Regression on 6 portfolios (Size - B/M) ----
port_test_df <- stocks %>% filter(!is.na(bm), bm > 0, !is.na(me)) %>%
  group_by(date) %>%
  mutate(
    size_grp = if_else(me >= median(me, na.rm = TRUE), "B", "S"),
    bm_grp   = case_when(bm <= quantile(bm, 0.30, na.rm = TRUE) ~ "L", bm >= quantile(bm, 0.70, na.rm = TRUE) ~ "H", TRUE ~ "M"),
    port = paste0(size_grp, "/", bm_grp)
  ) %>%
  group_by(date, port) %>%
  summarise(Rp = weighted.mean(ret, w = lag_me, na.rm = TRUE), .groups = "drop")

test_data <- port_test_df %>% inner_join(factors_df, by = "date") %>% mutate(Rp_excess = Rp - Rf) %>%
  filter(!is.na(Rp_excess), !is.na(MKT), !is.na(SMB), !is.na(HML), !is.na(MOM))

results_list <- list()
for (p in unique(test_data$port)) {
  fit <- lm(Rp_excess ~ MKT + SMB + HML + MOM, data = test_data %>% filter(port == p))
  res <- tidy(fit) %>% 
    mutate(significance = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.10 ~ "*", TRUE ~ ""),
           estimate_str = sprintf("%.4f %s", estimate, significance)) %>%
    select(term, estimate_str) %>% rename(!!p := estimate_str)
  adj_r2 <- tibble(term = "Adj. R-squared", !!p := sprintf("%.4f", glance(fit)$adj.r.squared))
  results_list[[p]] <- bind_rows(res, adj_r2)
}

final_results <- Reduce(function(x, y) left_join(x, y, by = "term"), results_list) %>%
  arrange(match(term, c("(Intercept)", "MKT", "SMB", "HML", "MOM", "Adj. R-squared")))
cat("\n===== REGRESSION RESULTS ON 6 SIZE - B/M PORTFOLIOS =====\n")
print(final_results)

# ================================================================
# 5.4 GRS TEST ON 5 PORTFOLIOS SORTED BY iMOM SIGNAL
# ================================================================

# 1. Academic standard GRS function
grs_test_safe <- function(ret_matrix, factor_matrix) {
  T <- nrow(ret_matrix)
  N <- ncol(ret_matrix)
  K <- ncol(factor_matrix)
  
  fit <- lm(ret_matrix ~ factor_matrix)
  alpha <- coef(fit)[1, ]
  res <- residuals(fit)
  
  Sigma <- cov(res) * (T - 1) / T
  Omega <- cov(factor_matrix) * (T - 1) / T
  mu_f <- colMeans(factor_matrix)
  
  inv_Sigma <- ginv(Sigma)
  inv_Omega <- ginv(Omega)
  
  grs_stat <- (T / N) * ((T - N - K) / (T - K - 1)) *
    as.numeric(t(alpha) %*% inv_Sigma %*% alpha) /
    as.numeric(1 + t(mu_f) %*% inv_Omega %*% mu_f)
  
  p_val <- 1 - pf(grs_stat, N, T - N - K)
  return(c(grs_stat, p_val))
}

cat("\n>> Step 1: Create 5 test portfolios (Test Assets) based on iMOM Quintiles...\n")

# 2. Construct 5 portfolios from the existing stocks_imom table
port_test_imom <- stocks_imom %>%
  filter(!is.na(imom_signal), !is.na(lag_me), !is.na(ret)) %>%
  group_by(date) %>%
  mutate(quintile = ntile(imom_signal, 5)) %>%
  group_by(date, quintile) %>%
  summarise(Rp = weighted.mean(ret, w = lag_me, na.rm = TRUE), .groups = "drop") %>%
  mutate(port_name = paste0("Q", quintile))

# Merge with Rf, calculate Excess Return, pivot to wide format
# FIX: use explicit dplyr::select() to avoid MASS::select masking
port_test_imom_excess <- port_test_imom %>%
  inner_join(final_factors_df %>% dplyr::select(date, Rf), by = "date") %>%
  mutate(Rp_excess = Rp - Rf) %>%
  dplyr::select(date, port_name, Rp_excess) %>%
  pivot_wider(names_from = port_name, values_from = Rp_excess) %>%
  arrange(date) %>%
  drop_na()

# Get list of valid dates
valid_dates_grs <- port_test_imom_excess$date

# Excess return matrix (N = 5 portfolios)
port_matrix <- port_test_imom_excess %>%
  dplyr::select(Q1, Q2, Q3, Q4, Q5) %>%
  as.matrix()

# Factor matrix
factors_matrix <- final_factors_df %>%
  filter(date %in% valid_dates_grs) %>%
  arrange(date)

cat(">> Step 2: Run GRS test for 3 models...\n")

# 3. Run GRS test
# Model 1: Fama-French 3
f_ff3 <- factors_matrix %>% dplyr::select(MKT, SMB, HML) %>% as.matrix()
grs_ff3 <- grs_test_safe(port_matrix, f_ff3)

# Model 2: Carhart 4 (with traditional MOM)
f_c4 <- factors_matrix %>% dplyr::select(MKT, SMB, HML, MOM) %>% as.matrix()
grs_c4 <- grs_test_safe(port_matrix, f_c4)

# Model 3: Proposed model (with iMOM instead of MOM)
f_imom4 <- factors_matrix %>% dplyr::select(MKT, SMB, HML, iMOM) %>% as.matrix()
grs_imom4 <- grs_test_safe(port_matrix, f_imom4)

# 4. Print results
cat("\n===== GRS TEST RESULTS ON 5 iMOM PORTFOLIOS =====\n")
grs_results <- data.frame(
  Model = c("1. Fama-French 3", "2. Carhart 4 (with MOM)", "3. Proposed (with iMOM)"),
  GRS_Statistic = round(c(grs_ff3[1], grs_c4[1], grs_imom4[1]), 4),
  P_Value = round(c(grs_ff3[2], grs_c4[2], grs_imom4[2]), 4)
)
print(grs_results)
cat("\n* Note: A smaller GRS Statistic and a larger P-value (especially > 0.05) indicate a better pricing model.\n")