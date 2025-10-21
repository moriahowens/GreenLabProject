# This is the code that makes the data used in Table 6 -- PAIRED stat tests
# ChatGPT was used for coding assistance

library(dplyr)
library(rstatix)
library(effsize)
library(tidyr)
library(purrr)

# You will need to add the path below if you are not in the Green Lab Project repo folder

## Data read (same as other files)
dat_data <- read.csv("run_table.csv") %>%
  select(run, energy_usage_joules, execution_time_sec, input_size_kb, memory_usage_mb, output_size_kb, cpu_percent, prompt_size, model, api, mobile) %>%
  mutate(
    prompt_size = trimws(as.character(prompt_size)),
    model = trimws(as.character(model)),
    api = trimws(as.character(api)),
    mobile = trimws(as.character(mobile)),

    # Safe numeric data conversion
    energy_usage_joules = suppressWarnings(as.numeric(trimws(as.character(energy_usage_joules)))),
    execution_time_sec  = suppressWarnings(as.numeric(trimws(as.character(execution_time_sec)))),
    input_size_kb       = suppressWarnings(as.numeric(trimws(as.character(input_size_kb)))),
    memory_usage_mb     = suppressWarnings(as.numeric(trimws(as.character(memory_usage_mb)))),
    output_size_kb      = suppressWarnings(as.numeric(trimws(as.character(output_size_kb)))),
    cpu_percent         = suppressWarnings(as.numeric(trimws(as.character(cpu_percent)))),

    prompt_size = factor(prompt_size),
    model = factor(model),
    api = factor(api, levels = c("no","yes")),
    llm_location = factor(
      ifelse(mobile == "no", "remote", "mobile"),
      levels = c("remote", "mobile")
    )

  )

get_residuals <- function(data, response) {
  formula <- as.formula(paste(response, "~ model + prompt_size"))
  lm(formula, data = data) %>% resid()
}

compare_groups <- function(data, response, group_var) {
  cat(paste("Test:", response, "by", group_var, "\n"))

  data$.resid <- get_residuals(data, response)

  # remove residuals
  g1 <- data %>% filter(!!sym(group_var) == levels(data[[group_var]])[1]) %>% pull(.resid)
  g2 <- data %>% filter(!!sym(group_var) == levels(data[[group_var]])[2]) %>% pull(.resid)

  g1_label <- levels(data[[group_var]])[1]
  g2_label <- levels(data[[group_var]])[2]

  # do shapiro-wilk
  sh1 <- shapiro.test(g1)
  sh2 <- shapiro.test(g2)

  cat(sprintf("\nShapiro-Wilk: %s p=%.5f | %s p=%.5f\n", g1_label, sh1$p.value, g2_label, sh2$p.value))

  if (sh1$p.value > 0.05 && sh2$p.value > 0.05) {
    cat("Normal distribution, parametric t test\n")
    test <- t.test(g1, g2)
    test_type <- "t-test"
  } else {
    cat("Non-normal, Mann-Whitney nonparametric test\n")
    test <- wilcox.test(g1, g2) #The wilcox test on 2 parameters is the same as Mann-Whitney
    test_type <- "Mann–Whitney"
  }

  cat(sprintf("Test type: %s\np-value: %.5f\n", test_type, test$p.value))

  if (test_type == "t-test") {
    mean1 <- mean(g1, na.rm = TRUE)
    mean2 <- mean(g2, na.rm = TRUE)
    sd_pooled <- sqrt(((length(g1) - 1) * var(g1, na.rm = TRUE) + (length(g2) - 1) * var(g2, na.rm = TRUE)) / (length(g1) + length(g2) - 2))
    cohen_d <- (mean1 - mean2) / sd_pooled
    cat(sprintf("Cohen’s d (effect size): %.3f\n", cohen_d))
  } else {
    cliff <- cliff.delta(g1, g2)
    cat(sprintf("Cliff’s delta: %.3f (%s)\n", cliff$estimate, cliff$magnitude))
  }
}

correlation_analysis <- function(data, var1, var2) {

  data$.resid_x <- get_residuals(data, var1)
  data$.resid_y <- get_residuals(data, var2)

  sh_x <- shapiro.test(data$.resid_x)
  sh_y <- shapiro.test(data$.resid_y)

  cat(sprintf("\nNormality check (Shapiro-Wilk): %s p=%.5f | %s p=%.5f\n",
              var1, sh_x$p.value, var2, sh_y$p.value))

  if (sh_x$p.value > 0.05 && sh_y$p.value > 0.05) {
    cat("Both normal, Pearson (parametric)\n")
    test <- cor.test(data$.resid_x, data$.resid_y, method = "pearson")
  } else {
    cat("Nonnormal, Spearman (nonparametric)\n")
    test <- cor.test(data$.resid_x, data$.resid_y, method = "spearman")
  }

  cat(sprintf("Correlation coefficient (r): %.3f\np-value: %.5f\n", test$estimate, test$p.value))
}

## Paired tests
model_pairs <- list(
  c("gemma", "gemma-1b"),
  c("llama2", "llama-1b"),
  c("qwen", "qwen-1.7b")
)

metrics <- c("energy_usage_joules", "execution_time_sec", "cpu_percent", "memory_usage_mb")

pairing_order_cols <- function(df) {
  if ("run" %in% names(df)) {
    return(df %>% arrange(run))
  } else if ("input_size_kb" %in% names(df)) {
    return(df %>% arrange(input_size_kb))
  } else {
    return(df)
  }
}

paired_compare_once <- function(df, metric, model_a, model_b, prompt_size_val) {
  # pair appropriately
  a_df <- df %>% filter(model == model_a, prompt_size == prompt_size_val)
  b_df <- df %>% filter(model == model_b, prompt_size == prompt_size_val)

  a_df <- pairing_order_cols(a_df)
  b_df <- pairing_order_cols(b_df)

  n_a <- nrow(a_df)
  n_b <- nrow(b_df)
  n_pair <- min(n_a, n_b)

  vec_a <- a_df[[metric]][1:n_pair]
  vec_b <- b_df[[metric]][1:n_pair]

  keep_idx <- which(!is.na(vec_a) & !is.na(vec_b))
  vec_a <- vec_a[keep_idx]
  vec_b <- vec_b[keep_idx]
  n_pair2 <- length(vec_a)

  diffs <- vec_a - vec_b

  # Shapiro-Wilk
  sh <- tryCatch(shapiro.test(diffs), error = function(e) list(p.value = NA))
  p_sh <- if (is.list(sh)) sh$p.value else sh$p.value

  # format p values
  format_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 1e-6) return(sprintf("%.2e", p))
    if (p < 0.001) return(sprintf("%.3f", p))
    return(sprintf("%.3f", p))
  }

  cat("\n-------------------------------------------------------------\n")
  cat(sprintf(
    "Paired comparison: %s (remote) vs %s (on-device)\nMetric: %s | prompt_size = %s\n",
    model_a, model_b, metric, prompt_size_val
  ))
  cat(sprintf("n matched pairs = %d\n", n_pair2))
  cat(sprintf("Shapiro-Wilk on paired differences: p = %s\n", format_p(p_sh)))

  if (!is.na(p_sh) && p_sh > 0.05) {
    cat("Differences normal, paired t-test\n")
    tt <- t.test(vec_a, vec_b, paired = TRUE)
    pval <- tt$p.value
    test_name <- "paired t-test"

    # Cohen's d
    cd <- tryCatch(effsize::cohen.d(vec_a, vec_b, paired = TRUE), error = function(e) NULL)
    cohen_d_val <- if (!is.null(cd) && !is.null(cd$estimate)) cd$estimate else NA_real_
    effect_label <- if (!is.na(cohen_d_val)) sprintf("Cohen's d = %.3f", cohen_d_val) else "Cohen's d = NA"

  } else {
    cat("Differences non-normal, Wilcoxon signed-rank test (paired)\n")
    wil <- tryCatch(stats::wilcox.test(vec_a, vec_b, paired = TRUE, exact = FALSE), 
                    error = function(e) NULL)

    if (is.null(wil)) {
      pval <- NA
      test_name <- "Wilcoxon signed-rank (paired)"
      effect_label <- "Wilcoxon effect size NA"
    } else {
      pval <- wil$p.value
      test_name <- "Wilcoxon signed-rank (paired)"

      # Compute effect size r = Z / sqrt(N)
      W <- wil$statistic
      n <- n_pair2
      mean_W <- n * (n + 1) / 4
      sd_W <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
      z <- (W - mean_W - 0.5 * sign(W - mean_W)) / sd_W
      r <- z / sqrt(n)

      mag <- dplyr::case_when(
        abs(r) < 0.1 ~ "negligible",
        abs(r) < 0.3 ~ "small",
        abs(r) < 0.5 ~ "medium",
        TRUE ~ "large"
      )
      effect_label <- sprintf("r = %.3f (%s)", r, mag)
    }
  }

  cat(sprintf("Test used: %s\np-value = %s\n%s\n", test_name, format_p(pval), effect_label))

  tibble(
    model_a = model_a,
    model_b = model_b,
    prompt_size = prompt_size_val,
    metric = metric,
    test = test_name,
    p_value = pval,   
    p_value_fmt = format_p(pval),
    effect = effect_label,
    n_pairs = n_pair2
  )
}

## Run paired comparisons for all model pairs, metrics, prompt sizes
paired_results <- list()
for (mp in model_pairs) {
  model_a <- mp[1]
  model_b <- mp[2]
  for (ps in levels(dat_data$prompt_size)) {
    for (metric in metrics) {
      res <- tryCatch(
        paired_compare_once(dat_data, metric, model_a, model_b, ps),
        error = function(e) {
          cat("Error in paired compare:", conditionMessage(e), "\n")
          NULL
        }
      )
      if (!is.null(res)) paired_results <- append(paired_results, list(res))
    }
  }
}