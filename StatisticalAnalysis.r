# Load required packages
install.packages("rstatix")
install.packages(c("bestNormalize", "ARTool", "lmerTest", "xtable"))
install.packages("ggplot2", type = "binary")
install.packages("tidyverse", type = "binary")
install.packages("ggpubr")
library(rstatix)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(bestNormalize)
library(ARTool)
library(lmerTest)
library(xtable)
library(rstatix)
library(ggpubr)

# If you are not in the same folder as run_table.csv, you will need to add the path.
run_table <- read_csv("run_table.csv")

# Set up output folder
out_dir <- "analysis_outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Read and clean data
dat_data <- read.csv("run_table.csv", stringsAsFactors = FALSE) %>%
  select(run, energy_usage_joules, execution_time_sec, input_size_kb,
         memory_usage_mb, output_size_kb, cpu_percent, prompt_size,
         model, api, mobile) %>%
  mutate(
    # Text data conversion
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
    mobile = factor(mobile, levels = c("no","yes"))
  )

# Quick checks
cat("Rows read:", nrow(dat_data), "\n")
cat("Mobile counts:\n"); print(table(dat_data$mobile, useNA = "ifany"))
cat("NA counts in numeric columns:\n"); print(colSums(is.na(select(dat_data, energy_usage_joules, execution_time_sec, input_size_kb, memory_usage_mb, output_size_kb, cpu_percent))))
# If everything is loaded correctly, Mobile counts should both read 180, and NA counts should all read 0

# Descriptive statistics
quant_vars <- c("energy_usage_joules", "execution_time_sec", "cpu_percent", "memory_usage_mb", "input_size_kb", "output_size_kb")

desc_by_group <- dat_data %>%
  group_by(mobile) %>%
  summarise(
    across(all_of(quant_vars),
           list(n = ~sum(!is.na(.)),
                mean = ~mean(., na.rm=TRUE),
                median = ~median(., na.rm=TRUE),
                sd = ~sd(., na.rm=TRUE),
                var = ~var(., na.rm=TRUE),
                min = ~min(., na.rm=TRUE),
                max = ~max(., na.rm=TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

write.csv(desc_by_group, file.path(out_dir, "descriptives_by_mobile.csv"), row.names = FALSE)
cat("\nDescriptive stats saved to", file.path(out_dir, "descriptives_by_mobile.csv"), "\n")

# Outlier inspection
# - report points outside 1.5*IQR per group-variable
find_outliers <- function(df, var) {
  df2 <- df %>% select(mobile, all_of(var)) %>% group_by(mobile) %>%
    summarize(
      Q1 = quantile(.data[[var]], 0.25, na.rm=TRUE),
      Q3 = quantile(.data[[var]], 0.75, na.rm=TRUE),
      IQR = IQR(.data[[var]], na.rm=TRUE),
      .groups = "drop_last"
    ) %>%
    ungroup()
  df_out <- df %>% left_join(df2, by = "mobile") %>%
    mutate(is_outlier = (.data[[var]] < (Q1 - 1.5*IQR)) | (.data[[var]] > (Q3 + 1.5*IQR))) %>%
    filter(is_outlier == TRUE) %>%
    select(mobile, everything(), is_outlier)
  df_out
}

outlier_list <- map(quant_vars, ~ find_outliers(dat_data, .x))
names(outlier_list) <- quant_vars
# Save brief summary of outliers
outlier_summary <- map_dfr(names(outlier_list), function(v) {
  df <- outlier_list[[v]]
  tibble(variable = v, n_outliers = nrow(df))
})
write.csv(outlier_summary, file.path(out_dir, "outlier_summary.csv"), row.names = FALSE)
cat("Outlier summary saved to", file.path(out_dir, "outlier_summary.csv"), "\n")

# Assumption checks

shapiro_results <- map_dfr(quant_vars, function(v) {
  df_no  <- dat_data %>% filter(mobile == "no")  %>% pull(all_of(v))
  df_yes <- dat_data %>% filter(mobile == "yes") %>% pull(all_of(v))

  df_no  <- df_no[!is.na(df_no)]
  df_yes <- df_yes[!is.na(df_yes)]

  if (!is.numeric(df_no) || length(df_no) < 3)  return(tibble(variable=v, shapiro_no_p=NA_real_, shapiro_yes_p=NA_real_, note="too few/no numeric"))
  if (!is.numeric(df_yes) || length(df_yes) < 3) return(tibble(variable=v, shapiro_no_p=NA_real_, shapiro_yes_p=NA_real_, note="too few/no numeric"))

  if (n_distinct(df_no) < 3 | n_distinct(df_yes) < 3) {
    tibble(variable=v, shapiro_no_p=NA_real_, shapiro_yes_p=NA_real_, note="too few unique values")
  } else {
    tibble(variable=v,
           shapiro_no_p=shapiro.test(df_no)$p.value,
           shapiro_yes_p=shapiro.test(df_yes)$p.value,
           note="ok")
  }
})

brown_forsythe <- map_dfr(quant_vars, function(v) {
  df <- dat_data %>% select(mobile, all_of(v)) %>% filter(!is.na(.data[[v]]))
  if (nrow(df) < 4) return(tibble(variable = v, bf_p = NA_real_, bf_note="too few"))
  df <- df %>% group_by(mobile) %>% mutate(med = median(.data[[v]], na.rm=TRUE)) %>% ungroup() %>%
    mutate(abs_dev = abs(.data[[v]] - med))
  a <- aov(abs_dev ~ mobile, data = df)
  pv <- tryCatch(summary(a)[[1]][["Pr(>F)"]][1], error = function(e) NA_real_)
  tibble(variable = v, bf_p = pv, bf_note="ok")
})

assumptions_tbl <- left_join(shapiro_results, brown_forsythe, by = "variable")
write.csv(assumptions_tbl, file.path(out_dir, "assumption_checks.csv"), row.names = FALSE)
cat("Assumptions saved to", file.path(out_dir, "assumption_checks.csv"), "\n")

# Generate Q-Q plots and save
for (v in quant_vars) {
  for (grp in c("no", "yes")) {
    vals <- dat_data %>% filter(mobile == grp) %>% pull(all_of(v)) %>% na.omit()
    if (length(vals) >= 3) {
      pqq <- ggplot(data.frame(x=vals), aes(sample = x)) +
        stat_qq() + stat_qq_line() +
        labs(title = paste("Q-Q plot:", v, "| mobile=", grp), x = "Theoretical quantiles", y = "Sample quantiles") +
        theme_minimal()
      ggsave(filename = file.path(out_dir, paste0("qqplot_", v, "_", grp, ".png")), plot = pqq, width = 6, height = 4)
    }
  }
}

# 5. Hypothesis testing & effect sizes (Q1–Q4, Q6)
# - For each variable: if both groups (no & yes) are approx normal and homogeneity holds -> t-test (pooled)
# - Else -> Wilcoxon rank-sum
# - Effect sizes: Cohen's d (independent) or Cliff's delta
cohen_d_indep <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  if (nx < 2 || ny < 2) return(NA_real_)
  sx <- sd(x); sy <- sd(y)
  pooled_sd <- sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / (nx + ny - 2))
  if (pooled_sd == 0) return(NA_real_)
  (mean(x) - mean(y)) / pooled_sd
}
cliff_delta <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  if (nx == 0 || ny == 0) return(NA_real_)
  comp <- outer(x, y, "-")
  greater <- sum(comp > 0)
  less <- sum(comp < 0)
  (greater - less) / (nx * ny)
}

test_results <- map_dfr(quant_vars[c(1:4,5)], function(v) {
  no_vals <- dat_data %>% filter(mobile=="no") %>% pull(all_of(v)) %>% na.omit()
  yes_vals <- dat_data %>% filter(mobile=="yes") %>% pull(all_of(v)) %>% na.omit()
  n_no <- length(no_vals); n_yes <- length(yes_vals)
  if (n_no < 2 || n_yes < 2) {
    return(tibble(variable = v, test = NA_character_, p_value = NA_real_, effect = NA_real_, effect_type = NA_character_, n_no = n_no, n_yes = n_yes))
  }
  # Get relevant assumption p-values
  p_sh_no <- if(length(no_vals) >= 3) shapiro.test(no_vals)$p.value else NA_real_
  p_sh_yes <- if(length(yes_vals) >= 3) shapiro.test(yes_vals)$p.value else NA_real_
  # Brown-Forsythe p
  bf_p <- brown_forsythe %>% filter(variable == v) %>% pull(bf_p)
  use_t <- (!is.na(p_sh_no) && !is.na(p_sh_yes) && p_sh_no > 0.05 && p_sh_yes > 0.05 && !is.na(bf_p) && bf_p > 0.05)
  if (use_t) {
    t_res <- t.test(yes_vals, no_vals, paired = FALSE, var.equal = TRUE)
    eff <- cohen_d_indep(yes_vals, no_vals)
    tibble(variable = v, test = "t-test (pooled var)", p_value = t_res$p.value, effect = eff, effect_type = "Cohen's d", n_no = n_no, n_yes = n_yes)
  } else {
    w_res <- wilcox.test(yes_vals, no_vals, paired = FALSE, exact = FALSE)
    eff <- cliff_delta(yes_vals, no_vals)
    tibble(variable = v, test = "Wilcoxon rank-sum", p_value = w_res$p.value, effect = eff, effect_type = "Cliff's delta", n_no = n_no, n_yes = n_yes)
  }
})

# multiple comparisons correction for these tests (Holm)
test_results <- test_results %>% mutate(p_adj = ifelse(!is.na(p_value), p.adjust(p_value, method = "holm"), NA_real_))
write.csv(test_results, file.path(out_dir, "Q1_Q4_Q6_test_results.csv"), row.names = FALSE)

cat("\nHypothesis test results saved to", file.path(out_dir, "Q1_Q4_Q6_test_results.csv"), "\n")
print(test_results)

# Q5: correlation analysis (energy vs CPU/GPU/memory)
# - Report Pearson, Spearman, Kendall; choose reporting depending on normality/robustness
corr_vars <- c("cpu_percent", "memory_usage_mb")
# include gpu_percent if present
if ("gpu_percent" %in% names(dat_data)) corr_vars <- c(corr_vars, "gpu_percent")

corr_results <- map_dfr(corr_vars, function(v) {
  x <- dat_data[[v]]; y <- dat_data$energy_usage_joules
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 3) {
    return(tibble(variable = v, pearson_r = NA_real_, pearson_p = NA_real_, spearman_r = NA_real_, spearman_p = NA_real_, kendall_r = NA_real_, kendall_p = NA_real_, n = n))
  }
  p1 <- cor.test(y, x, method = "pearson")
  p2 <- cor.test(y, x, method = "spearman")
  p3 <- cor.test(y, x, method = "kendall")
  tibble(variable = v,
         pearson_r = p1$estimate, pearson_p = p1$p.value,
         spearman_r = p2$estimate, spearman_p = p2$p.value,
         kendall_r = p3$estimate, kendall_p = p3$p.value,
         n = n)
})

write.csv(corr_results, file.path(out_dir, "Q5_correlation_results.csv"), row.names = FALSE)
cat("\nCorrelation results saved to", file.path(out_dir, "Q5_correlation_results.csv"), "\n")
print(corr_results)

# Plots: boxplot+jitter, density, Q-Q already saved, scatter with regression
# - We will both print them (so they show in R) and save PNGs.

# helper for saving+printing
save_and_print <- function(plot_obj, filename) {
  print(plot_obj)
  ggsave(filename = file.path(out_dir, filename), plot = plot_obj, width = 7, height = 4)
}

# Boxplots + jitter for each quantitative var
for (v in quant_vars) {
  p <- dat_data %>%
    ggplot(aes(x = mobile, y = .data[[v]], fill = mobile)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    labs(title = paste0(v, " by mobile (no=remote, yes=on-device)"), x = "mobile", y = v) +
    theme_minimal()
  save_and_print(p, paste0("boxplot_", v, ".png"))
}

# Density plots (grouped)
for (v in c("energy_usage_joules", "execution_time_sec")) {
  p <- dat_data %>%
    ggplot(aes(x = .data[[v]], fill = mobile)) +
    geom_density(alpha = 0.4, na.rm = TRUE) +
    labs(title = paste0("Density of ", v, " by mobile"), x = v) +
    theme_minimal()
  save_and_print(p, paste0("density_", v, ".png"))
}

# Scatter & regression: energy vs cpu_percent, energy vs memory
p_ecpu <- dat_data %>%
  ggplot(aes(x = cpu_percent, y = energy_usage_joules, color = mobile)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Energy vs CPU percent", x = "CPU %", y = "Energy (J)")
save_and_print(p_ecpu, "scatter_energy_cpu.png")

p_emem <- dat_data %>%
  ggplot(aes(x = memory_usage_mb, y = energy_usage_joules, color = mobile)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Energy vs Memory (MB)", x = "Memory (MB)", y = "Energy (J)")
save_and_print(p_emem, "scatter_energy_mem.png")

# Print and save summary tables
write.csv(desc_by_group, file.path(out_dir, "descriptives_by_mobile.csv"), row.names = FALSE)
write.csv(test_results, file.path(out_dir, "Q1_Q4_Q6_test_results.csv"), row.names = FALSE)
write.csv(corr_results, file.path(out_dir, "Q5_correlation_results.csv"), row.names = FALSE)

cat("\nAll outputs saved to folder:", out_dir, "\n")
cat("Files:\n"); print(list.files(out_dir))
