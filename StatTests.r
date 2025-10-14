## This is the code where I run statistical tests to find normality, then based on normality finding p values 

library(dplyr)
library(rstatix)
library(effsize)

# You will need to add the path below if you are not in the Green Lab Project repo folder

## Data read (same as DescStat)
dat_data <- read.csv("run_table.csv") %>%
  select(run, energy_usage_joules, execution_time_sec, input_size_kb, memory_usage_mb, output_size_kb, cpu_percent, prompt_size, model, api, mobile) %>%
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
  cat("\n=============================================================\n")
  cat(paste("👉 Testing:", response, "by", group_var, "\n"))
  cat("=============================================================\n")

  data$.resid <- get_residuals(data, response)

  g1 <- data %>% filter(!!sym(group_var) == levels(data[[group_var]])[1]) %>% pull(.resid)
  g2 <- data %>% filter(!!sym(group_var) == levels(data[[group_var]])[2]) %>% pull(.resid)

  g1_label <- levels(data[[group_var]])[1]
  g2_label <- levels(data[[group_var]])[2]

  sh1 <- shapiro.test(g1)
  sh2 <- shapiro.test(g2)

  cat(sprintf("\nNormality check (Shapiro-Wilk): %s p=%.5f | %s p=%.5f\n",
              g1_label, sh1$p.value, g2_label, sh2$p.value))

  if (sh1$p.value > 0.05 && sh2$p.value > 0.05) {
    cat("✅ Both groups appear normally distributed → using parametric t-test\n")
    test <- t.test(g1, g2)
    test_type <- "t-test"
  } else {
    cat("⚠️ At least one group is non-normal → using Mann–Whitney (nonparametric)\n")
    test <- wilcox.test(g1, g2)
    test_type <- "Mann–Whitney"
  }

  cat(sprintf("Test type: %s\np-value: %.5f\n", test_type, test$p.value))

  if (test_type == "t-test") {
    mean1 <- mean(g1, na.rm = TRUE)
    mean2 <- mean(g2, na.rm = TRUE)
    sd_pooled <- sqrt(((length(g1) - 1) * var(g1, na.rm = TRUE) +
                       (length(g2) - 1) * var(g2, na.rm = TRUE)) /
                       (length(g1) + length(g2) - 2))
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
    cat("Both appear normal, Pearson correlation (parametric)\n")
    test <- cor.test(data$.resid_x, data$.resid_y, method = "pearson")
  } else {
    cat("Non-normality detected, Spearman correlation (nonparametric)\n")
    test <- cor.test(data$.resid_x, data$.resid_y, method = "spearman")
  }

  cat(sprintf("Correlation coefficient (r): %.3f\np-value: %.5f\n", test$estimate, test$p.value))
}

# Q1
compare_groups(dat_data, "energy_usage_joules", "llm_location")

# Q2
compare_groups(dat_data, "execution_time_sec", "llm_location")

# Q3
compare_groups(dat_data, "cpu_percent", "llm_location")

# Q4
compare_groups(dat_data, "memory_usage_mb", "llm_location")

# Q5a CPU
correlation_analysis(dat_data, "cpu_percent", "energy_usage_joules")

# Q5b Memory
correlation_analysis(dat_data, "memory_usage_mb", "energy_usage_joules")

# Q6
compare_groups(dat_data, "energy_usage_joules", "prompt_size")