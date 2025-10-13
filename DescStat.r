## This is the code that makes the big Table 2 and all plots :)

install.packages(c("tidyverse", "ggplot2", "bestNormalize", "ARTool", "lmerTest", "xtable", "kableExtra", "effsize"))
install.packages(c("patchwork"))
install.packages(c("ggpubr"))
library(tidyverse)
library(ggplot2)
library(bestNormalize)
library(ARTool)
library(lmerTest)
library(xtable)
library(dplyr)
library(knitr)
library(kableExtra)
library(effsize)
library(patchwork) 
library(ggpubr)


## Load and Parse all data

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

glimpse(dat_data)
summary(dat_data)
head(dat_data)

## Get descriptive stats

quant_vars <- c("energy_usage_joules", "execution_time_sec", "cpu_percent", "memory_usage_mb", "input_size_kb", "output_size_kb")

desc_by_group <- dat_data %>%
  group_by(llm_location) %>%
  summarise(
    across(all_of(quant_vars),
           list(n = ~sum(!is.na(.)),
                mean = ~mean(., na.rm=TRUE),
                median = ~median(., na.rm=TRUE),
                sd = ~sd(., na.rm=TRUE),
                variance = ~var(., na.rm=TRUE),
                min = ~min(., na.rm=TRUE),
                max = ~max(., na.rm=TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

glimpse(desc_by_group)

## Identify outliers!

find_outliers <- function(df, var) {
  df2 <- df %>% select(llm_location, all_of(var)) %>% group_by(llm_location) %>%
    summarize(
      Q1 = quantile(.data[[var]], 0.25, na.rm=TRUE),
      Q3 = quantile(.data[[var]], 0.75, na.rm=TRUE),
      IQR = IQR(.data[[var]], na.rm=TRUE),
      .groups = "drop_last"
    ) %>%
    ungroup()
  df_out <- df %>% left_join(df2, by = "llm_location") %>%
    mutate(is_outlier = (.data[[var]] < (Q1 - 1.5*IQR)) | (.data[[var]] > (Q3 + 1.5*IQR))) %>%
    filter(is_outlier == TRUE) %>%
    select(mobile, everything(), is_outlier)
  df_out
}

outlier_list <- map(quant_vars, ~ find_outliers(dat_data, .x))
names(outlier_list) <- quant_vars
outlier_summary <- map_dfr(names(outlier_list), function(v) {
  df <- outlier_list[[v]]
  tibble(variable = v, n_outliers = nrow(df))
})

glimpse(outlier_list)
glimpse(outlier_summary)
# Outliers found in cpu_percent (9 outliers, no pattern) used and memory_usage (6 outliers, consective runs (175-180))
# Outliers will remain in the data set but be analyzed within the Threats to Validity section.

# RQ5: How do CPU usage, GPU usage, and memory usage impact energy consumption?

## Check for normality

# RQ5: ENERGY USAGE normality check, divided by LLM location - both NOT NORMAL
## This was before I realized I needed to split based on factors of model and content length
dat_data %>%
  group_by(llm_location) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(energy_usage_joules)$p.value else NA_real_
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

ggplot(dat_data, aes(x = seq_along(energy_usage_joules), y = energy_usage_joules)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  theme_minimal() +
  labs(
    title = "Energy Usage Across Observations",
    x = "Observation Index",
    y = "Energy Usage (Joules)"
  )

normality_results <- dat_data %>%
  group_by(llm_location, model) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(energy_usage_joules)$p.value else NA_real_,
    .groups = "drop"
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

# Plot Remote models
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(energy_usage_joules), y = energy_usage_joules)) +
  geom_point(color = "red", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Energy Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "Energy Usage (Joules)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(energy_usage_joules), y = energy_usage_joules)) +
  geom_point(color = "red", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Energy Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "Energy Usage (Joules)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# RQ5: CPU USAGE normality check, divided by LLM location - all NOT NORMAL
## This was before I realized I needed to split based on factors of model and content length
dat_data %>%
  group_by(llm_location) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(cpu_percent)$p.value else NA_real_
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

ggplot(dat_data, aes(x = seq_along(cpu_percent), y = cpu_percent)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  theme_minimal() +
  labs(
    title = "Percent CPU Usage Across Observations",
    x = "Observation Index",
    y = "CPU Usage (%)"
  )

normality_results <- dat_data %>%
  group_by(llm_location, model) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(cpu_percent)$p.value else NA_real_,
    .groups = "drop"
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

# Plot Remote models
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(cpu_percent), y = cpu_percent)) +
  geom_point(color = "red", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Percent CPU Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "CPU Usage (%)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(cpu_percent), y = cpu_percent)) +
  geom_point(color = "red", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Percent CPU Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "CPU Usage (%)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )



# RQ5: MEMORY USAGE normality check, divided by LLM location and model- qwen-1.7b is normal!
## This was before I realized I needed to split based on factors of model and content length

dat_data %>%
  group_by(llm_location) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(memory_usage_mb)$p.value else NA_real_
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)


ggplot(dat_data, aes(x = seq_along(memory_usage_mb), y = memory_usage_mb)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  theme_minimal() +
  labs(
    title = "Memory Usage Across Observations",
    x = "Observation Index",
    y = "Memory Usage (MB)"
  )

normality_results <- dat_data %>%
  group_by(llm_location, model) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(memory_usage_mb)$p.value else NA_real_,
    .groups = "drop"
  )

normality_results %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)


# Plot Remote models
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(memory_usage_mb), y = memory_usage_mb)) +
  geom_point(color = "orange", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Memory Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "Memory Usage (MB)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(memory_usage_mb), y = memory_usage_mb)) +
  geom_point(color = "orange", alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Memory Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "Memory Usage (MB)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


## This was AFTER -- these are the correct functions
# ENERGY USAGE
dat_data %>%
  group_by(llm_location, model, prompt_size) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(energy_usage_joules)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

# Plot Remote models (colored by prompt_size)
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(energy_usage_joules), y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Energy Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "Energy Usage (Joules)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models (colored by prompt_size)
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(energy_usage_joules), y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Energy Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "Energy Usage (Joules)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# CPU USAGE
dat_data %>%
  group_by(llm_location, model, prompt_size) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(cpu_percent)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

# Plot Remote models (colored by prompt_size)
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(cpu_percent), y = cpu_percent, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Percent CPU Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "CPU Usage (%)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models (colored by prompt_size)
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(cpu_percent), y = cpu_percent, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Percent CPU Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "CPU Usage (%)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# MEMORY USAGE
dat_data %>%
  group_by(llm_location, model, prompt_size) %>%
  summarise(
    n = n(),
    shapiro_p = if (n >= 3) shapiro.test(memory_usage_mb)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(normality = ifelse(shapiro_p > 0.05, "normal", "not normal")) %>%
  print(n = Inf)

# Plot Remote models (colored by prompt_size)
remote_data <- dat_data %>%
  filter(llm_location == "remote" & model %in% c("gemma", "llama2", "qwen"))

ggplot(remote_data, aes(x = seq_along(memory_usage_mb), y = memory_usage_mb, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Memory Usage Across Observations (Remote Models)",
    x = "Observation Index",
    y = "Memory Usage (MB)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot Mobile models (colored by prompt_size)
mobile_data <- dat_data %>%
  filter(llm_location == "mobile" & model %in% c("gemma-1b", "llama-1b", "qwen-1.7b"))

ggplot(mobile_data, aes(x = seq_along(memory_usage_mb), y = memory_usage_mb, color = prompt_size)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~ model, scales = "free_x", ncol = 3) +
  labs(
    title = "Memory Usage Across Observations (Mobile Models)",
    x = "Observation Index",
    y = "Memory Usage (MB)",
    color = "Prompt Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

## Used for RQ1 --------------
## Violin Plot for Energy Usage based on remote or mobile and prompt size
plot_energy_violin <- function(data, y_var = "energy_usage_joules") {
  ggplot(data, aes(
    x = llm_location,
    y = .data[[y_var]],
    fill = llm_location
  )) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6) +
    facet_wrap(~ prompt_size, nrow = 1, labeller = labeller(prompt_size = label_both)) +
    scale_fill_manual(values = c("#F28E2B", "#4E79A7")) + # soft orange + blue
    labs(
      x = NULL,
      y = "Energy Usage (J)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(face = "bold")
    )
}

plot_energy_violin(dat_data)

## Latex table generator function
make_summary_latex <- function(data) {
  # Summarize by model, device, and prompt size
  summary_table <- data %>%
    group_by(model, llm_location, prompt_size) %>%
    summarise(
      energy_mean = mean(energy_usage_joules, na.rm = TRUE),
      energy_median = median(energy_usage_joules, na.rm = TRUE),
      energy_sd = sd(energy_usage_joules, na.rm = TRUE),
      cpu_mean = mean(cpu_percent, na.rm = TRUE),
      cpu_median = median(cpu_percent, na.rm = TRUE),
      cpu_sd = sd(cpu_percent, na.rm = TRUE),
      memory_mean = mean(memory_usage_mb, na.rm = TRUE),
      memory_median = median(memory_usage_mb, na.rm = TRUE),
      memory_sd = sd(memory_usage_mb, na.rm = TRUE),
      time_mean = mean(execution_time_sec, na.rm = TRUE),
      time_median = median(execution_time_sec, na.rm = TRUE),
      time_sd = sd(execution_time_sec, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(model, llm_location, prompt_size)
  
  # Round to 2 decimals
  summary_table <- summary_table %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  # Create LaTeX table
  latex_table <- summary_table %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Descriptive statistics for each model, device type, and prompt length.",
      col.names = c(
        "Model", "Location", "Prompt Size",
        "Energy (Mean)", "Energy (Median)", "Energy (SD)",
        "CPU% (Mean)", "CPU% (Median)", "CPU% (SD)",
        "Memory (Mean)", "Memory (Median)", "Memory (SD)",
        "Exec. Time (Mean)", "Exec. Time (Median)", "Exec. Time (SD)"
      ),
      align = "lllrrrrrrrrrrrr"
    ) %>%
    kable_styling(
      latex_options = c("scale_down", "hold_position"),
      font_size = 7
    ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(1:nrow(summary_table), background = rep(c("gray!10", "white"), length.out = nrow(summary_table)))

  # Wrap in resizebox for full-width
  latex_table <- paste0(
    "\\begin{table}[H]\n\\centering\n\\resizebox{\\textwidth}{!}{%\n",
    latex_table,
    "\n}\n\\end{table}\n"
  )
  
  cat(latex_table)
  invisible(latex_table)
}

make_summary_latex(dat_data)


## Bar Chart for RQ1 ---------------------
dat_data <- dat_data %>%
  mutate(model = factor(model, levels = c("gemma", "gemma-1b", 
                                          "llama2", "llama-1b", 
                                          "qwen", "qwen-1.7b")))

energy_summary <- dat_data %>%
  group_by(model, llm_location) %>%
  summarise(mean_energy = mean(energy_usage_joules, na.rm = TRUE), .groups = "drop")

ggplot(energy_summary, aes(x = model, y = mean_energy, fill = llm_location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("remote" = "#0072B2", "mobile" = "#E69F00"),
    name = "Model location",
    labels = c("On-device", "Remote")
  ) +
  labs(
    title = "Energy Usage by Model and Location",
    x = "Model",
    y = "Mean Energy Usage (Joules)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

dat_data %>%
  filter(model == "llama-1b") %>%
  summarise(
    total_rows = n(),
    missing_energy = sum(is.na(energy_usage_joules))
  )

# ---------------
## RQ 2

## Violin Plot for execution time based on remote or mobile and prompt size
plot_energy_violin <- function(data, y_var = "execution_time_sec") {
  ggplot(data, aes(
    x = llm_location,
    y = .data[[y_var]],
    fill = llm_location
  )) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6) +
    facet_wrap(~ prompt_size, nrow = 1, labeller = labeller(prompt_size = label_both)) +
    scale_fill_manual(values = c("#F28E2B", "#4E79A7")) + # soft orange + blue
    labs(
      x = NULL,
      y = "Execution Time (sec)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(face = "bold")
    )
}

plot_energy_violin(dat_data)

## Bar chart Q2
dat_data <- dat_data %>%
  mutate(model = factor(model, levels = c("gemma", "gemma-1b", 
                                          "llama2", "llama-1b", 
                                          "qwen", "qwen-1.7b")))

execution_time_summary <- dat_data %>%
  group_by(model, llm_location) %>%
  summarise(mean_time = mean(execution_time_sec, na.rm = TRUE), .groups = "drop")

ggplot(execution_time_summary, aes(x = model, y = mean_time, fill = llm_location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("remote" = "#0072B2", "mobile" = "#E69F00"),
    name = "Model location",
    labels = c("On-device", "Remote")
  ) +
  labs(
    title = "Execution Time by Model and Location",
    x = "Model",
    y = "Mean Execution Time (Seconds)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# --------------------
## RQ3 and RQ4

# CPU usage plot
p_cpu <- ggplot(dat_data, aes(x = llm_location, y = cpu_percent, fill = llm_location)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("on_device" = "#4575b4", "remote" = "#d73027")) +
  labs(x = NULL, y = "CPU Usage (%)", title = "CPU Usage") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

p_cpu

# Memory usage plot
p_memory <- ggplot(dat_data, aes(x = llm_location, y = memory_usage_mb, fill = llm_location)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("on_device" = "#4575b4", "remote" = "#d73027")) +
  labs(x = NULL, y = "Memory Usage (MB)", title = "Memory Usage") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

p_memory

p_cpu + p_memory

## ---------------------------
## RQ5

# By LLM Location
p_cpu_energy <- ggplot(dat_data, aes(x = cpu_percent, y = energy_usage_joules, color = llm_location)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Energy Consumption vs CPU Usage",
    x = "CPU Usage (%)",
    y = "Energy Consumption (J)"
  ) +
  scale_color_manual(name = "LLM Location", values = c("remote" = "#d73027", "mobile" = "#4575b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13), 
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_cpu_energy

p_memory_energy <- ggplot(dat_data, aes(x = memory_usage_mb, y = energy_usage_joules, color = llm_location)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Energy Consumption vs Memory Usage",
    x = "Memory Usage (MB)",
    y = "Energy Consumption (J)"
  ) +
  scale_color_manual(name = "LLM Location", values = c("remote" = "#d73027", "mobile" = "#4575b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13), 
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_memory_energy

p_cpu_energy + p_memory_energy


# By Prompt Size
p_cpu_energy <- ggplot(dat_data, aes(x = cpu_percent, y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Energy Consumption vs CPU Usage",
    x = "CPU Usage (%)",
    y = "Energy Consumption (J)"
  ) +
  scale_color_manual(name = "Prompt Size", values = c("small" = "#d73027", "large" = "#4575b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13), 
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_cpu_energy

p_memory_energy <- ggplot(dat_data, aes(x = memory_usage_mb, y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Energy Consumption vs Memory Usage",
    x = "Memory Usage (MB)",
    y = "Energy Consumption (J)"
  ) +
  scale_color_manual(name = "Prompt Size", values = c("small" = "#d73027", "large" = "#4575b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13), 
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_memory_energy

p_cpu_energy + p_memory_energy

## --------------------
## RQ6

ggplot(dat_data, aes(x = factor(prompt_size, levels = c("small", "large")), 
                     y = energy_usage_joules, fill = llm_location)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +
  labs(
    title = "Energy Consumption by Content Size and LLM Location",
    x = "Content Size",
    y = "Energy Usage (J)",
    fill = "LLM Location"
  ) +
  scale_fill_manual(values = c("remote" = "#d73027", "mobile" = "#4575b4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, face = "bold"),
    axis.title = element_text(size = 8.5),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8.5),
    legend.text = element_text(size = 8)
  )

dat_data <- dat_data %>%
  mutate(model = factor(model, levels = c("gemma", "gemma-1b", 
                                          "llama2", "llama-1b", 
                                          "qwen", "qwen-1.7b")))

# Summarize mean energy by model and prompt size
energy_prompt_summary <- dat_data %>%
  group_by(model, prompt_size) %>%
  summarise(mean_energy = mean(energy_usage_joules, na.rm = TRUE), .groups = "drop")

ggplot(energy_prompt_summary, aes(x = model, y = mean_energy, fill = prompt_size)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("small" = "#56B4E9", "large" = "#D55E00"),
    name = "Prompt Size",
    labels = c("Large Prompt", "Small Prompt")
  ) +
  labs(
    title = "Mean Energy Consumption by Model and Prompt Size",
    x = "Model",
    y = "Mean Energy Usage (Joules)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )