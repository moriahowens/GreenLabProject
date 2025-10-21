## This is the code that makes the big Table 2 and all plots
# ChatGPT was used for coding assistance

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

# You will need to add the path below if you are not in the Green Lab Project repo folder

## Load and Parse all data ---------------------------------

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

## Get descriptive stats -----------------------------------------

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

## Identify outliers! ---------------------------------------------------

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


## Attempt normalization ---------------------------------------------
vars_to_check <- c(
  "energy_usage_joules",
  "execution_time_sec",
  "memory_usage_mb",
  "cpu_percent"
)

# Check pre-occuring normality with Shapiro Wilk
check_normality <- function(x) {
  x <- na.omit(x)
  if (length(unique(x)) < 3) return(NA)
  return(shapiro.test(x)$p.value)
}

# Loop through variables and conditions to attempt normalization
for (var in vars_to_check) {
  cat("Variable:", var, "\n")

  for (m in unique(dat_data$model)) {
    for (p in unique(dat_data$prompt_size)) {

      subset_data <- dat_data %>%
        filter(model == m, prompt_size == p) %>%
        pull(var)

      if (length(subset_data) < 3 || all(is.na(subset_data))) next

      p_original <- check_normality(subset_data)

      cat("\nModel:", m, ", Prompt size:", p, "\n")
      cat("Original normality p =", round(p_original, 4), "\n")

      # Only test transformations if non-normal
      if (!is.na(p_original) && p_original < 0.05) {

        transformations <- list(
          log = function(x) if (all(x > 0)) log(x) else NA,
          sqrt = function(x) if (all(x >= 0)) sqrt(x) else NA,
          square = function(x) x^2,
          cube = function(x) x^3
        )

        for (t_name in names(transformations)) {
          transformed <- suppressWarnings(transformations[[t_name]](subset_data))
          if (all(is.na(transformed))) {
            cat(t_name, ": Failed transformation)\n")
          } else {
            p_trans <- check_normality(transformed)
            if (!is.na(p_trans)) {
              cat("Normalized by: ", t_name, ", p =", p_trans,
                  ifelse(p_trans > 0.05, "Normalizable\n", "Still non-normal\n"))
            }
          }
        }

      } else if (!is.na(p_original)) {
        cat("Already normal (p > 0.05)\n")
      }
    }
  }
}

## Used for RQ1 --------------
## Violin Plot for Energy Usage based on remote or mobile and prompt size ----------------------------
plot_energy_violin <- function(data, y_var = "energy_usage_joules") {
  data <- data %>%
    mutate(
      llm_location = factor(llm_location, levels = c("remote", "mobile"), labels = c("Remote", "On-device")),
      prompt_size = factor(prompt_size, levels = c("small", "large"))
    )
  
  ggplot(data, aes(
    x = llm_location,
    y = .data[[y_var]],
    fill = llm_location
  )) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6) +
    facet_wrap(
      ~ prompt_size,
      nrow = 1,
      labeller = labeller(
        prompt_size = c(
          small = "Prompt Size: Small",
          large = "Prompt Size: Large"
        )
      )
    ) +
    scale_fill_manual(values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")) +
    labs(
      x = NULL,
      y = "Energy Usage (Joules)"
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

## Latex table generator function -----------------------------------------------------------------
# Summarize by model, device, and prompt size
make_summary_latex <- function(data) {
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
  
  summary_table <- summary_table %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
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

  latex_table <- paste0(
    "\\begin{table}[H]\n\\centering\n\\resizebox{\\textwidth}{!}{%\n",
    latex_table,
    "\n}\n\\end{table}\n"
  )
  
  cat(latex_table)
  invisible(latex_table)
}

make_summary_latex(dat_data)

## Bar Chart for RQ1 ----------------------------------------------------------------------------
dat_data <- dat_data %>%
  mutate(model = recode(model, "gemma" = "Remote Gemma", "gemma-1b" = "On-Device Gemma", "llama2" = "Remote Llama", "llama-1b" = "On-Device Llama", "qwen" = "Remote Qwen", "qwen-1.7b" = "On-Device Qwen")) %>%
  mutate(model = factor(model, levels = c("Remote Gemma", "On-Device Gemma",  "Remote Llama", "On-Device Llama", "Remote Qwen", "On-Device Qwen")))

energy_summary <- dat_data %>%
  group_by(model, llm_location) %>%
  summarise(mean_energy = mean(energy_usage_joules, na.rm = TRUE), .groups = "drop")

ggplot(energy_summary, aes(x = model, y = mean_energy, fill = llm_location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("mobile" = "#D81B60", "remote" = "#1E88E5"),
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

## Violin Plot for execution time based on remote or mobile and prompt size ---------------------
plot_energy_violin <- function(data, y_var = "execution_time_sec") {
  data <- data %>%
    mutate(
      llm_location = factor(llm_location, levels = c("remote", "mobile"), labels = c("Remote", "On-device")),
      prompt_size = factor(prompt_size, levels = c("small", "large"))
    )
  
  ggplot(data, aes(
    x = llm_location,
    y = .data[[y_var]],
    fill = llm_location
  )) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6) +
    facet_wrap(
      ~ prompt_size,
      nrow = 1,
      labeller = labeller(
        prompt_size = c(
          small = "Prompt Size: Small",
          large = "Prompt Size: Large"
        )
      )
    ) +
    scale_fill_manual(values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")) +
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

# Noted here that the violins for execution time and energy use looked remarkably similar, 
# so performed this as a check to make sure nothing had been accidentally overwritten.
summary(dat_data$energy_usage_joules)
summary(dat_data$execution_time_sec)

## Bar chart Q2 -------------------------------------------------------
dat_data <- dat_data %>%
  mutate(model = recode(model, "gemma" = "Remote Gemma", "gemma-1b" = "On-Device Gemma", "llama2" = "Remote Llama", "llama-1b" = "On-Device Llama", "qwen" = "Remote Qwen", "qwen-1.7b" = "On-Device Qwen")) %>%
  mutate(model = factor(model, levels = c("Remote Gemma", "On-Device Gemma",  "Remote Llama", "On-Device Llama",  "Remote Qwen", "On-Device Qwen")))

execution_time_summary <- dat_data %>%
  group_by(model, llm_location) %>%
  summarise(mean_time = mean(execution_time_sec, na.rm = TRUE), .groups = "drop")

ggplot(execution_time_summary, aes(x = model, y = mean_time, fill = llm_location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("mobile" = "#D81B60", "remote" = "#1E88E5"),
    name = "Model location",
    breaks = c("mobile", "remote"),
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
    legend.justification = "center",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


# --------------------
## RQ3 and RQ4

# CPU usage plot ------------------------------------
p_cpu <- dat_data %>%
  mutate(
    llm_location = factor(
      llm_location,
      levels = c("remote", "mobile"),
      labels = c("Remote", "On-device")  # capitalized
    )
  ) %>%
  ggplot(aes(x = llm_location, y = cpu_percent, fill = llm_location)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")) +
  labs(
    x = NULL,
    y = "CPU Usage (%)",
    title = "CPU Usage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_cpu

# Memory usage plot -----------------------------------------------
p_memory <- dat_data %>%
  mutate(
    llm_location = factor(
      llm_location,
      levels = c("remote", "mobile"),
      labels = c("Remote", "On-device")
    )
  ) %>%
  ggplot(aes(x = llm_location, y = memory_usage_mb, fill = llm_location)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")) +
  labs(
    x = NULL,
    y = "Memory Usage (MB)",
    title = "Memory Usage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_memory

p_cpu + p_memory

## ---------------------------
## RQ5

# By LLM Location -------------------------------------------
p_cpu_energy <- dat_data %>%
  mutate(
    llm_location = factor(
      llm_location,
      levels = c("remote", "mobile"),
      labels = c("Remote", "On-device")
    )
  ) %>%
  ggplot(aes(x = cpu_percent, y = energy_usage_joules, color = llm_location)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(
    name = "LLM Location",
    values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")
  ) +
  labs(
    title = "Energy Consumption vs CPU Usage",
    x = "CPU Usage (%)",
    y = "Energy Consumption (J)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_cpu_energy

p_memory_energy <- dat_data %>%
  mutate(
    llm_location = factor(
      llm_location,
      levels = c("remote", "mobile"),
      labels = c("Remote", "On-device")
    )
  ) %>%
  ggplot(aes(x = memory_usage_mb, y = energy_usage_joules, color = llm_location)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(
    name = "LLM Location",
    values = c("Remote" = "#1E88E5", "On-device" = "#D81B60")
  ) +
  labs(
    title = "Energy Consumption vs Memory Usage",
    x = "Memory Usage (MB)",
    y = "Energy Consumption (J)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_memory_energy

p_cpu_energy + p_memory_energy


# By Prompt Size ---------------------------------------

p_cpu_energy <- dat_data %>%
  mutate(
    prompt_size = factor(
      prompt_size,
      levels = c("small", "large"),
      labels = c("Small", "Large")
    )
  ) %>%
  ggplot(aes(x = cpu_percent, y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(
    name = "Prompt Size",
    values = c("Small" = "#FFC107", "Large" = "#004D40")
  ) +
  labs(
    title = "Energy Consumption vs CPU Usage",
    x = "CPU Usage (%)",
    y = "Energy Consumption (J)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_cpu_energy

p_memory_energy <- dat_data %>%
  mutate(
    prompt_size = factor(
      prompt_size,
      levels = c("small", "large"),
      labels = c("Small", "Large")
    )
  ) %>%
  ggplot(aes(x = memory_usage_mb, y = energy_usage_joules, color = prompt_size)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(
    name = "Prompt Size",
    values = c("Small" = "#FFC107", "Large" = "#004D40")
  ) +
  labs(
    title = "Energy Consumption vs Memory Usage",
    x = "Memory Usage (MB)",
    y = "Energy Consumption (J)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_memory_energy

p_cpu_energy + p_memory_energy

## --------------------
## RQ6
# Violin -------------------------------------------------------
p_energy_prompt_llm <- ggplot(
  dat_data, 
  aes(x = factor(prompt_size, levels = c("small", "large")), 
      y = energy_usage_joules, 
      fill = llm_location)
) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, alpha = 0.7) +
  labs(
    title = "Energy Consumption by Content Size and LLM Location",
    x = "Content Size",
    y = "Energy Usage (J)",
    fill = "LLM Location"
  ) +
  scale_x_discrete(labels = c("Small", "Large")) +
  scale_fill_manual(
    values = c("mobile" = "#4575b4", "remote" = "#d73027"),
    labels = c("On-device", "Remote")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_energy_prompt_llm

# Bar -----------------------------------------------------------
dat_data <- dat_data %>%
  mutate(model = recode(model, "gemma" = "Remote Gemma", "gemma-1b" = "On-Device Gemma", "llama2" = "Remote Llama", "llama-1b" = "On-Device Llama", "qwen" = "Remote Qwen", "qwen-1.7b" = "On-Device Qwen")) %>%
  mutate(model = factor(model, levels = c("Remote Gemma", "On-Device Gemma",  "Remote Llama", "On-Device Llama",  "Remote Qwen", "On-Device Qwen")))

energy_prompt_summary <- dat_data %>%
  group_by(model, prompt_size) %>%
  summarise(mean_energy = mean(energy_usage_joules, na.rm = TRUE), .groups = "drop")

p_energy_prompt_bar <- ggplot(energy_prompt_summary, aes(x = model, y = mean_energy, fill = prompt_size)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "white", width = 0.7) +
  scale_fill_manual(
    values = c("small" = "#FFC107", "large" = "#004D40"),
    name = "Prompt Size",
    labels = c("Small", "Large")
  ) +
  labs(
    title = "Mean Energy Consumption by Model and Prompt Size",
    x = "Model",
    y = "Mean Energy Usage (J)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_energy_prompt_bar