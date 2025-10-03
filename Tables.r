install.packages(c("tidyverse", "ggplot2", "bestNormalize", "ARTool", "lmerTest", "xtable"))
library(tidyverse)
library(ggplot2)
library(bestNormalize)
library(ARTool)
library(lmerTest)
library(xtable)

install.packages("ggplot2", type = "binary")
install.packages("tidyverse", type = "binary")

# Before running, edit the following line to path where Repo is located on your device:
setwd("Y1P1/GreenLabProject")

dat_data <- read.csv("run_table.csv") %>%
  select(run, energy_usage_joules, execution_time_sec, input_size_kb, memory_usage_mb, output_size_kb, cpu_percent, prompt_size, model, api, mobile) %>%
  mutate(
    run = factor(run),
    energy_usage_joules = factor(energy_usage_joules),
    execution_time_sec = factor(execution_time_sec),
    input_size_kb = factor(input_size_kb),
    memory_usage_mb = factor(memory_usage_mb),
    output_size_kb = factor(output_size_kb),
    cpu_percent = factor(cpu_percent),
    prompt_size = factor(prompt_size),
    model = factor(model),
    api = factor(api),
    mobile = factor(mobile)
  )

glimpse(dat_data)
summary(dat_data)
head(dat_data)

str(dat_data$energy_usage_joules)

# Histogram of overall energy usage -- this actually lowkey has no meaning for us :)
dat_data <- dat_data %>%
  mutate(energy_usage_joules = as.numeric(as.character(energy_usage_joules)))
ggplot(dat_data, aes(x = energy_usage_joules)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Histogram of Energy Usage",
    x = "Energy Usage",
    y = "Frequency"
  )

# Histogram split by on-device v. remote -- again not sure histogram is the chart to use 
ggplot(dat_data, aes(x = energy_usage_joules, fill = mobile)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  theme_minimal() +
  labs(
    title = "Energy Usage Distribution by Setting",
    x = "Energy Usage",
    y = "Frequency",
    fill = "On-device LLM?"
  )

# Boxplots of energy_usage grouped by library and dataframe_size to compare distributions across experimental conditions.
  theme_minimal() +
  labs(
    title = "Energy Usage by Library and Dataframe Size",
    x = "Library",
    y = "Energy Usage",
    fill = "Dataframe Size"
  )

# Scatterplots of energy_usage across trials. Assign different colors to the combinations of library and dataframe_size 
# (i.e., the “experiments”), and include a legend to distinguish them.
# Define "experiment" as library + dataframe_size
dat_data <- dat_data %>%
  mutate(experiment = interaction(library, dataframe_size))

ggplot(dat_data, aes(x = trial, y = energy_usage, color = experiment)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_minimal() +
  labs(
    title = "Energy Usage Across Trials",
    x = "Trial",
    y = "Energy Usage",
    color = "Experiment"
  )

# Run Shapiro-Wilk for each group
normality_results <- dat_data %>%
  group_by(library, dataframe_size) %>%
  summarise(
    shapiro_p = shapiro.test(energy_usage)$p.value,
    .groups = "drop"
  )

normality_results
# Interpretation: if p < 0.05, reject normality (data is not normal).

# Normalize the energy_usage column using the bestNormalize package and create a new normalized column
bn <- bestNormalize(dat_data$energy_usage)
dat_data$norm_energy_usage <- bn$x.t

# Plot the normalized data and repeat normality tests.
# Histogram of normalized values
ggplot(dat_data, aes(x = norm_energy_usage)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(
    title = "Histogram of Normalized Energy Usage",
    x = "Normalized Energy Usage",
    y = "Frequency"
  )
# Shapiro-Wilk again for normalized values
normality_results_norm <- dat_data %>%
  group_by(library, dataframe_size) %>%
  summarise(
    shapiro_p = shapiro.test(norm_energy_usage)$p.value,
    .groups = "drop"
  )
normality_results_norm

# Evaluate whether parametric tests can be applied after normalization
# If most *shapiro_p* values after normalization are greater than 0.05, then distributions can be considered normal, and parametric tests (ANOVA, t-test, LMER) are justified.#
#  If many are still below 0.05, you should stick to non-parametric tests (Wilcoxon, ART).

# Use a non-parametric test wilcox.test (Wilcoxon’s test) to check whether energy usage is different between libraries for small dataframes
pandas_small = dat_data[dat_data$dataframe_size == 'Small' & dat_data$library == 'Pandas',]$energy_usage
polars_small = dat_data[dat_data$dataframe_size == 'Small' & dat_data$library == 'Polars',]$energy_usage
wilcox.test(pandas_small, polars_small)

pandas_big = dat_data[dat_data$dataframe_size == 'Big' & dat_data$library == 'Pandas',]$energy_usage
polars_big = dat_data[dat_data$dataframe_size == 'Big' & dat_data$library == 'Polars',]$energy_usage
wilcox.test(pandas_big, polars_big)
# We shall use Wilcoxon test; it's non-parametric and we can't use a parametric test as our data is not normal.

# Fit an art() model to observe influence of library, dataframe_size and its interactions with energy_used. 
# Treat trial as a random effect for repeated measures experiment. What can you see? 
# Validate your model with summary(model).
library(ARTool)

model = art(energy_usage ~ library * dataframe_size + Error(trial), data = dat_data)
anova(model)
# To check if model is correct we simpy run command `summary(model)` and investigate whether all F-values are 0.
# `anova(model)` shows us information about influence of each variable and their interactions.
summary(model)

# Similarly to art, run lmer() model to observe similar effects, except your target variable should be norm_energy_used
library(lmerTest)
model = lmer(norm_energy_usage ~ library * dataframe_size + (1|trial), data = dat_data)
anova(model)

# Check residuals for normality and assess the validity of the model
qqnorm(resid(model))
qqline(resid(model))
shapiro.test(resid(model))
# We can see residuals are not normal, which might be the result that our data doesn't meet the conditional normality assumption, even though it's distribution with no regard to factors looks normal.

# Create a boxplot to visualize the distribution of energy_usage for each “experiment”. 
# Use facet_wrap() to adequately separate the experiments by library or dataframe size.
# Label the x-axis as “Dataframe Size” and the y-axis as “Energy Usage”
p_boxplot <- ggplot(dat_data, aes(x = dataframe_size, y = energy_usage, fill = library)) +
  geom_boxplot() +
  facet_wrap(~library) +
  theme_minimal() +
  labs(
    title = "Energy Usage by Library and Dataframe Size",
    x = "Dataframe Size",
    y = "Energy Usage",
    fill = "Library"
    ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
    )

p_boxplot

# Create a scatter plot (geom point()) to show how energy_usage varies over trial.
# Assign a unique color to each “experiment” and add a legend.
ggplot(dat_data, aes(x = trial, y=energy_usage, color = interaction(library,dataframe_size))) +
  geom_point(alpha = 0.75) +
  labs(
    title = "Energy Usage by Experiment Trial",
    x = "Trial",
    y = "Energy Usage",
    color = "Experiment"
  )

# Using faceting, create a grid with 4 scatter plots (one per “experiment”) showing how energy_usage correlates to execution_time, Including trend lines.
ggplot(dat_data, aes(x = execution_time, y = energy_usage)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(library ~ dataframe_size) +
  labs(
    title = "Energy Usage by Execution Time",
    x = "Execution Time",
    y = "Energy Usage"
  )

# Use dplyr functions to calculate the average cpu_usage, memory_usage and energy_usage for each “experiment"
# Load required libraries
# Using the xtable package, format the summary table as LaTeX. 
# You can copy the output for rendering in engines such as Overleaf.
library(xtable)
library(dplyr)

# Create summary statistics
summary_stats <- dat_data %>%
  group_by(library, dataframe_size) %>%
  summarise(
    cpu_avg = mean(cpu_usage),
    memory_avg = mean(memory_usage),
    energy_avg = mean(energy_usage),
    .groups = 'drop'
  )

# Create and format the xtable
summary_table <- xtable(summary_stats,
                       caption = "Summary Statistics by Library and DataFrame Size",
                       digits = c(0, 0, 0, 2, 2, 2))

# Print the table (you can adjust the type of output as needed)
print(summary_table, 
      include.rownames = TRUE,
      floating = FALSE)