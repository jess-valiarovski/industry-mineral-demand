UMN Applied Biostatistics Project: Clean Energy Industries and Mineral
Demand
================
Jess Valiarovski
2025-01-21

# Introduction

This report analyzes mineral demand across various industry sectors
using data from the **IEA Critical Minerals Dataset** ([IEA
Website](https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset)).
See README for more info  
License: **CC BY 4.0**

# Abbreviations

- **MD** = Mineral Demand  
- **sec** = Industry Sector  
- **clm** = Column

# Load Required Libraries

``` r
library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(tidyr)      # Data tidying
library(readxl)     # Read Excel files
library(forcats)    # Factor handling
library(janitor)    # Data cleaning
library(tidyverse)  # Data science suite
library(patchwork)  # Combining plots
library(stringr)    # String manipulation
library(broom)      # Linear Model summary
library(car)        # ANOVA
library(emmeans)    # Pairwise comparison
library(infer)      # permutation
library(multcomp)   # Post Hoc
library(multcompView) # Post Hoc
library(stringr)     # Splice character strings
```

# Load Dataset

``` r
multiplesheets <- function(fname) {
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  names(data_frame) <- sheets
  return(data_frame)
}

path <- "CM_Data_Explorer May 2024.xlsx"
energy <- multiplesheets(path)
```

# Data Cleaning and Transformation

``` r
MD_sec <- clean_names(energy[[4]])
#source: https://forum.posit.co/t/rename-columns-using-vector-of-names/181267
tidy_clm <- (c("Demand_by_sector","year_2023","empty1","Stated_Policies_Scenario_2030","year_2035",
               "year_2040","year_2045","year_2050","empty2","Announced_Pledges_Scenario_2030","year_2035.1","year_2040.1","year_2045.1","year_2050.1",
               "empty3","Net_Zero_Emissions_Scenario_2030","year_2035.2","year_2040.2","year_2045.2","year_2050.2"))
empty_clm <- c("empty1","empty2","empty3")
MD_sec <- MD_sec %>%
  setNames(tidy_clm) %>% #rename columns
  drop_na(`Demand_by_sector`) %>% #drop rows with no data
  dplyr::select(-any_of(empty_clm)) #remove spacer columns
MD_sec[,-1] <- apply(MD_sec[,-1],2,as.numeric) #convert response variable to numeric
MD_sec[,-1] <- round(MD_sec[,-1], digits = 3) #round the values to the 3rd digit
```

``` r
sectors <- c("Other low emissions power generation", "Solar PV", "Low emissions power generation",
             "Hydrogen technologies", "Grid battery storage", "Electricity networks", "Electric vehicles") # list of clean energy industries 

industry_MD <- subset(MD_sec, MD_sec$Demand_by_sector %in% sectors) %>%
  pivot_longer(cols = -Demand_by_sector, names_to = "year", values_to = "demand_kiloton_energy") %>%
  mutate(time = as.numeric(str_extract(year, "[0-9]{4}")),
         scenario = case_when(
           str_detect(year, "Announced_Pledges") ~ "Announced_Pledges",
           str_detect(year, "Net_Zero_Emissions") ~ "Net_Zero_Emissions",
           str_detect(year, "_2023") ~ "Now",
           TRUE ~ "Stated_Policies"
         )) %>%
   dplyr::select(-any_of("year"))
```

# Data Exploration

``` r
energy_summary <- industry_MD %>%  
  filter(scenario == "Now") %>%  
  group_by(Demand_by_sector) %>%  
  summarise(
    mean_energy = mean(demand_kiloton_energy, na.rm = TRUE),  # Mean mineral demand
    sd_energy = sd(demand_kiloton_energy, na.rm = TRUE),  # Standard deviation
    count = n(),  # Number of samples
    .groups = "drop"
  )



ggplot(industry_MD, aes(x = reorder(Demand_by_sector, demand_kiloton_energy, FUN = median), 
                        y = demand_kiloton_energy, 
                        fill = Demand_by_sector)) +
  geom_violin(trim = TRUE, alpha = 0.5, show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3.5, fill = "white", color = "black") +  # mean mineral demand
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5, size = 1, color = "black") +  # error bars
  scale_y_log10() +  # Normalize magnitudes with logarithm y-scale
  labs(title = "Violin Plot of Mineral Demand Across Sectors",
       subtitle = "Raw Data with Mean and Confidence Intervals",
       x = "Demand by Sector", 
       y = "Mineral Demand (Kiloton)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right")
```

<figure>
<img src="linear-model_files/figure-gfm/data-summary-1.png"
alt="Raw Data Distribution" />
<figcaption aria-hidden="true">Raw Data Distribution</figcaption>
</figure>

``` r
energy_summary
```

    ## # A tibble: 7 × 4
    ##   Demand_by_sector                     mean_energy sd_energy count
    ##   <chr>                                      <dbl>     <dbl> <int>
    ## 1 Electric vehicles                        200.      242.        8
    ## 2 Electricity networks                    4143.       NA         1
    ## 3 Grid battery storage                      21.9      31.6       7
    ## 4 Hydrogen technologies                      0.241     0.476     4
    ## 5 Low emissions power generation             0.016    NA         1
    ## 6 Other low emissions power generation      57.7      59.0       7
    ## 7 Solar PV                                 389.      597.        6

``` r
print(nrow(industry_MD %>%  
  filter(scenario == "Now")))
```

    ## [1] 34

The sample size is small at a total of 34 samples. The sampling from
this dataset is very variable across sectors with category mean ranges
from e-02 to e+04. The standard deviation for solar PV, grid battery
storage, and hydrogen technologies is large relative to its mean and
electricity network and low emissions power generation had a standard
deviation too low to report.

Electricity networks and low emissions power generation have only 1
sample representing the entire industry and will therefore be excluded
from data analysis.

# Linear Analysis

### Logirthmatically transform data to correct residual variance

``` r
# Linear Model
model_org <- lm(demand_kiloton_energy ~ Demand_by_sector, data = industry_MD %>% filter(scenario == "Now"))

# Extract residuals and sector information

residuals_data <- industry_MD %>%
  filter(scenario == "Now") %>%
  mutate(residuals = model_org$residuals)

# Plot QQ plots
resid_plot <- ggplot(residuals_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Demand_by_sector, scales = "free") + 
  theme_minimal() +
  labs(
    title = "Original QQ Plots for Each Industry",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
```

Because our data shows a lack of homoscedasticity across categories, we
do a logarithmic transform to minimize residual variability and make our
type 2 ANOVA statistical test assumptions valid.

### Log Transformation for Normality

``` r
log_industry_MD <- industry_MD %>%
  mutate(demand_kiloton_energy = log(demand_kiloton_energy + 1)) %>%
  filter(scenario == "Now")

# Filter out any levels with fewer than two observations 
log_industry_MD_filtered <- log_industry_MD %>%
  filter(scenario == "Now") %>%
  group_by(Demand_by_sector) %>%
  filter(n() > 1) 
```

Check QQ residual plots for improved homoscedasticity

``` r
# Fit the linear model
model <- lm(demand_kiloton_energy ~ Demand_by_sector, data = log_industry_MD_filtered)

# Compute residuals and merge back
residuals_data <- log_industry_MD_filtered %>%
  ungroup() %>%  # Ensure residuals are assigned correctly
  mutate(residuals = residuals(model))  # Compute residuals

# Create QQ plot
log_resid_plot <- ggplot(residuals_data, aes(sample = residuals)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ Demand_by_sector, scales = "free") +
  theme_minimal() +
  labs(title = "QQ Plots for Each Industry (Log Transformed & n > 1)", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")

# Display the plot
log_resid_plot
```

![](linear-model_files/figure-gfm/log-qq-plot-1.png)<!-- -->

``` r
resid_plot
```

![](linear-model_files/figure-gfm/log-qq-plot-2.png)<!-- -->

Residual variability has decreased & the data is ready for ANOVA type 2
test.

### ANOVA Analysis

``` r
Anova(model)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: demand_kiloton_energy
    ##                  Sum Sq Df F value  Pr(>F)  
    ## Demand_by_sector 53.063  4  3.6656 0.01647 *
    ## Residuals        97.714 27                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The type 2 ANOVA test result tells us mineral demand can be predicted by
the identity of the industry, with a sum of squares value of 53.063 and
degrees of freedom of 4 with an unexplained variance of 97.714 in
residual value (df = 27). The F value of this model is 3.6656 and the
p-value of observing this F value is 0.01647, revealing that the
industry sector identity is statistically significant (under the 5% null
threshold) in determining differences in mineral demand.

## Pairwise Comparisons

``` r
model_emmeans <- emmeans(model, ~ Demand_by_sector)
model_pairwise <- contrast(model_emmeans, method = "pairwise") %>%
  tidy()

model_pairwise <- model_pairwise %>%
  arrange(adj.p.value) %>%  # Sort from lowest to highest p-value
  mutate(cohens_d = abs(estimate / sd(log_industry_MD_filtered$demand_kiloton_energy))) %>%
  dplyr::select(-c(term,null.value))  # Remove the "term" column

print(model_pairwise, width = Inf)
```

    ## # A tibble: 10 × 7
    ##    contrast                                                     estimate
    ##    <chr>                                                           <dbl>
    ##  1 Electric vehicles - Hydrogen technologies                      4.27  
    ##  2 Hydrogen technologies - Other low emissions power generation  -3.12  
    ##  3 Hydrogen technologies - Solar PV                              -3.02  
    ##  4 Electric vehicles - Grid battery storage                       2.17  
    ##  5 Grid battery storage - Hydrogen technologies                   2.09  
    ##  6 Electric vehicles - Solar PV                                   1.25  
    ##  7 Electric vehicles - Other low emissions power generation       1.15  
    ##  8 Grid battery storage - Other low emissions power generation   -1.02  
    ##  9 Grid battery storage - Solar PV                               -0.927 
    ## 10 Other low emissions power generation - Solar PV                0.0963
    ##    std.error    df statistic adj.p.value cohens_d
    ##        <dbl> <dbl>     <dbl>       <dbl>    <dbl>
    ##  1     1.16     27    3.66       0.00869   1.94  
    ##  2     1.19     27   -2.61       0.0958    1.41  
    ##  3     1.23     27   -2.46       0.130     1.37  
    ##  4     0.985    27    2.21       0.207     0.986 
    ##  5     1.19     27    1.76       0.419     0.950 
    ##  6     1.03     27    1.21       0.744     0.565 
    ##  7     0.985    27    1.17       0.769     0.522 
    ##  8     1.02     27   -1.01       0.850     0.464 
    ##  9     1.06     27   -0.876      0.903     0.420 
    ## 10     1.06     27    0.0910     1.00      0.0437

``` r
sig_groups <- cld(model_emmeans, adjust = "holm", Letters = "abcd", sort = TRUE, reversed = TRUE)
```

The only grouping that was found to be statistically significant in the
difference of the amount of minerals demanded in the 2023 year is
electric vehicles and hydrogen technologies (t-test= 3.66, p = 0.00869
\< 0.05). We have high confidence evidence to reject the null hypothesis
that electric vehicles and hydrogen technologies differ in how much
minerals they demanded for in 2023. The effect size is moderate with a
Cohen’s d of 0.877 for this pair comparison.

# Post Hoc Holm-Šídák Multiple Comparsion Test

``` r
filtered_data <- log_industry_MD_filtered 

sector_order <- filtered_data %>%
  group_by(Demand_by_sector) %>%
  summarise(mean_demand = mean(demand_kiloton_energy, na.rm = TRUE)) %>%
  arrange(mean_demand) %>%
  pull(Demand_by_sector)

filtered_data <- filtered_data %>%
  mutate(Demand_by_sector = factor(Demand_by_sector, levels = sector_order))

sig_groups <- sig_groups %>%
  filter(Demand_by_sector %in% unique(filtered_data$Demand_by_sector)) %>% 
  mutate(Demand_by_sector = factor(Demand_by_sector, levels = sector_order))


# Create plot
pairwise_plot <- ggplot(filtered_data, aes(x = Demand_by_sector, y = demand_kiloton_energy, color = Demand_by_sector)) +
  geom_jitter(height = 0, width = .2, size = 4, alpha = .7, show.legend = FALSE) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2,
               position = position_nudge(x = .4), show.legend = FALSE) +
  geom_text(data = sig_groups, aes(label = .group, y = 4.5), size = 8, color = "black") +
  labs(title = "Significance Groupings in Industry Mineral Demand",
       x = "Demand by sector", y = "Energy demand (log kiloton)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Because the dataset has weak statistical power from its low sample size
(n=33), we will increase the sample size through bootstrapping to better
estimate the true distributions of mineral demand across industries. We
resample the dataset 1000 times by bootstrapping and summarize the
results by taking the mean of the resampling bootstrap simulation. From
bootstrapping we determine the 95% confidence for each industry,
quantitatively revealing the variance inside each sample within an
industry sector. We will then reassess our hypothesis.

# Bootstrapped sampling distribution of energy demand across sectors

``` r
# Order industries on graph based on mineral demand
sector_order <- log_industry_MD_filtered  %>%
  filter(scenario == "Now") %>%
  group_by(Demand_by_sector) %>%
  summarise(mean_demand = mean(demand_kiloton_energy)) %>%
  arrange(mean_demand) %>%
  pull(Demand_by_sector)  # Extract ordered sector names

# Apply the same order to both datasets
log_industry_MD_filtered$Demand_by_sector <- factor(log_industry_MD_filtered$Demand_by_sector, levels = sector_order)
sig_groups$Demand_by_sector <- factor(sig_groups$Demand_by_sector, levels = sector_order)

# Each time, we re sample our energy demand data within each industry sector. 
#Then, we calculate the mean difference between each replicate.

# Compute the sample mean (for comparison)
sample_estimate <- log_industry_MD_filtered %>%
  group_by(Demand_by_sector) %>%  # Group by industry sector
  summarise(
    Mean = mean(demand_kiloton_energy, na.rm = TRUE),  # Compute mean
    Std_Error = sd(demand_kiloton_energy, na.rm = TRUE) / sqrt(n()),  # Compute standard error (SE)
    n = n()   ) %>% # Number of observations
  ungroup() %>% 
  arrange(desc(Mean))

# Set number of bootstrap resamples
n_boots <- 1000

# Bootstrap sampling within each industry sector
industry_boot <- replicate(n = n_boots, simplify = FALSE, 
                           expr = log_industry_MD_filtered  %>%
                             group_by(Demand_by_sector) %>%  
                             slice_sample(prop = 1, replace = TRUE) %>%  
                             summarise(mean_boot_kiloton_energy = mean(demand_kiloton_energy, na.rm = TRUE))) %>%  
  bind_rows()  

# Compute Bootstrapped Confidence Intervals
boot_dist_CI <- industry_boot %>%  
  group_by(Demand_by_sector) %>%  
  summarise(
    SE = sd(mean_boot_kiloton_energy),  
    mean_kiloton_energy = mean(mean_boot_kiloton_energy),  
    lower_95 = quantile(mean_boot_kiloton_energy, probs = 0.025),  
    upper_95 = quantile(mean_boot_kiloton_energy, probs = 0.975)  
  )
```

``` r
boot_model <- lm(mean_boot_kiloton_energy ~ Demand_by_sector, data = industry_boot)
```

## Linear Model Analysis

``` r
print(sample_estimate)
```

    ## # A tibble: 5 × 4
    ##   Demand_by_sector                      Mean Std_Error     n
    ##   <fct>                                <dbl>     <dbl> <int>
    ## 1 Electric vehicles                    4.44      0.568     8
    ## 2 Other low emissions power generation 3.29      0.646     7
    ## 3 Solar PV                             3.19      1.27      6
    ## 4 Grid battery storage                 2.26      0.561     7
    ## 5 Hydrogen technologies                0.170     0.167     4

``` r
boot_summary <- summary(boot_model)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%  
  mutate(Variable = str_remove(Variable, "Demand_by_sector")) %>% 
  rename(
    Mean = Estimate,
    Std_Error = `Std. Error`,  # Standard Error
    t_test = `t value`,  # t-test
    p_value = `Pr(>|t|)`  # p-value
  ) %>%
  arrange(desc(abs(Mean)))  # Sort by mean

print(boot_summary)
```

    ##                               Variable      Mean Std_Error     t_test
    ## 1                    Electric vehicles 4.3045182 0.0296907 144.978651
    ## 2 Other low emissions power generation 3.1259210 0.0296907 105.282820
    ## 3                             Solar PV 2.9927909 0.0296907 100.798919
    ## 4                 Grid battery storage 2.1054165 0.0296907  70.911639
    ## 5                          (Intercept) 0.1660651 0.0209945   7.909933
    ##        p_value
    ## 1 0.000000e+00
    ## 2 0.000000e+00
    ## 3 0.000000e+00
    ## 4 0.000000e+00
    ## 5 3.146986e-15

## ANOVA Analysis

``` r
Anova(boot_model)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: mean_boot_kiloton_energy
    ##                   Sum Sq   Df F value    Pr(>F)    
    ## Demand_by_sector 10296.4    4    5840 < 2.2e-16 ***
    ## Residuals         2201.6 4995                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Pairwise Comparisons

``` r
boot_model_emmeans <- emmeans(boot_model, ~ Demand_by_sector)
boot_model_pairwise <- contrast(boot_model_emmeans, method = "pairwise") %>%
  tidy()

boot_model_pairwise
```

    ## # A tibble: 10 × 8
    ##    term       contrast null.value estimate std.error    df statistic adj.p.value
    ##    <chr>      <chr>         <dbl>    <dbl>     <dbl> <dbl>     <dbl>       <dbl>
    ##  1 Demand_by… Hydroge…          0   -2.11     0.0297  4995    -70.9    0        
    ##  2 Demand_by… Hydroge…          0   -2.99     0.0297  4995   -101.     0        
    ##  3 Demand_by… Hydroge…          0   -3.13     0.0297  4995   -105.     0        
    ##  4 Demand_by… Hydroge…          0   -4.30     0.0297  4995   -145.     0        
    ##  5 Demand_by… Grid ba…          0   -0.887    0.0297  4995    -29.9    0        
    ##  6 Demand_by… Grid ba…          0   -1.02     0.0297  4995    -34.4    0        
    ##  7 Demand_by… Grid ba…          0   -2.20     0.0297  4995    -74.1    0        
    ##  8 Demand_by… Solar P…          0   -0.133    0.0297  4995     -4.48   0.0000734
    ##  9 Demand_by… Solar P…          0   -1.31     0.0297  4995    -44.2    0        
    ## 10 Demand_by… Other l…          0   -1.18     0.0297  4995    -39.7    0

# Tukey Significance Groups

``` r
boot_sig_groups <- cld(boot_model_emmeans, adjust = "holm", Letters = "abcd", sort = TRUE, reversed = TRUE)
boot_sig_groups
```

    ##  Demand_by_sector                     emmean    SE   df lower.CL upper.CL
    ##  Electric vehicles                     4.471 0.021 4995    4.416     4.52
    ##  Other low emissions power generation  3.292 0.021 4995    3.238     3.35
    ##  Solar PV                              3.159 0.021 4995    3.105     3.21
    ##  Grid battery storage                  2.271 0.021 4995    2.217     2.33
    ##  Hydrogen technologies                 0.166 0.021 4995    0.112     0.22
    ##  .group  
    ##   a      
    ##    b     
    ##     c    
    ##      da  
    ##        db
    ## 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 5 estimates 
    ## P value adjustment: holm method for 10 tests 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
box_xmin_boot <- which(levels(boot_dist_CI$Demand_by_sector) == "Hydrogen technologies") - 0.5
box_xmax_boot <- which(levels(boot_dist_CI$Demand_by_sector) == "Electric vehicles") + 0.5

pairwise_boot_plot <- ggplot(industry_boot, 
                        aes(x = Demand_by_sector, y = mean_boot_kiloton_energy, fill = Demand_by_sector)) +
  geom_violin(trim = TRUE, alpha = 0.5, show.legend = FALSE) + 
   geom_errorbar(data = boot_dist_CI, 
                aes(x = Demand_by_sector, 
                    y = mean_kiloton_energy, 
                    ymin = lower_95, ymax = upper_95), 
                width = 0.2, size = 0.8, color = "black", show.legend = FALSE) + 
  geom_text(data = boot_sig_groups, aes(label = .group, y = 4.5), size = 8, color = "black", vjust = -0.5) +  
  labs(title = "2023 Clean Energy Industry Mineral Demand",
       subtitle = "Bootstrap Resampled 1000x",
       x = "Demand by Sector", y = "Energy Demand (Log Kiloton)") +
    annotate("rect", xmin = box_xmin_boot, xmax = box_xmax_boot, ymin = -Inf, ymax = Inf, 
            fill = "gray", alpha = 0.2) +  
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 70, hjust = 1)) + 
  scale_y_continuous(limits = c(0, 9))
```

``` r
pairwise_plot <- ggplot(log_industry_MD_filtered, 
                        aes(x = Demand_by_sector, y = demand_kiloton_energy, fill = Demand_by_sector)) +
  geom_violin(trim = TRUE, alpha = 0.5, show.legend = FALSE) + 
  geom_errorbar(data = sig_groups, 
                aes(x = Demand_by_sector, ymin = emmean - SE, ymax = emmean + SE, color = Demand_by_sector), 
                width = 0.2, size = 1.2, inherit.aes = FALSE) +  
  
  geom_text(data = sig_groups, aes(label = .group, y = 4.5), size = 8, color = "black") +
  labs(title = "2023 Clean Energy Industry Mineral Demand",
       subtitle = "Original Sample",
       x = "Demand by Sector", y = "Energy Demand (log kiloton)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(limits = c(0, 9))
```

``` r
print(sig_groups)
```

    ##                       Demand_by_sector    emmean        SE df   lower.CL
    ## 1                    Electric vehicles 4.4375029 0.6725915 27  2.5739653
    ## 2 Other low emissions power generation 3.2871829 0.7190305 27  1.2949773
    ## 3                             Solar PV 3.1908398 0.7766417 27  1.0390119
    ## 4                 Grid battery storage 2.2637248 0.7190305 27  0.2715192
    ## 5                Hydrogen technologies 0.1695938 0.9511880 27 -2.4658464
    ##   upper.CL .group
    ## 1 6.301041     a 
    ## 2 5.279388     ab
    ## 3 5.342668     ab
    ## 4 4.255930     ab
    ## 5 2.805034      b

``` r
summary(pairs(model_emmeans)) 
```

    ##  contrast                                                     estimate    SE df
    ##  Electric vehicles - Grid battery storage                       2.1738 0.985 27
    ##  Electric vehicles - Hydrogen technologies                      4.2679 1.160 27
    ##  Electric vehicles - Other low emissions power generation       1.1503 0.985 27
    ##  Electric vehicles - Solar PV                                   1.2467 1.030 27
    ##  Grid battery storage - Hydrogen technologies                   2.0941 1.190 27
    ##  Grid battery storage - Other low emissions power generation   -1.0235 1.020 27
    ##  Grid battery storage - Solar PV                               -0.9271 1.060 27
    ##  Hydrogen technologies - Other low emissions power generation  -3.1176 1.190 27
    ##  Hydrogen technologies - Solar PV                              -3.0212 1.230 27
    ##  Other low emissions power generation - Solar PV                0.0963 1.060 27
    ##  t.ratio p.value
    ##    2.208  0.2073
    ##    3.664  0.0087
    ##    1.168  0.7688
    ##    1.213  0.7438
    ##    1.756  0.4185
    ##   -1.006  0.8501
    ##   -0.876  0.9032
    ##   -2.615  0.0958
    ##   -2.460  0.1301
    ##    0.091  1.0000
    ## 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

``` r
print(boot_sig_groups)
```

    ##  Demand_by_sector                     emmean    SE   df lower.CL upper.CL
    ##  Electric vehicles                     4.471 0.021 4995    4.416     4.52
    ##  Other low emissions power generation  3.292 0.021 4995    3.238     3.35
    ##  Solar PV                              3.159 0.021 4995    3.105     3.21
    ##  Grid battery storage                  2.271 0.021 4995    2.217     2.33
    ##  Hydrogen technologies                 0.166 0.021 4995    0.112     0.22
    ##  .group  
    ##   a      
    ##    b     
    ##     c    
    ##      da  
    ##        db
    ## 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 5 estimates 
    ## P value adjustment: holm method for 10 tests 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
summary(pairs(boot_model_emmeans)) 
```

    ##  contrast                                                     estimate     SE
    ##  Hydrogen technologies - Grid battery storage                   -2.105 0.0297
    ##  Hydrogen technologies - Solar PV                               -2.993 0.0297
    ##  Hydrogen technologies - Other low emissions power generation   -3.126 0.0297
    ##  Hydrogen technologies - Electric vehicles                      -4.305 0.0297
    ##  Grid battery storage - Solar PV                                -0.887 0.0297
    ##  Grid battery storage - Other low emissions power generation    -1.021 0.0297
    ##  Grid battery storage - Electric vehicles                       -2.199 0.0297
    ##  Solar PV - Other low emissions power generation                -0.133 0.0297
    ##  Solar PV - Electric vehicles                                   -1.312 0.0297
    ##  Other low emissions power generation - Electric vehicles       -1.179 0.0297
    ##    df  t.ratio p.value
    ##  4995  -70.912  <.0001
    ##  4995 -100.799  <.0001
    ##  4995 -105.283  <.0001
    ##  4995 -144.979  <.0001
    ##  4995  -29.887  <.0001
    ##  4995  -34.371  <.0001
    ##  4995  -74.067  <.0001
    ##  4995   -4.484  0.0001
    ##  4995  -44.180  <.0001
    ##  4995  -39.696  <.0001
    ## 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

``` r
pairwise_plot
```

![](linear-model_files/figure-gfm/visualize-results-1.png)<!-- -->

``` r
pairwise_boot_plot
```

![](linear-model_files/figure-gfm/visualize-results-2.png)<!-- -->

# Conclusion and Implications

The purpose of this study is to test the hypothesis whether clean energy
industries differ in the abundance of minerals needed to accomplish
their goals in reducing carbon emissions. Testing this hypothesis is
useful because the focus of the dataset is projecting the future mineral
demand for these industries, so using real data for statistical analysis
will allow quantitative assessments of how reasonable their projections
are. In conclusion, we have found that from our linear model the
hydrogen technology industry has the lowest demand for minerals and the
electric vehicle industry has the highest demand for minerals and these
categories are sufficiently different when compared to null
distributions both in the raw dataset and bootstrapped.

Upon first observation of the raw dataset, the sampling of industries
showed to be highly variable in their mineral demand ranging from very
low to very high in their need for minerals, but the ANOVA test and post
hoc only indicated that mineral demand can only be predicted with
confidence if it is either the electric vehicle industry or the hydrogen
technology industry and the rest cannot be distinguished from a null
distribution.

Before considering the possibility of confounds, we decided to increase
the sample size of our dataset by resampling the data 1,000 times by
bootstrapping to generate a more robust estimate of the true
distributions of mineral demand across sectors. We found that each
industry sector had a uniquely different energy demand distribution
which was not shown in the post hoc test of the raw data. The
bootstrapped confidence interval ranges across categories revealed more
distinct separations in demand between the industry sectors. The post
hoc result indicates that in order from greatest to least in mineral
demand are: electric vehicles, other low emissions power generation,
solar PV, grid battery storage, and hydrogen technologies. While there
are appearences of overlaps between other low emissions power
generation, solar PV, grid battery storage, the ANOVA test on the
resampled dataset linear model is highly confident with regards to
predicting mineral demand based on industry sector type with a p-value
of 2.2e-16 in observing the F value 5576 with all pair comparsions
distinct from one another (p \<.0001).

Some shortcomings to this statistical analysis is that this analysis
excludes industries electricity networks and low emissions power
generation which also may be significantly different in their demand
than other industries but were not tested in the analysis since an ANOVA
test requires more than one data point. This dataset is also not
representative of global use, and it is not specified from what company
or region they are from. Finally, the values used to calculate the
response variables are approximated by the IEA from consulting World
Energy Outlook, Energy Technology Perspectives, and Global EV Outlook
journals as sources and are not direct measurements.

# My personal takeaways

My personal biggest insight with this project alongside learning post
hoc tests, linear modeling with mutiple categorical variables and
analysis, is how analyzing small sample sizes leads to vulnerability in
generating false positive artifacts that may be misleading.
Bootstrapping is a great tool kit to have when the data is limited in
sample size!
