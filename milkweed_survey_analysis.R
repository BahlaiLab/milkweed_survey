#analysis of milkweed survey strategies with Adam Baker, Davey tree

####################
#Data cleaning notes

#raw data stored in excel file Data_raw.xlsx
#data cleaning operations prior to import into R:

# cost.csv
# 1. separated time/cost data into separate tabs, cost data was reduced down to total time and cost figures
#(1 obs) by method, timing and site
# 2.renamed columns to R-friendly variable names, removes special characters and units
# 3. resolved typos (Transmission site square plot and CCA had total data entered under Habitat_proc)
# 4. NAs can be treated as true zeroes (because zero time was partioned to that activity) so sub 0 for NA
# 5. Assume missing vaules are zeros as well because these data have a total, so sub zeroes

#survey.csv
# 1.deleted data check column and timing data because that is compiled at the method level
# 2.deleted Sensor column- redundant with Timing
# 3.renamed columns to R-friendly variable names, removes special characters and units
# 4.added a column for area sampled in each observation, as by method, computed from methods
# sq plot subsample - 1m2, CCA single sample 139.4m2 in one standard plot per site, transect 4m2 per plot subsample, Site al, ML are both whole plot
# 5. added column with total site area (from methods)
# 6. added column for plot area (1/10 site area)
# 7.removed typo (special character in MW observation in line 132)
# 8. Changed unclassified NAs to zero in methods where groundcover was classified- 
#   method looked, but found zero unclassed, in comparison to ML that did record some unclassed
# 9. Missing value code of M or m used in some places- replace with NA

###################
#Bring in the data!

survey<-read.csv(file="data/survey.csv", header=T, stringsAsFactors=T)
cost<-read.csv(file="data/cost.csv", header=T, stringsAsFactors=T)

#do data QC

summary(survey)
#looks clean!
names(survey)

summary(cost)
# the times are going to need some attention from lubridate but we'll look at that later

#let's first look at the survey data
#we want to get everything onto the same scale so let's first combine subsamples within each plot

library(dplyr)

survey_combined <- survey %>%
  group_by(Site, Timing, Sensor, Method, plot) %>%
  summarise(
    MW = sum(MW, na.rm = TRUE),
    
    across(
      c(floral, woody, grass, broadleaf, bare, wetland, unclassified),
      ~ mean(.x, na.rm = TRUE)
    ),
    
    area_sampled = sum(area_sampled, na.rm = TRUE),
    site_area = mean(site_area, na.rm = TRUE),
    plot_area = mean(plot_area, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))
  )

#check on data structure
summary(survey_combined)

#now let's create a conversion of estimated MW per hectare for all methods


survey_combined$MW_density<-10000*survey_combined$MW/survey_combined$area_sampled

#quick and dirty violin plot of raw density data

library(ggplot2)

ggplot(survey_combined, aes(x = Method, y = MW_density)) +
  geom_violin(na.rm = TRUE, trim = FALSE) +
  theme_bw() +
  labs(  x = "Method", y = "MW density")+ 
  stat_summary(fun = median, geom = "point", size = 2)+ 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA)

# ok so now let's look at this RELATIVE to the Site al method (which is our proxy for Truth)

survey_diff <- survey_combined %>%
  group_by(Site, Timing, plot) %>%
  mutate(
    MW_density_siteal = MW_density[Method == "Site al"][1],
    MW_density_diff = MW_density - MW_density_siteal
  ) %>%
  ungroup() %>%
  filter(Method != "Site al")

#and let's look at a violin plot of that data:
ggplot(survey_diff, aes(x = Method, y = MW_density_diff)) +
  geom_violin(trim = FALSE, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Method",
    y = "Difference from Site al MW density"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#and by site
ggplot(survey_diff, aes(x = Method, y = MW_density_diff)) +
  geom_violin(trim = FALSE, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Site, scales = "free_y", ncol=1) +
  theme_bw() +
  labs(
    x = "Method",
    y = "Difference from Site al MW density"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90")
  )


# let's add medians to this violin plot 


med_lines <- survey_diff %>%
  group_by(Site, Method) %>%
  summarise(
    MW_density_diff_med = median(MW_density_diff, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(survey_diff, aes(x = Method, y = MW_density_diff)) +
  geom_violin(trim = FALSE, na.rm = TRUE) +
  
  ## median lines
  geom_segment(
    data = med_lines,
    aes(
      x = as.numeric(factor(Method)) - 0.4,
      xend = as.numeric(factor(Method)) + 0.4,
      y = MW_density_diff_med,
      yend = MW_density_diff_med
    ),
    color = "red",
    linewidth = 1
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Site, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Method",
    y = "Difference from Site al MW density"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90")
  )

#let's do the same thing, but with % difference rather than absolute difference

epsilon <- 0.1  # small constant to avoid division by zero

survey_pct_diff <- survey_combined %>%
  group_by(Site, Timing, plot) %>%
  mutate(
    MW_density_siteal = MW_density[Method == "Site al"][1],
    MW_density_pct_diff = 100 * (MW_density - MW_density_siteal) / (MW_density_siteal + epsilon)
  ) %>%
  ungroup() %>%
  filter(Method != "Site al")

med_lines <- survey_pct_diff %>%
  group_by(Site, Method) %>%
  summarise(
    MW_density_pct_med = median(MW_density_pct_diff, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(survey_pct_diff, aes(x = Method, y = MW_density_pct_diff)) +
  geom_violin(trim = FALSE, na.rm = TRUE) +
  geom_segment(
    data = med_lines,
    aes(
      x = as.numeric(factor(Method)) - 0.4,
      xend = as.numeric(factor(Method)) + 0.4,
      y = MW_density_pct_med,
      yend = MW_density_pct_med
    ),
    color = "red",
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Site, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Method",
    y = "Percent difference from Site al MW density (epsilon = 0.1)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90")
  )

#nah, that looks horrid- some absolutely giant % differences
#I think I'm going to go back to absolute differences, but I 
# actually think the mean might be more informative here compared to the median,
#because the metric we'd be reporting for any subsampling methods would be the 
# mean and standard error. 

#Back to the absolute differences, and let's add the standard error for siteal in the background of the violin plots

survey_diff_abs <- survey_combined %>%
  group_by(Site, Timing, plot) %>%
  mutate(
    MW_density_siteal = MW_density[Method == "Site al"][1],
    MW_density_diff = MW_density - MW_density_siteal
  ) %>%
  ungroup()

# Compute Site al mean and standard error per Site for background
siteal_stats <- survey_combined %>%
  filter(Method == "Site al") %>%
  group_by(Site) %>%
  summarise(
    siteal_mean = mean(MW_density, na.rm = TRUE),
    siteal_sd = sd(MW_density, na.rm = TRUE),
    .groups = "drop"
  )

# Compute mean difference per Method × Site (for red ticks)
med_lines <- survey_diff_abs %>%
  filter(Method != "Site al") %>%
  group_by(Site, Method) %>%
  summarise(
    MW_density_diff_mean = mean(MW_density_diff, na.rm = TRUE),
    .groups = "drop"
  )

#plot
#Put it on a pseudo-log scale so the outliers don't ruin our day
library(scales)

violin_difference<-ggplot(survey_diff_abs %>% filter(Method != "Site al"),
       aes(x = Method, y = MW_density_diff)) +
  
  # Violin plots
  geom_violin(trim = FALSE, na.rm = TRUE) +
  
  # Background: Site al mean ± SE
  geom_rect(
    data = siteal_stats,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = 0- siteal_sd,
      ymax = 0 + siteal_sd
    ),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.3
  ) +
  
  # Red mean ticks per Method × Site
  geom_segment(
    data = med_lines,
    aes(
      x = as.numeric(factor(Method)) - 0.4,
      xend = as.numeric(factor(Method)) + 0.4,
      y = MW_density_diff_mean,
      yend = MW_density_diff_mean
    ),
    color = "red",
    linewidth = 1
  ) +
  
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Facets: one column
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  
  # Pseudo-log y-axis with numeric labels
  scale_y_continuous(
    #trans = pseudo_log_trans(base = 10),
    breaks = function(lims) c(lims[1], 0, lims[2]),
    labels = function(x) {
      sapply(x, function(v) {
        if (v == 0) return("0")
        sign(v) * signif(abs(v), 1) / 1000000
      })
    }
  ) +
  
  theme_bw() +
  
  # X-axis and overall theme tweaks
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(size = 10)
  ) +
  
  labs(
    x = "Method",
    y = expression("Absolute difference from Siteal MW density (" * 10^6 * " stems)")
  )

violin_difference

#save to pdf
pdf("plots/violin_plot_MW difference.pdf", height=10, width=4)
violin_difference
dev.off()




################
#ok, now we're going to use some computations to define how likely a plot is to 
#over or underestimate the milkweed density, by method, at each site

#first create variables to define what we think are the percentile cutoffs for a 'right' answer

lower_p <- 0.25
upper_p <- 0.75

#compute what each cutoff is for being equivalent to siteal at all sites:

siteal_cutoffs <- survey_combined %>%
  filter(Method == "Site al") %>%
  group_by(Site) %>%
  summarise(
    siteal_mean = mean(MW_density, na.rm = TRUE),
    siteal_sd   = sd(MW_density, na.rm = TRUE),
    lower_cut = qnorm(lower_p, mean = siteal_mean, sd = siteal_sd),
    upper_cut = qnorm(upper_p, mean = siteal_mean, sd = siteal_sd),
    .groups = "drop"
  )

#compute whether each plot observation is over, under or within Siteal cutoffs
survey_classified <- survey_combined %>%
  filter(Method != "Site al") %>%
  left_join(siteal_cutoffs, by = "Site") %>%
  mutate(
    estimate_class = case_when(
      MW_density > upper_cut ~ "over",
      MW_density < lower_cut ~ "under",
      TRUE                   ~ "within"
    )
  )

#create table

prob_table <- survey_classified %>%
  group_by(Site, Method) %>%
  summarise(
    p_over   = mean(estimate_class == "over",   na.rm = TRUE),
    p_under  = mean(estimate_class == "under",  na.rm = TRUE),
    p_within = mean(estimate_class == "within", na.rm = TRUE),
    n_plots  = n(),
    .groups = "drop"
  )

#add in means and SDs for each method at each site

method_stats <- survey_combined %>%
  group_by(Site, Method) %>%
  summarise(
    mean_MW = mean(MW_density, na.rm = TRUE),
    sd_MW   = sd(MW_density, na.rm = TRUE),
    .groups = "drop"
  )

#join these!

prob_table <- prob_table %>%
  left_join(method_stats, by = c("Site", "Method"))

#add in the Siteal data as a basis for comparison

siteal_rows <- survey_combined %>%
  filter(Method == "Site al") %>%
  group_by(Site) %>%
  summarise(
    Method    = "Site al",
    p_over    = NA_real_,
    p_under   = NA_real_,
    p_within  = NA_real_,
    n_plots   = n(),
    mean_MW   = mean(MW_density, na.rm = TRUE),
    sd_MW     = sd(MW_density, na.rm = TRUE),
    .groups = "drop"
  )
prob_table <- bind_rows(prob_table, siteal_rows)

#export these data so we can make a cute little table for it
write.csv(prob_table, file = "plots/probability_by_method_milkweed.csv", row.names = FALSE)

# ok let's use this data to create some visualizations. A grouped bar chart to get more closely at means and SEs:

plot_data <- prob_table %>%
  filter(!is.na(n_plots)) %>%  
  mutate(se_MW = sd_MW / sqrt(n_plots))

ggplot(plot_data, aes(x = Site, y = mean_MW, fill = Method)) +
  
  # Bars grouped by Method within Site
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Error bars
  geom_errorbar(
    aes(ymin = mean_MW - se_MW, ymax = mean_MW + se_MW),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  
  # Pseudo-log y-axis
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    labels = label_number()
  ) +
  
  theme_bw() +
  labs(
    x = "Site",
    y = "Mean MW density ± SE",
    fill = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
# Now let's do some basic statistics on this and add labels.

# Fit two-way ANOVA with interaction
anova_model <- aov(MW_density ~ Site * Method, data = survey_combined)

# Summary table
summary(anova_model)

plot(anova_model)  # residuals, normality, homoscedasticity
# some outliers highly influential, but showing that is kind of the point: some methods will have estimates wildy affected by outliers.

TukeyHSD(anova_model, "Method")

interaction.plot(survey_combined$Site, survey_combined$Method, survey_combined$MW_density)


#bring this together into a plot with letter labels- workaround using single sample t-test for CCAA comparisons

library(rstatix)
library(ggpubr)
library(tidyr)

# Prepare bar plot data
plot_data <- prob_table %>%
  filter(!is.na(n_plots)) %>%
  mutate(se_MW = sd_MW / sqrt(n_plots))


# Regular pairwise t-tests (methods with ≥2 reps)
pairwise_tests <- survey_combined %>%
  filter(Method != "CCAA") %>%
  group_by(Site) %>%
  pairwise_t_test(
    MW_density ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup()

# Single-sample t-tests vs CCAA
ccaa_values <- survey_combined %>%
  filter(Method == "CCAA") %>%
  select(Site, MW_density_ccaa = MW_density)

single_sample_tests <- survey_combined %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    statistic = t.test(MW_density, mu = MW_density_ccaa[1])$statistic,
    df        = t.test(MW_density, mu = MW_density_ccaa[1])$parameter,
    p         = t.test(MW_density, mu = MW_density_ccaa[1])$p.value,
    conf.low  = t.test(MW_density, mu = MW_density_ccaa[1])$conf.int[1],
    conf.high = t.test(MW_density, mu = MW_density_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,  # tested method
    group2 = "CCAA",  # reference method
    p.adj = p.adjust(p, method = "bonferroni")
  )

#  Combine all tests
#  Standardize pairwise t-test output
pairwise_tests_clean <- pairwise_tests %>%
  select(Site, group1, group2, p.adj)

# Standardize single-sample t-test output 
single_sample_tests_clean <- single_sample_tests %>%
  select(Site, group1, group2, p.adj)

#  Combine
all_tests_clean<- bind_rows(pairwise_tests_clean, single_sample_tests_clean)

# Generate letters per site
library(multcompView)

# We only need the comparisons for letter generation
cld_list <- list()

for (s in unique(all_tests_clean$Site)) {
  df_site <- all_tests_clean %>% filter(Site == s)
  
  methods <- unique(c(df_site$group1, df_site$group2))
  n <- length(methods)
  
  # full p-value matrix
  pmat <- matrix(1, nrow = n, ncol = n, dimnames = list(methods, methods))
  
  for (i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  
  # replace any remaining NA with 1
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  cld_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}
letter_df <- bind_rows(cld_list)


#Merge letters with plotting data

plot_data_letters <- plot_data %>%
  left_join(letter_df, by = c("Site", "Method"))

#adjust position with error bars
plot_data_letters <- plot_data_letters %>%
  mutate(
    y_pos = if_else(is.na(se_MW), (1.2*mean_MW+1), mean_MW + (1.5*se_MW+1))
  )

#  Plot pseudo-log bar chart with SE and letters

milkweedbar<-ggplot(plot_data_letters, aes(x = Site, y = mean_MW, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_MW - se_MW, ymax = mean_MW + se_MW),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  geom_text(
    aes(y = y_pos, label = LETTERS),
    position = position_dodge(width = 0.8),
    vjust = 0  # top-align letters at y_pos
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    labels = label_number()
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Mean MW density ± SE",
    fill = "Method"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
milkweedbar

#save to pdf
pdf("plots/milkweedbymethodbar.pdf", height=6, width=8)
milkweedbar
dev.off()
###################
# now let's do the same ANOVA type analysis on floral, woody, grass, broadleaf

#remove Site al data because Site al does not produce groundcover data

survey_filtered <- survey_combined %>%
  filter(Method != "Site al")

#first a quick chisq- do methods capture different patterns of groundcover between sites?

# Choose the cover types to include 
#exclude unclassified because it only shows up for Machine learning. Remove wetland because of extremely low %cover (mostly 0)
cover_vars <- c("floral", "woody", "grass", "broadleaf", "bare")


# Aggregate data by Site and Method for chisqu

survey_agg <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(across(all_of(cover_vars), mean, na.rm = TRUE), .groups = "drop")  # one row per Site × Method

# Initialize results table
results <- data.frame(Site = character(),
                      chi_stat = numeric(),
                      df = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through Sites
for(s in unique(survey_agg$Site)) {
  
  df_site <- survey_agg %>% filter(Site == s)
  
  # Build contingency table: Method x Cover type
  tbl_mat <- df_site[, cover_vars]
  rownames(tbl_mat) <- df_site$Method
  tbl_mat <- as.matrix(tbl_mat * 100)  # convert percentages to counts
  
  # Remove Methods (rows) that are all zero
  tbl_mat <- tbl_mat[rowSums(tbl_mat) > 0, , drop = FALSE]
  
  # Remove cover types (columns) that are all zero
  tbl_mat <- tbl_mat[, colSums(tbl_mat) > 0, drop = FALSE]
  
  # Only run chi-square if valid
  if(nrow(tbl_mat) >= 2 && ncol(tbl_mat) >= 2) {
    tbl_mat <- tbl_mat + 0.5  # add small constant to avoid zero counts
    test <- chisq.test(tbl_mat)
    results <- rbind(results, data.frame(Site = s,
                                         chi_stat = test$statistic,
                                         df = test$parameter,
                                         p_value = test$p.value))
  } else {
    results <- rbind(results, data.frame(Site = s, chi_stat = NA, df = NA, p_value = NA))
  }
}

results

#at every site, the methods are classifying groundcover differently

####univariate comparisons
#first floral

library(dplyr)
library(ggplot2)
library(rstatix)




# ANOVA: floral ~ Site * Method

anova_mod <- aov(floral ~ Site * Method, data = survey_filtered)
summary(anova_mod)



# Regular pairwise t-tests (methods with ≥2 reps)

pairwise_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%  # exclude CCAA here
  group_by(Site) %>%
  pairwise_t_test(
    floral ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup() %>%
  select(Site, group1, group2, p, p.adj)


# Single-sample t-tests vs CCAA

ccaa_values <- survey_filtered %>%
  filter(Method == "CCAA") %>%
  select(Site, floral_ccaa = floral)

single_sample_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    t_stat   = t.test(floral, mu = floral_ccaa[1])$statistic,
    df       = t.test(floral, mu = floral_ccaa[1])$parameter,
    p_val    = t.test(floral, mu = floral_ccaa[1])$p.value,
    conf_low = t.test(floral, mu = floral_ccaa[1])$conf.int[1],
    conf_high= t.test(floral, mu = floral_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,
    group2 = "CCAA",
    p.adj  = p.adjust(p_val, method = "bonferroni")
  ) %>%
  select(Site, group1, group2, p.adj)


# Combine pairwise and single-sample tests

all_tests <- bind_rows(pairwise_tests, single_sample_tests)


#  Generate compact letters per Site

letter_list <- list()
for(s in unique(all_tests$Site)) {
  
  df_site <- all_tests %>% filter(Site == s)
  methods <- unique(c(df_site$group1, df_site$group2))
  
  pmat <- matrix(1, nrow = length(methods), ncol = length(methods),
                 dimnames = list(methods, methods))
  for(i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letter_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}

letter_df <- bind_rows(letter_list)


# Compute Q3 for letter placement- adjusted to 95th percentile

plot_data <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(
    Q3_floral = quantile(floral, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df, by = c("Site", "Method")) %>%
  mutate(
    y_pos = Q3_floral+2  # slightly above box top
  )


# Boxplot with jitter and Tukey letters

floralbox<-ggplot(survey_filtered, aes(x = Site, y = floral, fill = Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge2(width = 0.8)) +
  #geom_jitter(aes(color = Method),
  #            position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #            size = 1.5, alpha = 0.8) +
  geom_text(
    data = plot_data,
    aes(x = Site, y = y_pos, label = LETTERS),
    position = position_dodge2(width = 0.8),
    vjust = 0
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Percent Floral Cover",
    fill = "Method",
    color = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
floralbox

#now woody
# ANOVA: woody ~ Site * Method

anova_mod <- aov(woody ~ Site * Method, data = survey_filtered)
summary(anova_mod)



# Regular pairwise t-tests (methods with ≥2 reps)

pairwise_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%  # exclude CCAA here
  group_by(Site) %>%
  pairwise_t_test(
    woody ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup() %>%
  select(Site, group1, group2, p, p.adj)


# Single-sample t-tests vs CCAA

ccaa_values <- survey_filtered %>%
  filter(Method == "CCAA") %>%
  select(Site, woody_ccaa = woody)

single_sample_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    t_stat   = t.test(woody, mu = woody_ccaa[1])$statistic,
    df       = t.test(woody, mu = woody_ccaa[1])$parameter,
    p_val    = t.test(woody, mu = woody_ccaa[1])$p.value,
    conf_low = t.test(woody, mu = woody_ccaa[1])$conf.int[1],
    conf_high= t.test(woody, mu = woody_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,
    group2 = "CCAA",
    p.adj  = p.adjust(p_val, method = "bonferroni")
  ) %>%
  select(Site, group1, group2, p.adj)


# Combine pairwise and single-sample tests

all_tests <- bind_rows(pairwise_tests, single_sample_tests)


#  Generate compact letters per Site

letter_list <- list()
for(s in unique(all_tests$Site)) {
  
  df_site <- all_tests %>% filter(Site == s)
  methods <- unique(c(df_site$group1, df_site$group2))
  
  pmat <- matrix(1, nrow = length(methods), ncol = length(methods),
                 dimnames = list(methods, methods))
  for(i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letter_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}

letter_df <- bind_rows(letter_list)


# Compute Q3 for letter placement

plot_data <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(
    Q3_woody = quantile(woody, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df, by = c("Site", "Method")) %>%
  mutate(
    y_pos = Q3_woody+2  # slightly above box top
  )


# Boxplot with jitter and Tukey letters

woodybox<-ggplot(survey_filtered, aes(x = Site, y = woody, fill = Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge2(width = 0.8)) +
  # geom_jitter(aes(color = Method),
  #             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #             size = 1.5, alpha = 0.8) +
  geom_text(
    data = plot_data,
    aes(x = Site, y = y_pos, label = LETTERS),
    position = position_dodge2(width = 0.8),
    vjust = 0
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Percent Woody Cover",
    fill = "Method",
    color = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
woodybox
#now grass
# ANOVA: grass ~ Site * Method

anova_mod <- aov(grass ~ Site * Method, data = survey_filtered)
summary(anova_mod)



# Regular pairwise t-tests (methods with ≥2 reps)

pairwise_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%  # exclude CCAA here
  group_by(Site) %>%
  pairwise_t_test(
    grass ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup() %>%
  select(Site, group1, group2, p, p.adj)


# Single-sample t-tests vs CCAA

ccaa_values <- survey_filtered %>%
  filter(Method == "CCAA") %>%
  select(Site, grass_ccaa = grass)

single_sample_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    t_stat   = t.test(grass, mu = grass_ccaa[1])$statistic,
    df       = t.test(grass, mu = grass_ccaa[1])$parameter,
    p_val    = t.test(grass, mu = grass_ccaa[1])$p.value,
    conf_low = t.test(grass, mu = grass_ccaa[1])$conf.int[1],
    conf_high= t.test(grass, mu = grass_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,
    group2 = "CCAA",
    p.adj  = p.adjust(p_val, method = "bonferroni")
  ) %>%
  select(Site, group1, group2, p.adj)


# Combine pairwise and single-sample tests

all_tests <- bind_rows(pairwise_tests, single_sample_tests)


#  Generate compact letters per Site

letter_list <- list()
for(s in unique(all_tests$Site)) {
  
  df_site <- all_tests %>% filter(Site == s)
  methods <- unique(c(df_site$group1, df_site$group2))
  
  pmat <- matrix(1, nrow = length(methods), ncol = length(methods),
                 dimnames = list(methods, methods))
  for(i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letter_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}

letter_df <- bind_rows(letter_list)


# Compute Q3 for letter placement

plot_data <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(
    Q3_grass = quantile(grass, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df, by = c("Site", "Method")) %>%
  mutate(
    y_pos = Q3_grass+2  # slightly above box top
  )


# Boxplot with jitter and Tukey letters

grassbox<-ggplot(survey_filtered, aes(x = Site, y = grass, fill = Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge2(width = 0.8)) +
  # geom_jitter(aes(color = Method),
  #             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #             size = 1.5, alpha = 0.8) +
  geom_text(
    data = plot_data,
    aes(x = Site, y = y_pos, label = LETTERS),
    position = position_dodge2(width = 0.8),
    vjust = 0
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Percent Grass Cover",
    fill = "Method",
    color = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
grassbox



#now broadleaf
# ANOVA: broadleaf ~ Site * Method

anova_mod <- aov(broadleaf ~ Site * Method, data = survey_filtered)
summary(anova_mod)



# Regular pairwise t-tests (methods with ≥2 reps)

pairwise_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%  # exclude CCAA here
  group_by(Site) %>%
  pairwise_t_test(
    broadleaf ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup() %>%
  select(Site, group1, group2, p, p.adj)


# Single-sample t-tests vs CCAA

ccaa_values <- survey_filtered %>%
  filter(Method == "CCAA") %>%
  select(Site, broadleaf_ccaa = broadleaf)

single_sample_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    t_stat   = t.test(broadleaf, mu = broadleaf_ccaa[1])$statistic,
    df       = t.test(broadleaf, mu = broadleaf_ccaa[1])$parameter,
    p_val    = t.test(broadleaf, mu = broadleaf_ccaa[1])$p.value,
    conf_low = t.test(broadleaf, mu = broadleaf_ccaa[1])$conf.int[1],
    conf_high= t.test(broadleaf, mu = broadleaf_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,
    group2 = "CCAA",
    p.adj  = p.adjust(p_val, method = "bonferroni")
  ) %>%
  select(Site, group1, group2, p.adj)


# Combine pairwise and single-sample tests

all_tests <- bind_rows(pairwise_tests, single_sample_tests)


#  Generate compact letters per Site

letter_list <- list()
for(s in unique(all_tests$Site)) {
  
  df_site <- all_tests %>% filter(Site == s)
  methods <- unique(c(df_site$group1, df_site$group2))
  
  pmat <- matrix(1, nrow = length(methods), ncol = length(methods),
                 dimnames = list(methods, methods))
  for(i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letter_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}

letter_df <- bind_rows(letter_list)


# Compute Q3 for letter placement

plot_data <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(
    Q3_broadleaf = quantile(broadleaf, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df, by = c("Site", "Method")) %>%
  mutate(
    y_pos = Q3_broadleaf+2  # slightly above box top
  )


# Boxplot with jitter and Tukey letters

broadleafbox<-ggplot(survey_filtered, aes(x = Site, y = broadleaf, fill = Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge2(width = 0.8)) +
  # geom_jitter(aes(color = Method),
  #             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #             size = 1.5, alpha = 0.8) +
  geom_text(
    data = plot_data,
    aes(x = Site, y = y_pos, label = LETTERS),
    position = position_dodge2(width = 0.8),
    vjust = 0
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Percent Broadleaf Cover",
    fill = "Method",
    color = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
broadleafbox

#now bare
# ANOVA: bare ~ Site * Method

anova_mod <- aov(bare ~ Site * Method, data = survey_filtered)
summary(anova_mod)



# Regular pairwise t-tests (methods with ≥2 reps)

pairwise_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%  # exclude CCAA here
  group_by(Site) %>%
  pairwise_t_test(
    bare ~ Method,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup() %>%
  select(Site, group1, group2, p, p.adj)


# Single-sample t-tests vs CCAA

ccaa_values <- survey_filtered %>%
  filter(Method == "CCAA") %>%
  select(Site, bare_ccaa = bare)

single_sample_tests <- survey_filtered %>%
  filter(Method != "CCAA") %>%
  left_join(ccaa_values, by = "Site") %>%
  group_by(Site, Method) %>%
  summarise(
    t_stat   = t.test(bare, mu = bare_ccaa[1])$statistic,
    df       = t.test(bare, mu = bare_ccaa[1])$parameter,
    p_val    = t.test(bare, mu = bare_ccaa[1])$p.value,
    conf_low = t.test(bare, mu = bare_ccaa[1])$conf.int[1],
    conf_high= t.test(bare, mu = bare_ccaa[1])$conf.int[2],
    .groups = "drop"
  ) %>%
  mutate(
    group1 = Method,
    group2 = "CCAA",
    p.adj  = p.adjust(p_val, method = "bonferroni")
  ) %>%
  select(Site, group1, group2, p.adj)


# Combine pairwise and single-sample tests

all_tests <- bind_rows(pairwise_tests, single_sample_tests)


#  Generate compact letters per Site

letter_list <- list()
for(s in unique(all_tests$Site)) {
  
  df_site <- all_tests %>% filter(Site == s)
  methods <- unique(c(df_site$group1, df_site$group2))
  
  pmat <- matrix(1, nrow = length(methods), ncol = length(methods),
                 dimnames = list(methods, methods))
  for(i in seq_len(nrow(df_site))) {
    g1 <- df_site$group1[i]
    g2 <- df_site$group2[i]
    pmat[g1, g2] <- df_site$p.adj[i]
    pmat[g2, g1] <- df_site$p.adj[i]
  }
  pmat[is.na(pmat)] <- 1
  
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letter_list[[s]] <- data.frame(
    Site = s,
    Method = names(letters),
    LETTERS = unname(letters),
    stringsAsFactors = FALSE
  )
}

letter_df <- bind_rows(letter_list)


# Compute Q3 for letter placement

plot_data <- survey_filtered %>%
  group_by(Site, Method) %>%
  summarise(
    Q3_bare = quantile(bare, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letter_df, by = c("Site", "Method")) %>%
  mutate(
    y_pos = Q3_bare+2  # slightly above box top
  )


# Boxplot with jitter and Tukey letters

barebox<-ggplot(survey_filtered, aes(x = Site, y = bare, fill = Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge2(width = 0.8)) +
  # geom_jitter(aes(color = Method),
  #             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  #             size = 1.5, alpha = 0.8) +
  geom_text(
    data = plot_data,
    aes(x = Site, y = y_pos, label = LETTERS),
    position = position_dodge2(width = 0.8),
    vjust = 0
  ) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Percent Bare Cover",
    fill = "Method",
    color = "Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
barebox

######
#combine figures into a single object

library(patchwork)
library(cowplot)

# Combine the five plots without their legends
uniform_theme <- theme(
  plot.margin = margin(12, 6, 6, 6),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  legend.position = "none"
)
# Panel label theme: left-aligned over the plotting area
panel_label_theme <- theme(
  plot.tag = element_text(size = 12, face = "bold"),
  plot.tag.position = c(0.1, 1.07)  # x=0: left edge of plot, y just above top
)


# Apply panel labels to each plot
floralbox_noleg    <- floralbox   + labs(tag = "Floral")    + panel_label_theme + uniform_theme
woodybox_noleg     <- woodybox     + labs(tag = "Woody")     + panel_label_theme + uniform_theme
grassbox_noleg     <- grassbox     + labs(tag = "Grass")     + panel_label_theme + uniform_theme
broadleafbox_noleg <- broadleafbox + labs(tag = "Broadleaf") + panel_label_theme + uniform_theme
barebox_noleg      <- barebox      + labs(tag = "Bare")      + panel_label_theme + uniform_theme


# Extract legend from one plot (all have same legend)
legend <- get_legend(floralbox + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)))  # this returns a grob

# Turn the legend grob into a plot for patchwork
legend_plot <- ggplot() +
  theme_void() +
  annotation_custom(legend)

# Combine plots in 3 rows x 2 columns
# first build the  grid (legend in bottom-right)
grid <- plot_grid(
  floralbox_noleg, woodybox_noleg,
  grassbox_noleg,  broadleafbox_noleg,
  barebox_noleg,   legend_plot,
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

# Add shared y-axis label, left-aligned to the y-axis
final_plot <- ggdraw(grid) +
  draw_label(
    "% cover",
    x = 0.005,      # left margin
    y = 0.5,        # vertical center
    angle = 90,
    fontface = "bold",
    size = 14,
    hjust = 0
  ) +
  theme(plot.margin = margin(t = 25, r = 5, b = 5, l = 5))  # add top margin so panel labels are visible


# Display the final figure
final_plot

#save to pdf
pdf("plots/groundcover_performance_comparison.pdf", height=10, width=10)
final_plot
dev.off()


##########################
#let's do a scatterplot matrix for pairwise comparisons of methods on  our response variables, 
#using plot*site as the unit, colored by Site, and with trendlines.

# Pivot data so each method is a column
mw_wide <- survey_combined %>%
  select(Site, plot, Method, MW_density) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = MW_density)

#Generate all pairwise method comparisons
methods <- names(mw_wide)[!(names(mw_wide) %in% c("Site", "plot", "plot_site"))]
pairs <- combn(methods, 2, simplify = FALSE)

#make plots for each method pair
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(mw_wide, aes(x = !!sym(xvar), y = !!sym(yvar), color = Site)) +  #mod to accept column names with spaces (from reshaping methods column)
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})


#pull out legend
legend <- get_legend(
  plots[[1]] + 
    theme(legend.position = "right",        # ensure it’s visible
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

#Remove legends from all individual plots
plots_noleg <- lapply(plots, function(p) p + theme(legend.position = "none"))

#convert sale to thousands to shrink those numbers
plots_thousands <- lapply(plots_noleg, function(p) {
  p +
    scale_x_continuous(labels = function(x) x / 1000) +
    scale_y_continuous(labels = function(x) x / 1000)
})

#Combine the plots into a grid and add the legend as the last panel
# Add an empty placeholder for legend
legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

# Combine all scatterplots + legend panel
final_grid <- plot_grid(
  plotlist = c(plots_thousands, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid

# ok this will be more informative if we annotate each of these plots with a correlation coefficient
#and  a slope- should be 1 if they methods are behaving the same

#Compute slope and R² for each pair
library(broom)

# Function to fit linear model and extract slope and R^2
get_lm_stats_safe <- function(df, xvar, yvar) {
  # create formula safely with backticks
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- broom::tidy(fit)
  glance_fit <- broom::glance(fit)
  
  slope <- tidy_fit$estimate[2]  # coefficient for x
  r2 <- glance_fit$r.squared
  
  list(slope = slope, r2 = r2)
}


#Annotate each plot
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(mw_wide, xvar, yvar)
  slope <- round(stats$slope, 2)
  r2 <- round(stats$r2, 2)
  
  ggplot(mw_wide, aes(x = .data[[xvar]], y = .data[[yvar]], color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "darkgrey", se = FALSE, lty="dotted") +
    theme_minimal() +
    scale_x_continuous(labels = function(x) x / 1000) +
    scale_y_continuous(labels = function(x) x / 1000) +
    annotate(
      "text",
      x = max(mw_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(mw_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0("slope = ", slope, "\nR² = ", r2),
      hjust = 0,
      size = 3.5
    ) +
    scale_color_brewer(palette = "Set2")
})

#pull out legend
legend <- get_legend(
  plots_annotated[[1]] + 
    theme(legend.position = "right",        # ensure it’s visible
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

#Remove legends from all individual plots
plots_a_noleg <- lapply(plots_annotated, function(p) p + theme(legend.position = "none"))


#Combine the plots into a grid and add the legend as the last panel
# Add an empty placeholder for legend
legend_panel <- ggplot() + theme_void() + annotation_custom(legend)



# Combine into grid with legend
final_grid_annotated <- plot_grid(
  plotlist = c(plots_a_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#save to pdf
pdf("plots/estimatecorrelation_bymethod_mw.pdf", height=8, width=8)
final_grid_annotated
dev.off()


######################################################################
# Same analysis as above, mostly, but with the groundcover data

#######
#do floral

#First strip out Site al data:

survey_floral <- survey_combined %>%
  filter(Method != "Site al")

# Pivot to wide using floral
floral_wide <- survey_floral %>%
  select(Site, plot, Method, floral) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = floral)

# Generate pairwise method combinations
methods <- names(floral_wide)[
  !(names(floral_wide) %in% c("Site", "plot", "plot_site"))
]
pairs <- combn(methods, 2, simplify = FALSE)

#build the base scatterplots
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(floral_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Function to compute slope + R²
#repeat but maybe I broke it with all the other analyses
#paste in to be sure
library(broom)

get_lm_stats_safe <- function(df, xvar, yvar) {
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- tidy(fit)
  glance_fit <- glance(fit)
  
  list(
    slope = tidy_fit$estimate[2],
    r2    = glance_fit$r.squared
  )
}

#Annotate the plots with slope + R²
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(floral_wide, xvar, yvar)
  
  ggplot(floral_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm",
                color = "darkgrey",
                se = FALSE,
                lty = "dotted") +
    annotate(
      "text",
      x = max(floral_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(floral_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0(
        "slope = ", round(stats$slope, 2),
        "\nR² = ", round(stats$r2, 2)
      ),
      hjust = 0,
      size = 3.5
    ) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Extract legend, remove from panels, and assemble grid
library(cowplot)

legend <- get_legend(
  plots_annotated[[1]] +
    theme(legend.position = "right",
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

plots_noleg <- lapply(plots_annotated, function(p) {
  p + theme(legend.position = "none")
})

legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

final_grid_annotated <- plot_grid(
  plotlist = c(plots_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#add title so we'll be able to tell apart the megaplots with different cover types

#first put a little space on the top of the plot

final_with_title <- plot_grid(
  ggplot() + theme_void(),   # empty spacer
  final_grid_annotated,
  ncol = 1,
  rel_heights = c(0.08, 1)   # increase first value for more space
)

final_with_title <- ggdraw(final_with_title) +
  draw_label(
    "Estimated floral cover",
    x = 0.05,
    hjust = 0,
    y = 0.98,
    vjust = 1,
    fontface = "bold",
    size = 16
  )

final_with_title

#save to pdf
pdf("plots/estimatecorrelation_bymethod_floral.pdf", height=8, width=8)
final_with_title
dev.off()

#######
#do woody

#First strip out Site al data:

survey_woody <- survey_combined %>%
  filter(Method != "Site al")

# Pivot to wide using woody
woody_wide <- survey_woody %>%
  select(Site, plot, Method, woody) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = woody)

# Generate pairwise method combinations
methods <- names(woody_wide)[
  !(names(woody_wide) %in% c("Site", "plot", "plot_site"))
]
pairs <- combn(methods, 2, simplify = FALSE)

#build the base scatterplots
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(woody_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Function to compute slope + R²
#repeat but maybe I broke it with all the other analyses
#paste in to be sure
library(broom)

get_lm_stats_safe <- function(df, xvar, yvar) {
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- tidy(fit)
  glance_fit <- glance(fit)
  
  list(
    slope = tidy_fit$estimate[2],
    r2    = glance_fit$r.squared
  )
}

#Annotate the plots with slope + R²
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(woody_wide, xvar, yvar)
  
  ggplot(woody_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm",
                color = "darkgrey",
                se = FALSE,
                lty = "dotted") +
    annotate(
      "text",
      x = max(woody_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(woody_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0(
        "slope = ", round(stats$slope, 2),
        "\nR² = ", round(stats$r2, 2)
      ),
      hjust = 0,
      size = 3.5
    ) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Extract legend, remove from panels, and assemble grid
library(cowplot)

legend <- get_legend(
  plots_annotated[[1]] +
    theme(legend.position = "right",
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

plots_noleg <- lapply(plots_annotated, function(p) {
  p + theme(legend.position = "none")
})

legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

final_grid_annotated <- plot_grid(
  plotlist = c(plots_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#add title so we'll be able to tell apart the megaplots with different cover types

#first put a little space on the top of the plot

final_with_title <- plot_grid(
  ggplot() + theme_void(),   # empty spacer
  final_grid_annotated,
  ncol = 1,
  rel_heights = c(0.08, 1)   # increase first value for more space
)

final_with_title <- ggdraw(final_with_title) +
  draw_label(
    "Estimated woody cover",
    x = 0.05,
    hjust = 0,
    y = 0.98,
    vjust = 1,
    fontface = "bold",
    size = 16
  )

final_with_title

#save to pdf
pdf("plots/estimatecorrelation_bymethod_woody.pdf", height=8, width=8)
final_with_title
dev.off()


#######
#do grass

#First strip out Site al data:

survey_grass <- survey_combined %>%
  filter(Method != "Site al")

# Pivot to wide using grass
grass_wide <- survey_grass %>%
  select(Site, plot, Method, grass) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = grass)

# Generate pairwise method combinations
methods <- names(grass_wide)[
  !(names(grass_wide) %in% c("Site", "plot", "plot_site"))
]
pairs <- combn(methods, 2, simplify = FALSE)

#build the base scatterplots
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(grass_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Function to compute slope + R²
#repeat but maybe I broke it with all the other analyses
#paste in to be sure
library(broom)

get_lm_stats_safe <- function(df, xvar, yvar) {
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- tidy(fit)
  glance_fit <- glance(fit)
  
  list(
    slope = tidy_fit$estimate[2],
    r2    = glance_fit$r.squared
  )
}

#Annotate the plots with slope + R²
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(grass_wide, xvar, yvar)
  
  ggplot(grass_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm",
                color = "darkgrey",
                se = FALSE,
                lty = "dotted") +
    annotate(
      "text",
      x = max(grass_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(grass_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0(
        "slope = ", round(stats$slope, 2),
        "\nR² = ", round(stats$r2, 2)
      ),
      hjust = 0,
      size = 3.5
    ) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Extract legend, remove from panels, and assemble grid
library(cowplot)

legend <- get_legend(
  plots_annotated[[1]] +
    theme(legend.position = "right",
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

plots_noleg <- lapply(plots_annotated, function(p) {
  p + theme(legend.position = "none")
})

legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

final_grid_annotated <- plot_grid(
  plotlist = c(plots_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#add title so we'll be able to tell apart the megaplots with different cover types

#first put a little space on the top of the plot

final_with_title <- plot_grid(
  ggplot() + theme_void(),   # empty spacer
  final_grid_annotated,
  ncol = 1,
  rel_heights = c(0.08, 1)   # increase first value for more space
)

final_with_title <- ggdraw(final_with_title) +
  draw_label(
    "Estimated grass cover",
    x = 0.05,
    hjust = 0,
    y = 0.98,
    vjust = 1,
    fontface = "bold",
    size = 16
  )

final_with_title

#save to pdf
pdf("plots/estimatecorrelation_bymethod_grass.pdf", height=8, width=8)
final_with_title
dev.off()

#######
#do broadleaf

#First strip out Site al data:

survey_broadleaf <- survey_combined %>%
  filter(Method != "Site al")

# Pivot to wide using broadleaf
broadleaf_wide <- survey_broadleaf %>%
  select(Site, plot, Method, broadleaf) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = broadleaf)

# Generate pairwise method combinations
methods <- names(broadleaf_wide)[
  !(names(broadleaf_wide) %in% c("Site", "plot", "plot_site"))
]
pairs <- combn(methods, 2, simplify = FALSE)

#build the base scatterplots
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(broadleaf_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Function to compute slope + R²
#repeat but maybe I broke it with all the other analyses
#paste in to be sure
library(broom)

get_lm_stats_safe <- function(df, xvar, yvar) {
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- tidy(fit)
  glance_fit <- glance(fit)
  
  list(
    slope = tidy_fit$estimate[2],
    r2    = glance_fit$r.squared
  )
}

#Annotate the plots with slope + R²
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(broadleaf_wide, xvar, yvar)
  
  ggplot(broadleaf_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm",
                color = "darkgrey",
                se = FALSE,
                lty = "dotted") +
    annotate(
      "text",
      x = max(broadleaf_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(broadleaf_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0(
        "slope = ", round(stats$slope, 2),
        "\nR² = ", round(stats$r2, 2)
      ),
      hjust = 0,
      size = 3.5
    ) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Extract legend, remove from panels, and assemble grid
library(cowplot)

legend <- get_legend(
  plots_annotated[[1]] +
    theme(legend.position = "right",
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

plots_noleg <- lapply(plots_annotated, function(p) {
  p + theme(legend.position = "none")
})

legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

final_grid_annotated <- plot_grid(
  plotlist = c(plots_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#add title so we'll be able to tell apart the megaplots with different cover types

#first put a little space on the top of the plot

final_with_title <- plot_grid(
  ggplot() + theme_void(),   # empty spacer
  final_grid_annotated,
  ncol = 1,
  rel_heights = c(0.08, 1)   # increase first value for more space
)

final_with_title <- ggdraw(final_with_title) +
  draw_label(
    "Estimated broadleaf cover",
    x = 0.05,
    hjust = 0,
    y = 0.98,
    vjust = 1,
    fontface = "bold",
    size = 16
  )

final_with_title

#save to pdf
pdf("plots/estimatecorrelation_bymethod_broadleaf.pdf", height=8, width=8)
final_with_title
dev.off()
#######
#do bare

#First strip out Site al data:

survey_bare <- survey_combined %>%
  filter(Method != "Site al")

# Pivot to wide using bare
bare_wide <- survey_bare %>%
  select(Site, plot, Method, bare) %>%
  unite(plot_site, Site, plot, remove = FALSE) %>%
  pivot_wider(names_from = Method, values_from = bare)

# Generate pairwise method combinations
methods <- names(bare_wide)[
  !(names(bare_wide) %in% c("Site", "plot", "plot_site"))
]
pairs <- combn(methods, 2, simplify = FALSE)

#build the base scatterplots
library(rlang)

plots <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  ggplot(bare_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Function to compute slope + R²
#repeat but maybe I broke it with all the other analyses
#paste in to be sure
library(broom)

get_lm_stats_safe <- function(df, xvar, yvar) {
  formula <- as.formula(paste0("`", yvar, "` ~ `", xvar, "`"))
  fit <- lm(formula, data = df)
  
  tidy_fit <- tidy(fit)
  glance_fit <- glance(fit)
  
  list(
    slope = tidy_fit$estimate[2],
    r2    = glance_fit$r.squared
  )
}

#Annotate the plots with slope + R²
plots_annotated <- lapply(pairs, function(vars) {
  xvar <- vars[1]
  yvar <- vars[2]
  
  stats <- get_lm_stats_safe(bare_wide, xvar, yvar)
  
  ggplot(bare_wide,
         aes(x = .data[[xvar]],
             y = .data[[yvar]],
             color = Site)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm",
                color = "darkgrey",
                se = FALSE,
                lty = "dotted") +
    annotate(
      "text",
      x = max(bare_wide[[xvar]], na.rm = TRUE) * 0.1,
      y = max(bare_wide[[yvar]], na.rm = TRUE) * 0.85,
      label = paste0(
        "slope = ", round(stats$slope, 2),
        "\nR² = ", round(stats$r2, 2)
      ),
      hjust = 0,
      size = 3.5
    ) +
    theme_minimal() +
    labs(x = xvar, y = yvar) +
    scale_color_brewer(palette = "Set2")
})

#Extract legend, remove from panels, and assemble grid
library(cowplot)

legend <- get_legend(
  plots_annotated[[1]] +
    theme(legend.position = "right",
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 12))
)

plots_noleg <- lapply(plots_annotated, function(p) {
  p + theme(legend.position = "none")
})

legend_panel <- ggplot() + theme_void() + annotation_custom(legend)

final_grid_annotated <- plot_grid(
  plotlist = c(plots_noleg, list(legend_panel)),
  ncol = 3,
  align = "hv"
)

final_grid_annotated

#add title so we'll be able to tell apart the megaplots with different cover types

#first put a little space on the top of the plot

final_with_title <- plot_grid(
  ggplot() + theme_void(),   # empty spacer
  final_grid_annotated,
  ncol = 1,
  rel_heights = c(0.08, 1)   # increase first value for more space
)

final_with_title <- ggdraw(final_with_title) +
  draw_label(
    "Estimated bare cover",
    x = 0.05,
    hjust = 0,
    y = 0.98,
    vjust = 1,
    fontface = "bold",
    size = 16
  )

final_with_title

#save to pdf
pdf("plots/estimatecorrelation_bymethod_bare.pdf", height=8, width=8)
final_with_title
dev.off()

