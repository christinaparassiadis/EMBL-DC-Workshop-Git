# RNA-seq analysis
# transformed file is normalized counts from DeSeq2
# Create for objects called:

raw_cts <- counts_raw
trans_cts <- counts_transformed
sample_info <- sample_info
test_result <- test_result

# load tidyverse
# then run: raw_cts <- read.csv("data_RNA-seq/counts_raw.csv)
library(tidyverse)

raw_cts <- read.csv("data_RNA-seq/counts_raw.csv")
trans_cts <- read_csv("data_RNA-seq/counts_transformed.csv")
sample_info <- read_csv("data_RNA-seq/sample_info.csv")
test_result <- read_csv("data_RNA-seq/test_result.csv")

# for visualization, transform data to a long format to use ggplot

trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

# combine informations from two tables with join function 
# inner join only takes rows of tables with information
# left join joins two tables and will fill up information, even if there is no info

trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

# plot data from long format joined table in a histogram 
trans_cts_long %>% 
  ggplot(aes(x = cts)) +
  geom_freqpoly()

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

# same plot with raw_cts 
raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

raw_cts_long <- full_join(raw_cts_long, sample_info, by = "sample")

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) 

# transform scale 
raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) +
  scale_x_log10()

raw_cts_long %>% 
  ggplot(aes(x = log10(cts + 1), colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

# Making a boxplot 
raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts +1), fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(cols = vars(replicate))

# Correlation between wt sample at 0 and 30 minutes 
# Use the wide format table for this 

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point() +
  geom_abline(colour = "brown")

# Look at the correlation of count data across all samples 
trans_cts_corr <- trans_cts %>% 
  select(-gene) %>% 
  cor(method = "spearman")

# To visualize this, create a heatmap with the package corrr
library(corrr)

rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# compare trans_cts and raw_cts 
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point()

raw_cts %>% 
  ggplot(aes(x = wt_0_r1 +1, y = wt_0_r2)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

raw_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()







