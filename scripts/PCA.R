# Principal component analysis 
# dimensionality reduction technique 
# which are the genes that contribute to the components, 
# can be extracted out of this analysis

library(tidyverse)

trans_cts <-  read_csv("data_RNA-seq/counts_transformed.csv")
sample_info <- read_csv("data_RNA-seq/sample_info.csv")

# convert dataframe to a matrix
# t is to transposase matrix 
pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t()

# prcomb function 
sample_pca <- prcomp(pca_matrix)

class(sample_pca)
str(sample_pca)
summary(sample_pca)

pca_matrix[1:10, 1:5]
as.tibble(pca_matrix)
as.tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))

# plotting: pareto plot
# upper line is cumulative of each component 
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  labs(x = "Principal component", y = "Fraction variance explained")

# plot PCA

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

# PCA plot
pca_plot <- pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, y = PC2, 
             colour = factor(minute), 
             shape = strain )) +
  geom_point()

pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull (gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

# Loadings plot
loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene), nudge_y = 0.005, size =3) +
  scale_x_continuous(expand = c(0.02, 0.02))

# Create panel of plots 
library(patchwork)

# horizontal alignment 
(pca_plot | loadings_plot)

# vertical alignment 
(pca_plot / loadings_plot)

(pca_plot | pca_plot | pca_plot) / loadings_plot +
  plot_annotation(tag_levels = "A")

# Shortcuts
# 
# to directly show PCA plot without tibbles
library(ggfortify)

PCA_plt <- autoplot(sample_pca, 
         data = sample_info %>% mutate(minute = as.factor(minute)), 
         color = "minute", 
         shape = "strain")

#
library(broom)

tidy(sample_pca, matrix = "eigenvalues")
tidy(sample_pca, matrix = "loadings")


# Differential expression results 
# in the test results file DeSeq2 was done comparing samples to the 0 minute point 
# comparison column indicates time point 
test_results

# MA plot to plot base mean (x) against log2fc (y)
# organize panels by comparing time points 
# consider log transformation of base mean 

test_result %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

# filter data for significant log2fc
# could also mutate data to show the according gene 
MA_plot <- test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato", size = 1) +
  geom_hline(yintercept = 0, color = "dodgerblue") + 
  facet_wrap(facets = vars(comparison))

# genes with higher expression change less, which is why we see the shape  
 
# compile both plots 
(MA_plot | PCA_plt)

# compile list of significantly changed genes 
# Visualizing expression trends 
# Step 1: Get candidate genes (pdaj < 0.01)
# pull function to filter out rows, table becomes a vector

candidate_genes <- test_result %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>% 
  unique()

# we need the trans_cts_long
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

# Step 2: filter trans_cts_long for candidate genes and compute mean expression
# value for each gene in each timepoint and each genotype 

trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts = mean(cts), nrep = n()) %>% 
  ungroup()

# Plot trends 
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# Scaling data to improve visualization
#  Z-score transformation
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene) %>% 
  mutate (cts_scaled = (cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>% 
  ungroup()

trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "brown", linetype = "dashed") +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))

# each line shows one gene 
# there are groups of genes behaving in a similar manner
# which is why it makes sense to do clustering analysis 
# trying different clustering methods to see different results
# 
# Clustering 
# we need trans_cts file, which is in a wide format
# Step 1: Create a matrix of counts

hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

# assign rownames
# transpose the matrix so genes are columns
# apply scaling to each column of the matrix
# transpose back so genes are as rows again 
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_genes,]

hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>% 
  t()

genes_dist <- dist(hclust_matrix)

# hierarchical clustering
gene_hclust <- hclust(genes_dist, method = "complete")

plot(gene_hclust, labels = F)
abline(h = 8, col = "brown", lwd =2)

# make clusters based on the number that I want
# change clusters adjusting to the plots, first try 5 then 10 

cutree(gene_hclust, k = 10)

gene_cluster <- cutree(gene_hclust, k =10) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) + 
  facet_grid(cols = vars(cluster), rows = vars(strain))

# draw a heatmap
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)












