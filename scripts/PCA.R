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

autoplot(sample_pca, 
         data = sample_info, 
         color = "minute", 
         shape = "strain")

#
library(broom)

tidy(sample_pca, matrix = "eigenvalues")
tidy(sample_pca, matrix = "loadings")





