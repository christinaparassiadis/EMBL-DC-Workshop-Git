plt <- ggplot(data = surveys_complete,
              mapping = aes(x = weight, y = hindfoot_length)) + geom_point()

plt + ggtitle("My first plot")

# hexbin has more possibilites for plotting 
install.packages("hexbin")
library(hexbin)

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) 
+ geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.25, aes(color = species_id))

# challenge: scatterplot of weight vs species_id color by plot_type 

ggplot(data = surveys_complete, mapping = aes(x = weight, y = species_id)) +
  geom_point(aes(color = plot_type))

# this plot is not good at reflecting the data, better achieved by boxplots
# jitter adds value for each coordinate
ggplot(data = surveys_complete, mapping = aes(x = weight, y = species_id)) +
  geom_boxplot()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = species_id)) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3, color = "salmon")

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin()

# transform the scale 
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin() + scale_y_log10() + ylab("weight(log10)")

# Create a boxplot + jittered scatter plot of hindfoot_length by species_id
# Boxplot in front of the dots and filled with white 

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length)) +
  geom_jitter(aes(color = factor(plot_id))) + 
  geom_boxplot(outlier.shape = NA)


#
yearly_count <- surveys_complete %>% count(year, genus)

ggplot(data = yearly_count, 
       mapping = aes(
         x=year, 
         y=n, 
         color = genus)) + 
  geom_line() +
  theme(panel.grid = element_blank())


# one plot per genus with facetting 
plt <- ggplot(data = yearly_count, mapping = aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

plt <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
      mapping = aes(
        x=year, 
        y=n, 
        color = sex)) + 
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  xlab("Year of observation") +
  ylab("Number of individuals")+
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom", 
        aspect.ratio = 1, 
        axis.text.x = element_text(
          angle = 45,
          hjust = 1), 
        panel.grid = element_blank())
plt
ggsave(filename = "data/plot.pdf", plot = plt, width = 10, height = 10)

#
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x=year, 
      y=n, 
      color = sex)) + 
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
  )

# if you want to have labels for specific dots (for ex for genes),
# create a new column with NAs and these genes, 
# then use geom_text to annotate these 






