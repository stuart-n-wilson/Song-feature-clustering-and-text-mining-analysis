# Libraries ---------------------------------------------------------------

# Wrangling
library(tidyverse)
library(reshape2)
library(fastmatch)

# Machine Learning
library(factoextra)

# Visualisation
library(patchwork) 
library(gt)
library(viridis)
library(ggwordcloud)

# Text mining
library(tm)
library(textclean)
library(textstem)
library(tidytext)
library(qdapDictionaries)


# Importing data ----------------------------------------------------------

# Files should be in working directory.
acoustic_features <- read.delim(
  "acoustic_features.csv",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

lyrics <- read.delim(
  "lyrics.csv",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Merge dataframes by song_id. Remove songs without lyrics.
df <- inner_join(acoustic_features, lyrics, by = "song_id") |>
  filter(!(lyrics == ""))

# Check for any remaining NA's or empties.
sum(is.na(df) | df == "")

# Check for duplicate songs
length(unique(df$song_id))
nrow(df)


# Exploring the data ------------------------------------------------------

# Look at data, except lyrics and song_id, for display reasons.

df |>
  select(-lyrics) |>
  select(-song_id) |>
  head() |>
  gt() |>
  tab_header(
    title = "Song features"
  )

# We see that we should treat key, mode and time_signature as factors.

df <- df |>
  mutate(
    key = as.factor(key),
    mode = as.factor(mode),
    time_signature = as.factor(time_signature)
  )

# Feature visualisation ---------------------------------------------------

# Generate 10 colour palette from viridis.
# show_col(viridis_pal()(10))

# 440154FF acoustic
# 482878FF dance
# 3E4A89FF energy
# 31688EFF instrumentalness
# 26828EFF liveness
# 1F9E89FF loudness
# 35B779FF song duration
# 6DCD59FF speechiness
# B4DE2CFF tempo
# FDE725FF valence

p_acou <- df |>
  ggplot(aes(acousticness)) +
  geom_density(fill = "#440154FF") +
  theme_minimal() +
  labs(
    title = "Acousticness",
    x = ""
  )

p_dance <- df |>
  ggplot(aes(danceability)) +
  geom_density(fill = "#482878FF") +
  theme_minimal() +
  labs(
    title = "Danceability",
    x = ""
  )

p_energy <- df |>
  ggplot(aes(energy)) +
  geom_density(fill = "#3E4A89FF") +
  theme_minimal() +
  labs(
    title = "Energy",
    x = ""
  )

p_instru <- df |>
  ggplot(aes(instrumentalness)) +
  geom_density(fill = "#31688EFF", bw = 0.003) +
  coord_cartesian(xlim = c(0, 0.05)) +
  theme_minimal() +
  labs(
    title = "Instrumentalness (< 0.05)",
    x = ""
  )

p_key <- df |>
  ggplot(aes(key, fill = key)) +
  geom_bar() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(
    title = "Key",
    x = ""
  ) +
  theme(legend.position = "none")

p_live <- df |>
  ggplot(aes(liveness)) +
  geom_density(fill = "#26828EFF") +
  theme_minimal() +
  labs(
    title = "Liveness",
    x = ""
  )

p_loud <- df |>
  ggplot(aes(loudness)) +
  geom_density(fill = "#1F9E89FF") +
  theme_minimal() +
  labs(
    title = "Loudness",
    x = ""
  )

p_mode <- df |>
  ggplot(aes(mode, fill = mode)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "Mode",
    x = ""
  ) +
  theme(legend.position = "none")

p_s_dur <- df |>
  ggplot(aes(duration_ms)) +
  geom_density(fill = "#35B779FF") +
  theme_minimal() +
  labs(
    title = "Song duration",
    x = ""
  )

p_speech <- df |>
  filter(speechiness > 0) |>
  ggplot(aes(speechiness)) +
  geom_density(fill = "#6DCD59FF") +
  scale_x_continuous(transform = "log10") +
  theme_minimal() +
  labs(
    title = "Speechiness (log transform)",
    x = ""
  )

p_t_sig <- df |>
  ggplot(aes(time_signature, fill = time_signature)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Time signature",
    x = ""
  ) +
  theme(legend.position = "none")

p_tempo <- df |>
  ggplot(aes(tempo)) +
  geom_density(fill = "#B4DE2CFF") +
  theme_minimal() +
  labs(
    title = "Tempo",
    x = ""
  )

p_val <- df |>
  ggplot(aes(valence)) +
  geom_density(fill = "#FDE725FF") +
  theme_minimal() +
  labs(
    title = "Valence",
    x = ""
  )

# Format the combined plot.

design <- "
   ABCD
   EFGH
   IJKK
   MMPP
   "

p_acou + p_dance + p_energy + p_instru + p_live + p_loud + p_speech +
  p_s_dur + p_tempo + p_val + p_mode + p_t_sig + p_key +
  plot_layout(design = design) +
  plot_annotation(title = "Song features")


# Correlation matrix ------------------------------------------------------

# We consider only the numeric values, as this will be for the model.
df_numeric <- df |>
  select(-song_id, -key, -mode, -time_signature, -lyrics)

corr_matrix <- cor(df_numeric, method = "spearman")

# Turn matrix into table for ggplot
corr_matrix <- melt(corr_matrix)

# Fill with absolute value for readability
ggplot(
  corr_matrix,
  aes(x = Var1, y = Var2, fill = abs(value))
) +
  geom_tile() +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme(
    legend.key.height = unit(3, "cm"),
    legend.title = element_blank()
  ) +
  labs(
    x = "",
    y = "",
    title = "Absolute Spearman correlation matrix"
  )

# Further investigation into variable correlations.

ggplot(df, aes(x = loudness, y = energy, colour = acousticness)) +
  geom_point() +
  theme_minimal() +
  scale_colour_viridis_c() +
  labs(title = "Loudness energy correlation") +
  theme(legend.key.height = unit(3, "cm"))

ggplot(df, aes(x = acousticness, y = energy, colour = loudness)) +
  geom_point() +
  theme_minimal() +
  scale_colour_viridis_c() +
  labs(title = "Acousticness energy correlation") +
  theme(legend.key.height = unit(3, "cm"))

ggplot(df, aes(x = acousticness, y = loudness, colour = energy)) +
  geom_point() +
  theme_minimal() +
  scale_colour_viridis_c() +
  labs(title = "Acousticness loudness correlation") +
  theme(legend.key.height = unit(3, "cm"))

ggplot(df, aes(x = valence, y = danceability)) +
  geom_point(colour = "#440154FF") +
  theme_minimal() +
  labs(title = "Valence danceability correlation")


# Principal component analysis --------------------------------------------

# Run PCA on numeric data, scale it.
pca <- prcomp(df_numeric, scale. = TRUE)

# Convert summary into df, simply for presenting
pca_summary <- summary(pca)

pca_df <- as.data.frame(t(pca_summary$importance)) |>
  rownames_to_column(var = "PC") |>
  gt() |>
  tab_header(title = "Principal component summary")
pca_df


# Investigate loadings for each PC
pca_loadings_df <- as.data.frame(t(pca$rotation[, 1:5]))

# Sort col names alphabetically first
pca_loadings_df <- pca_loadings_df[, order(colnames(pca_loadings_df))]

pca_loadings_gt <- pca_loadings_df |>
  rownames_to_column(var = "Variable") |>
  gt() |>
  tab_header(title = "Principal component loadings") |>
  fmt_number(
    columns = -Variable,
    decimals = 3
  )
pca_loadings_gt

# Visualise loadings
pca_loadings_df <- pca_loadings_df |>
  rownames_to_column(var = "PC") |>
  pivot_longer(
    cols = -PC,
    names_to = "Variable",
    values_to = "Loading"
  )

ggplot(pca_loadings_df, aes(x = Variable, y = PC, fill = abs(Loading))) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.height = unit(3, "cm")
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Principal component loadings heatmap",
    x = "Variable",
    y = "Principal Component",
    fill = "Loading (abs)"
  )

# Extract first 5 PCs, this explains about 70% of the variance
pca_data <- as.data.frame(pca$x[, 1:5])


# Scree plot --------------------------------------------------------------

screeplot <- fviz_nbclust(
  pca_data,
  kmeans,
  method = "wss",
  k.max = 10,
  nstart = 25,
  iter.max = 100
)

screeplot +
  labs(title = "Scree plot - Elbow Method for Optimal Clusters") +
  theme_minimal() +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 3)

# No obvious 'elbow'


# k-means model -----------------------------------------------------------

# Create and visualise model for k = 2 to k = 5

cluster_plots <- list()

set.seed(123)

for (k in 2:10) {
  # km model
  km.out <- kmeans(pca_data, centers = k, nstart = 25, iter.max = 100)

  # Visualisation
  cluster_plot <- fviz_pca_ind(
    pca,
    habillage = km.out$cluster,
    label = "none",
    geom = "point",
    palette = viridis(k)
  ) +
    ggtitle(paste("k = ", k)) +
    theme(legend.position = "none")

  cluster_plots[[paste0("k", k)]] <- cluster_plot
}

cluster_plots$k2 + cluster_plots$k3 + cluster_plots$k4 + cluster_plots$k5 +
   plot_annotation(title = "k-means clustering for k = 2 - 5")


# Confirm correct number of clusters with silhouette plot
silhouette_plot <- fviz_nbclust(pca_data,
  kmeans,
  method = "silhouette"
)

silhouette_plot +
  theme_minimal() +
  labs(title = "Optimal number of clusters - silhouette method") +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 3)


# This confirms that k = 2 is optimal.
set.seed(123)
km.out <- kmeans(pca_data, centers = 2, nstart = 25, iter.max = 100)

fviz_pca_ind(
  pca,
  habillage = km.out$cluster,
  label = "none",
  geom = "point",
  palette = viridis(2)
) +
  labs(title = "Song feature k-means clustering, with k = 2")


# Numeric feature cluster analysis ----------------------------------------

# Add cluster col to df_numeric
df_clusters_num <- df_numeric |>
  mutate(cluster = factor(km.out$cluster))

# Sort col names alphabetically then produce gt table
df_clusters_num <- df_clusters_num[, order(colnames(df_clusters_num))]
df_clusters_num |>
  group_by(cluster) |>
  summarise(across(everything(), median)) |>
  gt() |>
  tab_header("Numeric features for the two clusters") |>
  fmt_number(decimals = 3)

# Pivot long for plotting numeric feature comparison
df_long <- df_clusters_num %>%
  select(acousticness, energy, valence, cluster) |>
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value")

ggplot(df_long, aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~feature) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "cm"),
    legend.position = "none"
  ) +
  labs(
    title = "Numeric feature distributions by cluster",
    x = "Cluster",
    y = "Value"
  ) +
  scale_fill_viridis_d()


# Categorical feature cluster analysis ------------------------------------

df_clusters_cat <- df |>
  select(key, mode, time_signature) |>
  mutate(cluster = factor(km.out$cluster))

df_clusters_cat_long <- df_clusters_cat |>
  group_by(cluster) |>
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value")

ggplot(
  df_clusters_cat_long,
  aes(x = value, y = after_stat(prop), fill = cluster, group = cluster)
) +
  geom_bar(position = "dodge") +
  facet_wrap(~feature, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Category value",
    y = "Proportion",
    title = "Proportion of categorical features by cluster"
  ) +
  scale_fill_viridis_d()

# No significant changes between the clusters.


# Lyrics text mining ------------------------------------------------------

# Pre-processing

df_lyrics <- df |>
  select(lyrics) |>
  mutate(cluster = km.out$cluster) |>
  filter(lyrics != "")


clean_lyrics <- function(x) {
  x |>
    # remove leading [" or ['
    str_remove("^\\[[\"']") |>
    # remove trailing "]
    str_remove("[\"']\\]$") |>
    # convert \n to space
    str_replace_all("\\\\n", " ") |>
    # convert \xa0 to space
    str_replace_all("\\\\xa0", " ") |>
    # remove any [Verse], [Chorus], etc.
    str_remove_all("\\[.*?\\]") |>
    # fix \'
    str_replace_all("\\\\'", "'") |>
    # expand contractions
    replace_contraction() |>
    # lower-case
    str_to_lower() |>
    # remove punctuation
    removePunctuation() |>
    # remove numbers
    removeNumbers() |>
    # lemmatize
    lemmatize_strings()
}


# Create new col with cleaned lyrics (this takes a long time to run).
df_lyrics <- df_lyrics |>
  mutate(clean_lyrics = clean_lyrics(lyrics))


# Create word df with tokenisation, remove stopwords in process, use only
# word in dictionary.
data("stop_words")

words_df <- df_lyrics |>
  filter(lyrics != "") |>
  unnest_tokens(word, clean_lyrics) |>
  anti_join(stop_words, by = "word") |>
  filter(word %fin% GradyAugmented) |>
  filter(nchar(word) > 2)


# Lyric visualisations ----------------------------------------------------

# Top words for all songs
word_count <- words_df |>
  select(word, cluster) |>
  group_by(word) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  # rotate about 85% of the words for the wordcloud
  mutate(angle = 45 * sample(-2:2, n(),replace = TRUE, prob = c(1, 1, 6, 1, 1))
         )

ggplot(word_count[1:40, ], aes(
  label = word,
  size = count,
  angle = angle,
  color = factor(sample.int(40, 40, replace = TRUE))
)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 50) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(
    title = "40 most common lyrics from over 20,000 songs",
    subtitle = "Lyrics have been filtered and lemmatised",
    caption = "Data source: MusicOSet"
  )

# Top words in cluster 1 (higher energy)
word_count_cluster_1 <- words_df |>
  select(word, cluster) |>
  filter(cluster == 1) |>
  group_by(word) |>
  summarise(count = n()) |>
  arrange(desc(count))

cluster1_words <- ggplot(word_count_cluster_1[1:40, ], aes(
  label = word,
  size = count,
  color = factor(sample.int(40, 40, replace = TRUE))
)) +
  geom_text_wordcloud_area(shape = "square", eccentricity = 1) +
  scale_size_area(max_size = 40) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(title = "Cluster 1")

# Top words in cluster 2 (more acoustic)
word_count_cluster_2 <- words_df |>
  select(word, cluster) |>
  filter(cluster == 2) |>
  group_by(word) |>
  summarise(count = n()) |>
  arrange(desc(count))

cluster2_words <- ggplot(word_count_cluster_2[1:40, ], aes(
  label = word,
  size = count,
  color = factor(sample.int(40, 40, replace = TRUE))
)) +
  geom_text_wordcloud_area(shape = "square", eccentricity = 1) +
  scale_size_area(max_size = 40) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(title = "Cluster 2")


cluster1_words / cluster2_words +
   plot_annotation(title = "40 most common lyrics, by cluster")


