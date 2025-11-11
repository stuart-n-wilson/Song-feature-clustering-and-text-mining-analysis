# Libraries ---------------------------------------------------------------
library(tidyverse)
library(patchwork) # plot multiple ggplots together
library(paletteer) # colour themes
library(gt) # view data neatly
library(factoextra) # visualise clusters
library(reshape2) # for melt
library(viridis) # colours
# Data preparation --------------------------------------------------------

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

#440154FF acousticness
#482878FF danceability
#31688EFF energy
#26828EFF instrumentalness
#1F9E89FF liveness
#35B779FF loudness
#6DCD59FF speechiness
#3E4A89FF song duration
#B4DE2CFF tempo
#FDE725FF valence

p_acou <- df |> 
   ggplot(aes(acousticness)) +
   geom_density(fill = "#440154FF") +
   theme_minimal() +
   labs(title = "Acousticness",
        x = "")

p_dance <- df |> 
   ggplot(aes(danceability)) +
   geom_density(fill = "#482878FF") +
   theme_minimal() +
   labs(title = "Danceability",
        x = "")

p_energy <- df |> 
   ggplot(aes(energy)) +
   geom_density(fill = "#31688EFF") +
   theme_minimal() +
   labs(title = "Energy",
        x = "")

p_instru <- df |> 
   ggplot(aes(instrumentalness)) +
   geom_density(fill = "#26828EFF", bw = 0.003) +
   coord_cartesian(xlim = c(0, 0.05)) +
   theme_minimal() +
   labs(title = "Instrumentalness (< 0.05)",
        x = "")

p_key <- df |> 
   ggplot(aes(key, fill = key)) +
   geom_bar() +
   scale_fill_viridis_d() +
   theme_minimal() +
   labs(title = "Key",
        x = "") +
   theme(legend.position = "none")

p_live <- df |> 
   ggplot(aes(liveness)) +
   geom_density(fill = "#1F9E89FF") +
   theme_minimal() +
   labs(title = "Liveness",
        x = "")

p_loud <- df |> 
   ggplot(aes(loudness)) +
   geom_density(fill = "#35B779FF") +
   theme_minimal() +
   labs(title = "Loudness",
        x = "")

p_mode <- df |> 
   ggplot(aes(mode, fill = mode)) +
   geom_bar() +
   theme_minimal() +
   scale_fill_viridis_d() +
   labs(title = "Mode",
        x = "") + 
   theme(legend.position = "none")

p_s_dur <- df |> 
   ggplot(aes(duration_ms)) +
   geom_density(fill = "3E4A89FF") +
   theme_minimal() +
   labs(title = "Song duration",
        x = "")

p_speech <- df |>
   filter(speechiness > 0) |> 
   ggplot(aes(speechiness)) +
   geom_density(fill = "#6DCD59FF") +
   scale_x_continuous(transform = "log10") +
   theme_minimal() +
   labs(title = "Speechiness (log transform)",
        x = "")

p_t_sig <- df |> 
   ggplot(aes(time_signature, fill = time_signature)) +
   geom_bar() +
   theme_minimal() +
   labs(title = "Time signature",
        x = "") +
   theme(legend.position = "none")

p_tempo <- df |> 
   ggplot(aes(tempo)) +
   geom_density(fill = "#B4DE2CFF") +
   theme_minimal() +
   labs(title = "Tempo",
        x = "")

p_val <- df |> 
   ggplot(aes(valence)) +
   geom_density(fill = "#FDE725FF") +
   theme_minimal() +
   labs(title = "Valence",
        x = "")

# Format the combined plot.

design <- "
   ABCD
   EFGH
   IJKK
   MMPP
   "

p_acou + p_dance  + p_energy + p_instru  + p_live + p_loud + p_s_dur + 
   p_speech + p_tempo + p_val + p_mode + p_t_sig + p_key + 
   plot_layout(design = design) +
   plot_annotation(title = "Song features")



# Correlation matrix ------------------------------------------------------

# We consider only the numeric values
df_numeric <- df |> 
   select(!c("song_id", "key", "mode", "time_signature", "lyrics"))

corr_matrix <- cor(df_numeric, method = "spearman")

# Turn matrix into table for ggplot
corr_matrix <- melt(corr_matrix)

# Fill with absolute value for readability
ggplot(corr_matrix,
       aes(x = Var1, y = Var2, fill = abs(value))) +
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

ggplot(df, aes(x = loudness, y = energy)) +
   geom_point() +
   theme_minimal()

ggplot(df, aes(x = acousticness, y = energy, colour = loudness)) +
   geom_point() +
   theme_minimal() +
   scale_colour_viridis_c()


ggplot(df, aes(x = acousticness, y = loudness, colour = energy)) +
   geom_point() +
   theme_minimal() +
   scale_colour_viridis_c()

ggplot(df, aes(x = valence, y = danceability)) +
   geom_point() +
   theme_minimal()

# Normalising the values --------------------------------------------------


# DO NOT SPLIT DATA. FIX THIS!!