library(dplyr)
library(ggplot2)

df <- read.csv("results/intermediate/cleaned_data.csv")

# Per-station mean concentration profile across the 6 pollutants
station_profiles <- df %>%
  group_by(station) %>%
  summarise(
    PM2.5 = mean(PM2.5, na.rm = TRUE),
    PM10  = mean(PM10,  na.rm = TRUE),
    SO2   = mean(SO2,   na.rm = TRUE),
    NO2   = mean(NO2,   na.rm = TRUE),
    CO    = mean(CO,    na.rm = TRUE),
    O3    = mean(O3,    na.rm = TRUE)
  )

profile_matrix <- station_profiles %>% select(-station) %>% scale()

# WSS elbow plot to justify k=3 (vs picking from a PCA projection by eye)
wss <- sapply(1:8, function(k) {
  set.seed(42)
  kmeans(profile_matrix, centers = k, nstart = 25)$tot.withinss
})
elbow_df <- data.frame(k = 1:8, WSS = wss)
p_elbow <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line() + geom_point(size = 3) +
  scale_x_continuous(breaks = 1:8) +
  theme_minimal() +
  labs(title = "K-Means Elbow: Within-Cluster SS vs k",
       x = "Number of clusters (k)", y = "Total within-cluster SS")
ggsave("plots/kmeans_elbow.png", p_elbow, width = 7, height = 5)

set.seed(42)
km_res <- kmeans(profile_matrix, centers = 3, nstart = 25)

station_profiles$Cluster <- as.factor(km_res$cluster)

# Label clusters by mean PM2.5: highest -> Industrial, lowest -> Residential
cluster_order <- station_profiles %>%
  group_by(Cluster) %>%
  summarise(m = mean(PM2.5)) %>%
  arrange(desc(m))

station_profiles <- station_profiles %>%
  mutate(Zone = case_when(
    Cluster == cluster_order$Cluster[1] ~ "Industrial/Heavy",
    Cluster == cluster_order$Cluster[2] ~ "Mixed/Urban",
    Cluster == cluster_order$Cluster[3] ~ "Residential/Green"
  ))

print(station_profiles %>% select(station, Zone))

# PCA projection just for visualization
pca_res <- prcomp(profile_matrix)
pca_df  <- data.frame(pca_res$x[, 1:2],
                      Zone = station_profiles$Zone,
                      station = station_profiles$station)

p_cluster <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Zone)) +
  geom_point(size = 4) +
  geom_text(aes(label = station), vjust = -1, size = 3) +
  theme_minimal() +
  labs(title = "K-Means Clustering of Beijing Monitoring Stations",
       subtitle = "Pollutant profiles, PCA projection")

ggsave("plots/station_clusters.png", p_cluster, width = 10, height = 7)
write.csv(station_profiles, "results/intermediate/station_clusters.csv", row.names = FALSE)
