suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(xgboost); library(pROC)
})

df <- read.csv("results/intermediate/featured_data.csv")
df$datetime <- as.POSIXct(df$datetime)

# Strict temporal split. Threshold tuning happens on val only.
train_df <- df %>% filter(year %in% c(2013, 2014))
val_df   <- df %>% filter(year == 2015)
test_df  <- df %>% filter(year == 2016)


# ----- Helpers -----

get_f1 <- function(actual, pred) {
  cm <- table(factor(actual, levels = c(0, 1)), factor(pred, levels = c(0, 1)))
  tp <- cm[2, 2]; fp <- cm[1, 2]; fn <- cm[2, 1]
  if (tp + fp == 0 || tp + fn == 0) return(0)
  prec <- tp / (tp + fp); rec <- tp / (tp + fn)
  if (prec + rec == 0) return(0)
  2 * prec * rec / (prec + rec)
}

precision_at_recall <- function(probs, actual, target_recall = 0.9) {
  ord       <- order(probs, decreasing = TRUE)
  y_sorted  <- actual[ord]
  cum_tp    <- cumsum(y_sorted == 1)
  recalls   <- cum_tp / sum(actual == 1)
  precisions <- cum_tp / seq_along(actual)
  precisions[which.min(abs(recalls - target_recall))]
}

find_best_threshold <- function(probs, actual) {
  ts <- seq(0.05, 0.7, by = 0.013)
  scores <- sapply(ts, function(t) get_f1(actual, as.numeric(probs > t)))
  ts[which.max(scores)]
}

evaluate <- function(probs, actual, threshold = NULL,
                     persistence_threshold_aqi = NULL) {
  preds <- if (!is.null(persistence_threshold_aqi))
             as.numeric(probs > persistence_threshold_aqi)
           else
             as.numeric(probs > threshold)
  list(F1            = get_f1(actual, preds),
       AUC           = as.numeric(suppressMessages(pROC::auc(actual, probs))),
       Prec_at_90Rec = precision_at_recall(probs, actual, 0.9))
}

train_xgb <- function(features, target_col, train_set = train_df,
                      val_set = val_df, test_set = test_df) {
  dtrain <- xgb.DMatrix(as.matrix(train_set[, features]), label = train_set[[target_col]])
  dval   <- xgb.DMatrix(as.matrix(val_set[, features]),   label = val_set[[target_col]])
  dtest  <- xgb.DMatrix(as.matrix(test_set[, features]),  label = test_set[[target_col]])
  params <- list(objective = "binary:logistic", eta = 0.1, max_depth = 6,
                 eval_metric = "auc", nthread = 2, verbosity = 0)
  m <- xgb.train(params, dtrain, nrounds = 300,
                 evals = list(val = dval),
                 early_stopping_rounds = 20, verbose = 0)
  list(model = m,
       val_probs  = predict(m, dval),
       test_probs = predict(m, dtest))
}


# ----- Feature sets -----

neighbor_cols <- grep("^Neighbor_", names(df), value = TRUE)

features_lv1 <- c("AQI_lag1", "AQI_lag24", "AQI_lag168", "AQI_roll24",
                  "TEMP", "PRES", "DEWP", "WSPM",
                  "hour_sin", "hour_cos", "month_sin", "month_cos")
features_lv3 <- c(features_lv1, neighbor_cols)
features_lv4 <- c(features_lv3,
                  "wind_u_lag6", "wind_v_lag6", "wind_u_lag12", "wind_v_lag12",
                  "cluster_mean_AQI",
                  "zone_industrial", "zone_mixed", "zone_residential")


# ----- Main 24h ladder -----

results <- list()

# Lv0 - Persistence (current AQI as the score, threshold 150)
e <- evaluate(test_df$AQI, test_df$Target_Alert_24h, persistence_threshold_aqi = 150)
results[[1]] <- data.frame(Level = "Lv0", Model = "Persistence",
                           F1 = e$F1, AUC = e$AUC,
                           Prec_at_90Rec = e$Prec_at_90Rec, Threshold = 150)

# Lv1 - Logistic
m_lr <- glm(as.formula(paste("Target_Alert_24h ~", paste(features_lv1, collapse = " + "))),
            data = train_df, family = binomial)
val_probs  <- predict(m_lr, val_df,  type = "response")
test_probs <- predict(m_lr, test_df, type = "response")
val_probs[is.na(val_probs)]   <- mean(train_df$Target_Alert_24h)
test_probs[is.na(test_probs)] <- mean(train_df$Target_Alert_24h)
best_t <- find_best_threshold(val_probs, val_df$Target_Alert_24h)
e <- evaluate(test_probs, test_df$Target_Alert_24h, threshold = best_t)
results[[2]] <- data.frame(Level = "Lv1", Model = "Logistic",
                           F1 = e$F1, AUC = e$AUC,
                           Prec_at_90Rec = e$Prec_at_90Rec, Threshold = best_t)

# Lv2 - XGBoost on Lv1 features only
xgb_res <- train_xgb(features_lv1, "Target_Alert_24h")
best_t  <- find_best_threshold(xgb_res$val_probs, val_df$Target_Alert_24h)
e <- evaluate(xgb_res$test_probs, test_df$Target_Alert_24h, threshold = best_t)
results[[3]] <- data.frame(Level = "Lv2", Model = "XGBoost",
                           F1 = e$F1, AUC = e$AUC,
                           Prec_at_90Rec = e$Prec_at_90Rec, Threshold = best_t)

# Lv3 - + neighbor stations
xgb_res3 <- train_xgb(features_lv3, "Target_Alert_24h")
best_t   <- find_best_threshold(xgb_res3$val_probs, val_df$Target_Alert_24h)
e <- evaluate(xgb_res3$test_probs, test_df$Target_Alert_24h, threshold = best_t)
results[[4]] <- data.frame(Level = "Lv3", Model = "+Spatial",
                           F1 = e$F1, AUC = e$AUC,
                           Prec_at_90Rec = e$Prec_at_90Rec, Threshold = best_t)

# Lv4 - + lagged wind + K-means cluster
xgb_res4 <- train_xgb(features_lv4, "Target_Alert_24h")
best_t   <- find_best_threshold(xgb_res4$val_probs, val_df$Target_Alert_24h)
e <- evaluate(xgb_res4$test_probs, test_df$Target_Alert_24h, threshold = best_t)
results[[5]] <- data.frame(Level = "Lv4", Model = "+Wind+Cluster",
                           F1 = e$F1, AUC = e$AUC,
                           Prec_at_90Rec = e$Prec_at_90Rec, Threshold = best_t)

ladder_df <- do.call(rbind, results)
write.csv(ladder_df, "results/final/ladder_results.csv", row.names = FALSE)
print(ladder_df)


# Plot: ladder across the three metrics
ladder_long <- ladder_df %>%
  pivot_longer(cols = c(F1, AUC, Prec_at_90Rec),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("F1", "AUC", "Prec_at_90Rec"),
                         labels = c("F1-Score", "AUC", "Precision @ 90% Recall")),
         label = sprintf("%.3f", Value),
         Level_Model = paste(Level, Model))

p_ladder <- ggplot(ladder_long, aes(x = Level_Model, y = Value, fill = Level_Model)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.4, size = 3) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none",
        strip.text = element_text(face = "bold")) +
  labs(title = "Model Ladder: 24h AQI Alert Prediction (Test = 2016)",
       x = NULL, y = NULL) +
  expand_limits(y = c(0, max(ladder_long$Value) * 1.15))

ggsave("plots/model_ladder_performance.png", p_ladder, width = 12, height = 5, dpi = 130)


# Lv4 feature importance
imp <- xgb.importance(feature_names = features_lv4, model = xgb_res4$model)
imp_top <- head(imp[order(-Gain)], 15)
imp_top$kind <- ifelse(imp_top$Feature == "cluster_mean_AQI", "Cluster",
                ifelse(grepl("^Neighbor_", imp_top$Feature), "Neighbor",
                ifelse(grepl("^wind", imp_top$Feature), "Wind", "Other")))

p_imp <- ggplot(imp_top, aes(x = reorder(Feature, Gain), y = Gain, fill = kind)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Cluster" = "#034e7b", "Neighbor" = "#0570b0",
                               "Wind" = "#74a9cf", "Other" = "#999999")) +
  theme_minimal() +
  labs(title = "Lv4 Feature Importance: Cluster-mean AQI is the top predictor",
       x = NULL, y = "XGBoost Gain", fill = "Feature type")

ggsave("plots/feature_importance.png", p_imp, width = 9, height = 6, dpi = 130)
write.csv(imp, "results/final/feature_importance.csv", row.names = FALSE)


# ----- Multi-horizon ladder -----

horizons <- c(6, 24, 48, 72)
multi_results <- list()

for (h in horizons) {
  target_col <- sprintf("Target_Alert_%dh", h)
  if (!target_col %in% names(df)) next

  tr <- train_df %>% filter(!is.na(.data[[target_col]]))
  vl <- val_df   %>% filter(!is.na(.data[[target_col]]))
  te <- test_df  %>% filter(!is.na(.data[[target_col]]))

  # Lv0 persistence
  e <- evaluate(te$AQI, te[[target_col]], persistence_threshold_aqi = 150)
  multi_results[[length(multi_results) + 1]] <- data.frame(
    Horizon = h, Level = "Lv0", Model = "Persistence",
    F1 = e$F1, AUC = e$AUC, Prec_at_90Rec = e$Prec_at_90Rec)

  # Lv1 Logistic
  m_lr_h <- glm(as.formula(paste(target_col, "~", paste(features_lv1, collapse = " + "))),
                data = tr, family = binomial)
  vp <- predict(m_lr_h, vl, type = "response"); vp[is.na(vp)] <- mean(tr[[target_col]])
  tp <- predict(m_lr_h, te, type = "response"); tp[is.na(tp)] <- mean(tr[[target_col]])
  bt <- find_best_threshold(vp, vl[[target_col]])
  e  <- evaluate(tp, te[[target_col]], threshold = bt)
  multi_results[[length(multi_results) + 1]] <- data.frame(
    Horizon = h, Level = "Lv1", Model = "Logistic",
    F1 = e$F1, AUC = e$AUC, Prec_at_90Rec = e$Prec_at_90Rec)

  # Lv2/3/4 XGBoost variants
  for (lvl in c("Lv2", "Lv3", "Lv4")) {
    feats <- if (lvl == "Lv2") features_lv1
             else if (lvl == "Lv3") features_lv3
             else features_lv4
    res_h <- train_xgb(feats, target_col, train_set = tr,
                       val_set = vl, test_set = te)
    bt <- find_best_threshold(res_h$val_probs, vl[[target_col]])
    e  <- evaluate(res_h$test_probs, te[[target_col]], threshold = bt)
    multi_results[[length(multi_results) + 1]] <- data.frame(
      Horizon = h, Level = lvl,
      Model = ifelse(lvl == "Lv2", "XGBoost",
              ifelse(lvl == "Lv3", "+Spatial", "+Wind+Cluster")),
      F1 = e$F1, AUC = e$AUC, Prec_at_90Rec = e$Prec_at_90Rec)
  }
}

multi_df <- do.call(rbind, multi_results)
write.csv(multi_df, "results/final/multi_horizon_results.csv", row.names = FALSE)

p_multi <- ggplot(multi_df, aes(x = Horizon, y = AUC, color = Level, group = Level)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = horizons) +
  scale_color_brewer(palette = "Blues", direction = 1) +
  theme_minimal() +
  labs(title = "AUC vs Forecast Horizon",
       subtitle = "Persistence collapses past 24h; ML models maintain predictive power",
       x = "Forecast horizon (hours)", y = "AUC")

ggsave("plots/multi_horizon.png", p_multi, width = 9, height = 5, dpi = 130)


# ----- Cost-sensitive evaluation (24h horizon) -----

# Pick threshold on val that minimizes total cost; report cost on test.
cost_eval_persist <- function(C_miss, C_false) {
  best_cost <- Inf; best_thr <- 150
  for (thr in seq(50, 250, by = 5)) {
    pv <- as.numeric(val_df$AQI > thr)
    fn <- sum(val_df$Target_Alert_24h == 1 & pv == 0)
    fp <- sum(val_df$Target_Alert_24h == 0 & pv == 1)
    c  <- fn * C_miss + fp * C_false
    if (c < best_cost) { best_cost <- c; best_thr <- thr }
  }
  pt <- as.numeric(test_df$AQI > best_thr)
  fn <- sum(test_df$Target_Alert_24h == 1 & pt == 0)
  fp <- sum(test_df$Target_Alert_24h == 0 & pt == 1)
  list(cost = fn * C_miss + fp * C_false, FN = fn, FP = fp, thr = best_thr)
}

cost_eval_probs <- function(val_probs, test_probs, C_miss, C_false) {
  best_cost <- Inf; best_thr <- 0.5
  for (thr in seq(0.01, 0.99, by = 0.02)) {
    pv <- as.numeric(val_probs > thr)
    fn <- sum(val_df$Target_Alert_24h == 1 & pv == 0)
    fp <- sum(val_df$Target_Alert_24h == 0 & pv == 1)
    c  <- fn * C_miss + fp * C_false
    if (c < best_cost) { best_cost <- c; best_thr <- thr }
  }
  pt <- as.numeric(test_probs > best_thr)
  fn <- sum(test_df$Target_Alert_24h == 1 & pt == 0)
  fp <- sum(test_df$Target_Alert_24h == 0 & pt == 1)
  list(cost = fn * C_miss + fp * C_false, FN = fn, FP = fp, thr = best_thr)
}

# Re-fit Lv1 logistic for the cost analysis
m_lr <- glm(as.formula(paste("Target_Alert_24h ~", paste(features_lv1, collapse = " + "))),
            data = train_df, family = binomial)
lr_val  <- predict(m_lr, val_df,  type = "response")
lr_test <- predict(m_lr, test_df, type = "response")
lr_val[is.na(lr_val)]   <- mean(train_df$Target_Alert_24h)
lr_test[is.na(lr_test)] <- mean(train_df$Target_Alert_24h)

cost_results <- list()
ratios <- list(c("Equal (1:1)", 1, 1),
               c("Moderate (5:1)", 5, 1),
               c("Public health (10:1)", 10, 1),
               c("Severe (20:1)", 20, 1))

for (r in ratios) {
  ratio_name <- r[1]
  cm <- as.numeric(r[2]); cf <- as.numeric(r[3])

  e <- cost_eval_persist(cm, cf)
  cost_results[[length(cost_results) + 1]] <- data.frame(
    Ratio = ratio_name, Model = "Lv0 Persistence",
    Cost = e$cost, FN = e$FN, FP = e$FP)

  for (m_label in c("Lv1 Logistic", "Lv2 XGBoost", "Lv3 +Spatial", "Lv4 +Wind+Cluster")) {
    if (m_label == "Lv1 Logistic")      { vp <- lr_val;            tp <- lr_test }
    if (m_label == "Lv2 XGBoost")       { vp <- xgb_res$val_probs; tp <- xgb_res$test_probs }
    if (m_label == "Lv3 +Spatial")      { vp <- xgb_res3$val_probs; tp <- xgb_res3$test_probs }
    if (m_label == "Lv4 +Wind+Cluster") { vp <- xgb_res4$val_probs; tp <- xgb_res4$test_probs }
    e <- cost_eval_probs(vp, tp, cm, cf)
    cost_results[[length(cost_results) + 1]] <- data.frame(
      Ratio = ratio_name, Model = m_label,
      Cost = e$cost, FN = e$FN, FP = e$FP)
  }
}

cost_df <- do.call(rbind, cost_results)
write.csv(cost_df, "results/final/cost_results.csv", row.names = FALSE)

cost_df_norm <- cost_df %>%
  group_by(Ratio) %>%
  mutate(persist_cost = Cost[Model == "Lv0 Persistence"],
         pct = 100 * Cost / persist_cost) %>%
  ungroup()
cost_df_norm$Ratio <- factor(cost_df_norm$Ratio,
  levels = c("Equal (1:1)", "Moderate (5:1)", "Public health (10:1)", "Severe (20:1)"))
cost_df_norm$Model <- factor(cost_df_norm$Model,
  levels = c("Lv0 Persistence", "Lv1 Logistic", "Lv2 XGBoost", "Lv3 +Spatial", "Lv4 +Wind+Cluster"))

p_cost <- ggplot(cost_df_norm, aes(x = Ratio, y = pct, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "Cost-Sensitive Evaluation: ML wins more as missed-alert cost grows",
       y = "Total cost (% of Persistence baseline)",
       x = "Cost ratio (miss : false_alert)")

ggsave("plots/cost_sensitive.png", p_cost, width = 10, height = 5, dpi = 130)
