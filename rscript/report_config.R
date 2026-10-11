# Replace these historical IDs with your completed training run IDs.
# 04/05/06共用展示配置；训练配置仍在analysis_config.R。 ----
report_config <- function() {
  list(
    run_id = "full_20261005_101934",
    restricted_run_id = "full_20261006_095146",
    plot_format = "png", # 每次只导出一种格式：png、pdf或tiff。
    model_labels = c(
      logistic = "Logistic regression",
      logistic_spline = "Logistic regression with splines",
      tree = "Decision tree", rf = "Random forest", svm = "SVM",
      xgboost = "XGBoost", knn = "KNN", naive_bayes = "Naive Bayes"
    ),
    model_colors = c(
      logistic = "#E64B35", logistic_spline = "#4DBBD5", tree = "#00A087",
      rf = "#3C5488", svm = "#F39B7F", xgboost = "#8491B4",
      knn = "#91D1C2", naive_bayes = "#DC0000"
    )
  )
}
