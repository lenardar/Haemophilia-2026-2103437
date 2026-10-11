# 成人血友病调查描述性分析 ----
# 从项目根目录运行：Rscript rscript/02_eda.R
# 可用第一个命令行参数指定项目根目录。

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)

PLOT_FORMAT <- "png" # 定稿时按期刊要求改为 "tiff" 或 "pdf"。
stopifnot(length(PLOT_FORMAT) == 1L, PLOT_FORMAT %in% c("png", "pdf", "tiff"))

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
project_dir <- if (length(args) == 1L) args[[1]] else "."
project_dir <- normalizePath(project_dir, mustWork = TRUE)
source(file.path(project_dir, "rscript", "tool_functions.R"))
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
table_dir <- file.path(project_dir, "output", "tables", "eda")
plot_dir <- file.path(project_dir, "output", "plots", "eda")
log_dir <- file.path(project_dir, "output", "logs", "eda")
for (path in c(table_dir, plot_dir, log_dir)) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  stopifnot(dir.exists(path))
  stopifnot(!dir.exists(file.path(path, "backup", run_stamp)))
  dir.create(file.path(path, "backup", run_stamp), recursive = TRUE)
}

save_eda_plot <- function(x, stem, width = 10, height = 7) {
  save_plot(
    filename = file.path(plot_dir, paste0(stem, ".", PLOT_FORMAT)),
    plot = x,
    width = width,
    height = height,
    run_stamp = run_stamp
  )
}

# 数据与编码检查 ----
data_env <- new.env()
load(
  file.path(project_dir, "data_interim", "recode_data.RData"),
  envir = data_env
)
stopifnot(exists("recode_data", envir = data_env, inherits = FALSE))
d <- data_env$recode_data
book <- read_csv(
  file.path(project_dir, "data_interim", "code_book.csv"),
  show_col_types = FALSE
)
raw <- read_excel(
  file.path(project_dir, "data_raw", "hemophilia_data.xlsx"),
  sheet = "成人"
)
stopifnot(
  identical(names(d), book$variable),
  !anyDuplicated(d$id),
  identical(d$id, raw[[1]])
)
binary_vars <- book$variable[book$shap_display == "binary_no_yes"]
stopifnot(all(vapply(d[binary_vars], function(x) {
  identical(levels(x), c("no", "yes"))
}, logical(1))))

# 原始空值、跳题和未知回答分别统计；不将跳过计入普通缺失。
raw_audit <- bind_rows(lapply(seq_along(raw), function(i) {
  x <- as.character(raw[[i]])
  tibble(
    variable = if (i == 1L) "id" else paste0("v", i - 1L),
    question = names(raw)[i],
    n = length(x),
    n_missing = sum(is.na(x)),
    n_blank = sum(x %in% c("", " ")),
    n_skip = sum(x %in% "(跳过)"),
    n_unknown = sum(x %in% c("不确定", "不清楚", "我不记得了"))
  )
}))
save_table(raw_audit, file.path(table_dir, "raw_response_audit.csv"), run_stamp)
data_audit <- book |>
  select(variable, source_question, factor_levels, reference_level, review_note)
data_audit$n_missing <- vapply(d, function(x) sum(is.na(x)), integer(1))
data_audit$n_distinct <- vapply(d, n_distinct, integer(1))
data_audit$is_constant <- data_audit$n_distinct == 1L
save_table(data_audit, file.path(table_dir, "encoded_data_audit.csv"), run_stamp)

# 当前编码数据应无 NA；若数据更新后出现缺失，先审查上面的审计表。
stopifnot(!anyNA(d), is.numeric(d$v50), all(d$v50 >= 0))
d$abr_group <- factor(
  ifelse(d$v50 <= 1, "Low", "High"),
  levels = c("Low", "High")
)
groups <- list(
  Overall = seq_len(nrow(d)),
  Low = which(d$abr_group == "Low"),
  High = which(d$abr_group == "High")
)
stopifnot(all(lengths(groups) > 0L))
group_summary <- d |>
  count(abr_group, name = "n", .drop = FALSE) |>
  mutate(percent = 100 * n / sum(n))
save_table(group_summary, file.path(table_dir, "abr_groups.csv"), run_stamp)

# 总体及 ABR 分组描述；不进行单变量显著性筛选 ----
numeric_vars <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], "id")
factor_vars <- names(d)[vapply(d, is.factor, logical(1))]
factor_vars <- setdiff(factor_vars, "abr_group")

numeric_summary <- bind_rows(lapply(names(groups), function(group_name) {
  rows <- groups[[group_name]]
  bind_rows(lapply(numeric_vars, function(v) {
    x <- d[[v]][rows]
    tibble(
      group = group_name,
      variable = v,
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      minimum = min(x),
      q1 = unname(quantile(x, 0.25)),
      median = median(x),
      q3 = unname(quantile(x, 0.75)),
      maximum = max(x),
      n_zero = sum(x == 0)
    )
  }))
})) |>
  left_join(
    book |> select(variable, source_question, review_note),
    by = "variable"
  )
save_table(numeric_summary, file.path(table_dir, "numeric_summary.csv"), run_stamp)

categorical_summary <- bind_rows(lapply(names(groups), function(group_name) {
  rows <- groups[[group_name]]
  bind_rows(lapply(factor_vars, function(v) {
    counts <- table(d[[v]][rows])
    tibble(
      group = group_name,
      variable = v,
      level = names(counts),
      n = as.integer(counts),
      denominator = length(rows),
      percent = 100 * as.integer(counts) / length(rows)
    )
  }))
})) |>
  left_join(
    book |> select(variable, source_question, reference_level, review_note),
    by = "variable"
  )
save_table(categorical_summary, file.path(table_dir, "categorical_summary.csv"), run_stamp)

# 字符型多选原文保留在数据中，其拆分列已进入分类统计。
baseline <- bind_rows(
  numeric_summary |>
    transmute(
      variable, source_question, level = "", group,
      summary = sprintf("%.2f [%.2f, %.2f]", median, q1, q3),
      statistic = "median [Q1, Q3]", review_note
    ),
  categorical_summary |>
    transmute(
      variable, source_question, level, group,
      summary = sprintf("%d (%.1f%%)", n, percent),
      statistic = "n (% within ABR group)", review_note
    )
) |>
  pivot_wider(names_from = group, values_from = summary)
save_table(baseline, file.path(table_dir, "baseline_by_abr.csv"), run_stamp)

# 正文研究对象特征候选表；完整变量仍保留在 baseline_by_abr.csv。 ----
table1_spec <- list(
  v7 = c(
    "婚姻状况",
    single = "未婚",
    married = "已婚",
    divorced = "离异",
    widowed = "丧偶"
  ),
  v8 = c(
    "教育程度",
    no = "未上学",
    primary = "小学",
    junior = "初中",
    senior = "高中/中专",
    college = "大专",
    undergraduate = "本科",
    graduate = "硕士及以上"
  ),
  v9 = c(
    "户籍",
    rural = "农村",
    urban = "城市"
  ),
  v17 = c(
    "基本医疗保险",
    employee = "职工医保",
    resident = "城乡居民医保",
    public = "公费医疗",
    no = "无"
  ),
  v32 = c(
    "血友病分型",
    A = "A型",
    B = "B型"
  ),
  v33 = c(
    "疾病严重程度",
    mild = "轻型",
    moderate = "中间型",
    severe = "重型"
  ),
  v34 = c("确诊年龄（岁）"),
  v37 = c(
    "关节相关手术史",
    no = "无",
    yes = "有"
  ),
  v38 = c(
    "血友病相关残疾",
    no = "无",
    physical = "身体残疾",
    mental = "智力或精神残疾"
  ),
  v39 = c(
    "血友病家族史",
    no = "无",
    yes = "有"
  ),
  v43 = c(
    "治疗方式",
    regular = "规律预防治疗",
    on_demand = "按需治疗"
  ),
  v47 = c(
    "凝血因子抑制物史（现在或既往）",
    no = "无",
    yes = "有",
    not_sure = "不确定"
  )
)
table1_groups <- groups[c(
  "Overall",
  "High",
  "Low"
)]
table1_rows <- list()
for (v in names(table1_spec)) {
  spec <- table1_spec[[v]]
  stopifnot(!anyNA(d[[v]]))
  if (is.numeric(d[[v]])) {
    values <- vapply(
      table1_groups,
      function(rows) {
        q <- quantile(
          d[[v]][rows],
          c(
            0.5,
            0.25,
            0.75
          )
        )
        sprintf(
          "%.1f [%.1f, %.1f]",
          q[1],
          q[2],
          q[3]
        )
      },
      character(1)
    )
    table1_rows[[length(table1_rows) + 1L]] <- data.frame(
      variable = v,
      characteristic = unname(spec[1]),
      level = "",
      Overall = values[1],
      High = values[2],
      Low = values[3]
    )
  } else {
    stopifnot(setequal(
      levels(d[[v]]),
      names(spec)[-1]
    ))
    for (lev in names(spec)[-1]) {
      values <- vapply(
        table1_groups,
        function(rows) {
          n <- sum(d[[v]][rows] == lev)
          sprintf(
            "%d (%.1f%%)",
            n,
            100 * n / length(rows)
          )
        },
        character(1)
      )
      table1_rows[[length(table1_rows) + 1L]] <- data.frame(
        variable = v,
        characteristic = unname(spec[1]),
        level = unname(spec[lev]),
        Overall = values[1],
        High = values[2],
        Low = values[3]
      )
    }
    stopifnot(sum(table(d[[v]])) == nrow(d))
  }
}
table1 <- do.call(
  rbind,
  table1_rows
)
rownames(table1) <- NULL
save_table(
  table1,
  file.path(
    table_dir,
    "table1_selected.csv"
  ),
  run_stamp
)
header <- sprintf(
  "<tr><th>特征</th><th>总体（N=%d）</th><th>High（N=%d）</th><th>Low（N=%d）</th></tr>",
  length(table1_groups$Overall),
  length(table1_groups$High),
  length(table1_groups$Low)
)
html_rows <- character()
for (i in seq_len(nrow(table1))) {
  x <- table1[i, ]
  if (x$level != "" && (i == 1L || x$variable != table1$variable[i - 1L])) {
    html_rows <- c(
      html_rows,
      sprintf(
        '<tr class="section"><td colspan="4">%s</td></tr>',
        x$characteristic
      )
    )
  }
  label <- if (x$level == "") {
    x$characteristic
  } else {
    paste0(
      "　",
      x$level
    )
  }
  html_rows <- c(
    html_rows,
    sprintf(
      "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      label,
      x$Overall,
      x$High,
      x$Low
    )
  )
}
html_file <- file.path(
  table_dir,
  "table1_selected.html"
)
writeLines(
  c(
    '<!DOCTYPE html><html lang="zh-CN"><meta charset="utf-8">',
    "<title>表1 研究对象特征（候选版）</title><style>",
    'body{font-family:Arial,"PingFang SC",sans-serif;color:#243344;max-width:980px;margin:40px auto;padding:0 24px}',
    "table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:8px 12px;text-align:right;border-bottom:1px solid #e6ebef}",
    "th:first-child,td:first-child{text-align:left}th{background:#183c52;color:white}.section td{background:#edf3f6;font-weight:bold}",
    "p{line-height:1.7;color:#52616b}h1{font-size:24px}</style>",
    "<h1>表1　研究对象特征（候选版）</h1>",
    "<p>成人男性血友病患者；按观测到的 ABR 分组。</p><table>",
    header,
    html_rows,
    "</table>",
    "<p>注：High：ABR &gt; 1；Low：ABR ≤ 1。分类变量为人数（列内百分比）；确诊年龄为中位数［第25、第75百分位数］。各项分母为对应列的全部患者，四舍五入后百分比之和可能不等于100%。不确定回答单列，未并入“无”。所选字段无编码后缺失；这不代表原问卷不存在跳题或未知回答。未进行组间显著性检验。</p>",
    "<p>数据来源：data_interim/recode_data.RData；字段含义依据 code_book.csv。当前年龄未展示：原字段为出生年份，待核对调查时间后换算。候选特征按人群描述用途选择，未依据P值或模型重要性选择。</p></html>"
  ),
  html_file,
  useBytes = TRUE
)
backup_file(
  html_file,
  run_stamp
)

# 图表 ----
theme_set(theme_minimal(base_size = 11))
palette <- c(Low = "#3178A5", High = "#CC704B")
p <- ggplot(group_summary, aes(abr_group, n, fill = abr_group)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%d (%.1f%%)", n, percent)),
    vjust = -0.5
  ) +
  scale_fill_manual(values = palette, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Participants", title = "ABR groups",
       subtitle = "Low: ABR <= 1; High: ABR > 1")
save_eda_plot(p, "abr_groups", width = 7, height = 5)

p <- ggplot(d, aes(v50)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#3178A5") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#CC704B") +
  labs(x = "ABR", y = "Participants", title = "ABR distribution",
       subtitle = "All observations retained; dashed line: ABR = 1")
save_eda_plot(p, "abr_distribution")

numeric_labels <- c(
  v4 = "Birth year", v5 = "Height", v6 = "Weight",
  v12 = "Household size", v22 = "Hospitalizations needed",
  v23 = "Actual hospitalizations", v24 = "Hospital days (2022)",
  v29 = "Regular MDT years (skip coded 0)", v34 = "Age at diagnosis",
  v41 = "Injection dose", v50 = "ABR", v51 = "AJBR"
)
stopifnot(setequal(numeric_vars, names(numeric_labels)))
continuous_long <- d |>
  select(all_of(numeric_vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = names(numeric_labels)))
p <- ggplot(continuous_long, aes(value)) +
  geom_histogram(bins = 30, fill = "#3178A5") +
  facet_wrap(
    vars(variable),
    scales = "free",
    ncol = 3,
    labeller = as_labeller(numeric_labels)
  ) +
  labs(x = "Recorded value", y = "Participants",
       title = "Numeric variables (no records trimmed)")
save_eda_plot(p, "numeric_distributions", width = 12, height = 12)

clinical_labels <- c(
  v32 = "Hemophilia type", v33 = "Severity", v38 = "Disability",
  v43 = "Treatment regimen (source classification)",
  v47 = "Inhibitor history", v35_hiv = "HIV"
)
clinical_plots <- categorical_summary |>
  filter(group == "Overall", variable %in% names(clinical_labels)) |>
  mutate(category = paste(variable, level, sep = ": "))
clinical_plots$category <- factor(
  clinical_plots$category,
  levels = rev(clinical_plots$category)
)
p <- ggplot(clinical_plots, aes(n, category)) +
  geom_col(fill = "#3178A5") +
  geom_text(aes(label = sprintf("%d (%.1f%%)", n, percent)), hjust = -0.1) +
  facet_wrap(
    vars(variable),
    scales = "free",
    ncol = 2,
    labeller = as_labeller(clinical_labels)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_y_discrete(labels = function(x) sub("^[^:]+: ", "", x)) +
  labs(x = "Participants", y = NULL, title = "Clinical characteristics")
save_eda_plot(p, "clinical_characteristics", width = 13, height = 10)

# 仅真实数值字段计算 Spearman 相关；类别编号不参与。
correlation <- cor(d[numeric_vars], method = "spearman")
stopifnot(!anyNA(correlation))
correlation_long <- as.data.frame(as.table(correlation)) |>
  transmute(variable_x = Var1, variable_y = Var2, spearman_rho = Freq)
save_table(correlation_long, file.path(table_dir, "numeric_spearman.csv"), run_stamp)
p <- ggplot(correlation_long, aes(variable_x, variable_y, fill = spearman_rho)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#3178A5", mid = "white", high = "#CC704B",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(labels = numeric_labels) +
  scale_y_discrete(labels = numeric_labels) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Spearman rho",
       title = "Numeric-variable associations",
       subtitle = "Descriptive only; skip-coded zeros remain in MDT years")
save_eda_plot(p, "numeric_spearman", width = 12, height = 11)

# 可复现性记录，不输出个体记录 ----
log_file <- file.path(log_dir, "session_info.txt")
writeLines(
  c(
    paste("Run:", run_stamp),
    paste("Participants:", nrow(d)),
    "ABR groups: Low <= 1; High > 1.",
    "Descriptive summaries only; no imputation, record deletion or feature selection.",
    "Original skip recoding retained; see raw_response_audit and review_note.",
    capture.output(sessionInfo())
  ),
  log_file
)
backup_file(log_file, run_stamp)
message("EDA完成：", nrow(d), " 人；表格：", table_dir, "；图形：", plot_dir)
