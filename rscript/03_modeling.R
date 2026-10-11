# 模型训练：在 IDE 中打开项目后，直接运行整个文件。 ----

# 运行设置 ----
profile <- "full" # 正式训练；快速检查时改为 "smoke"。
workers <- 4L # 正式运行使用四个并行进程。
# 取消注释即可加入本次运行；只运行下面选中的实验。
# First run: original_full and smotenc_full.
# Second run: set selected_scenarios to c("original_restricted").
# Enter the two completed run IDs in report_config.R before running 04-06.
selected_scenarios <- c(
  "original_full",
  "smotenc_full"
)
project_dir <- getwd()

# 支持工作目录位于项目根目录或其子目录，不改变 IDE 的工作目录。
repeat {
  if (file.exists(file.path(
    project_dir,
    "rscript",
    "analysis_config.R"
  )) && file.exists(file.path(
    project_dir,
    "data_interim",
    "recode_data.RData"
  ))) {
    break
  }
  parent_dir <- dirname(project_dir)
  if (identical(
    parent_dir,
    project_dir
  )) {
    stop("无法定位项目：请在 IDE 中打开 hemophilia 项目文件夹后运行。")
  }
  project_dir <- parent_dir
}
project_dir <- normalizePath(
  project_dir,
  mustWork = TRUE
)
run_id <- paste0(
  profile,
  "_",
  format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )
)

# 启动会先检查全部模型；同一代码、数据和环境下重跑会自动读取训练缓存。
# 进度和状态保存在 output/logs/training/。
# 加载函数并训练 ----
if (!requireNamespace(
  "renv",
  quietly = TRUE
)) {
  stop("请先在项目根目录恢复 renv 环境，再运行训练。")
}
renv::load(project_dir)
source(file.path(
  project_dir,
  "rscript",
  "tool_functions.R"
))
source(file.path(
  project_dir,
  "rscript",
  "analysis_config.R"
))
source(file.path(
  project_dir,
  "rscript",
  "model_functions.R"
))
config <- analysis_config(
  profile,
  selected_scenarios
)
if (profile == "full") config$workers <- workers
run_training(
  project_dir,
  config,
  run_id
)
