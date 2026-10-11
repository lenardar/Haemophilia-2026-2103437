# 血友病成人调查数据编码 ----
# 在项目根目录运行：Rscript rscript/01_recode.R
# 或指定项目目录：Rscript /path/to/01_recode.R /path/to/hemophilia
# 跳题等尚待核对的旧规则保留在 code_book$review_note 中。

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(tibble)

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
project_dir <- if (length(args) == 1L) args[[1]] else "."
project_dir <- normalizePath(project_dir, mustWork = TRUE)
source(file.path(project_dir, "rscript", "tool_functions.R"))
input_file <- file.path(project_dir, "data_raw", "hemophilia_data.xlsx")
interim_dir <- file.path(project_dir, "data_interim")
stopifnot(file.exists(input_file), dir.exists(interim_dir))

# 数据导入与字段映射 ----

source_data <- read_excel(input_file, sheet = "成人")
stopifnot(
  ncol(source_data) == 55L,
  names(source_data)[1] == "序号",
  names(source_data)[51] == "ABR",
  names(source_data)[52] == "AJBR"
)
raw_data <- source_data

colnames_data <- data.frame(column_names = colnames(raw_data))

colnames(raw_data)[1] <- "id"
colnames(raw_data)[2:ncol(raw_data)] <- paste0("v", 1:(ncol(raw_data) - 1))

variable_names <- data.frame(variable = colnames(raw_data))

mapping_table <- variable_names |>
  mutate(original_name = colnames_data$column_names)

code_book <- mapping_table |>
  mutate(
    description = NA,
    type = NA,
    values = NA
  )

# v1:类型 ----

raw_data <- raw_data |>
  mutate(v1 = case_when(
    v1 == "成人" ~ "adult",
    TRUE ~ as.character(v1)
  )) |>
  mutate(v1 = factor(v1, levels = c("adult")))

code_book <- code_book |>
  filter(variable == "v1") |>
  mutate(
    description = "",
    type = "categorical",
    values = "adult: 成人"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v1")
  )

# v2:是否为患者 ----

raw_data <- raw_data |>
  mutate(v2 = case_when(
    v2 == "血友病患者本人" ~ "patient",
    TRUE ~ as.character(v2)
  )) |>
  mutate(v2 = factor(v2, levels = c("patient")))

code_book <- code_book |>
  filter(variable == "v2") |>
  mutate(
    description = "",
    type = "categorical",
    values = "patient: 血友病患者本人"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v2")
  )

# v3:性别 ----

raw_data <- raw_data |>
  mutate(v3 = case_when(
    v3 == "男" ~ "male",
    TRUE ~ as.character(v3)
  )) |>
  mutate(v3 = factor(v3, levels = c("male")))

code_book <- code_book |>
  filter(variable == "v3") |>
  mutate(
    description = "",
    type = "categorical",
    values = "male: 男"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v3")
  )

# v4:出生年份 ----

raw_data <- raw_data |>
  mutate(v4 = as.numeric(v4))

code_book <- code_book |>
  filter(variable == "v4") |>
  mutate(
    description = "出生年份",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v4")
  )

# v5:身高 ----

raw_data <- raw_data |>
  mutate(v5 = as.numeric(v5))

code_book <- code_book |>
  filter(variable == "v5") |>
  mutate(
    description = "身高",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v5")
  )

# v6:体重 ----

raw_data <- raw_data |>
  mutate(v6 = as.numeric(v6))

code_book <- code_book |>
  filter(variable == "v6") |>
  mutate(
    description = "体重",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v6")
  )

# v7:婚姻类型 ----

raw_data <- raw_data |>
  mutate(v7 = case_when(
    v7 == "1未婚" ~ "single",
    v7 == "2已婚" ~ "married",
    v7 == "离异" ~ "divorced",
    v7 == "丧偶" ~ "widowed",
    TRUE ~ as.character(v7)
  )) |>
  mutate(
    v7 = factor(v7, levels = c("single", "married", "divorced", "widowed"))
  )

code_book <- code_book |>
  filter(variable == "v7") |>
  mutate(
    description = "婚姻类型",
    type = "categorical",
    values = "single: 1未婚; married: 2已婚; divorced: 离异; widowed: 丧偶"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v7")
  )

# v8:教育水平 ----

raw_data <- raw_data |>
  mutate(v8 = case_when(
    v8 == "没上过学" ~ "no",
    v8 == "小学" ~ "primary",
    v8 == "初中" ~ "junior",
    v8 == "高中/中专" ~ "senior",
    v8 == "大专" ~ "college",
    v8 == "本科" ~ "undergraduate",
    v8 == "硕士及以上" ~ "graduate",
    TRUE ~ as.character(v8)
  )) |>
  mutate(
    v8 = factor(
      v8,
      levels = c(
        "no", "primary", "junior", "senior",
        "college", "undergraduate", "graduate"
      )
    )
  )

code_book <- code_book |>
  filter(variable == "v8") |>
  mutate(
    description = "教育水平",
    type = "categorical",
    values = "
      no: 没上过学; primary: 小学;
      junior: 初中; senior: 高中/中专;
      college: 大专; undergraduate: 本科;
      graduate: 硕士及以上
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v8")
  )

# v9:户籍 ----

raw_data <- raw_data |>
  mutate(v9 = case_when(
    v9 == "农村户籍" ~ "rural",
    v9 == "城市户籍" ~ "urban",
    TRUE ~ as.character(v9)
  )) |>
  mutate(v9 = factor(v9, levels = c("rural", "urban")))

code_book <- code_book |>
  filter(variable == "v9") |>
  mutate(
    description = "户籍",
    type = "categorical",
    values = "rural: 农村户籍; urban: 城市户籍"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v9")
  )

# v10:就业状况 ----

raw_data <- raw_data |>
  mutate(v10 = case_when(
    v10 == "全职工作" ~ "employed",
    v10 == "兼职工作" ~ "parttime",
    v10 == "无业" ~ "unemployed",
    v10 == "退休" ~ "retired",
    v10 == "在读学生" ~ "student",
    TRUE ~ as.character(v10)
  )) |>
  mutate(v10 = factor(
    v10,
    levels = c("employed", "parttime", "unemployed", "retired", "student")
  ))

code_book <- code_book |>
  filter(variable == "v10") |>
  mutate(
    description = "就业状况",
    type = "categorical",
    values = "
      employed: 全职工作; parttime: 兼职工作;
      unemployed: 无业; retired: 退休;
      student: 在读学生
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v10")
  )

# v11:职业 ----

raw_data <- raw_data |>
  mutate(v11 = case_when(
    v11 == "自由职业者" ~ "freelancer",
    v11 == "工人" ~ "worker",
    v11 == "职员" ~ "staff",
    v11 == "个体经营者" ~ "self_employed",
    v11 == "农民" ~ "farmer",
    v11 == "专业技术人员" ~ "technician",
    v11 == "企业管理人员" ~ "manager",
    v11 == "国家公务员" ~ "civil_servant",
    v11 == "(跳过)" ~ "no",
    TRUE ~ as.character(v11)
  )) |>
  mutate(v11 = factor(v11, levels = c(
    "freelancer", "worker", "staff", "self_employed",
    "farmer", "technician", "manager", "civil_servant", "no"
  )))

code_book <- code_book |>
  filter(variable == "v11") |>
  mutate(
    description = "职业",
    type = "categorical",
    values = "
      freelancer: 自由职业者; worker: 工人;
      staff: 职员; self_employed: 个体经营者;
      farmer: 农民; technician: 专业技术人员;
      manager: 企业管理人员; civil_servant: 国家公务员;
      no: (跳过)
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v11")
  )

# v12:家庭成员数 ----

raw_data <- raw_data |>
  mutate(v12 = str_extract(v12, "\\d+")) |>
  mutate(v12 = as.numeric(v12))

code_book <- code_book |>
  filter(variable == "v12") |>
  mutate(
    description = "家庭成员数",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v12")
  )

# v13:家庭年收入 ----

raw_data <- raw_data |>
  mutate(v13 = case_when(
    v13 == "10万元及以下" ~ "below_100k",
    v13 == "10-20万元（含20万）" ~ "100k_200k",
    v13 == "20-30万元（含30万）" ~ "200k_300k",
    v13 == "30-40万元（含40万）" ~ "300k_400k",
    v13 == "40-50万元（含50万）" ~ "400k_500k",
    v13 == "50万元以上" ~ "above_500k",
    TRUE ~ as.character(v13)
  )) |>
  mutate(v13 = factor(v13, levels = c(
    "below_100k", "100k_200k", "200k_300k",
    "300k_400k", "400k_500k", "above_500k"
  )))

code_book <- code_book |>
  filter(variable == "v13") |>
  mutate(
    description = "家庭年收入",
    type = "categorical",
    values = "
      below_100k: 10万元及以下; 100k_200k: 10-20万元（含20万）;
      200k_300k: 20-30万元（含30万）; 300k_400k: 30-40万元（含40万）;
      400k_500k: 40-50万元（含50万）; above_500k: 50万元以上
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v13")
  )

# v14:居住地 ----

raw_data <- raw_data |>
  separate(v14, into = c("v14_province", "v14_city"), sep = "-")

east_provinces <- c(
  "北京", "天津", "河北", "上海", "江苏", "浙江", "福建",
  "山东", "广东", "海南", "辽宁", "吉林", "黑龙江"
)
central_provinces <- c(
  "山西", "安徽", "江西", "河南", "湖北", "湖南"
)
west_provinces <- c(
  "内蒙古", "广西", "重庆", "四川", "贵州", "云南", "西藏",
  "陕西", "甘肃", "青海", "宁夏", "新疆"
)
raw_data <- raw_data |>
  mutate(
    v14_region = case_when(
      v14_province %in% east_provinces ~ "east",
      v14_province %in% central_provinces ~ "central",
      v14_province %in% west_provinces ~ "west",
      TRUE ~ "other"
    )
  ) |>
  mutate(v14_region = factor(v14_region, levels = c("east", "central", "west", "other")))

code_book <- code_book |>
  bind_rows(
    data.frame(
      variable = "v14_province",
      original_name = "居住地（省份-城市）",
      description = "居住地省份",
      type = "categorical",
      values = "省份名称"
    ),
    data.frame(
      variable = "v14_city",
      original_name = "居住地（省份-城市）",
      description = "居住地城市",
      type = "categorical",
      values = "城市名称"
    ),
    data.frame(
      variable = "v14_region",
      original_name = "居住地地区",
      description = "居住地地区（东中西部）",
      type = "categorical",
      values = "east: 东部; central: 中部; west: 西部; other: 其他"
    )
  )

# v15:户籍 ----

raw_data <- raw_data |>
  mutate(v15 = case_when(
    v15 == "农村户籍" ~ "rural",
    v15 == "城市户籍" ~ "urban",
    TRUE ~ as.character(v15)
  )) |>
  mutate(v15 = factor(v15, levels = c("rural", "urban")))

code_book <- code_book |>
  filter(variable == "v15") |>
  mutate(
    description = "户籍",
    type = "categorical",
    values = "rural: 农村户籍; urban: 城市户籍"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v15")
  )

# v16:慢性病类型 ----

raw_data <- raw_data |>
  mutate(
    v16_hypertension = if_else(str_detect(v16, "高血压"), "yes", "no"),
    v16_diabetes = if_else(str_detect(v16, "糖尿病"), "yes", "no"),
    v16_hyperlipidemia = if_else(str_detect(v16, "高血脂"), "yes", "no"),
    v16_coronary_heart_disease = if_else(str_detect(v16, "冠心病"), "yes", "no"),
    v16_osteoarthritis = if_else(str_detect(v16, "骨关节病"), "yes", "no"),
    v16_cerebrovascular_disease = if_else(str_detect(v16, "脑血管病"), "yes", "no"),
    v16_other = if_else(str_detect(v16, "其他"), "yes", "no")
  ) |>
  mutate(across(starts_with("v16_"), ~ factor(.x, levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v16") |>
  mutate(
    description = "是否有慢性病，未映射，创建了多个二分类变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v16")
  )

code_book <- code_book |>
  add_row(
    variable = c(
      "v16_hypertension",
      "v16_diabetes",
      "v16_hyperlipidemia",
      "v16_coronary_heart_disease",
      "v16_osteoarthritis",
      "v16_cerebrovascular_disease",
      "v16_other"
    ),
    description = c(
      "是否患有高血压",
      "是否患有糖尿病",
      "是否患有高血脂",
      "是否患有冠心病",
      "是否患有骨关节病",
      "是否患有脑血管病",
      "是否患有其他慢性病"
    ),
    type = rep("categorical", 7),
    values = rep("yes, no", 7)
  )

# v17:基本医疗保险 ----

raw_data <- raw_data |>
  mutate(v17 = case_when(
    v17 == "职工基本医疗保险" ~ "employee",
    v17 == "城乡居民基本医疗保险（含“新农合”）" ~ "resident",
    v17 == "公费医疗" ~ "public",
    v17 == "无" ~ "no",
    TRUE ~ as.character(v17)
  )) |>
  mutate(v17 = factor(v17, levels = c("employee", "resident", "public", "no")))

code_book <- code_book |>
  filter(variable == "v17") |>
  mutate(
    description = "基本医疗保险",
    type = "categorical",
    values = "
      employee: 职工基本医疗保险;
      resident: 城乡居民基本医疗保险（含“新农合”）;
      public: 公费医疗; no: 无
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v17")
  )

# v18:是否有商业保险 ----

raw_data <- raw_data |>
  mutate(v18 = case_when(
    v18 == "是" ~ "yes",
    v18 == "否" ~ "no",
    TRUE ~ as.character(v18)
  )) |>
  mutate(v18 = factor(v18, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v18") |>
  mutate(
    description = "是否有商业保险",
    type = "categorical",
    values = "yes: 是; no: 否"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v18")
  )

# v19:是否接受慈善捐助 ----

raw_data <- raw_data |>
  mutate(v19 = case_when(
    v19 == "是" ~ "yes",
    v19 == "否" ~ "no",
    TRUE ~ as.character(v19)
  )) |>
  mutate(v19 = factor(v19, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v19") |>
  mutate(
    description = "是否接受慈善捐助",
    type = "categorical",
    values = "yes: 是; no: 否"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v19")
  )

# v20:自负金额 ----

raw_data <- raw_data |>
  mutate(v20 = case_when(
    v20 == "1万及以下" ~ "below_10k",
    v20 == "1至5万（含5万）" ~ "10k_50k",
    v20 == "5至10万（含10万）" ~ "50k_100k",
    v20 == "10至15万（含15万）" ~ "100k_150k",
    v20 == "15至20万（含20万）" ~ "150k_200k",
    v20 == "20万以上" ~ "above_200k",
    TRUE ~ as.character(v20)
  )) |>
  mutate(v20 = factor(v20, levels = c(
    "below_10k", "10k_50k", "50k_100k",
    "100k_150k", "150k_200k", "above_200k"
  )))

code_book <- code_book |>
  filter(variable == "v20") |>
  mutate(
    description = "自负金额",
    type = "categorical",
    values = "
      below_10k: 1万及以下; 10k_50k: 1至5万（含5万）;
      50k_100k: 5至10万（含10万）; 100k_150k: 10至15万（含15万）;
      150k_200k: 15至20万（含20万）; above_200k: 20万以上
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v20")
  )

# v21:是否因血友病就诊 ----

raw_data <- raw_data |>
  mutate(v21 = case_when(
    v21 == "是" ~ "yes",
    v21 == "否" ~ "no",
    TRUE ~ as.character(v21)
  )) |>
  mutate(v21 = factor(v21, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v21") |>
  mutate(
    description = "是否因血友病就诊",
    type = "categorical",
    values = "yes: 是; no: 否"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v21")
  )

# v22:需要住院几次 ----

raw_data <- raw_data |>
  mutate(v22 = case_when(
    v22 == "(跳过)" ~ "0",
    TRUE ~ as.character(v22)
  )) |>
  mutate(v22 = as.numeric(v22))

code_book <- code_book |>
  filter(variable == "v22") |>
  mutate(
    description = "需要住院几次，(跳过)记为0",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v22")
  )

# v23:实际住院几次 ----

raw_data <- raw_data |>
  mutate(v23 = case_when(
    v23 == "(跳过)" ~ "0",
    TRUE ~ as.character(v23)
  )) |>
  mutate(v23 = as.numeric(v23))

# v24:2022年总住院天数 ----

raw_data <- raw_data |>
  mutate(v24 = as.numeric(v24))

code_book <- code_book |>
  filter(variable == "v24") |>
  mutate(
    description = "2022年总住院天数",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v24")
  )

# v25:多久参与血友病评估 ----

raw_data <- raw_data |>
  mutate(v25 = case_when(
    v25 == "小于1个月（含1个月）" ~ "within_1_month",
    v25 == "1至3个月（含3个月）" ~ "1_3_months",
    v25 == "3至6个月（含6个月）" ~ "3_6_months",
    v25 == "6至12个月（含12个月）" ~ "6_12_months",
    v25 == "大于每年一次" ~ "above_12_months",
    TRUE ~ as.character(v25)
  )) |>
  mutate(v25 = factor(
    v25,
    levels = c(
      "within_1_month", "1_3_months", "3_6_months",
      "6_12_months", "above_12_months"
    )
  ))

code_book <- code_book |>
  filter(variable == "v25") |>
  mutate(
    description = "多久参与血友病评估",
    type = "categorical",
    values = "
      within_1_month: 小于1个月（含1个月）;
      1_3_months: 1至3个月（含3个月）;
      3_6_months: 3至6个月（含6个月）;
      6_12_months: 6至12个月（含12个月）;
      above_12_months: 大于每年一次
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v25")
  )

# v26:参与评估单程路程时间 ----

raw_data <- raw_data |>
  mutate(v26 = case_when(
    v26 == "1小时以内（含1小时）" ~ "within_1_hour",
    v26 == "1至2小时（含2小时）" ~ "1_2_hours",
    v26 == "2小时以上" ~ "above_2_hours",
    TRUE ~ as.character(v26)
  )) |>
  mutate(v26 = factor(
    v26,
    levels = c("within_1_hour", "1_2_hours", "above_2_hours")
  ))

code_book <- code_book |>
  filter(variable == "v26") |>
  mutate(
    description = "参与评估单程路程时间",
    type = "categorical",
    values = "
      within_1_hour: 1小时以内（含1小时）;
      1_2_hours: 1至2小时（含2小时）;
      above_2_hours: 2小时以上
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v26")
  )

# v27:是否参与血友病多学科综合治疗 ----

raw_data <- raw_data |>
  mutate(v27 = case_when(
    v27 == "做过" ~ "yes",
    v27 == "从来没做过" ~ "no",
    TRUE ~ as.character(v27)
  )) |>
  mutate(v27 = factor(v27, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v27") |>
  mutate(
    description = "是否做过血友病相关评估",
    type = "categorical",
    values = "yes: 做过; no: 从来没做过"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v27")
  )

# v28:定期接受血友病多学科综合治疗时间 ----

raw_data <- raw_data |>
  mutate(v28 = case_when(
    v28 == "每6个月以内（含6个月）" ~ "within_6_months",
    v28 == "每隔6~12个月（含12个月）" ~ "6_12_months",
    v28 == "每隔1年~2年（含2年）" ~ "1_2_years",
    v28 == "每隔2年以上" ~ "above_2_years",
    v28 == "没有时间规律，不定期" ~ "no_regular",
    v28 == "我不记得了" ~ "not_remember",
    v28 == "(跳过)" ~ "no",
    TRUE ~ as.character(v28)
  )) |>
  mutate(v28 = factor(
    v28,
    levels = c(
      "within_6_months", "6_12_months", "1_2_years",
      "above_2_years", "no_regular", "not_remember", "no"
    )
  ))

code_book <- code_book |>
  filter(variable == "v28") |>
  mutate(
    description = "多久做一次血友病相关评估",
    type = "categorical",
    values = "
      within_6_months: 每6个月以内（含6个月）;
      6_12_months: 每隔6~12个月（含12个月）;
      1_2_years: 每隔1年~2年（含2年）;
      above_2_years: 每隔2年以上;
      no_regular: 没有时间规律，不定期;
      not_remember: 我不记得了; no: (跳过)
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v28")
  )

# v29:接受血友病多学科综合治疗多少年？ ----

raw_data <- raw_data |>
  mutate(v29 = case_when(
    v29 == "(跳过)" ~ "0",
    TRUE ~ as.character(v29)
  )) |>
  mutate(v29 = as.numeric(v29))

code_book <- code_book |>
  filter(variable == "v29") |>
  mutate(
    description = "定期接受血友病多学科综合治疗的年限；旧规则将跳过记为0，待核对跳题逻辑",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v29")
  )

# v30:与哪些患者支持组织联系 ----

raw_data <- raw_data |>
  mutate(
    v30_patient_organization = if_else(
      str_detect(v30, "当地血友病患者组织"), "yes", "no"
    ),
    v30_rare_disease_alliance = if_else(
      str_detect(v30, "罕见病联盟"), "yes", "no"
    ),
    v30_pain_challenge_foundation = if_else(
      str_detect(v30, "病痛挑战公益基金会"), "yes", "no"
    ),
    v30_code_rare_disease_center = if_else(
      str_detect(v30, "蔻德罕见病中心"), "yes", "no"
    ),
    v30_other = if_else(
      str_detect(v30, "其他"), "yes", "no"
    ),
    v30_no_contact = if_else(
      !str_detect(v30, "当地血友病患者组织|罕见病联盟|病痛挑战公益基金会|蔻德罕见病中心|其他"), "yes", "no"
    )
  ) |>
  mutate(across(starts_with("v30_"), ~ factor(.x, levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v30") |>
  mutate(
    description = "参与那些组织，未映射，创建了多个二分类变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v30")
  )

code_book <- code_book |>
  add_row(
    variable = c(
      "v30_patient_organization", "v30_rare_disease_alliance",
      "v30_pain_challenge_foundation", "v30_code_rare_disease_center",
      "v30_other", "v30_no_contact"
    ),
    description = c(
      "联系当地血友病患者组织", "联系罕见病联盟",
      "联系病痛挑战公益基金会", "联系蔻德罕见病中心",
      "联系其他组织", "没有联系患者支持组织"
    ),
    type = rep("categorical", 6),
    values = rep("no: 无; yes: 有", 6)
  )

# v31:寻求哪些帮助 ----

raw_data <- raw_data |>
  mutate(
    v31_disease_knowledge = if_else(
      str_detect(v31, "疾病知识"), "yes", "no"
    ),
    v31_treatment_information = if_else(
      str_detect(v31, "治疗信息"), "yes", "no"
    ),
    v31_patient_social = if_else(
      str_detect(v31, "病友社交"), "yes", "no"
    ),
    v31_policy_advice = if_else(
      str_detect(v31, "政策建议"), "yes", "no"
    ),
    v31_medical_choice = if_else(
      str_detect(v31, "就医选择"), "yes", "no"
    ),
    v31_charity_donation = if_else(
      str_detect(v31, "慈善捐助"), "yes", "no"
    ),
    v31_resource_contact = if_else(
      str_detect(v31, "资源联系"), "yes", "no"
    ),
    v31_rights_consultation = if_else(
      str_detect(v31, "权益咨询"), "yes", "no"
    ),
    v31_other = if_else(
      str_detect(v31, "其他"), "yes", "no"
    ),
    v31_no_option = if_else(
      !str_detect(
        v31,
        "疾病知识|治疗信息|病友社交|政策建议|就医选择|慈善捐助|资源联系|权益咨询|其他"
      ), "yes", "no"
    )
  ) |>
  mutate(across(starts_with("v31_"), ~ factor(.x, levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v30") |>
  mutate(
    description = "从患者组织获得哪些帮助，未映射，创建了多个二分类变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v30")
  )

code_book <- code_book |>
  add_row(
    variable = c(
      "v31_disease_knowledge",
      "v31_treatment_information",
      "v31_patient_social",
      "v31_policy_advice",
      "v31_medical_choice",
      "v31_charity_donation",
      "v31_resource_contact",
      "v31_rights_consultation",
      "v31_other"
    ),
    description = c(
      "是否从患者组织获得疾病知识",
      "是否从患者组织获得治疗信息",
      "是否从患者组织获得病友社交",
      "是否从患者组织获得政策建议",
      "是否从患者组织获得就医选择",
      "是否从患者组织获得慈善捐助",
      "是否从患者组织获得资源联系",
      "是否从患者组织获得权益咨询",
      "是否从患者组织获得其他帮助"
    ),
    type = rep("categorical", 9),
    values = rep("yes, no", 9)
  )

# v32:血友病分型 ----

raw_data <- raw_data |>
  mutate(v32 = case_when(
    v32 == "A (甲）" ~ "A",
    v32 == "B (乙）" ~ "B",
    TRUE ~ as.character(v32)
  )) |>
  mutate(v32 = factor(v32, levels = c("A", "B")))

code_book <- code_book |>
  filter(variable == "v32") |>
  mutate(
    description = "血友病分型",
    type = "categorical",
    value = "A: A (甲）; B: B (乙）"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v32")
  )

# v33:严重程度 ----

raw_data <- raw_data |>
  mutate(v33 = case_when(
    v33 == "轻型 (>5-40 IU/dl）" ~ "mild",
    v33 == "中间型 (1-5 IU/dl）" ~ "moderate",
    v33 == "重型 (＜1 IU/dl）" ~ "severe",
    TRUE ~ as.character(v33)
  )) |>
  mutate(v33 = factor(v33, levels = c("mild", "moderate", "severe")))

code_book <- code_book |>
  filter(variable == "v33") |>
  mutate(
    description = "血友病严重程度",
    type = "categorical",
    values = "
      mild: 轻型 (>5-40 IU/dl）;
      moderate: 中间型 (1-5 IU/dl）;
      severe: 重型 (＜1 IU/dl）
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v33")
  )

# v34:确诊年龄 ----

raw_data <- raw_data |>
  mutate(v34 = as.numeric(v34))

code_book <- code_book |>
  filter(variable == "v34") |>
  mutate(
    description = "确诊年龄",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v34")
  )

# v35:是否患以下疾病 ----

raw_data <- raw_data |>
  mutate(v35_arthritis = if_else(str_detect(v35, "关节炎"), "yes", "no")) |>
  mutate(v35_arthritis = factor(v35_arthritis, levels = c("no", "yes"))) |>
  mutate(
    v35_overweight_obesity = if_else(str_detect(v35, "超重/ 肥胖"), "yes", "no")
  ) |>
  mutate(v35_hepatitis_b = if_else(str_detect(v35, "乙型肝炎"), "yes", "no")) |>
  mutate(v35_hepatitis_c = if_else(str_detect(v35, "丙型肝炎"), "yes", "no")) |>
  mutate(v35_pseudotumor = if_else(str_detect(v35, "假肿瘤"), "yes", "no")) |>
  mutate(
    v35_hiv = if_else(str_detect(v35, fixed("人类免疫缺陷病毒感染 (艾滋病/HIV)")), "yes", "no")
  ) |>
  mutate(across(starts_with("v35_"), ~ factor(., levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v35") |>
  mutate(
    description = "并发症类型，未映射，但是创建了多个二值变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v35")
  )

# v36:合并症/并发症 ----

raw_data <- raw_data |>
  mutate(v36 = case_when(
    v36 == "有" ~ "yes",
    v36 == "无" ~ "no",
    TRUE ~ as.character(v36)
  )) |>
  mutate(v36 = factor(v36, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v36") |>
  mutate(
    description = "合并症/并发症",
    type = "categorical",
    values = "yes: 有; no: 无"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v36")
  )

# v37:是否接受过关节相关手术治疗 ----

raw_data <- raw_data |>
  mutate(v37 = case_when(
    v37 == "接受过" ~ "yes",
    v37 == "没接受过" ~ "no",
    TRUE ~ as.character(v37)
  )) |>
  mutate(v37 = factor(v37, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v37") |>
  mutate(
    description = "是否接受过关节相关手术治疗",
    type = "categorical",
    values = "yes: 接受过; no: 没接受过"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v37")
  )

# v38:是否有血友病导致的残疾 ----

raw_data <- raw_data |>
  mutate(v38 = case_when(
    v38 == "没有残疾" ~ "no",
    v38 == "身体残疾" ~ "physical",
    v38 == "智力或精神残疾" ~ "mental",
    TRUE ~ as.character(v38)
  )) |>
  mutate(v38 = factor(v38, levels = c("no", "physical", "mental")))

code_book <- code_book |>
  filter(variable == "v38") |>
  mutate(
    description = "残疾情况",
    type = "categorical",
    values = "
      no: 没有残疾; physical: 身体残疾;
      mental: 智力或精神残疾
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v38")
  )

# v39:是否有血友病家族史 ----

raw_data <- raw_data |>
  mutate(v39 = case_when(
    v39 == "是" ~ "yes",
    v39 == "不" ~ "no",
    TRUE ~ as.character(v39)
  )) |>
  mutate(v39 = factor(v39, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v39") |>
  mutate(
    description = "是否有血友病家族史",
    type = "categorical",
    values = "yes: 是; no: 不"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v39")
  )

# v40:使用哪种凝血因子/非凝血因子治疗 ----

raw_data <- raw_data |>
  mutate(v40_recombinant_factor = if_else(
    str_detect(v40, "重组人凝血因子"),
    "yes",
    "no"
  )) |>
  mutate(v40_plasma_factor = if_else(
    str_detect(v40, "血浆源性凝血因子"),
    "yes",
    "no"
  )) |>
  mutate(v40_pcc = if_else(
    str_detect(v40, "凝血酶原复合物（PCC）"),
    "yes",
    "no"
  )) |>
  mutate(v40_emicizumab = if_else(
    str_detect(v40, "舒友立乐（艾美赛珠单抗）"),
    "yes",
    "no"
  )) |>
  mutate(v40_other = if_else(
    str_detect(v40, "其他"),
    "yes",
    "no"
  )) |>
  mutate(v40_not_sure = if_else(
    str_detect(v40, "不清楚"),
    "yes",
    "no"
  )) |>
  mutate(across(starts_with("v40_"), ~ factor(., levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v40") |>
  mutate(
    description = "使用的治疗类型，未映射，但是创建了多个二值变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v40")
  )

code_book <- code_book |>
  add_row(
    variable = c(
      "v40_recombinant_factor",
      "v40_plasma_factor",
      "v40_pcc",
      "v40_emicizumab",
      "v40_other",
      "v40_not_sure"
    ),
    description = c(
      "是否使用重组人凝血因子",
      "是否使用血浆源性凝血因子",
      "是否使用凝血酶原复合物（PCC）",
      "是否使用舒友立乐（艾美赛珠单抗）",
      "是否使用其他治疗类型",
      "是否不清楚使用的治疗类型"
    ),
    type = rep("categorical", 6),
    values = rep("yes, no", 6)
  )

# v41:注射剂量 ----

raw_data <- raw_data |>
  mutate(v41 = as.numeric(v41))

code_book <- code_book |>
  filter(variable == "v41") |>
  mutate(
    description = "剂量",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v41")
  )

# v42:您注射凝血因子的频次？ ----

raw_data <- raw_data |>
  mutate(v42 = case_when(
    v42 == "0" ~ "zero",
    v42 == "少于每周一次" ~ "below_once_weekly",
    v42 == "每周一次" ~ "once_weekly",
    v42 == "每周两次" ~ "twice_weekly",
    v42 == "每周三次" ~ "three_times_weekly",
    v42 == "每周四次及以上" ~ "four_times_weekly_above",
    v42 == "仅在确定或怀疑出血时（按需治疗），没有规律注射" ~ "on_demand",
    TRUE ~ as.character(v42)
  )) |>
  mutate(v42 = factor(
    v42,
    levels = c(
      "zero", "below_once_weekly", "once_weekly", "twice_weekly",
      "three_times_weekly", "four_times_weekly_above", "on_demand"
    )
  ))

code_book <- code_book |>
  filter(variable == "v42") |>
  mutate(
    description = "使用频率",
    type = "categorical",
    values = "
      zero: 0; below_once_weekly: 少于每周一次;
      once_weekly: 每周一次; twice_weekly: 每周两次;
      three_times_weekly: 每周三次; four_times_weekly_above: 每周四次及以上;
      on_demand: 仅在确定或怀疑出血时（按需治疗），没有规律注射
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v42")
  )

# v43:注射频次2 ----

raw_data <- raw_data |>
  mutate(v43 = case_when(
    v43 == "规律预防治疗" ~ "regular",
    v43 == "按需治疗" ~ "on_demand",
    TRUE ~ as.character(v43)
  )) |>
  mutate(v43 = factor(v43, levels = c("regular", "on_demand")))

code_book <- code_book |>
  filter(variable == "v43") |>
  mutate(
    description = "使用方式",
    type = "categorical",
    values = "regular: 规律预防治疗; on_demand: 按需治疗"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v43")
  )

# v44:为什么选择这样的注射频率 ----

raw_data <- raw_data |>
  mutate(v44_doctor_advice = if_else(
    str_detect(v44, "医生建议"),
    "yes",
    "no"
  )) |>
  mutate(v44_economic_price = if_else(
    str_detect(v44, "经济、价格因素"),
    "yes",
    "no"
  )) |>
  mutate(v44_symptom_change = if_else(
    str_detect(v44, "症状的变化"),
    "yes",
    "no"
  )) |>
  mutate(v44_objective_conditions = if_else(
    str_detect(v44, "客观条件不便"),
    "yes",
    "no"
  )) |>
  mutate(v44_personal_reasons = if_else(
    str_detect(v44, "个人主观原因"),
    "yes",
    "no"
  )) |>
  mutate(v44_other = if_else(
    str_detect(v44, "其他"),
    "yes",
    "no"
  )) |>
  mutate(across(starts_with("v44_"), ~ factor(., levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v44") |>
  mutate(
    description = "不规律使用的原因，未映射，但是创建了多个二值变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v44")
  )

code_book <- code_book |>
  bind_rows(
    data.frame(
      variable = c(
        "v44_doctor_advice", "v44_economic_price", "v44_symptom_change",
        "v44_objective_conditions", "v44_personal_reasons", "v44_other"
      ),
      original_name = c(
        "不规律使用原因-医生建议", "不规律使用原因-经济、价格因素",
        "不规律使用原因-症状的变化", "不规律使用原因-客观条件不便",
        "不规律使用原因-个人主观原因", "不规律使用原因-其他"
      ),
      description = c(
        "是否因为医生建议而不规律使用", "是否因为经济、价格因素而不规律使用",
        "是否因为症状的变化而不规律使用", "是否因为客观条件不便而不规律使用",
        "是否因为个人主观原因而不规律使用", "是否因为其他原因而不规律使用"
      ),
      type = rep("categorical", 6),
      values = rep("yes: 是; no: 否", 6)
    )
  )

# v45:谁为您注射？ ----

raw_data <- raw_data |>
  mutate(v45_self = if_else(
    str_detect(v45, "自己注射"),
    "yes",
    "no"
  )) |>
  mutate(v45_family = if_else(
    str_detect(v45, "家人"),
    "yes",
    "no"
  )) |>
  mutate(v45_healthcare_staff = if_else(
    str_detect(v45, "医院、诊所、卫生室等场所的医护人员"),
    "yes",
    "no"
  )) |>
  mutate(v45_hired_caregiver = if_else(
    str_detect(v45, "家中专门聘请的护工"),
    "yes",
    "no"
  )) |>
  mutate(across(starts_with("v45_"), ~ factor(., levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v45") |>
  mutate(
    description = "谁为你注射凝血因子，未映射，但是创建了多个二值变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v45")
  )

code_book <- code_book |>
  bind_rows(
    data.frame(
      variable = c(
        "v45_self", "v45_family", "v45_healthcare_staff", "v45_hired_caregiver"
      ),
      original_name = c(
        "注射人-自己注射", "注射人-家人",
        "注射人-医院、诊所、卫生室等场所的医护人员", "注射人-家中专门聘请的护工"
      ),
      description = c(
        "是否自己注射凝血因子", "是否家人注射凝血因子",
        "是否医院、诊所、卫生室等场所的医护人员注射凝血因子", "是否家中专门聘请的护工注射凝血因子"
      ),
      type = rep("categorical", 4),
      values = rep("yes: 是; no: 否", 4)
    )
  )

# v46:能否家庭注射（本人+家人+保姆） ----

raw_data <- raw_data |>
  mutate(v46 = case_when(
    v46 == "能" ~ "yes",
    v46 == "不能" ~ "no",
    TRUE ~ as.character(v46)
  )) |>
  mutate(v46 = factor(v46, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v46") |>
  mutate(
    description = "能否家庭注射（本人+家人+保姆）",
    type = "categorical",
    values = "yes: 能; no: 不能"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v46")
  )

# v47:您有产生过凝血因子抑制物吗？（现在或以前都算） ----

raw_data <- raw_data |>
  mutate(v47 = case_when(
    v47 == "有" ~ "yes",
    v47 == "没有" ~ "no",
    v47 == "不确定" ~ "not_sure",
    TRUE ~ as.character(v47)
  )) |>
  mutate(v47 = factor(v47, levels = c("no", "yes", "not_sure")))

code_book <- code_book |>
  filter(variable == "v47") |>
  mutate(
    description = "产生过凝血因子抑制物",
    type = "categorical",
    values = "yes: 有; no: 没有; not_sure: 不确定"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v47")
  )

# v48:您过去2个月内, 有发生过自发性出血吗？ ----

raw_data <- raw_data |>
  mutate(v48 = case_when(
    v48 == "有" ~ "yes",
    v48 == "没有" ~ "no",
    v48 == "不清楚" ~ "not_sure",
    TRUE ~ as.character(v48)
  )) |>
  mutate(v48 = factor(v48, levels = c("no", "yes", "not_sure")))

code_book <- code_book |>
  filter(variable == "v48") |>
  mutate(
    description = "自发性出血",
    type = "categorical",
    values = "yes: 有; no: 没有; not_sure: 不清楚"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v48")
  )

# v49:您多久发生一次自发性出血？ ----

raw_data <- raw_data |>
  mutate(v49 = case_when(
    v49 == "每月不到一次" ~ "below_once_monthly",
    v49 == "每月数次" ~ "several_times_monthly",
    v49 == "每周数次" ~ "several_times_weekly",
    TRUE ~ as.character(v49)
  )) |>
  mutate(v49 = factor(
    v49,
    levels = c(
      "below_once_monthly", "several_times_monthly", "several_times_weekly"
    )
  ))

code_book <- code_book |>
  filter(variable == "v49") |>
  mutate(
    description = "自发性出血频率",
    type = "categorical",
    values = "
      below_once_monthly: 每月不到一次;
      several_times_monthly: 每月数次;
      several_times_weekly: 每周数次
    "
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v49")
  )

# v50:ABR ----

raw_data <- raw_data |>
  mutate(v50 = as.numeric(v50))

code_book <- code_book |>
  filter(variable == "v50") |>
  mutate(
    description = "ABR",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v50")
  )

# v51:AJBR ----

raw_data <- raw_data |>
  mutate(v51 = as.numeric(v51))

code_book <- code_book |>
  filter(variable == "v51") |>
  mutate(
    description = "AJBR",
    type = "numeric",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v51")
  )

# v52:您有哪些部位是靶关节？ ----

raw_data <- raw_data |>
  mutate(v52_left_knee = if_else(
    str_detect(v52, "左侧膝关节（膝盖）"),
    "yes",
    "no"
  )) |>
  mutate(v52_right_knee = if_else(
    str_detect(v52, "右侧膝关节（膝盖）"),
    "yes",
    "no"
  )) |>
  mutate(v52_left_elbow = if_else(
    str_detect(v52, "左侧肘关节（手肘）"),
    "yes",
    "no"
  )) |>
  mutate(v52_right_elbow = if_else(
    str_detect(v52, "右侧肘关节（手肘）"),
    "yes",
    "no"
  )) |>
  mutate(v52_left_ankle = if_else(
    str_detect(v52, "左侧踝关节（脚踝）"),
    "yes",
    "no"
  )) |>
  mutate(v52_right_ankle = if_else(
    str_detect(v52, "右侧踝关节（脚踝）"),
    "yes",
    "no"
  )) |>
  mutate(v52_left_shoulder = if_else(
    str_detect(v52, "左侧肩关节（肩膀）"),
    "yes",
    "no"
  )) |>
  mutate(v52_right_shoulder = if_else(
    str_detect(v52, "右侧肩关节（肩膀）"),
    "yes",
    "no"
  )) |>
  mutate(v52_finger = if_else(
    str_detect(v52, "指关节（手指）"),
    "yes",
    "no"
  )) |>
  mutate(v52_toe = if_else(
    str_detect(v52, "趾关节（脚趾）"),
    "yes",
    "no"
  )) |>
  mutate(v52_other = if_else(
    str_detect(v52, "其他"),
    "yes",
    "no"
  )) |>
  mutate(across(starts_with("v52_"), ~ factor(., levels = c("no", "yes"))))

code_book <- code_book |>
  filter(variable == "v52") |>
  mutate(
    description = "靶关节类型，未映射，但是创建了多个二值变量",
    type = "categorical",
    values = ""
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v52")
  )

code_book <- code_book |>
  bind_rows(
    data.frame(
      variable = c(
        "v52_left_knee", "v52_right_knee", "v52_left_elbow",
        "v52_right_elbow", "v52_left_ankle", "v52_right_ankle",
        "v52_left_shoulder", "v52_right_shoulder", "v52_finger",
        "v52_toe", "v52_other"
      ),
      original_name = c(
        "靶关节-左侧膝关节（膝盖）", "靶关节-右侧膝关节（膝盖）",
        "靶关节-左侧肘关节（手肘）", "靶关节-右侧肘关节（手肘）",
        "靶关节-左侧踝关节（脚踝）", "靶关节-右侧踝关节（脚踝）",
        "靶关节-左侧肩关节（肩膀）", "靶关节-右侧肩关节（肩膀）",
        "靶关节-指关节（手指）", "靶关节-趾关节（脚趾）",
        "靶关节-其他"
      ),
      description = c(
        "是否有左侧膝关节（膝盖）为靶关节", "是否有右侧膝关节（膝盖）为靶关节",
        "是否有左侧肘关节（手肘）为靶关节", "是否有右侧肘关节（手肘）为靶关节",
        "是否有左侧踝关节（脚踝）为靶关节", "是否有右侧踝关节（脚踝）为靶关节",
        "是否有左侧肩关节（肩膀）为靶关节", "是否有右侧肩关节（肩膀）为靶关节",
        "是否有指关节（手指）为靶关节", "是否有趾关节（脚趾）为靶关节",
        "是否有其他靶关节"
      ),
      type = rep("categorical", 11),
      values = rep("yes: 是; no: 否", 11)
    )
  )

# v53:是否会在家中进行简单的康复和理疗 ----

raw_data <- raw_data |>
  mutate(v53 = case_when(
    v53 == "会" ~ "yes",
    v53 == "不会" ~ "no",
    TRUE ~ as.character(v53)
  )) |>
  mutate(v53 = factor(v53, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v53") |>
  mutate(
    description = "是否会在家中进行简单的康复和理疗",
    type = "categorical",
    values = "yes: 会; no: 不会"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v53")
  )

# v54:基因检测 ----

raw_data <- raw_data |>
  mutate(v54 = case_when(
    v54 == "做过" ~ "yes",
    v54 == "从来没做过" ~ "no",
    TRUE ~ as.character(v54)
  )) |>
  mutate(v54 = factor(v54, levels = c("no", "yes")))

code_book <- code_book |>
  filter(variable == "v54") |>
  mutate(
    description = "基因检测",
    type = "categorical",
    values = "yes: 做过; no: 从来没做过"
  ) |>
  bind_rows(
    code_book |>
      filter(variable != "v54")
  )

# 最终数据与码本 ----

recode_data <- raw_data |>
  select(all_of(sort(names(raw_data)))) |>
  select(id, v1, v2, v3, v4, v5, v6, v7, v8, v9, everything())

missing_entries <- tibble(
  variable = c(
    "v31_no_option", "v35_arthritis", "v35_overweight_obesity",
    "v35_hepatitis_b", "v35_hepatitis_c", "v35_pseudotumor", "v35_hiv"
  ),
  description = c(
    "未选择支持内容（含跳过）", "关节炎", "超重或肥胖",
    "乙型肝炎", "丙型肝炎", "假肿瘤", "HIV感染"
  ),
  type = "categorical",
  values = "no: 无; yes: 有"
)
code_book <- bind_rows(code_book, missing_entries)
stopifnot(!anyDuplicated(code_book$variable))
stopifnot(all(names(recode_data) %in% code_book$variable))
code_book <- code_book[match(names(recode_data), code_book$variable), ]
code_book$source_variable <- sub("_.*$", "", code_book$variable)
code_book$source_question <- mapping_table$original_name[
  match(code_book$source_variable, mapping_table$variable)
]
code_book$original_name <- coalesce(
  code_book$original_name, code_book$source_question
)
code_book$type <- vapply(recode_data, function(x) {
  if (is.factor(x)) "categorical" else if (is.numeric(x)) "numeric" else "text"
}, character(1))
code_book$factor_levels <- vapply(recode_data, function(x) {
  if (is.factor(x)) paste(levels(x), collapse = " | ") else ""
}, character(1))
code_book$reference_level <- vapply(recode_data, function(x) {
  if (is.factor(x) && nlevels(x) > 1) levels(x)[1] else ""
}, character(1))
binary_columns <- vapply(recode_data, function(x) {
  is.factor(x) && identical(levels(x), c("no", "yes"))
}, logical(1))
code_book$values[binary_columns] <- "no: 0; yes: 1 (treatment encoding)"
code_book$shap_display <- ifelse(
  code_book$type == "numeric", "continuous", "categorical"
)
code_book$shap_display[binary_columns] <- "binary_no_yes"
ordinal_variables <- c("v8", "v13", "v20", "v25", "v26", "v33", "v49")
code_book$shap_display[code_book$variable %in% ordinal_variables] <-
  "ordered_categories_in_factor_level_order"
code_book$review_note <- ""
code_book$review_note[code_book$variable %in% c("v1", "v2", "v3")] <-
  "样本内常量；保留用于人群描述，不提供预测信息"
code_book$review_note[code_book$variable == "v15"] <-
  "当前与v9逐行一致；后续建模时避免重复纳入"
code_book$review_note[code_book$variable == "v11"] <-
  "no表示职业题跳过（无业、学生、退休），不等同于没有工作"
code_book$review_note[code_book$variable %in% c("v22", "v23")] <-
  "旧规则将跳过记为0；需结合v21和原问卷核实"
code_book$review_note[code_book$variable == "v28"] <-
  "no表示跳过，not_remember表示不记得；需结合v27解释"
code_book$review_note[code_book$variable == "v29"] <-
  "待确认：跳过统一记0混合从未治疗、不定期、不记得等来源"
code_book$review_note[startsWith(code_book$variable, "v31_")] <-
  "当前拆分不能区分未选择与跳过；需核对问卷逻辑"
code_book$review_note[code_book$variable %in% c("v47", "v48")] <-
  "not_sure为明确报告的未知状态，不并入no"
code_book$review_note[code_book$variable == "v43"] <-
  "原始表派生字段；少于每周一次含两种治疗分类，需确认派生规则"

stopifnot(nrow(recode_data) == nrow(source_data))
stopifnot(!anyDuplicated(recode_data$id))
stopifnot(identical(recode_data$id, source_data[[1]]))
stopifnot(!anyNA(recode_data))
stopifnot(identical(code_book$variable, names(recode_data)))
stopifnot(sum(recode_data$v35_hiv == "yes") == sum(
  str_detect(source_data[[36]], fixed("人类免疫缺陷病毒感染 (艾滋病/HIV)"))
))

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_dir <- file.path(interim_dir, "backup", run_stamp)
stopifnot(!dir.exists(backup_dir))
dir.create(backup_dir, recursive = TRUE)
output_files <- file.path(interim_dir, c(
  "mapping_table.csv", "code_book.csv", "recode_data.RData", "recode_data.csv"
))
existing_files <- output_files[file.exists(output_files)]
for (path in existing_files) {
  backup_file(path, run_stamp)
}
write_csv(mapping_table, file.path(interim_dir, "mapping_table.csv"))
write_csv(code_book, file.path(interim_dir, "code_book.csv"))

save(recode_data, file = file.path(interim_dir, "recode_data.RData"))

write_csv(recode_data, file.path(interim_dir, "recode_data.csv"))

message(
  "数据编码完成：", nrow(recode_data), " 人，", ncol(recode_data), " 列；",
  sum(binary_columns), " 个 Yes/No 因子以 no 为参考水平。"
)
message("中间数据目录：", interim_dir)
message("覆盖前备份：", backup_dir)
message("跳题含义等待确认事项见 code_book.csv 的 review_note 列。")
