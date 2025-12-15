# _common.R - 全書共用設定
# 在每個 .qmd 檔案開頭載入此檔案以確保一致性

# 載入必要套件 ----------------------------------------------------------------
library(ggplot2)
library(patchwork)
library(showtext)
library(here)

# 字型設定 --------------------------------------------------------------------
# 使用 jf-openhuninn 開源字型支援中文顯示

# 註冊字型（使用 here() 確保從專案根目錄找到字型檔案）
font_add("jf-openhuninn", here("jf-openhuninn-2.1.ttf"))

# 啟用 showtext 自動渲染
showtext_auto()

# 設定 showtext 的 DPI（針對不同輸出格式）
showtext_opts(dpi = 300)

# 配色方案 --------------------------------------------------------------------
colors_calculus <- list(
  primary = "#2E86AB",
  accent = "#E94F37",
  neutral_dark = "#333333",
  neutral_mid = "#666666",
  neutral_light = "#CCCCCC"
)

# 統一主題 --------------------------------------------------------------------
theme_calculus <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "jf-openhuninn") +
    theme(
      # 標題
      plot.title = element_text(
        size = base_size * 1.2,
        face = "bold",
        color = colors_calculus$neutral_dark,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = base_size * 0.9,
        color = colors_calculus$neutral_mid,
        margin = margin(b = 15)
      ),
      # 座標軸
      axis.title = element_text(
        size = base_size * 0.95,
        color = colors_calculus$neutral_dark
      ),
      axis.text = element_text(
        size = base_size * 0.85,
        color = colors_calculus$neutral_mid
      ),
      # 圖例
      legend.title = element_text(
        size = base_size * 0.9,
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.85
      ),
      # 網格線
      panel.grid.major = element_line(
        color = colors_calculus$neutral_light,
        linewidth = 0.3
      ),
      panel.grid.minor = element_line(
        color = colors_calculus$neutral_light,
        linewidth = 0.15
      ),
      # 背景
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# 設定 ggplot2 預設主題 -------------------------------------------------------
theme_set(theme_calculus())

# 設定 ggplot2 預設字型
update_geom_defaults("text", list(family = "jf-openhuninn"))
update_geom_defaults("label", list(family = "jf-openhuninn"))

# 訊息 ------------------------------------------------------------------------
message("已載入 _common.R：字型設定為 jf-openhuninn，主題設定為 theme_calculus()")
