# Helper Functions for Calculus for Medical Statistics Book
# 醫學統計微積分教材共用函數

# Color Palette -----------------------------------------------------------

# 主要配色方案
colors <- list(
  primary = "#2E86AB",      # 主色：藍色
  accent = "#E94F37",       # 強調色：紅色
  neutral_dark = "#333333", # 深灰色（文字）
  neutral_mid = "#666666",  # 中灰色
  neutral_light = "#CCCCCC",# 淺灰色
  background = "#F8F9FA",   # 背景色
  success = "#28A745",      # 成功/正確
  warning = "#FFC107",      # 警告
  info = "#17A2B8"          # 資訊
)

# Theme Settings ----------------------------------------------------------

#' 書籍統一主題
#'
#' @param base_size 基礎字體大小
#' @param base_family 字體家族
#' @return ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_calculus()
theme_calculus <- function(base_size = 14, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # 標題
      plot.title = ggplot2::element_text(
        size = base_size * 1.2,
        face = "bold",
        color = colors$neutral_dark,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 0.9,
        color = colors$neutral_mid,
        margin = ggplot2::margin(b = 15)
      ),

      # 座標軸
      axis.title = ggplot2::element_text(
        size = base_size * 0.95,
        color = colors$neutral_dark
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.85,
        color = colors$neutral_mid
      ),

      # 圖例
      legend.title = ggplot2::element_text(
        size = base_size * 0.9,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.85
      ),

      # 網格線
      panel.grid.major = ggplot2::element_line(
        color = colors$neutral_light,
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_line(
        color = colors$neutral_light,
        linewidth = 0.15
      ),

      # 背景
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      )
    )
}

# Plotting Functions ------------------------------------------------------

#' 繪製函數曲線
#'
#' @param f 函數
#' @param xlim x 軸範圍，向量 c(min, max)
#' @param n 點的數量
#' @param color 線條顏色
#' @param title 標題
#' @param subtitle 副標題
#' @param xlab x 軸標籤
#' @param ylab y 軸標籤
#' @return ggplot object
#' @export
plot_function <- function(f, xlim = c(-5, 5), n = 500,
                         color = colors$primary,
                         title = NULL, subtitle = NULL,
                         xlab = "x", ylab = "f(x)") {
  x <- seq(xlim[1], xlim[2], length.out = n)
  y <- sapply(x, f)

  df <- data.frame(x = x, y = y)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = color, linewidth = 1.2) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    theme_calculus()
}

#' 繪製函數與其導數
#'
#' @param f 原函數
#' @param f_prime 導數函數
#' @param xlim x 軸範圍
#' @param n 點的數量
#' @return patchwork object (兩個圖垂直排列)
#' @export
plot_function_and_derivative <- function(f, f_prime, xlim = c(-5, 5), n = 500) {
  x <- seq(xlim[1], xlim[2], length.out = n)
  y <- sapply(x, f)
  y_prime <- sapply(x, f_prime)

  df <- data.frame(x = x, y = y, y_prime = y_prime)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = colors$primary, linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, color = "gray70") +
    ggplot2::labs(title = "原函數 f(x)", y = "f(x)") +
    theme_calculus()

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y_prime)) +
    ggplot2::geom_line(color = colors$accent, linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, color = "gray70") +
    ggplot2::labs(title = "導數 f'(x)", y = "f'(x)") +
    theme_calculus()

  p1 / p2
}

#' 繪製切線
#'
#' @param f 函數
#' @param x0 切點的 x 座標
#' @param f_prime 導數函數
#' @param xlim x 軸範圍
#' @param show_secant 是否顯示割線（預設 FALSE）
#' @param h 割線的 h 值（當 show_secant = TRUE）
#' @return ggplot object
#' @export
plot_tangent_line <- function(f, x0, f_prime, xlim = c(-5, 5),
                             show_secant = FALSE, h = 1) {
  x <- seq(xlim[1], xlim[2], length.out = 500)
  y <- sapply(x, f)

  y0 <- f(x0)
  slope <- f_prime(x0)

  p <- ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = colors$primary, linewidth = 1.2) +
    ggplot2::geom_abline(
      intercept = y0 - slope * x0,
      slope = slope,
      color = colors$accent,
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = x0, y = y0),
      color = colors$accent,
      size = 4
    ) +
    ggplot2::labs(
      title = paste0("在 x = ", x0, " 的切線"),
      subtitle = paste0("斜率 = ", round(slope, 3))
    ) +
    theme_calculus()

  if (show_secant) {
    x1 <- x0 + h
    y1 <- f(x1)
    slope_secant <- (y1 - y0) / h

    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1),
        color = colors$warning,
        linewidth = 0.8,
        linetype = "dotted"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = x1, y = y1),
        color = colors$warning,
        size = 3
      )
  }

  p
}

#' 繪製 Riemann Sum
#'
#' @param f 函數
#' @param a 積分下界
#' @param b 積分上界
#' @param n 長方形數量
#' @param method "left", "right", "midpoint"
#' @return ggplot object
#' @export
plot_riemann_sum <- function(f, a, b, n, method = "left") {
  x_breaks <- seq(a, b, length.out = n + 1)
  width <- (b - a) / n

  if (method == "left") {
    x_sample <- x_breaks[1:n]
  } else if (method == "right") {
    x_sample <- x_breaks[2:(n + 1)]
  } else if (method == "midpoint") {
    x_sample <- (x_breaks[1:n] + x_breaks[2:(n + 1)]) / 2
  }

  heights <- sapply(x_sample, f)

  rects <- data.frame(
    xmin = x_breaks[1:n],
    xmax = x_breaks[2:(n + 1)],
    ymin = 0,
    ymax = heights
  )

  x_curve <- seq(a, b, length.out = 500)
  y_curve <- sapply(x_curve, f)

  area <- sum(heights * width)

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$primary,
      alpha = 0.5,
      color = "white"
    ) +
    ggplot2::geom_line(
      data = data.frame(x = x_curve, y = y_curve),
      ggplot2::aes(x = x, y = y),
      color = colors$accent,
      linewidth = 1.2
    ) +
    ggplot2::labs(
      title = paste0("Riemann Sum (", n, " 個長方形)"),
      subtitle = paste0("面積近似值 = ", round(area, 4)),
      x = "x",
      y = "f(x)"
    ) +
    theme_calculus()
}

#' 繪製機率密度函數
#'
#' @param dist 分布名稱："norm", "exp", "beta", "gamma"
#' @param params 參數列表
#' @param xlim x 軸範圍
#' @param shade_area 是否著色某區域
#' @param shade_from 著色起點
#' @param shade_to 著色終點
#' @return ggplot object
#' @export
plot_pdf <- function(dist = "norm", params = list(), xlim = NULL,
                    shade_area = FALSE, shade_from = NULL, shade_to = NULL) {
  if (is.null(xlim)) {
    xlim <- switch(dist,
                   norm = c(-4, 4),
                   exp = c(0, 5),
                   beta = c(0, 1),
                   gamma = c(0, 10),
                   c(-5, 5))
  }

  x <- seq(xlim[1], xlim[2], length.out = 500)

  y <- switch(dist,
              norm = dnorm(x, mean = params$mean %||% 0, sd = params$sd %||% 1),
              exp = dexp(x, rate = params$rate %||% 1),
              beta = dbeta(x, shape1 = params$shape1 %||% 2, shape2 = params$shape2 %||% 2),
              gamma = dgamma(x, shape = params$shape %||% 2, rate = params$rate %||% 1),
              rep(0, length(x)))

  df <- data.frame(x = x, y = y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = colors$primary, linewidth = 1.2) +
    ggplot2::labs(
      title = paste0(toupper(dist), " 分布"),
      x = "x",
      y = "機率密度 f(x)"
    ) +
    theme_calculus()

  if (shade_area && !is.null(shade_from) && !is.null(shade_to)) {
    shade_data <- df[df$x >= shade_from & df$x <= shade_to, ]

    p <- p +
      ggplot2::geom_area(
        data = shade_data,
        fill = colors$accent,
        alpha = 0.4
      )
  }

  p
}

# Utility Functions -------------------------------------------------------

#' Null coalescing operator (helper)
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' 數值微分（近似）
#'
#' @param f 函數
#' @param x 求導點
#' @param h 步長（預設 1e-5）
#' @return 導數的數值近似值
#' @export
numerical_derivative <- function(f, x, h = 1e-5) {
  (f(x + h) - f(x - h)) / (2 * h)
}

#' 數值積分（梯形法）
#'
#' @param f 函數
#' @param a 積分下界
#' @param b 積分上界
#' @param n 分割數（預設 1000）
#' @return 積分的數值近似值
#' @export
numerical_integral <- function(f, a, b, n = 1000) {
  x <- seq(a, b, length.out = n + 1)
  y <- sapply(x, f)
  h <- (b - a) / n

  # 梯形法
  sum((y[-1] + y[-(n + 1)]) / 2 * h)
}

# Message on load ---------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("醫學統計微積分教材輔助函數已載入")
  packageStartupMessage("主要配色：primary = #2E86AB, accent = #E94F37")
  packageStartupMessage("使用 theme_calculus() 套用統一主題")
}
