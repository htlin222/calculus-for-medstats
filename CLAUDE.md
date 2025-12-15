# CLAUDE.md - 醫學統計微積分視覺化教材

## 專案概述

這是一本用 R 視覺化來教醫師統計學所需微積分的 Quarto 書籍。目標讀者是數學背景較弱的醫師，需要理解統計方法背後的微積分概念。

## 專案結構

```
calculus-for-medical-statistics/
├── _quarto.yml
├── index.qmd                 # 書籍首頁
├── intro.qmd                 # 導論：為什麼醫師需要懂微積分
├── part1-foundations/        # Part I: 基礎概念
│   ├── _part.qmd
│   ├── 01-functions.qmd      # Ch1: 函數與圖形
│   ├── 02-limits.qmd         # Ch2: 極限
│   └── 03-continuity.qmd     # Ch3: 連續性
├── part2-differentiation/    # Part II: 微分
│   ├── _part.qmd
│   ├── 04-derivative-concept.qmd    # Ch4: 導數的概念
│   ├── 05-derivative-rules.qmd      # Ch5: 微分規則
│   ├── 06-exp-log.qmd               # Ch6: 指數與對數函數
│   └── 07-optimization.qmd          # Ch7: 最佳化
├── part3-integration/        # Part III: 積分
│   ├── _part.qmd
│   ├── 08-integral-concept.qmd      # Ch8: 積分的概念
│   ├── 09-integral-rules.qmd        # Ch9: 積分技巧
│   └── 10-improper-integral.qmd     # Ch10: 瑕積分
├── part4-multivariate/       # Part IV: 多變量微積分
│   ├── _part.qmd
│   ├── 11-partial-derivatives.qmd   # Ch11: 偏微分
│   └── 12-multiple-integrals.qmd    # Ch12: 多重積分
├── part5-applications/       # Part V: 統計應用總整理
│   ├── _part.qmd
│   ├── 13-probability-distributions.qmd  # Ch13: 機率分布
│   ├── 14-mle.qmd                        # Ch14: 最大概似估計
│   ├── 15-regression.qmd                 # Ch15: 迴歸分析
│   ├── 16-survival.qmd                   # Ch16: 存活分析
│   └── 17-bayesian.qmd                   # Ch17: 貝氏統計
├── appendix/
│   ├── a-r-basics.qmd        # 附錄 A: R 快速入門（選讀）
│   ├── b-formulas.qmd        # 附錄 B: 公式速查表
│   └── c-answers.qmd         # 附錄 C: 練習題解答
├── references.bib
├── R/
│   └── helper-functions.R    # 共用繪圖函數
└── images/
```

## 技術規格

### Quarto 設定

```yaml
# _quarto.yml
project:
  type: book
  output-dir: _book

book:
  title: "醫學統計的微積分基礎"
  subtitle: "用 R 視覺化理解統計背後的數學"
  author: "【作者名】"
  date: today
  language: zh-TW

  chapters:
    - index.qmd
    - intro.qmd
    - part: part1-foundations/_part.qmd
      chapters:
        - part1-foundations/01-functions.qmd
        - part1-foundations/02-limits.qmd
        - part1-foundations/03-continuity.qmd
    - part: part2-differentiation/_part.qmd
      chapters:
        - part2-differentiation/04-derivative-concept.qmd
        - part2-differentiation/05-derivative-rules.qmd
        - part2-differentiation/06-exp-log.qmd
        - part2-differentiation/07-optimization.qmd
    - part: part3-integration/_part.qmd
      chapters:
        - part3-integration/08-integral-concept.qmd
        - part3-integration/09-integral-rules.qmd
        - part3-integration/10-improper-integral.qmd
    - part: part4-multivariate/_part.qmd
      chapters:
        - part4-multivariate/11-partial-derivatives.qmd
        - part4-multivariate/12-multiple-integrals.qmd
    - part: part5-applications/_part.qmd
      chapters:
        - part5-applications/13-probability-distributions.qmd
        - part5-applications/14-mle.qmd
        - part5-applications/15-regression.qmd
        - part5-applications/16-survival.qmd
        - part5-applications/17-bayesian.qmd
    - part: "附錄"
      chapters:
        - appendix/a-r-basics.qmd
        - appendix/b-formulas.qmd
        - appendix/c-answers.qmd
    - references.qmd

format:
  html:
    theme: cosmo
    code-fold: false
    code-tools: true
    toc: true
    toc-depth: 3
    number-sections: true
    highlight-style: github

execute:
  echo: true
  warning: false
  message: false

bibliography: references.bib
```

### R 環境

主要使用的 R packages：

- `ggplot2`: 主要繪圖工具
- `dplyr`, `tidyr`: 資料處理
- `patchwork`: 組合多個圖
- `gganimate`: 動態視覺化（選用）
- `latex2exp`: 在圖中顯示數學符號

## 章節內容規格

### 每章標準結構

```markdown
---
title: "章節標題"
---

## 學習目標 {.unnumbered}

- 目標 1
- 目標 2
- 目標 3

## 概念說明

【用白話文解釋概念，避免過度數學化】
【適時穿插生活或醫學比喻】

## 視覺化理解

【R 程式碼產生的互動/靜態圖】
【圖下方有詳細說明】

## 數學定義

【正式數學表達，用 LaTeX】
【對照前面的視覺化解釋】

## 練習題

### 觀念題
【不需要計算，測試理解】

### 計算題
【簡單計算，可用 R 驗證】

### R 操作題
【修改範例程式碼，觀察變化】

## 統計應用

【明確連結到醫學統計的應用】
【預告會在哪個統計方法中用到】

## 本章重點整理 {.unnumbered}

[3-5 個 bullet points]
```

---

## 各章節詳細內容

### Part I: 基礎概念

#### Chapter 1: 函數與圖形

**概念**：

- 什麼是函數：input → output 的對應關係
- 醫學例子：劑量-反應曲線、藥物濃度隨時間變化
- 常見函數類型：線性、二次、指數、對數

**視覺化任務**：

1. 繪製不同類型函數的基本形狀
2. 藥物動力學曲線範例
3. 互動：調整參數觀察曲線變化

**統計連結**：

- 所有統計模型都是函數
- Logistic regression 的 S 型曲線
- 存活曲線是時間的函數

**R 視覺化範例**：

```r
# 劑量反應曲線 (Emax model)
library(ggplot2)

dose <- seq(0, 100, by = 1)
emax <- 100
ec50 <- 20

response <- (emax * dose) / (ec50 + dose)

ggplot(data.frame(dose, response), aes(dose, response)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_hline(yintercept = emax, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = ec50, linetype = "dashed", color = "gray50") +
  annotate("point", x = ec50, y = emax/2, size = 3, color = "red") +
  annotate("text", x = ec50 + 5, y = emax/2 + 5, label = "EC50", hjust = 0) +
  labs(
    title = "劑量-反應曲線 (Dose-Response Curve)",
    subtitle = "Emax Model: Response = (Emax × Dose) / (EC50 + Dose)",
    x = "劑量 (Dose)",
    y = "反應 (Response %)"
  ) +
  theme_minimal(base_size = 14)
```

---

#### Chapter 2: 極限 (Limits)

**概念**：

- 極限是「趨近」的數學語言
- 不是「等於」，而是「無限接近」
- 左極限與右極限

**視覺化任務**：

1. 動畫：點沿著曲線移動，y 值趨近某值
2. 放大某一點附近，觀察函數行為
3. 極限存在 vs 不存在的視覺對比

**統計連結**：

- 大數法則 (Law of Large Numbers)
- 連續機率分布的定義基礎
- p-value 的極端情況

**R 視覺化範例**：

```r
# 視覺化極限：當 x → 2 時，f(x) = (x² - 4)/(x - 2) → ?
library(ggplot2)

x <- seq(0, 4, by = 0.01)
x <- x[x != 2]  # 移除 x = 2

f_x <- (x^2 - 4) / (x - 2)  # 其實 = x + 2

df <- data.frame(x = x, y = f_x)

ggplot(df, aes(x, y)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  geom_point(aes(x = 2, y = 4), shape = 21, size = 4,
             fill = "white", color = "red", stroke = 1.5) +
  annotate("text", x = 2.3, y = 4,
           label = "極限值 = 4\n（但 f(2) 未定義）", hjust = 0) +
  labs(
    title = "極限的視覺化",
    subtitle = expression(lim(x %->% 2)~frac(x^2 - 4, x - 2) == 4),
    x = "x", y = "f(x)"
  ) +
  theme_minimal(base_size = 14)
```

**放大鏡效果**：

```r
# 用多個 panel 展示「放大」的效果
library(patchwork)

create_limit_plot <- function(xlim_range, title) {
  ggplot(df, aes(x, y)) +
    geom_line(color = "#2E86AB", linewidth = 1) +
    geom_point(aes(x = 2, y = 4), shape = 21, size = 3,
               fill = "white", color = "red") +
    coord_cartesian(xlim = xlim_range, ylim = c(xlim_range[1] + 2, xlim_range[2] + 2)) +
    labs(title = title, x = "x", y = "f(x)") +
    theme_minimal()
}

p1 <- create_limit_plot(c(0, 4), "全貌")
p2 <- create_limit_plot(c(1, 3), "放大 1x")
p3 <- create_limit_plot(c(1.5, 2.5), "放大 2x")
p4 <- create_limit_plot(c(1.9, 2.1), "放大 10x")

(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "極限的「放大鏡」視角",
    subtitle = "越放大，越確定 f(x) 趨近於 4"
  )
```

---

#### Chapter 3: 連續性 (Continuity)

**概念**：

- 連續 = 可以一筆畫完，不用抬起筆
- 連續的三個條件：函數有定義、極限存在、極限等於函數值
- 不連續的類型：跳躍、空洞、漸近線

**視覺化任務**：

1. 連續 vs 不連續函數的對比
2. 機率密度函數為何必須連續
3. 存活函數的 step function（不連續但有意義）

**統計連結**：

- 連續型 vs 離散型隨機變數
- PDF 必須是連續函數
- Kaplan-Meier 曲線的階梯狀特性

---

### Part II: 微分

#### Chapter 4: 導數的概念 (The Derivative)

**概念**：

- 導數 = 瞬時變化率 = 切線斜率
- 從平均變化率 （割線） 到瞬時變化率 （切線）
- 導數的符號：$f'(x)$, $\frac{df}{dx}$, $\frac{dy}{dx}$

**視覺化任務**：

1. 動畫：割線逐漸變成切線
2. 不同點的切線斜率比較
3. 導數函數的繪製

**統計連結**：

- Hazard rate：死亡風險的瞬時變化率
- Score function in MLE
- Gradient descent 的基本概念

**R 視覺化範例**：

```r
# 割線 → 切線的動畫概念（靜態版本）
library(ggplot2)
library(patchwork)

f <- function(x) x^2
f_prime <- function(x) 2*x

x0 <- 1  # 切點
y0 <- f(x0)

# 不同的 h 值
h_values <- c(2, 1, 0.5, 0.1)

plots <- lapply(h_values, function(h) {
  x1 <- x0 + h
  y1 <- f(x1)
  slope_secant <- (y1 - y0) / h

  x_range <- seq(-0.5, 3, by = 0.01)

  ggplot() +
    # 原函數
    geom_line(aes(x = x_range, y = f(x_range)),
              color = "#2E86AB", linewidth = 1) +
    # 割線
    geom_abline(intercept = y0 - slope_secant * x0,
                slope = slope_secant,
                color = "red", linewidth = 0.8, linetype = "dashed") +
    # 兩點
    geom_point(aes(x = c(x0, x1), y = c(y0, y1)),
               color = "red", size = 3) +
    coord_cartesian(xlim = c(-0.5, 3), ylim = c(-0.5, 5)) +
    labs(
      title = paste0("h = ", h),
      subtitle = paste0("割線斜率 = ", round(slope_secant, 2)),
      x = "x", y = "f(x)"
    ) +
    theme_minimal()
})

wrap_plots(plots, ncol = 2) +
  plot_annotation(
    title = "從割線到切線",
    subtitle = "當 h → 0，割線斜率 → 導數值 (= 2)"
  )
```

---

#### Chapter 5: 微分規則 (Differentiation Rules)

**概念**：

- 常數規則、冪次規則
- 和差規則、乘法規則、除法規則
- 連鎖律 (Chain Rule) — 最重要！

**視覺化任務**：

1. 冪次規則的圖形理解
2. 連鎖律的「層層剝開」視覺化
3. 原函數與導函數的對照圖

**統計連結**：

- Logit 函數的微分（logistic regression）
- 機率密度函數與累積分布函數的關係：$f(x) = F'(x)$

**R 視覺化範例**：

```r
# 函數與其導數的對照
library(ggplot2)
library(patchwork)

x <- seq(-3, 3, by = 0.01)

# f(x) = x³ - 3x
f <- x^3 - 3*x
f_prime <- 3*x^2 - 3

df <- data.frame(x, f, f_prime)

p1 <- ggplot(df, aes(x, f)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray70") +
  # 標記極值點
  geom_point(aes(x = -1, y = 2), color = "red", size = 3) +
  geom_point(aes(x = 1, y = -2), color = "red", size = 3) +
  labs(title = expression(f(x) == x^3 - 3*x), y = "f(x)") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x, f_prime)) +
  geom_line(color = "#E94F37", linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray70") +
  # 導數為零的點
  geom_point(aes(x = c(-1, 1), y = c(0, 0)), color = "red", size = 3) +
  labs(title = expression(f*"'"*(x) == 3*x^2 - 3), y = "f'(x)") +
  theme_minimal(base_size = 12)

p1 / p2 +
  plot_annotation(
    title = "函數與導數的關係",
    subtitle = "f'(x) = 0 的點，對應 f(x) 的極值"
  )
```

---

#### Chapter 6: 指數與對數函數 (Exponential & Logarithmic Functions)

**概念**：

- $e$ 的特殊性：$\frac{d}{dx}e^x = e^x$
- 對數微分：$\frac{d}{dx}\ln x = \frac{1}{x}$
- 為什麼統計學愛用 log：乘法變加法、壓縮尺度

**視覺化任務**：

1. $e^x$ 的切線斜率恰好等於 $y$ 值
2. Log transformation 的效果
3. Log-odds 的視覺化

**統計連結**：

- Log-likelihood 為什麼取 log
- Logistic regression 的 log-odds
- Cox regression 的 hazard ratio （取 log 後）
- 為什麼很多分布用 $e^{-x}$ 形式

**R 視覺化範例**：

```r
# e^x 的神奇性質：斜率 = 函數值
library(ggplot2)

x <- seq(-2, 2, by = 0.01)
y <- exp(x)

# 在幾個點畫切線
points_x <- c(-1, 0, 1)

ggplot(data.frame(x, y), aes(x, y)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  # 切線
  geom_abline(intercept = 0, slope = exp(-1),
              color = "red", alpha = 0.5) +
  geom_abline(intercept = -1 + 1, slope = exp(0),
              color = "red", alpha = 0.5) +
  geom_abline(intercept = -exp(1) + exp(1), slope = exp(1),
              color = "red", alpha = 0.5) +
  # 切點
  geom_point(aes(x = points_x, y = exp(points_x)),
             color = "red", size = 3) +
  # 標註
  annotate("text", x = -1, y = exp(-1) + 0.5,
           label = "斜率 = e⁻¹ ≈ 0.37", hjust = 0, size = 3) +
  annotate("text", x = 0, y = exp(0) + 0.5,
           label = "斜率 = e⁰ = 1", hjust = 0, size = 3) +
  annotate("text", x = 1, y = exp(1) + 0.5,
           label = "斜率 = e¹ ≈ 2.72", hjust = 0, size = 3) +
  labs(
    title = expression(e^x~"的神奇性質"),
    subtitle = "每一點的切線斜率，恰好等於該點的 y 值",
    x = "x", y = expression(e^x)
  ) +
  theme_minimal(base_size = 14)
```

**Log transformation 效果**：

```r
# 右偏資料的 log transformation
library(ggplot2)
library(patchwork)

set.seed(42)
# 模擬醫療費用資料（右偏）
cost <- rlnorm(1000, meanlog = 10, sdlog = 1)

p1 <- ggplot(data.frame(cost), aes(cost)) +
  geom_histogram(fill = "#2E86AB", color = "white", bins = 50) +
  labs(title = "原始資料：醫療費用",
       subtitle = "嚴重右偏",
       x = "費用 （元）", y = "次數") +
  theme_minimal()

p2 <- ggplot(data.frame(cost), aes(log(cost))) +
  geom_histogram(fill = "#E94F37", color = "white", bins = 50) +
  labs(title = "Log 轉換後",
       subtitle = "接近常態分布",
       x = "log（費用）", y = "次數") +
  theme_minimal()

p1 + p2
```

---

#### Chapter 7: 最佳化 (Optimization)

**概念**：

- 找極值：令 $f'(x) = 0$
- 二階導數判斷凹凸性
- 局部極值 vs 全域極值

**視覺化任務**：

1. 極值點的視覺辨識
2. 凹向上 vs 凹向下
3. MLE 的最佳化過程動畫

**統計連結**：

- Maximum Likelihood Estimation 的核心
- Least Squares 的最佳化
- Gradient descent 的直觀理解

**R 視覺化範例**：

```r
# MLE 視覺化：估計常態分布的平均數
library(ggplot2)

# 假設觀察到的資料
set.seed(123)
data <- rnorm(20, mean = 5, sd = 2)

# Log-likelihood 函數（固定 sigma = 2）
log_lik <- function(mu) {
  sum(dnorm(data, mean = mu, sd = 2, log = TRUE))
}

mu_range <- seq(2, 8, by = 0.01)
ll_values <- sapply(mu_range, log_lik)

# MLE
mu_mle <- mu_range[which.max(ll_values)]

ggplot(data.frame(mu = mu_range, ll = ll_values), aes(mu, ll)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_vline(xintercept = mu_mle, color = "red", linetype = "dashed") +
  geom_point(aes(x = mu_mle, y = max(ll_values)),
             color = "red", size = 4) +
  annotate("text", x = mu_mle + 0.3, y = max(ll_values),
           label = paste0("MLE = ", round(mu_mle, 2)), hjust = 0) +
  labs(
    title = "Maximum Likelihood Estimation",
    subtitle = "找到讓 log-likelihood 最大的參數值",
    x = expression(mu),
    y = "Log-Likelihood"
  ) +
  theme_minimal(base_size = 14)
```

---

### Part III: 積分

#### Chapter 8: 積分的概念 (The Integral)

**概念**：

- 積分 = 曲線下面積
- 從長方形近似到精確面積
- 定積分 vs 不定積分

**視覺化任務**：

1. Riemann sum 動畫：長方形越來越多
2. 機率密度函數下的面積 = 機率
3. 累積分布函數的形成

**統計連結**：

- PDF 下的面積 = 機率
- CDF 是 PDF 的積分
- P-value 就是一個積分值

**R 視覺化範例**：

```r
# Riemann Sum 視覺化
library(ggplot2)
library(patchwork)

f <- function(x) dnorm(x)  # 標準常態 PDF

create_riemann_plot <- function(n_rect) {
  x_breaks <- seq(-3, 3, length.out = n_rect + 1)
  width <- diff(x_breaks)[1]

  # 用左端點
  x_left <- x_breaks[-(n_rect + 1)]
  heights <- f(x_left)

  rects <- data.frame(
    xmin = x_left,
    xmax = x_left + width,
    ymin = 0,
    ymax = heights
  )

  area <- sum(heights * width)

  ggplot() +
    geom_rect(data = rects,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#2E86AB", alpha = 0.5, color = "white") +
    stat_function(fun = f, linewidth = 1, color = "#E94F37") +
    labs(
      title = paste0(n_rect, " 個長方形"),
      subtitle = paste0("面積 ≈ ", round(area, 4)),
      x = "x", y = "f(x)"
    ) +
    coord_cartesian(xlim = c(-3, 3), ylim = c(0, 0.45)) +
    theme_minimal()
}

p1 <- create_riemann_plot(5)
p2 <- create_riemann_plot(10)
p3 <- create_riemann_plot(20)
p4 <- create_riemann_plot(50)

(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Riemann Sum → 定積分",
    subtitle = "長方形越多，面積越精確 （真實值 ≈ 0.9974）"
  )
```

---

#### Chapter 9: 積分技巧 (Integration Techniques)

**概念**：

- 基本積分公式
- 換元積分法 (Substitution)
- 分部積分法 (Integration by Parts) — 簡介即可

**視覺化任務**：

1. 反導數的圖形意義
2. 常見機率分布的積分

**統計連結**：

- 計算各種分布的期望值
- 計算累積機率

---

#### Chapter 10: 瑕積分 (Improper Integrals)

**概念**：

- 積分範圍到無窮大
- 為什麼 $\int_{-\infty}^{\infty} f(x)dx = 1$ 對 PDF 很重要
- 收斂 vs 發散

**視覺化任務**：

1. PDF 曲線下總面積 = 1
2. 常態分布的尾端行為
3. 指數分布的「無記憶性」

**統計連結**：

- 機率分布的正規化
- 期望值的計算
- Survival function: $S(t) = \int_t^{\infty} f(x)dx$

**R 視覺化範例**：

```r
# 常態分布的面積 = 1
library(ggplot2)

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, geom = "area",
                fill = "#2E86AB", alpha = 0.5) +
  stat_function(fun = dnorm, linewidth = 1) +
  annotate("text", x = 0, y = 0.15,
           label = "總面積 = 1", size = 6, fontface = "bold") +
  labs(
    title = "機率密度函數的基本性質",
    subtitle = expression(integral(f(x)*dx, -infinity, infinity) == 1),
    x = "x", y = "f(x)"
  ) +
  theme_minimal(base_size = 14)
```

---

### Part IV: 多變量微積分

#### Chapter 11: 偏微分 (Partial Derivatives)

**概念**：

- 多變量函數：$f(x, y)$
- 偏微分：固定其他變數，對一個變數微分
- 幾何意義：在某個方向的變化率

**視覺化任務**：

1. 3D 曲面圖
2. 固定一個變數，看另一個方向的切面
3. 梯度向量的方向

**統計連結**：

- 多元迴歸的係數
- 多參數 MLE
- 聯合機率分布

**R 視覺化範例**：

```r
# 3D 曲面與偏微分
library(plotly)

x <- seq(-3, 3, length.out = 50)
y <- seq(-3, 3, length.out = 50)

z <- outer(x, y, function(x, y) x^2 + y^2)

plot_ly(x = x, y = y, z = z, type = "surface") %>%
  layout(
    title = "f(x, y) = x² + y²",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)")
    )
  )
```

---

#### Chapter 12: 多重積分 (Multiple Integrals)

**概念**：

- 雙重積分：體積的計算
- 積分順序
- 聯合機率的計算

**視覺化任務**：

1. 雙變量常態分布的 3D 圖
2. 邊際分布的計算（積分掉一個變數）
3. 相關係數對聯合分布形狀的影響

**統計連結**：

- 聯合機率分布
- 邊際分布的推導
- 期望值的計算

---

### Part V: 統計應用總整理

#### Chapter 13: 機率分布 (Probability Distributions)

**內容**：

- PDF 和 CDF 的微積分關係
- 常見分布的數學形式
- 期望值、變異數的積分定義

**重點分布**：

- 常態分布
- 指數分布（存活分析）
- Beta 分布（貝氏）
- Gamma 分布

---

#### Chapter 14: 最大概似估計 (Maximum Likelihood Estimation)

**內容**：

- Likelihood function 的建構
- 為什麼取 log
- 令導數 = 0 求解
- Fisher Information 與標準誤

**完整範例**：

- 估計常態分布參數
- 估計 Poisson 參數
- Logistic regression 的 MLE

---

#### Chapter 15: 迴歸分析 (Regression Analysis)

**內容**：

- OLS 的微積分推導
- 偏微分求解迴歸係數
- Logistic regression 的 odds 與微分

**視覺化**：

- 殘差平方和的 3D 曲面
- 最小值的位置 = 迴歸係數

---

#### Chapter 16: 存活分析 (Survival Analysis)

**內容**：

- Survival function: $S(t) = 1 - F(t)$
- Hazard function: $h(t) = \frac{f(t)}{S(t)}$
- 累積風險函數：$H(t) = \int_0^t h(u)du$

**視覺化**：

- S(t), f(t), h(t) 的關係圖
- Exponential vs Weibull 分布的 hazard 形狀

---

#### Chapter 17: 貝氏統計 (Bayesian Statistics)

**內容**：

- 貝氏定理的連續版本
- Prior × Likelihood ∝ Posterior
- 共軛先驗的數學原理

**視覺化**：

- Prior → Posterior 的更新過程
- Beta-Binomial 的動態視覺化

---

## 設計原則

### 語言風格

- 中英夾雜，專有名詞保留英文：derivative, integral, likelihood, hazard, survival function
- 避免過度數學化的語言
- 多用醫學場景舉例
- 口語化但不失專業

### 程式碼風格

- 每段程式碼都有註解
- 可直接複製執行
- 使用 tidyverse 風格
- 圖形美觀、配色一致（主色 #2E86AB, 強調色 #E94F37）

### 視覺化原則

- 每個概念至少一張圖
- 優先使用靜態圖（`ggplot2`）
- 複雜概念可用多圖並排（`patchwork`）
- 3D 圖用 `plotly` 增加互動性
- 圖的標題和副標題要解釋重點

### 練習題設計

- 觀念題：不需計算，測試理解
- 計算題：簡單計算，可用 R 驗證答案
- R 操作題：修改範例程式碼，觀察變化
- 統計連結題：說明某統計方法用到哪個微積分概念

---

## 執行指令

### 建立專案

```bash
# 建立專案資料夾結構
mkdir -p calculus-for-medical-statistics/{part1-foundations,part2-differentiation,part3-integration,part4-multivariate,part5-applications,appendix,R,images}

# 初始化 Quarto book
cd calculus-for-medical-statistics
quarto create-project . --type book
```

### 建立章節檔案

根據上述結構建立所有 .qmd 檔案，每個檔案遵循標準章節結構。

### 預覽書籍

```bash
quarto preview
```

### 編譯輸出

```bash
quarto render
```

---

## 補充資源

### 參考書籍

- Caserta & Berger - Statistical Inference
- Gelman et al. - Bayesian Data Analysis
- Collett - Modelling Survival Data in Medical Research

### R 套件文件

- ggplot2: <https://ggplot2.tidyverse.org/>
- patchwork: <https://patchwork.data-imaginist.com/>
- plotly: <https://plotly.com/r/>

---

## 版本紀錄

- v0.1: 初始架構設計
