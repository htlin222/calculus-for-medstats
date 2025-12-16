# 醫學統計的微積分基礎

**用 R 視覺化理解統計背後的數學**

[![Publish to GitHub Pages](https://github.com/htlin222/calculus-for-medstats/actions/workflows/publish.yml/badge.svg)](https://github.com/htlin222/calculus-for-medstats/actions/workflows/publish.yml)

## 線上閱讀

**https://htlin222.github.io/calculus-for-medstats/**

## 關於本書

這是一本用 R 視覺化來教醫師統計學所需微積分的 Quarto 書籍。目標讀者是數學背景較弱的醫師，需要理解統計方法背後的微積分概念。

### 適合誰閱讀？

- 在醫學研究中需要使用統計方法的醫師和研究人員
- 想理解統計背後數學原理的臨床工作者
- 閱讀統計教科書時對積分符號感到困惑的人
- 希望能與統計學家進行更有深度討論的研究者

### 本書特色

- **視覺化優先**：每個數學概念都先用 R 產生的圖形來解釋
- **醫學場景**：使用劑量反應曲線、藥物動力學、存活分析等醫學情境作為範例
- **統計應用導向**：每章明確說明該微積分概念在哪些統計方法中會用到

## 目錄結構

### Part I: 基礎概念

- Chapter 1: 函數與圖形
- Chapter 2: 極限
- Chapter 3: 連續性

### Part II: 微分

- Chapter 4: 導數的概念
- Chapter 5: 微分規則
- Chapter 6: 指數與對數函數
- Chapter 7: 最佳化

### Part III: 積分

- Chapter 8: 積分的概念
- Chapter 9: 積分技巧
- Chapter 10: 瑕積分

### Part IV: 多變量微積分

- Chapter 11: 偏微分
- Chapter 12: 多重積分

### Part V: 統計應用總整理

- Chapter 13: 機率分布
- Chapter 14: 最大概似估計
- Chapter 15: 迴歸分析
- Chapter 16: 存活分析
- Chapter 17: 貝氏統計

### 附錄

- 附錄 A: R 快速入門
- 附錄 B: 公式速查表
- 附錄 C: 練習題解答

## 本地開發

### 環境需求

- [R](https://www.r-project.org/) (>= 4.0)
- [Quarto](https://quarto.org/) (>= 1.3)

### 安裝 R 套件

```r
install.packages(c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "patchwork",
  "latex2exp",
  "plotly",
  "showtext",
  "here"
))
```

### 預覽書籍

```bash
quarto preview
```

### 編譯輸出

```bash
quarto render
```

## 配色方案

本書使用一致的配色方案：

- **主色** `#2E86AB`：用於主要曲線和資料
- **強調色** `#E94F37`：用於重點標示和對比

## 授權

本書內容版權所有。

## 作者

林協霆
