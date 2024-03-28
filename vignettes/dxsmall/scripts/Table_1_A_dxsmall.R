
suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(ggpubr)
  library(data.table)
  library(parallel)
  library(mclust)
  library(gtsummary)
  library(tidyverse)
  library(gt)
  library(mixR)
})

setwd("/work/larylab/Mao_Ding/bifurcatoR/vignettes/dxsmall")

### Explore the Data
data(dxsmall)
show(dxsmall)

### Cohort Characteristics

df = as.data.frame(dxsmall@colData@listData)
df_filtered = data.frame(AgeGroup = df$AgeGroup,
                         Sex = df$Sex,
                         FAB = df$FAB,
                         FusionGroup = df$FusionGroup,
                         BlastPercent = df$BlastPercent,
                         OS = df$OS,
                         OSI = df$OSI)

df_filtered = df_filtered %>%
  mutate(AgeGroup = case_when(
    AgeGroup == "AYA" ~ "Adolescent and Young Adult", 
    is.na(AgeGroup) ~ NA_character_,
    TRUE ~ AgeGroup)) %>%
  mutate(FAB = case_when(
    is.na(FAB) ~ NA_character_,
    TRUE ~ FAB)) %>%
  mutate(OS = OS/365) %>%  # convert days to years
  mutate(OSI = case_when(
    OSI == 1 ~ "Dead",
    OSI == 0 ~ "Alive",
    is.na(OSI) ~ NA_character_,
  ))

tbl <- df_filtered %>% 
  tbl_summary(type = list(c(AgeGroup, Sex, FAB, OSI) ~ "categorical",
                          c(BlastPercent, OS) ~ "continuous"),
              by = FusionGroup,
              missing = "ifany",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 2, all_categorical() ~ c(0,1)),
              label = c(AgeGroup ~ "Age Group", 
                        BlastPercent ~ "Blast Percent",
                        OS ~ "Overall Survival(Years)",
                        OSI ~ "Vital Status"),
              missing_text = "Unknown"
  ) %>%
  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Fusion Group**") %>%
  modify_caption("**Table 1A. TARGET Pediatric AML Patient Characteristics** (N= {N})") %>%
  modify_header(label = "**Variables**") 

tbl_highlighted <- 
  as_gt(tbl) %>%
  gt::tab_style(style = list(cell_fill(color = "lightgrey")),
                locations = list(cells_column_labels(),cells_column_spanners())) 

print(tbl_highlighted)

tbl_highlighted |> gt::gtsave(filename = "results/Table_1_A_dxsmall.docx")
