rm(list=ls())
library(tidyverse)

rstudioapi::showDialog(title = "Select Output Folder",
                       message = "Select Folder where you would like Plots PDF to be saved.")
setwd(rstudioapi::selectDirectory())

rstudioapi::showDialog(title = "Select Output Folder",
                       message = "Select CSV data file")
df <- read_csv(rstudioapi::selectFile(caption = "Select CSV data file",
                                      filter = "CSV Files (*.csv)",
                                      existing = TRUE)) %>% 
  mutate_if(is.character, str_trim)

#change all empty values for control groups in BAL column to 0 
df$BAL[which(is.na(df$BAL))] <- 0

# replace all negative values with NA
df <- df %>% replace((df < 0), NA)

# find indices of all negative values in df
which(df < 0, arr.ind = TRUE)

#df %>% names()

# This is just to make sure the other filter method is working properly

#cutoffs <- df %>% group_by(`Sample Type`) %>% 
#  summarise(cutoff_leu_mpfc = mean(`Leu mPFC`) + 2*sd(`Leu mPFC`),
#            cutoff_met_mpfc = mean(`Met mPFC`) + 2*sd(`Met mPFC`),
#            cutoff_dyn_mpfc = mean(`Dyn mPFC`) + 2*sd(`Dyn mPFC`),
#            cutoff_leu_striatum = mean(`Leu Striatum`, na.rm = TRUE) + 2*sd(`Leu Striatum`, na.rm = TRUE),
#            cutoff_met_striatum = mean(`Met Striatum`, na.rm = TRUE) + 2*sd(`Met Striatum`, na.rm = TRUE),
#            cutoff_dyn_striatum = mean(`Dyn Striatum`, na.rm = TRUE) + 2*sd(`Dyn Striatum`, na.rm = TRUE),
#            cutoff_ttc = mean(`TTC shift`) + 2*sd(`TTC shift`),
#            cutoff_etc = mean(`ETC shift`) + 2*sd(`ETC shift`),
#            cutoff_dmts = mean(`DMTS 24s`) + 2*sd(`DMTS 24s`),
#            cutoff_BAL = mean(BAL) + 2*sd(BAL))

# filtering out outliers over 2 SD from mean
leu_mPFC_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Leu mPFC` < (mean(`Leu mPFC`) + 2*sd(`Leu mPFC`)) & 
           `Leu mPFC` > (mean(`Leu mPFC`) - 2*sd(`Leu mPFC`)))

met_mPFC_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Met mPFC` < (mean(`Met mPFC`) + 2*sd(`Met mPFC`)) & 
           `Met mPFC` > (mean(`Met mPFC`) - 2*sd(`Met mPFC`)))

dyn_mPFC_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Dyn mPFC` < (mean(`Dyn mPFC`) + 2*sd(`Dyn mPFC`)) & 
           `Dyn mPFC` > (mean(`Dyn mPFC`) - 2*sd(`Dyn mPFC`)))

theme_update(axis.text.x = element_text(face = "bold", size = 5),
             axis.text.y = element_text(face = "bold", size = 5),
             axis.title = element_text(size = 10, face = "bold"))

p1 <- ggplot(data = leu_mPFC_data, 
             aes(x = `Leu mPFC`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p2 <- ggplot(data = met_mPFC_data, 
             aes(x = `Met mPFC`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p3 <- ggplot(data = dyn_mPFC_data, 
             aes(x = `Dyn mPFC`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

# filtering out outliers by 2 SD from mean

leu_striatum_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Leu Striatum` < (mean(`Leu Striatum`, na.rm = TRUE) + 2*sd(`Leu Striatum`, na.rm = TRUE)) & 
           `Leu Striatum` > (mean(`Leu Striatum`, na.rm = TRUE) - 2*sd(`Leu Striatum`, na.rm = TRUE)))

met_striatum_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Met Striatum` < (mean(`Met Striatum`, na.rm = TRUE) + 2*sd(`Met Striatum`, na.rm = TRUE)) & 
           `Met Striatum` > (mean(`Met Striatum`, na.rm = TRUE) - 2*sd(`Met Striatum`, na.rm = TRUE)))

# dynorphin striatum is only one with negative value so need to make these above 0 
dyn_striatum_data <- df %>% 
  group_by(`Sample Type`) %>% 
  filter(`Dyn Striatum` < (mean(`Dyn Striatum`, na.rm = TRUE) + 2*sd(`Dyn Striatum`, na.rm = TRUE)) & 
           `Dyn Striatum` > (mean(`Dyn Striatum`, na.rm = TRUE) - 2*sd(`Dyn Striatum`, na.rm = TRUE)))

p4 <- ggplot(data = leu_striatum_data, 
             aes(x = `Leu Striatum`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p5 <- ggplot(data = met_striatum_data, 
             aes(x = `Met Striatum`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p6 <- ggplot(data = dyn_striatum_data, 
             aes(x = `Dyn Striatum`, 
                 y = `TTC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p7 <- ggplot(data = leu_mPFC_data, 
             aes(x = `Leu mPFC`, 
                 y = `ETC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p8 <- ggplot(data = met_mPFC_data, 
             aes(x = `Met mPFC`, 
                 y = `ETC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p9 <- ggplot(data = dyn_mPFC_data, 
             aes(x = `Dyn mPFC`, 
                 y = `ETC shift`, 
                 color = `Sample Type`,
                 fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p10 <- ggplot(data = leu_striatum_data, 
              aes(x = `Leu Striatum`, 
                  y = `ETC shift`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p11 <- ggplot(data = met_striatum_data, 
              aes(x = `Met Striatum`, 
                  y = `ETC shift`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p12 <- ggplot(data = dyn_striatum_data, 
              aes(x = `Dyn Striatum`, 
                  y = `ETC shift`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p13 <- ggplot(data = leu_mPFC_data, 
              aes(x = `Leu mPFC`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p14 <- ggplot(data = met_mPFC_data, 
              aes(x = `Met mPFC`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p15 <- ggplot(data = dyn_mPFC_data, 
              aes(x = `Dyn mPFC`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p16 <- ggplot(data = leu_striatum_data, 
              aes(x = `Leu Striatum`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p17 <- ggplot(data = met_striatum_data, 
              aes(x = `Met Striatum`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p18 <- ggplot(data = dyn_striatum_data, 
              aes(x = `Dyn Striatum`, 
                  y = `DMTS 24s`, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p19 <- ggplot(data = leu_mPFC_data %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Leu mPFC`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p20 <- ggplot(data = met_mPFC_data %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Met mPFC`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p21 <- ggplot(data = dyn_mPFC_data %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Dyn mPFC`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p22 <- ggplot(data = leu_striatum_data %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Leu Striatum`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p23 <- ggplot(data = met_striatum_data %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Met Striatum`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p24 <- ggplot(data = dyn_striatum_data  %>% 
                filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female"), 
              aes(x = `Dyn Striatum`, 
                  y = BAL, 
                  color = `Sample Type`,
                  fill = `Sample Type`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), 
                                 sep = "*\", \"*"))) +
  facet_wrap(~`Sample Type`) + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p25 <- df %>% 
  gather(Striatum, Amount, `Leu Striatum`:`Dyn Striatum`) %>% 
  group_by(Striatum, `Sample Type`) %>% 
  filter(Amount < (mean(Amount, na.rm = TRUE) + 2*sd(Amount, na.rm = TRUE)) & 
           Amount > (mean(Amount, na.rm = TRUE) - 2*sd(Amount, na.rm = TRUE))) %>% 
  ggplot(aes(x = Striatum, y = Amount, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))


p26 <- df %>% 
  gather(mPFC, Amount, `Leu mPFC`:`Dyn mPFC`) %>% 
  group_by(mPFC, `Sample Type`) %>% 
  filter(Amount < (mean(Amount, na.rm = TRUE) + 2*sd(Amount, na.rm = TRUE)) & 
           Amount > (mean(Amount, na.rm = TRUE) - 2*sd(Amount, na.rm = TRUE))) %>% 
  ggplot(aes(x = mPFC, y = Amount, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p27 <- df %>% 
  group_by(`Sample Type`) %>% 
  ggplot(aes(x = `Sample Type`, y = `TTC shift`, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p28 <- df %>% 
  group_by(`Sample Type`) %>% 
  ggplot(aes(x = `Sample Type`, y = `ETC shift`, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p29 <- df %>% 
  group_by(`Sample Type`) %>% 
  ggplot(aes(x = `Sample Type`, y = `DMTS 24s`, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

p30 <- df %>% 
  filter(`Sample Type` == "Alcohol Male" | `Sample Type` == "Alcohol Female") %>% 
  ggplot(aes(x = `Sample Type`, y = BAL, fill = `Sample Type`)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

#df1 <- df %>% 
#  group_by(`Sample Type`) %>% 
#  filter(`TTC shift`< (mean(`TTC shift`) + 2*sd(`TTC shift`)) & 
#         `TTC shift` > (mean(`TTC shift`) - 2*sd(`TTC shift`)) &
#         `DMTS 24s` < (mean(`DMTS 24s`) + 2*sd(`DMTS 24s`)) &
#         `DMTS 24s` > (mean(`DMTS 24s`) - 2*sd(`DMTS 24s`)))

# these two plots are just to see if TTC performance correlates with DMTS
# the axes are flipped for p25 and p26

#p25 <- ggplot(data = df1,
#              aes(x = `TTC shift`,
#                  y = `DMTS 24s`, 
#                  color = `Sample Type`,
#                  fill = `Sample Type`)) +
#  geom_point() + 
#  geom_smooth(method = "lm", se = FALSE) + 
#  stat_poly_line() +
#  stat_poly_eq(aes(label = paste(after_stat(eq.label),
#                                 after_stat(rr.label), 
#                                 sep = "*\", \"*"))) +
#  facet_wrap(~`Sample Type`)

#p26 <- ggplot(data = df1, 
#              aes(x = `DMTS 24s`, 
#                  y = `TTC shift`,
#                  color = `Sample Type`,
#                  fill = `Sample Type`)) +
#  geom_point() + 
#  geom_smooth(method = "lm", se = FALSE) + 
#  stat_poly_line() +
#  stat_poly_eq(aes(label = paste(after_stat(eq.label),
#                                 after_stat(rr.label), 
#                                 sep = "*\", \"*"))) +
#  facet_wrap(~`Sample Type`)

# write plots to PDF
pdf("Plots no outliers.pdf", width = 10, height = 6.5)
p1;
p2;
p3;
p4;
p5;
p6;
p7;
p8;
p9;
p10;
p11;
p12;
p13;
p14;
p15;
p16;
p17;
p18;
p19;
p20;
p21;
p22;
p23;
p24;
p25;
p26;
p27;
p28;
p29;
p30;
dev.off()

rm(p1,
   p2,
   p3,
   p4,
   p5,
   p6,
   p7,
   p8,
   p9,
   p10,
   p11,
   p12,
   p13,
   p14,
   p15,
   p16,
   p17,
   p18,
   p19,
   p20,
   p21,
   p22,
   p23,
   p24,
   p25,
   p26,
   p27,
   p28,
   p29,
   p30)

rm(leu_mPFC_data,
   met_mPFC_data,
   dyn_mPFC_data,
   leu_striatum_data,
   met_striatum_data,
   dyn_striatum_data)


# This is to run Structural Equation Model, uncomment if needed

#library(lavaan)
# rename columns without spaces so SEM can recognize them

#df2 <- df %>% rename_all(~str_replace_all(., "\\s+", "_"))

# check this worked
# df2 %>% names()

#model <- 'TTC_shift ~ Leu_mPFC + Met_mPFC + Dyn_mPFC + Leu_Striatum + Met_Striatum + Dyn_Striatum + BAL
#          ETC_shift ~ Leu_mPFC + Met_mPFC + Dyn_mPFC + Leu_Striatum + Met_Striatum + Dyn_Striatum + BAL
#          DMTS_24s ~ Leu_mPFC + Met_mPFC + Dyn_mPFC + Leu_Striatum + Met_Striatum + Dyn_Striatum + BAL'

#model_sem <- sem(model, data = df2)
#summary(model_sem, standardize = T, fit.measures = TRUE, , modindices = TRUE, rsq = T)
#inspect(model_sem)

#lavaan_parameters <- parameterestimates(model_sem, 
#                                        se = FALSE, 
#                                        zstat = FALSE, 
#                                        pvalue = TRUE, 
#                                        ci = FALSE, 
#                                        standardized = TRUE,
#                                        rsquare = TRUE) %>% 
#                     select(lhs, op, rhs, std.all)


#badrats <- c(setdiff(df$Rat, leu_mPFC_data$Rat), 
#             setdiff(df$Rat, met_mPFC_data$Rat),
#             setdiff(df$Rat, dyn_mPFC_data$Rat),
#             setdiff(df$Rat, leu_striatum_data$Rat),
#             setdiff(df$Rat, met_striatum_data$Rat),
#             setdiff(df$Rat, dyn_striatum_data$Rat)) %>% 
#            unique() %>% 
#            sort()

#setdiff(df$Rat, leu_mPFC_data$Rat) 
#setdiff(df$Rat, met_mPFC_data$Rat)
#setdiff(df$Rat, dyn_mPFC_data$Rat)
#setdiff(df$Rat, leu_striatum_data$Rat)
#setdiff(df$Rat, met_striatum_data$Rat)
#setdiff(df$Rat, dyn_striatum_data$Rat)


rstudioapi::showDialog(title = "Data Output",
                       message = "The PDF was saved as 'Plots no outliers.pdf' in the folder you previously selected.")