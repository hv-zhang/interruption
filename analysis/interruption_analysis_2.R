library("tidyverse")
library("cowplot")
library("patchwork")
library("jpeg")
#library("ImageMagick")
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

# IIC color = #0096FF
# CII color = #FC7676
# Overall color = #9866C7
## dirty outlier check:
# cg_interruption_long %>%
#   filter(month == "Five") %>%
#   ggplot(aes(y=duration)) +
#     geom_boxplot()
cg_interruption_long = read.csv("../data/cg_interruption_long.csv")
inf_interruption_long = read.csv("../data/inf_interruption_long.csv")
overall_interruption_long = read.csv("../data/overall_interruption_long.csv")
inf_interruption = read.csv("../data/inf_interruption.csv")
cg_interruption = read.csv("../data/cg_interruption.csv")
overall_interruption = read.csv("../data/overall_interruption.csv")


cg_interruption_long$month [cg_interruption_long$month == 5] <- "Five"
cg_interruption_long$month [cg_interruption_long$month == 10] <- "Ten"
inf_interruption_long$month [inf_interruption_long$month == 5] <- "Five"
inf_interruption_long$month [inf_interruption_long$month == 10] <- "Ten"
overall_interruption_long$month [overall_interruption_long$month == 5] <- "Five"
overall_interruption_long$month [overall_interruption_long$month == 10] <- "Ten"

# Figure 1
# CII proportion 
label.p1 <- data.frame(month=1.5, proportion = .35)
p1 <- ggplot(cg_interruption_long, aes(month, proportion)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="#FC7676",size = 2) +
  labs(y = str_wrap("Proportion of infant vocalizations interrupted by caregiver",36),
       x = "") +
  ylim(0, .6) +
  geom_text(data = label.p1, label = "*",size=8) +
  theme_classic(12)
p1

# t test
model1 <- t.test(proportion ~ month, data = cg_interruption_long, paired = TRUE)
model1

# IIC proportion
label.p2 <- data.frame(month=1.5, proportion = .6)

p2 <- ggplot(inf_interruption_long, aes(month, proportion)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="#0096FF",size = 2) +
  labs(y = str_wrap("Proportion of infant vocalizations that interrupted caregiver",36),
       x = "") +
  ylim(0, .6) +
  geom_text(data = label.p2, label = "***",size=8) +
  theme_classic(12) 
p2

# t test
model2 <- t.test(proportion ~ month, data = inf_interruption_long, paired = TRUE)
model2

# Overall proportion
label.p3 <- data.frame(month=1.5, proportion = .6)

p3 <- ggplot(overall_interruption_long, aes(month, proportion)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 2) +
  labs(y = str_wrap("Proportion of infant vocalizations that interrupted caregiver",36),
       x = "Infant age") +
  ylim(0, .6) +
  geom_text(data = label.p2, label = "***",size=8) +
  theme_classic(12) 
p3

# t test
model3 <- t.test(proportion ~ month, data = overall_interruption_long, paired = TRUE)
model3



CII_image <- readJPEG("/Users/vivianzhang/Documents/GitHub/interruption/figures/CII.jpeg", native = TRUE)
CII <- inset_element(p = CII_image,
               left = 0.05,
               bottom = 0.65,
               right = 0.5,
               top = 0.95)

IIC_image <- readJPEG("/Users/vivianzhang/Documents/GitHub/interruption/figures/IIC.jpeg", native = TRUE)
IIC <- inset_element(p = IIC_image,
                     left = 0.05,
                     bottom = 0.65,
                     right = 0.5,
                     top = 0.95)

CII_fig <- ggplot() + annotation_custom(grid::rasterGrob(CII_image, 
                                        width=unit(1,"npc"),
                                        height=unit(1,"npc")),
                             -Inf, Inf, -Inf, Inf)
IIC_fig <- ggplot() + annotation_custom(grid::rasterGrob(IIC_image, 
                                                         width=unit(1,"npc"),
                                                         height=unit(1,"npc")),
                                        -Inf, Inf, -Inf, Inf)
plot_grid(CII_fig, IIC_fig, p1, p2, ncol=2, rel_heights=c(1,2.7))

          
######################################################################
######################################################################
######################################################################

# Figure 2

# ---- CII
p4 <- ggplot(cg_interruption_long, aes(month, dur_total)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#FC7676",size = 1.5) +
  coord_flip(ylim = c(0,50)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  labs(title = "CII Total Duration", y = "", x = "") +
  theme_classic(12)+
  theme(plot.title = element_text(size = 12, face="bold"))
p4

# t test
model4 <- t.test(dur_total ~ month, data = cg_interruption_long, paired = TRUE)
model4


cg_interruption_long_2 <- cg_interruption_long[
  which(cg_interruption_long$sub != 6 & 
        cg_interruption_long$sub != 28 &
          cg_interruption_long$sub != 43 &
          cg_interruption_long$sub != 47 &
          cg_interruption_long$sub != 48 &
          cg_interruption_long$sub != 72 &
          cg_interruption_long$sub != 85 &
          cg_interruption_long$sub != 87 &
          cg_interruption_long$sub != 97 &
          cg_interruption_long$sub != 118 ), ]
p5 <- ggplot(cg_interruption_long_2, aes(month, dur_mean)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#FC7676",size = 1.5) +
  coord_flip(ylim = c(0,2)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  labs(title = "CII Mean Duration", y = "", x = "") +
  theme_classic(12)+
  theme(plot.title = element_text(size = 12, face="bold"))
p5

# t test
model5 <- t.test(dur_mean ~ month, data = cg_interruption_long, paired = TRUE)
model5


# ---- IIC
p6 <- ggplot(inf_interruption_long, aes(month, dur_total)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#0096FF",size = 1.5) +
  coord_flip(ylim = c(0,50)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  labs(title = "IIC Total Duration", y = "", x = "") +
  theme_classic(12)+
  theme(plot.title = element_text(size = 12, face="bold"))
p6

#t test
model6 <- t.test(dur_total ~ month, data = inf_interruption_long, paired = TRUE)
model6


p7 <- ggplot(inf_interruption_long, aes(month, dur_mean)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#0096FF",size = 1.5) +
  coord_flip(ylim = c(0,2)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  labs(title = "IIC Mean Duration", y = "", x = "") +
  theme_classic(12)+
  theme(plot.title = element_text(size = 12, face="bold"))
p7

#t test
model7 <- t.test(dur_mean ~ month, data = inf_interruption_long, paired = TRUE)
model7


#-------------Overall

p8 <- ggplot(overall_interruption_long, aes(month, dur_total)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#9866C7",size = 1.5) +
  labs(title = "Overall Total Duration", y = "", x = "") +
  coord_flip(ylim = c(0,50)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  theme_classic(12) +
  theme(plot.title = element_text(size = 12, face="bold"))
p8
#t test
model8 <- t.test(dur_total ~ month, data = overall_interruption_long, paired = TRUE)
model8

p9 <- ggplot(overall_interruption_long, aes(month, dur_mean)) +
  geom_line(aes(group = sub), color="black", size = 0.7, alpha = .35) +
  geom_point(aes(month), color="#9866C7",size = 1.5) +
  labs(title = "Overall Mean Duration", y = "", x = "") +
  coord_flip(ylim = c(0,2)) +
  scale_x_discrete(limits = c("Ten", "Five"))+
  theme_classic(12) +
  theme(plot.title = element_text(size = 12, face="bold"))
p9

#t test
model9 <- t.test(dur_mean ~ month, data = overall_interruption_long, paired = TRUE)
model9

figure2 <- plot_grid(p4, p5,p6, p7,p8, p9, ncol=2)
y.grob <- textGrob("Age (months)", 
                    gp=gpar(fontface="bold", fontsize=12), rot=90)
x.grob <- textGrob("Interruption Duration (seconds)", 
                    gp=gpar(fontface="bold", fontsize=12))
grid.arrange(arrangeGrob(figure2, left = y.grob, bottom = x.grob))


########################################################################
########################################################################
########################################################################

# Figure 3
x = overall_interruption_long[which(overall_interruption_long$month == "Five"), ]
correlation = merge(x, inf_interruption, by = "sub")
correlation = correlation[, c("sub", "proportion", "dur_total", "dur_mean",
                              "proportion_difference_score",
                              "dur_total_difference_score",
                              "dur_mean_difference_score")]

p10 <- ggplot(correlation, aes(x = proportion, y = proportion_difference_score)) +
  geom_point()+
  labs(title = "Proportion",
       x = "",
       y = "") +
  ylim(-0.5, 0.5)+
  stat_smooth(method = "lm",col = "#C42126",se = FALSE,size = 1)+
  geom_text(x= 0.5, y= 0.4, label="r = -0.77")+
  geom_text(x= 0.5, y= 0.35, label="p < 0.001")+
  theme(plot.title = element_text(size = 12, face="bold"))
p10
cor.test(correlation$proportion, correlation$proportion_difference_score, method = "pearson")

p11 <- ggplot(correlation, aes(x = dur_total, y = dur_total_difference_score)) +
  geom_point()+
  labs(title = "Total duration",
       x = "",
       y = "") +
  ylim(-50,50)+
  stat_smooth(method = "lm",col = "#C42126",se = FALSE,size = 1)+
  geom_text(x= 33, y= 40, label="r = -0.86")+
  geom_text(x= 33, y= 35, label="p < 0.001")+
  theme(plot.title = element_text(size = 12, face="bold"))
p11
cor.test(correlation$dur_total, correlation$dur_total_difference_score, method = "pearson")

p12 <- ggplot(correlation, aes(x = dur_mean, y = dur_mean_difference_score)) +
  geom_point()+
  labs(title = "Mean duration",
       x = "",
       y = "") +
  ylim(-2,2)+
  stat_smooth(method = "lm",col = "#C42126",se = FALSE,size = 1)+
  geom_text(x= 1.05, y= 1.6, label="r = -0.83")+
  geom_text(x= 1.05, y= 1.4, label="p < 0.001")+
  theme(plot.title = element_text(size = 12, face="bold"))
p12
cor.test(correlation$dur_mean, correlation$dur_mean_difference_score, method = "pearson")


figure3 <- plot_grid(p10, p11, p12, ncol=3)
y.grob <- textGrob("IIC difference score between 10 and 5 months", 
                   gp=gpar(fontface="bold", fontsize=12), rot=90)
x.grob <- textGrob("Overall interruption experienced at 5 months", 
                   gp=gpar(fontface="bold", fontsize=12))
grid.arrange(arrangeGrob(figure3, left = y.grob, bottom = x.grob))

########################################################################
########################################################################