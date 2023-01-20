library("tidyverse")
library("cowplot")

cg_interruption_long = read.csv("../data/cg_interruption_long.csv")

inf_interruption_long = read.csv("../data/inf_interruption_long.csv")

overall_interruption_long = read.csv("../data/overall_interruption_long.csv")

# ---- proportion of vocalizations interrupted by caregiver

cg_interruption_long$month [cg_interruption_long$month == 5] <- "Five"
cg_interruption_long$month [cg_interruption_long$month == 10] <- "Ten"

p1 <- ggplot(cg_interruption_long, aes(month, proportion)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 5) +
  labs(y = str_wrap("Proportion of infant vocalizations interrupted by caregiver",36),
       x = "") +
  theme_classic(12)
/Users/stevenelmlinger/Library/CloudStorage/Dropbox/Cornell/Fall_2022/projects/Sequence Compression/pre-processing/generate_variables.ipynb
p1

# t test
model1 <- t.test(proportion ~ month, data = cg_interruption_long, paired = TRUE)
model1

# ---- total caregiver interruption time 

p2 <- ggplot(cg_interruption_long, aes(month, duration)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 5) +
  labs(y = "Total duration of caregivers interruptions (s)",
       x = "") +
  theme_classic(12)

p2

# t test
model2 <- t.test(duration ~ month, data = cg_interruption_long, paired = TRUE)
model2

# ---- proportion of infant vocalizations which interrupted caregiver

inf_interruption_long$month [inf_interruption_long$month == 5] <- "Five"
inf_interruption_long$month [inf_interruption_long$month == 10] <- "Ten"

p3 <- ggplot(inf_interruption_long, aes(month, proportion)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 5) +
  labs(y = str_wrap("Proportion of infant vocalizations that interrupted caregiver",36),
       x = "Infant age") +
  theme_classic(12)

p3

# t test
model3 <- t.test(proportion ~ month, data = inf_interruption_long, paired = TRUE)
model3

# ---- total infant interruption time 

p4 <- ggplot(inf_interruption_long, aes(month, duration)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 5) +
  labs(y = "Total duration of infant interruptions of caregiver (s)",
       x = "Infant age") +
  theme_classic(12)

p4

#t test
model4 <- t.test(duration ~ month, data = inf_interruption_long, paired = TRUE)
model4

# ---- overall interruption time

overall_interruption_long$month [overall_interruption_long$month == 5] <- "Five"
overall_interruption_long$month [overall_interruption_long$month == 10] <- "Ten"

p5 <- ggplot(overall_interruption_long, aes(month, duration)) +
  geom_line(aes(group = sub), color="black", size = 1, alpha = .35) +
  geom_point(aes(month), color="navyblue",size = 5) +
  labs(title = "Overall interruption duration",
       y = "Total interruption time overall (s)",
       x = "Infant age") +
  theme_classic(12)

p5

#t test
model4 <- t.test(duration ~ month, data = overall_interruption_long, paired = TRUE)
model4

# ---- joined plot

# shared title (under construction)

# https://wilkelab.org/cowplot/articles/plot_grid.html (Joint plot titles)


top_title <- ggdraw() + 
  draw_label(
    "Caregiver interrupts infant (CII)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

top_row <- plot_grid(p1, p2)

top_row <- plot_grid(top_title, top_row,
                     ncol = 1,
                     rel_heights = c(0.1, 1))

bottom_title <- ggdraw() + 
  draw_label(
    "Infant interrupts caregiver (IIC)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
                     
bottom_row <- plot_grid(p3, p4)

bottom_row <- plot_grid(bottom_title, bottom_row,
                        ncol = 1,
                        rel_heights = c(0.1, 1))

grid1 <- plot_grid(top_row, bottom_row, nrow = 2)

plot_grid(grid1, p5, ncol = 2,
          rel_widths = c(3, 1.5))

# ---- Predicting avoidance of interruptions

# CII prop at 5 months predicts difference in proportion of interrupts over development

# CII dur

# overall dur
