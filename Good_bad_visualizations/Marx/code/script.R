library("dplyr")
library("ggplot2")
library("tidyr")
library("GGally")

data <- read.csv("data/Alachua county trees.csv", stringsAsFactors = FALSE)


#
# Bad Plot ----
#

data <- data %>% 
  select(PLOT, SPCD, DIA) %>% 
  filter(!is.na(DIA)) %>% 
  group_by(PLOT, SPCD) %>% 
  summarise(AVGDIA = mean(DIA))

plots <- data %>%
  group_by(PLOT) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

data <- data[data$PLOT %in% plots$PLOT, ]

data_wide <- data %>% pivot_wider(names_from = SPCD, values_from = AVGDIA)

dir.create("output")

jpeg("output/bad.jpg", width = 2048, height = 2048, pointsize = 24, quality = 50)
par(bg = 'blue')
pairs(data_wide[2:ncol(data_wide)],
      pch = 21, col = "red", bg = "green", cex = 3, col.axis = "red",
      main = "Pairwise Average Diameters Across Plots", col.main = "red")
dev.off()


#
# Good Plot ----
#

species <- data %>%
  group_by(SPCD) %>% 
  summarise(count = n()) %>% 
  filter(count >= 10)

data2 <- data[data$SPCD %in% species$SPCD,]

plots <- data2 %>%
  group_by(PLOT) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

data2 <- data2[data2$PLOT %in% plots$PLOT, ]

spc <- c(`111` = "slash pine",
         `131` = "loblolly pine",
         `316` = "red maple",
         `611` = "sweetgum",
         `762` = "black cherry",
         `820` = "laurel oak",
         `827` = "water oak",
         `838` = "live oak")

data2$SPCD <- spc[as.character(data2$SPCD)]

data_wide <- data2 %>% pivot_wider(names_from = SPCD, values_from = AVGDIA)

lower_ggplot <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = "black") +
    geom_smooth(method = method, color = "blue", se = FALSE, ...)
  return(p)
}

good_plot <- ggpairs(data_wide[-1], lower = list(continuous = wrap(lower_ggplot, method = "lm"))) +
  labs(title = "Pairwise Average Diameters Across Plots") +
  theme_bw()

ggsave("output/good.png", good_plot, width = 10, height = 12, units = "in")
