library(ggplot2)

df <- data.frame(x = (1:25 * 4)/100)
df$y1 <- plogis(df$x * 3 - 1)
df$y2 <- plogis(df$x * 6 - 1.5)
df

pal <- list(blue = "#4063D8", green = "#389826", purple = "#9558B2")

p <- ggplot(df, aes(x)) +
  geom_ribbon(
    aes(ymin = y1, ymax = y2),
    fill = alpha("black", .5),
    data = ~ .x[10:18,]
  ) +
  geom_line(
    aes(y = y1),
    linewidth = 2,
    color = pal$green
  ) +
  geom_line(
    aes(y = y2),
    linewidth = 2,
    color = pal$purple
  ) +
  geom_point(
    aes(y = y1),
    fill = pal$green,
    size = 3,
    shape = 21
  ) +
  geom_point(
    aes(y = y2),
    fill = pal$purple,
    size = 3,
    shape = 21
  ) +
  geom_segment(
    aes(x = x[10], xend = x[18], y = -Inf, yend = -Inf),
    linewidth = 10,
    color = pal$blue
  ) +
  scale_x_continuous(n.breaks = nrow(df)) +
  labs(x = "jlmerclusterperm", y = NULL) +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_text(
      family = "JuliaMono",
      face = "bold",
      size = 32,
      vjust = -3
    ),
    plot.background = element_rect(color = NA, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(0, 1, 1, 1, "cm")
  )

ggsave("man/figures/jlmerclusterperm_logo_plot.png", p, width = 4.6, height = 3)
