
library(data.table)
library(ggplot2)

.args <- if (interactive()) c(
  file.path("local", "data", "intermediate.rds"),
  file.path("local", "figures", "incidence.png")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])

p <- ggplot(dt) + aes(date, confirm, color = province) +
  geom_line() +
  scale_y_continuous("Daily Incidence", transform = "log2") +
  scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
  theme_minimal()

ggsave(tail(.args, 1), p, bg = "white")
