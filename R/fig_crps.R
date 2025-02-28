library(data.table)
library(ggplot2)
library(ggh4x)
library(patchwork)

.args <- if (interactive()) c(
    file.path("local", "output", "score_GP.rds"), # scores
    file.path("local", "figures", "score_scatter_GP.png") # diagnostics
) else commandArgs(trailingOnly = TRUE)

# Scores
scores <- readRDS(.args[1])
## TODO currently aggregating scores via mean - probably just keep the actual dates?
scores[data == "daily", date := date + 6 ]
scores[forecast != "rescale", slide := slide / 14L]
scores[forecast == "rescale", slide := slide / 2L]

# scores plot - maybe combine w/ distro?
# score_plt <- ggplot(data = scores) +
# 	geom_line(
# 	    aes(x = date,
# 	        y = crps,
# 	        color = forecast
# 	    )
# 	) +
#     facet_nested("Target" + data ~ .) +
#     scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
#     scale_y_log10() +
#     scale_color_brewer(na.translate = FALSE, palette = "Dark2") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#     labs(y = "CRPS (log10)",
#          linetype = "Data",
#          color = "Forecast scale"
#     )

scores_ref <- scores[forecast == "daily"][, .SD, .SDcols = -c("forecast")]

scores_rel <- scores[forecast != "daily"][scores_ref, on = .(slide, date, data), nomatch = 0]

rel_plot <- ggplot(data = scores_rel) +
    aes(x = interaction(forecast, data), y = log10(crps/i.crps)) +
    theme_minimal() +
    geom_point(position = position_jitter(width = 0.1)) +
    scale_x_discrete("Training Data => Target") +
    scale_y_continuous("Log 10 (CRPS / ref CRPS)\n ref: Daily Training => Matched Outcome")
    

ggsave(tail(.args, 1), rel_plot, bg = "white", width = 12, height = 6)
