library(data.table)
library(ggplot2)
library(ggh4x)
library(geomtextpath)
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
    aes(x = interaction(forecast, data), y = crps/i.crps) +
    theme_minimal() +
    geom_point(position = position_jitter(width = 0.1)) +
    geom_labelsegment(
        aes(
            x = (as.integer(interaction(forecast, data))-1)-0.25,
            xend = (as.integer(interaction(forecast, data))-1)+0.25,
            y = geomean, yend = geomean, label = sprintf("~%.0fx",geomean)
        ),
        data = \(dt) dt[,.(geomean = exp(mean(log(crps/i.crps)))), by=.(forecast, data)],
        color = "red", hjust = 1.1, fill = alpha("white", 0.9)
    ) +
    geom_texthline(
        mapping = aes(yintercept = yint),
        data = \(dt) dt[1, .(yint = 1)],
        linetype = "dashed", label = "vs. daily data\n=> ... target",
        hjust = 0
    ) +
    geom_text(aes(x = 0.5, y = ratio, label = perf), \(dt) dt[, .(ratio = c(10, 1/10), perf = c("worse", "better"))], vjust = 0.5, hjust = 0) +
    coord_cartesian(ylim = 10^c(-2, 4), clip = "off") +
    scale_x_discrete(
        NULL, label = \(b) lapply(
            strsplit(b,".",fixed = TRUE),
            \(spl) sprintf("%s data => %s target", spl[1], spl[2])
        )
    ) +
    scale_y_continuous(
        "relative CRPS",
        transform = "log10",
        breaks = 10^c(-2:4), minor_breaks = NULL,
        labels = \(b) fifelse(b < 1, sprintf("1/%ix", as.integer(1/b)), sprintf("%ix", as.integer(b)))
    )
    

ggsave(tail(.args, 1), rel_plot, bg = "white", width = 12, height = 6)
