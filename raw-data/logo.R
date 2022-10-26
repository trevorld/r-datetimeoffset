library("bittermelon")
library("grid")
library("gridpattern")
library("piecepackr")

font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
l_date <- as_bm_list("date", font = font)
l_date <- bm_glow(l_date, corner = TRUE, value = 1L)
date <- do.call(cbind, l_date)
l_time <- as_bm_list("time", font = font)
l_time <- bm_glow(l_time, corner = TRUE, value = 1L)
time <- do.call(cbind, l_time)
l_offset <- as_bm_list("offset", font = font)
l_offset <- bm_glow(l_offset, corner = TRUE, value = 1L)
offset <- do.call(cbind, l_offset)

draw_logo <- function() {
    hex <- pp_shape("convex6")
    grid.newpage()
    grid.draw(hex$shape(gp = gpar(col = NA, fill = "black")))

    grid.text("date", x = 0.63, y = 0.7,
              gp = gpar(fontsize = 36, col = "white"))
    grid.text("time", x = 0.4, y = 0.55,
              gp = gpar(fontsize = 36, col = "white"))
    grid.text("offset", x = 0.55, y = 0.38,
              gp = gpar(fontsize = 36, col = "white"))

    grid.draw(hex$mat(mat_width = 0.03, gp = gpar(col = NA, fill = "grey")))
}

w <- 3.0
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in",
    res = 72, bg = "transparent")
draw_logo()
dev.off()
