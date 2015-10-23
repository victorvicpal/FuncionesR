VisualShots <- function(shoots)
{
	geom_point(data = shoots, aes(x = x, y = y, col = result), alpha = .8) +
+     scale_color_manual(values = c("made" = "#00FF00", "missed" = "#FF0000"))
}