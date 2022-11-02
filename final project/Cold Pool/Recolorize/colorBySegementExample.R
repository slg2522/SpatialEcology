# get original fit
corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
recolored_corbetti <- recolorize::recolorize(corbetti, plotting = TRUE)
# to reset graphical parameters:
current_par <- graphics::par(no.readonly = TRUE)
# make a layout
layout(matrix(c(1, 1:9), nrow = 2))
par(mar = c(0, 0, 2, 0))
# plot original
plotImageArray(recolored_corbetti$original_img)
# plot layers
corbetti_layers <- splitByColor(recolored_corbetti, plot_method = "over")
# plot binary maps
plotImageArray(recolored_corbetti$original_img)
for (i in 1:length(corbetti_layers)) {
  plotImageArray(corbetti_layers[[i]])
}
graphics::par(current_par)

plotImageArray(corbetti_layers[[6]])
