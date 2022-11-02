img <- readPNG("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/R2200aRBRZBGCcaaa03aTrueColors.png")

# get original fit
recolored_corbetti <- recolorize::recolorize(img, method = "hist", color_space = "Lab", bins = 2, plotting = TRUE) # mostly blue
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

#cold pool
plotImageArray(corbetti_layers[[5]])
#cold pool border
plotImageArray(corbetti_layers[[6]])
