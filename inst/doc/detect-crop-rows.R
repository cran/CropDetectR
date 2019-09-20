## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message=F,
  warning=F
)

## ---- message=F, warning=F-----------------------------------------------
library(CropDetectR)
library(imager)
library(ggplot2)
library(dplyr)
library(reshape2)
library(EBImage)
library(stats)

## ------------------------------------------------------------------------
img <- readImage('https://raw.githubusercontent.com/niconaut/CropDetectR/master/images/before_rows.JPG')
img <- as.cimg(img[1:length(img)], dim = c(dim(img)[1], dim(img)[2], 1, dim(img)[3]))

## ------------------------------------------------------------------------
img_grayscale <- make_ExG(img)

plot(img)
plot(img_grayscale)

## ------------------------------------------------------------------------
black_white <- make_bw(img_grayscale)

plot(black_white)

## ------------------------------------------------------------------------
img_blob <- blobify(black_white, 3) # min input value of 1

plot(img_blob)

## ------------------------------------------------------------------------
oldpar <- par(mfrow = c(1,2))

par(mar = rep(2,4))
rotations_list <- rotations(img_blob, 90) 

plot(rotations_list)

par(oldpar)

## ------------------------------------------------------------------------
jagged <- as.data.frame(rotations_list[1]) %>%
  select(x,y,value) %>%
  group_by(x) %>%
  summarise(row_threshold = mean(value))

jagged_array <- as.array(jagged$row_threshold)

plot(jagged_array, type = 'l')

smooth <- smoothing(rotations_list[1], 0.25)

plot(smooth, type = 'l')

## ------------------------------------------------------------------------
best_img <- best_rotation(rotations_list, 0.5, 0.25)

paste("The best rotation was rotation ", best_img - 1, ".", sep = "")

plot(smoothing(rotations_list[best_img], 0.25), type = 'l')

## ------------------------------------------------------------------------
picture_list <- rotations_list

crop_rows <- crop_row_finder(rotations_list, 0.5, 0.05, 0.25)

plot(smoothing(rotations_list[best_img], 0.25), type = 'l')
abline(v = crop_rows, col = "red")

plot(img)
abline(v = crop_rows, col = "red", lwd = 1.5)

