suppressPackageStartupMessages({
  if (!require('pacman')){install.packages("pacman")}; library("pacman")
  p_load(markdown,OpenImageR,ClusterR,SuperpixelImageSegmentation,pixmap,Biobase,NMF)
})
