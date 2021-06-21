suppressPackageStartupMessages({
  if (!require('pacman')){install.packages("pacman")}; library("pacman")
  p_load(shiny,markdown,OpenImageR,ClusterR,SuperpixelImageSegmentation,pixmap,Biobase,NMF)
})
