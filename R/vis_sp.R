
#' Visualize rasterbrick
#'
#' @param rb   raster brick object
#' @param bands numbers of the bands
#' @param cols pallettes
#' @param ncol number of columns
#' @param outfn file name of the output
#' @param save  indicate export of pdf
#' @param width (inchs) of single band image files
#' @param height (inchs) of single band image files
#'
#' @return
#' @export
#' @examples
#' fn <- "I:/projects/fire/victoria/input/GLDAS_bilinear/GLDAS_V21_8d_2000-01-09.tif"
#' rb <- brick(fn)
#' names(rb)
#' cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
#' vis_rasterbrick(rb)
#' vis_rasterbrick(rb, bands = c(1,3,5,7), outfn = "vis_bands2.pdf", cols = cols, save = TRUE)
#'
vis_rasterbrick <- function(rb = NULL, bands = NULL, cols  = NULL, ncol = 2,
                            outfn = "vis_bands.pdf", save = FALSE, width = 4, height = 4){

  if(is.null(rb))
    stop("must input a rasterbrick object")
  if(is.null(bands))
    bands <- 1:length(names(rb))
  if(is.null(cols))
    cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
  if(length(names(rb)) < max(bands))
    stop(paste("seqs settings exceeds the number of bands in the rasterbrick:", length(names(rb))))

  levelplot2 <- function(i, rb, cols){
    r <- raseter::raster(rb, layer = i)
    p <- rasterVis::levelplot(r,
                   margin = FALSE,
                   xlab = "",
                   ylab = "",
                   main = names(rb)[i],
                   between = list(x = 0, y = 0),
                   #maxpixels = maxpixels,
                   col.regions = cols)
  }

  gs <- lapply(bands, rb, cols, FUN = levelplot2)

  width <- ncol*width
  height <- ceiling(length(bands)/ncol)*height

  if(save == TRUE)
    pdf(outfn, width = width, height = height)
  gridExtra::grid.arrange(grobs = gs, ncol = ncol)
  if(save == TRUE)
    dev.off()
}
