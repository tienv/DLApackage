#' Plot 2d label matrix
#'
#' Display 2d label matrix, different labels will have different colors.
#'
#' @param x label matrix to be displayed.
#' @param cellcolors color matrix corresponding to x.
#' @param border color for cell borders. If NA, not showing cell borders.
#' @param xlab Label of x axis.
#' @param ylab label of y axis.
#' @param ... arguments passed to plot().
#'
#' @return Plot of x. No return value.
#'
#'
color2D <- function(x, cellcolors, border, xlab ="", ylab ="", ...)
{
  xdim <- dim(x)
  x <- as.vector(x)
  oldpar <- par("xaxs", "yaxs", "xpd", "mar")
  par(xaxs = "i", yaxs = "i")
  plot(c(0, xdim[2]), c(0, xdim[1]), xlab = xlab, ylab = ylab, 
       type = "n", axes = FALSE, ...)
  oldpar$usr <- par("usr")
  box()
  pos <- 0
  xat <- pretty(0:xdim[2])[-1]
  axis(1, at = xat - 0.5, labels = xat, pos = pos)
  yat <- pretty(0:xdim[1])[-1]
  axis(2, at = xdim[1] - yat + 0.5, labels = yat)
  cellsize <- matrix(1, nrow = xdim[1], ncol = xdim[2])
  y0 <- rep(seq(xdim[1] - 1, 0, by = -1), xdim[2]) 
  y1 <- rep(seq(xdim[1], 1, by = -1), xdim[2])
  rect(sort(rep((1:xdim[2]) - 1, xdim[1])), 
       y0, sort(rep(1:xdim[2], xdim[1])), y1, 
       col = cellcolors, border = border, xlab=xlab,ylab=ylab)
}

#' Plot 3d label array
#'
#' Display 3d label array slice by slice, different labels will have different colors.
#'
#' @param label.array Label array to be displayed.
#' @param sl.dim The dimension that will be used for slicing. The default is 2nd dimension.
#' @param foldername Name of the folder that will contain the result images.
#' @param filename_pre The prefix for the filename of result images. A result image will 
#' have name \emph{filename_pre.n.jpg} where \emph{n} is the slice index.
#' @param bg.index The logical matrix of the same size as \emph{label.array} specifying the 
#' background. Background cells have value TRUE and non-background cells have
#' value FALSE. All the background cells will be display with one common color specified
#' by \emph{bg.col}.
#' @param bg.col Color for the background. The default is black.
#' @param border.col Color for cell borders. If NA, not showing cell border.
#'
#' @return A new folder is created in working directory, containing slice-by-slice images of 
#' the 3D label array.
#'
#' @export
#'
#' @examples
#' data(LabelArrayExample)
#' bg.index <- array(LabelArrayExample==0,dim(LabelArrayExample))
#' DisplayLabArr(label.array=LabelArrayExample, bg.index = bg.index, 
#' foldername="Images", filename_pre="Image")
DisplayLabArr <- function(label.array, sl.dim = 2, foldername="Images", filename_pre="Image",
                         bg.index = NA, bg.col = "black", border.col=NA)
{
  block = unique(c(label.array))
  col.vector = sample(rainbow(length(block)))
  color = array("black",dim(label.array))
  for (j in 1:length(block))
  {
    ind.arr = array(label.array==block[j],dim(label.array))
    color[ind.arr] = col.vector[j]
    color[bg.index] = bg.col
  }
  dir.create(foldername)
  for (sl in 1:dim(label.array)[sl.dim])
  {
    filename = paste(filename_pre,".",sl,".jpg",sep="")
    jpeg(paste(foldername,filename,sep="/"))
    if (sl.dim == 1)
      color2D(x=label.array[sl,,],cellcolors=color[sl,,],border=border.col,
                      xlab=paste("Number of clusters in slice and in total: ",length(unique(c(color[sl,,]))),
                                 ";  ", length(block)), ylab="")
    if (sl.dim == 2)    
      color2D(x=label.array[,sl,],cellcolors=color[,sl,],border=border.col,
                    xlab=paste("Number of clusters in slice and in total: ",length(unique(c(color[,sl,]))),
                               ";  ", length(block)),ylab = "")
    if (sl.dim == 3)    
      color2D(x=label.array[,,sl],cellcolors=color[,,sl],border=border.col,
                      xlab=paste("Number of clusters in slice and in total: ",length(unique(c(color[,,sl]))),
                                 ";  ", length(block)),ylab = "")
    dev.off()
  }
}


