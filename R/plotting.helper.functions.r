#' Put a gam trendline on a plot
#'
#' @param vec vector of data
#' @description pops off the final value on a vector. You do not need to assign the vector to itself again, it is done internally so
#'         somewhat against the R way of doing things and it could get you into trouble.
#' @return pulls/pops off the final value of a vector4
#' @author Daniel Duplisea
#' @export
gam.trend.line.f= function(x, y, ...){
  gf= gam(y~ s(x))
  newdata= data.frame(x=seq(min(x),max(x),length=length(x)*10))
  lines(newdata$x,predict(gf, newdata=newdata), ...)
}


#' Draw polygon confidence intervals
#'
#' @param
#' @keywords helper function
#' @export
#' @examples
#'
confint.f= function(x,ylow,yhigh,...){
	polygon(x = c(x, rev(x)), y = c(ylow,rev(yhigh)), border = NA,...)
}

#' Put error bars on an x y plot given a standard deviation of normal distributed errors
#'
#' @param x independent data series
#' @param y.mean mean or central value of the y series
#' @param y.sd the standard deviation of the y series
#' @param SE if T then y.sd is the standard error
#' @keywords
#' @export
#' @examples
#'
error.bars.sd.f= function(x,y.mean,y.sd,SE=T, ...){
  if (SE){
    error.high= y.mean+1.96*y.sd
    error.low= y.mean-1.96*y.sd
  }
  if (!SE){
    error.high= y.mean+y.sd
    error.low= y.mean-y.sd
  }
  arrows(x, error.low, x, error.high, length=0.02, angle=90, code=3)
}

#' Put error bars on an x y plot given high and low data series
#'
#' @param x independent data series
#' @param error.low the time series of lower values
#' @param error.high the time series of upper values
#' @param SE if T then y.sd is the standard error
#' @keywords
#' @export
#' @examples
#'
error.bars.data.f= function(x,error.low,error.high, ...){
  arrows(x, error.low, x, error.high, length=0.02, angle=90, code=3)
}


#' Create a second y axis on the right side
#'
#' @param
#' @keywords helper function
#' @export
#' @examples
#'
yaxis2.f= function(x,y, ylabel="2ndyaxis",cex=0.75, ...){
  par(new=T)
  plot(x, y, axes=F, xlab=NA, ylab=NA, cex=cex, ...)
  axis(side=4)
  mtext(side=4, line=2.5, ylabel, cex=cex*.9)
}


#' Add text with background box to a plot
#'
#' \code{boxtext} places a text given in the vector \code{labels}
#' onto a plot in the base graphics system and places a coloured box behind
#' it to make it stand out from the background.
#'
#' @param x numeric vector of x-coordinates where the text labels should be
#' written. If the length of \code{x} and \code{y} differs, the shorter one
#' is recycled.
#' @param y numeric vector of y-coordinates where the text labels should be
#' written.
#' @param labels a character vector specifying the text to be written.
#' @param col.text the colour of the text
#' @param col.bg color(s) to fill or shade the rectangle(s) with. The default
#' \code{NA} means do not fill, i.e., draw transparent rectangles.
#' @param border.bg color(s) for rectangle border(s). The default \code{NA}
#' omits borders.
#' @param adj one or two values in [0, 1] which specify the x (and optionally
#' y) adjustment of the labels.
#' @param pos a position specifier for the text. If specified this overrides
#' any adj value given. Values of 1, 2, 3 and 4, respectively indicate
#' positions below, to the left of, above and to the right of the specified
#' coordinates.
#' @param offset when \code{pos} is specified, this value gives the offset of
#' the label from the specified coordinate in fractions of a character width.
#' @param padding factor used for the padding of the box around
#' the text. Padding is specified in fractions of a character width. If a
#' vector of length two is specified then different factors are used for the
#' padding in x- and y-direction.
#' @param cex numeric character expansion factor; multiplied by
#' code{par("cex")} yields the final character size.
#' @param font the font to be used
#'
#' @return Returns the coordinates of the background rectangle(s). If
#' multiple labels are placed in a vactor then the coordinates are returned
#' as a matrix with columns corresponding to xleft, xright, ybottom, ytop.
#' If just one label is placed, the coordinates are returned as a vector.
#' @author Ian Kopacka
#' @examples
#' ## Create noisy background
#' plot(x = runif(1000), y = runif(1000), type = "p", pch = 16,
#' col = "#40404060")
#' boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
#'     pos = 4, font = 2, cex = 1.3, padding = 1)
#' @export
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
  border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
  padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){

  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
      lx <- length(x)
      ly <- length(y)
      if (lx > ly){
          y <- rep(y, ceiling(lx/ly))[1:lx]
      } else {
          x <- rep(x, ceiling(ly/lx))[1:ly]
      }
  }

  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
      if (length(adj == 1)){
          adj <- c(adj[1], 0.5)
      }
  } else {
      adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
      if (pos == 1){
          adj <- c(0.5, 1)
          offsetVec <- c(0, -offset*charWidth)
      } else if (pos == 2){
          adj <- c(1, 0.5)
          offsetVec <- c(-offset*charWidth, 0)
      } else if (pos == 3){
          adj <- c(0.5, 0)
          offsetVec <- c(0, offset*charWidth)
      } else if (pos == 4){
          adj <- c(0, 0.5)
          offsetVec <- c(offset*charWidth, 0)
      } else {
          stop('Invalid argument pos')
      }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
      padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  graphics::rect(xleft = xMid - rectWidth/2,
          ybottom = yMid - rectHeight/2,
          xright = xMid + rectWidth/2,
          ytop = yMid + rectHeight/2,
          col = col.bg, border = border.bg)

  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
          adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
      invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                      yMid + rectHeight/2))
  } else {
      invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                      yMid + rectHeight/2))
  }
}


#' Wrapper for the PBSmapping plotMap function
#'
#' @param longs longitude decimal degrees (- for western hemisphere)
#' @param lats latitude decimal degrees (- for southern hemisphere)
#' @param land.col colour of the land
#' @param sea.col colour of the oceans and water bodies
#' @param bathy do you want to show bathymetric lines. Default = T
#' @param isob the depth of the isobaths you want to show
#' @description Make a map using PBSmapping
#' @export
#' @examples
#'
map.f= function(longs=c(-74,-50),lats=c(43,52.25),land.colour="sienna3",sea.colour="lightblue", bathy=T, isob=c(100,200)){
  data(worldLLhigh)
  worldLLhigh$X=(worldLLhigh$X+180)%%360-180
  xlim=longs
  ylim=lats
  map.data=clipPolys(worldLLhigh,xlim=xlim,ylim=ylim)
  plotMap(map.data,xlim=xlim,ylim=ylim,lty=1,lwd=.05 ,col="tan",
          bg=rgb(224,253,254,maxColorValue=255),las=1,xaxt="n", yaxt="n",
          xlab="",ylab="")
  xint= seq(longs[1],longs[2],length=5)
  yint= seq(lats[1],lats[2],length=6)
  #mtext("Longitude west",side=1,line=3)
  #mtext("Latitude north",side=2,line=3)
  if (bathy){
    isob.col= c("blue","slategrey")
    ocCL = contourLines(ocBathy, levels =isob)
    ocCP = convCP(ocCL, projection = "LL")
    ocPoly = ocCP$PolySet
    addLines(thinPolys(ocPoly, tol=1,filter = 5), col =isob.col)
    legend("bottomleft", bty = "n", col = isob.col, lwd = 1, legend = as.character(isob), inset = 0.05,
       title = "Isobaths (m)",cex=0.7)
  }
  axis(1, at=xint, labels=xint*-1, lty=1,lwd=1,lwd.ticks= 1, cex.axis=.7)
  axis(2, at=yint, labels=yint*1, lty=1,lwd=1,lwd.ticks=1,las=1, cex.axis=.7)
}


#' An inset map for the main map function
#'
#' @param longs longitude decimal degrees (- for western hemisphere)
#' @param lats latitude decimal degrees (- for southern hemisphere)
#' @param land.col colour of the land
#' @param sea.col colour of the oceans and water bodies
#' @description A simple stripped down map without annotation that can be used as an inset to the larger scale map
#' @keywords helper function
#' @export
#' @examples
#'
map.inset.f= function(longs=c(-71,-50),lats=c(43,52.25),land.colour="sienna3",sea.colour="lightblue"){
  data(worldLLhigh)
  worldLLhigh$X=(worldLLhigh$X+180)%%360-180
  xlim=longs
  ylim=lats
  map.data=clipPolys(worldLLhigh,xlim=xlim,ylim=ylim)
  plotMap(map.data,xlim=xlim,ylim=ylim,lty=1,lwd=.05 ,col="tan",
          bg=rgb(224,253,254,maxColorValue=255),las=1,xaxt="n", yaxt="n",
          xlab="",ylab="")
  #xint= seq(longs[1],longs[2],length=5)
  #yint= seq(lats[1],lats[2],length=6)
  #mtext("Longitude west",side=1,line=3)
  #mtext("Latitude north",side=2,line=3)
  #axis(1, at=xint, labels=xint*-1, lty=1,lwd=1,lwd.ticks= 1, cex.axis=.7)
  #axis(2, at=yint, labels=yint*1, lty=1,lwd=1,lwd.ticks=1,las=1, cex.axis=.7)
}


#' Plot bathymetry on PBSmapping map from a data object
#'
#' @param bathyFname
#' @description The default dataset is for eastern Canada. You need to go to NOAA to get data for your region.
#'         You will likely need to use the joinBathy to make your downloaded data work properly
#' @references http://maps.ngdc.noaa.gov/viewers/wcs-client/
#' @keywords helper function
#' @export
#' @examples
#'
getBathy <- function(bathyFname, isob=c(100,200,500,800,1200,2400), minVerts=3 )
{
  # data source: http://maps.ngdc.noaa.gov/viewers/wcs-client/
  icol    = rgb(0,0,seq(255,100,len=length(isob)),max=255)
  ocCL   = contourLines(ocBathy,levels=isob)
  if (length(ocCL)==0) stop("No contours available for selected isobaths. Choose again.")
  ocCP   = convCP(ocCL, projection="LL")
  ocPoly = ocCP$PolySet
  par(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
  plotMap(ocPoly,type="n",plt=NULL)
  addLines(thinPolys(ocPoly,filter=minVerts),col=icol)
  data(nepacLL)
  addPolys(nepacLL,col="lemonchiffon")
  legend(x="topright",bty="n",col=icol,lwd=2,legend=as.character(isob),inset=0.05,title="Isobaths")
  save("ocCP",file="ocCP.rda")
  invisible(ocCP)
}


#' Join bathymetric contours
#'
#' @param
#' @keywords helper function
#' @export
#' @examples
#'
joinBathy <- function(CP, isobA = 100, isobB = 300, col = "grey") {
    pdatA = CP$PolyData[is.element(CP$PolyData$level, isobA), ]
    pdatB = CP$PolyData[is.element(CP$PolyData$level, isobB), ]

    Apoly = CP$PolySet[is.element(CP$PolySet$PID, pdatA$PID) & is.element(CP$PolySet$SID, pdatA$SID),
        ]
    Bpoly = CP$PolySet[is.element(CP$PolySet$PID, pdatB$PID) & is.element(CP$PolySet$SID, pdatB$SID),
        ]

    ### extract the longest polyline from A and B
    Apoly$ID = .createIDs(Apoly, c("PID", "SID"))
    IDA = rev(sort(sapply(split(Apoly$ID, Apoly$ID), length)))[1]
    Aline = Apoly[is.element(Apoly$ID, names(IDA)), ]
    Bpoly$ID = .createIDs(Bpoly, c("PID", "SID"))
    IDB = rev(sort(sapply(split(Bpoly$ID, Bpoly$ID), length)))[1]
    Bline = Bpoly[is.element(Bpoly$ID, names(IDB)), ]

    ABpoly = convLP(Aline, Bline)
    addPolys(ABpoly, col = col)
    invisible(ABpoly)
}


#' Make a bubble plot to look at residuals
#'
#' @param x,y,z the independent, depedent and residual variables
#' @description
#' @references
#' @keywords
#' @export
#' @examples
bubs= function(x,y,z, xlim=c(0,55),ylim=c(1969,2016),xlab="Length (cm)", ylab="Year") {
  #alignment= order(z)
  #x=x[alignment]; y=y[alignment]; z=z[alignment]
  plot(x,y,xlab=xlab,ylab=ylab,type="n",xlim=xlim,ylim=ylim,xaxs="r",yaxs="r",las=1,yaxs="i",xaxs="i")
  z.scaled= round(rescale(sqrt(abs(z)),to=c(0.2,6)),2)
  col.grad= colorRampPalette(c("black","grey90"),interpolate="spline")
  col.grad= col.grad(length(unique(z.scaled)))
  bubble.fill= col.grad[match(z.scaled,sort(unique(z.scaled)))]
  points(x,y,pch=21,lwd=0.3,cex=z.scaled,col="black",bg=bubble.fill)
}

#' Make a bubble plot to look at residuals
#'
#' @param x,y,z the independent, depedent and residual variables
#' @description
#' @references
#' @keywords
#' @export
#' @examples
bubs.resid= function(x,y,z, xlim=c(0,55),ylim=c(1969,2016),xlab="Length (cm)", ylab="Year") {
  #alignment= order(z)
  #x=x[alignment]; y=y[alignment]; z=z[alignment]
  plot(x,y,xlab=xlab,ylab=ylab,type="n",xlim=xlim,ylim=ylim,xaxs="r",yaxs="r",las=1,yaxs="i",xaxs="i")
  z.scaled= round(rescale(sqrt(abs(z)),to=c(0.2,6)),2)
  sign=z/abs(z)
  sign= rescale(sign,to=c(1,2))
  points(x,y,pch=21,lwd=0.3,cex=z.scaled,col="black",bg=sign)
}
