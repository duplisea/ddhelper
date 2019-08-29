#' functions to extract P fit diagnostics from an lm object
#'
#' @param lm.fit.object a linear model fit objects
#' @description Calculated the p value from a linear model fit (ANOVA F-test)
#' @return The p value
#' @keywords
#' @export
#' @examples
#' fake.data= data.frame(x=1:100, y=.05*(1:100)^2 - 5*(1:100) + 2 + rnorm(100,0,5)) #fit a linear to quadratic just for the hell of it
#' plot(fake.data)
#' lm.fit.object= lm(y~ x, data=fake.data)
#' lines(1:100,predict(lm.fit.object))
#' lmP.f(lm.fit.object=lm.fit.object)
lmP.f= function(lm.fit.object){
  P= round(anova(lm.fit.object)$'Pr(>F)'[1],4)
  if(P<0.0001) P="<0.0001"
  P
}

#' Adjusted r squared value from lm
#'
#' @param lm.fit.object a linear model fit objects
#' @description Pulls the r-squared value out of a linear model fit.
#' @return The adjusted r-squared value
#' @keywords
#' @export
#' @examples
#' fake.data= data.frame(x=1:100, y=.05*(1:100)^2 - 5*(1:100) + 2 + rnorm(100,0,5)) #fit a linear to quadratic just for the hell of it
#' plot(fake.data)
#' lm.fit.object= lm(y~ x, data=fake.data)
#' lines(1:100,predict(lm.fit.object))
#' lmRsq.f(lm.fit.object=lm.fit.object)
lmRsq.f= function(lm.fit.object){
  adjR= round(summary(lm.fit.object)$adj.r.squared,2)
  adjR
}


#' Draw polygon confidence intervals
#'
#' @param x the independent variable vector
#' @param ylow the lower confidence interval y series
#' @param yhigh the higher confidence interval y series
#' @param ... other argument from par that can be used with the polygon function
#' @description just a call to polygon but with more intuitive arguments in the context on making confidence intervals.
#'      You need to call this before you overlay your median with the line function.
#' @keywords helper function
#' @export
#' @examples
#'
confint= function(x,ylow,yhigh,...){
	polygon(x = c(x, rev(x)), y = c(ylow,rev(yhigh)), border = NA,...)
}
