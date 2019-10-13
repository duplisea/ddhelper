#' Extract sub-diagonals of a matrix
#'
#' @param mat data matrix
#' @param diag.pos the sub-diagonal position, i.e. -1 extract the first lower subdiagonal, 0 is the main diagonal
#' @description  I have only used it for Leslie matrix modelling. Not sure where else it would be useful.
#' @return the specified diagonal as vector
#' @author Daniel Duplisea
#' @examples
#' data.matrix= matrix(1:100,ncol=10)
#' diags.f(data.matrix,0)
#' diags.f(data.matrix,-2)
#' diags.f(data.matrix,8)
#' @export
diags.f= function(mat, diag.pos=-1){
  delta.matrix= col(mat)-row(mat)
  selected.diagonal= mat[delta.matrix==diag.pos]
  selected.diagonal
}

#' Counts distinct values of a vector
#'
#' @param x The vector for which you want distinct values counted
#' @description  mimics count(distinct value) in mysql when using a group by statement. Used most in apply.
#' @return a scalar with the count of unique values in a vector
#' @author Daniel Duplisea
#' @examples
#' mat=matrix(rep(1:10,each=10),ncol=10)
#' apply(mat,2,length); apply(mat,2,count.distinct)
#' apply(mat,1,length); apply(mat,1,count.distinct)
#' @export
count.distinct= function(x){
  n.dist= length(unique(x))
  n.dist
}

#' Push a value onto the end of vector
#'
#' @param vec vector of data
#' @param item the new values you want to append
#' @description pushes values onto the end of vector. Used by data science people when they do not know how big their
#'         final object size should be. You do not need to assign the vector to itself again, it is done internally so
#'         somewhat against the R way of doing things and it could get you into trouble
#' @return a vector with new values pushed onto the end of the old vector
#' @author Kevin Wright
#' @references
#'         Kevin Wright http://www.johnmyleswhite.com/notebook/2009/12/07/implementing-push-and-pop-in-r/#comment-17810
#' @export
push <- function(vec, item){
  vec=substitute(vec)
  eval.parent(parse(text = paste(vec, ' <- c(', vec, ', ', item, ')', sep = '')), n = 1)
}

#' Pop the final value off the end of vector
#'
#' @param vec vector of data
#' @description pops off the final value on a vector. You do not need to assign the vector to itself again, it is done internally so
#'         somewhat against the R way of doing things and it could get you into trouble.
#' @return pulls/pops off the final value of a vector4
#' @author Kevin Wright
#' @references
#'         Kevin Wright http://www.johnmyleswhite.com/notebook/2009/12/07/implementing-push-and-pop-in-r/#comment-17810
#' @export
pop <- function(vec){
  tmp <- vec[length(vec)]
  vec=substitute(vec)
  eval.parent(parse(text = paste(vec, ' <- ', vec, '[-length(', vec, ')]',
  sep = '')),
  n = 1)
  tmp
}


#' Convert geoegraphic coordinates in degrees minutes seconds to decimal degrees
#'
#' @param degminsec all in one number without commas or decimals, e.g. 44 38' 52", input as 443852
#' @description a function to convert degrees, minutes, seconds coordinates into decimal degrees.
#' @return
#' @author Daniel Duplisea
#' @export
decdeg.f= function(degminsec){
  if(degminsec<0) stop("you cannot use negative coordinates. Maybe you actually want -decdeg.f(coords) instead of decdeg.f(-coords)")
  deg= floor(degminsec/10000)
  min= floor((degminsec - deg*10000)/100)/60
  sec= (degminsec-100*floor(degminsec/100))/3600
  dd= deg+min+sec
  dd
}
