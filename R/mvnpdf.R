#'  Fonction de densité multivariée
#'
#' Calcul de la densité d’une loi normale multivariée
#'
#'This gives more details on the function
#'
#' @param x  matrice de p lignes et n colonnes (n=observations)
#' @param mean vecteur de p moyennes
#' @param varcovM matrice carrée de variance covariance de dimension p
#' @param Log dans l'échelle logarithme (facilite le calcul)
#'
#' @return: une liste : points x et les valeurs de la densité évaluées sur ces points
#' @export
#'
#' @examples
mvnpdf<-function(x,mean=rep(0,nrow(x)),varcovM=diag(nrow(x)),Log=TRUE){
  #x p lignes , n colonnes
  #p<-dim(x)[1]
  #if(p !=dim(varcovM)[1]){
  # if(p!= length(mean)){ cat("Please verify the dimension of the mean vector and the var cov matrix")
  # }else{cat("Please verify the dimension of the var cov matrix, it should be squared ")}
  #}

  #det.var<-det(varcovM)
  #for(i in 1:p){
  #y<-(1/(((2*pi)^(p/2))*(det.var)^(1/2)))* exp(-1/2*(t(t(x)-mean))*solve(varcovM)*(t(x)-mean))}
#}
### test changement Chloe
n<-ncol(x)
p<-nrow(x)
x0<-x-mean
Rinv<-solve(varcovM)
logDetvarCovM<-log(det(varcovM))
y<-NULL
  for(j in 1:n){
    yj<- -p/2*log(2*pi)-0.5*logDetvarCovM- 0.5*t(x0[,j])%*%Rinv%*%x0[,j]
    y<-c(y,yj)
  }
  if(!Log){ y<-exp(y)}
cat("pouet")
#change travis
# ret<-list("vector",2)
#  ret$x<-x
# ret$y<-y
  return(list(x=x,y=y))
}

