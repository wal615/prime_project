pwlkh <- function(y,x,niter=50,eps=1e-6,vect="col") {
  dyn.load("./pairwiseLKH.so")
  if(is.matrix(y)==T){
    n=dim(y)[1]
    np=dim(y)[2]
  }else{
    n=length(y)
    np=1
  }
  if(is.matrix(x)==T){
    nq=dim(x)[2]
  }else{
    nq=1
  }

  theta=rep(0,np*nq)
  estv=matrix(0,nrow=np*nq,ncol=np*nq)

  converge=0
  if(vect=="col"){
    fit=.Fortran("pwMLEcol",as.double(y),as.double(x),as.integer(n),as.integer(np),
           as.integer(nq),as.double(theta),as.double(estv),
           as.integer(niter),as.double(eps),as.integer(converge))
  }else{
    fit=.Fortran("pwMLErow",as.double(y),as.double(x),as.integer(n),as.integer(np),
           as.integer(nq),as.double(theta),as.double(estv),
           as.integer(niter),as.double(eps),as.integer(converge))
  }
  if(fit[[10]]==0){print("Convergence criterion is not met")}

  return(list(fit[[6]],matrix(fit[[7]],ncol=np*nq)))
}
