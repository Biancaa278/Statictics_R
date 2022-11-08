
#D1

#deviatia=30km/h
#n=20
#alfa=1-0.9

zconfidence_interval=function(n,sample_mean,dispersia,alfa){
  sigma=sqrt(dispersia)
  critical_z=(-1)*qnorm(alfa/2, 0, 1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  cat("Intervalul de incredere este: (",a,",",b,")")
  
}

zconfidence_interval(20,300,30^2,1-0.9)
zconfidence_interval(20,300,30^2,1-0.95)

#D2

t_conf_interval=function(n,sample_mean,s,alfa){
  se=s/sqrt(n)
  critical_t=qt(1-alfa/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  #cat("Intervalul de incredere este: (",a,",",b,")")
  return(c(a,b))
}

D2=function(N, alfa){
  ct=0
  val=vector()
  for (i in 1:N)
  {
    vec=vector()
    vec=sample(0:9,40,replace=TRUE)
    n=40
    sample_mean=mean(vec)
    s=sd(vec)
    val=t_conf_interval(n,sample_mean,s,1-alfa)
    if((4.5>val[1])&(4.5<val[2]))
      ct=ct+1
  }
  return (ct)
}
D2(100, 0.95)
D2(100, 0.99)





TestZ_proportii=function(tip,alfa,n,succese,p0)
{ #left=asimetric la st
  #right=asimetric la dr
  #simetric=simetric
  
  p_prim=succese/n
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  if (tip=='left')
  {
    z_critic=qnorm(alfa,0,1)
    if (z_score<z_critic) {cat("Ip nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)}
    else {cat("nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)}
  }
  else 
    if (tip=='right')
    {
      z_critic=qnorm(1-alfa,0,1)
      if (z_score>z_critic) cat("Ip nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)
      else cat("nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)
    }
  else
  {
    if (tip=='simetric')
    {
      z_critic=qnorm(1-alfa/2,0,1)
      if (abs(z_score)>abs(z_critic)) cat("Ip nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)
      else cat("nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)
    }
  }
}

#D.3
TestZ_proportii('left',0.01,1250,852,72/100)
TestZ_proportii('left',0.05,1250,852,72/100)

#D.4-------
TestZ_proportii('right',0.01,1020,623,60/100)
TestZ_proportii('right',0.05,1020,623,60/100)