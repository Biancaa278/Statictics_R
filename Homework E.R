
#E.1

T_test=function(tip,alfa,n,sample_mean,mu0,sigma)
{
  t_score = (sample_mean-mu0)/(sigma/sqrt(n))
  if (tip=='left')
  {
    t_critic=qt(alfa,n-1)
    if (t_score<t_critic) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. T_score este ", t_score,"si T_critic este ",t_critic)
    else 
        cat("Nu avem suficiente dovezi. T_score este ", t_score,"\nsi t_critic este ",t_critic)
  }
  else 
    if (tip=='right')
    {
      t_critic=qt(1-alfa,n-1)
      if (t_score>t_critic) 
          cat("Ipoteza nula respinsa, se accepta ip alternativa. T_score este ", t_score,"si T_critic este ",t_critic)
      else 
          cat("Nu avem suficiente dovezi. T_score este ", t_score,"\nsi T_critic este ",t_critic)
    }
  else
  {
    if (tip=='simetric')
    {
      t_critic=qt(1-alfa/2,n-1)
      if (abs(t_score)>abs(t_critic)) 
          cat("Ipoteza nula respinsa, se accepta ip alternativa. T_score este ", t_score,"si T_critic este ",t_critic)
      else 
          cat("Nu avem suficiente dovezi. T_score este ", t_score,"si T_critic este ",t_critic)
    }
  }
  return(c(t_critic,t_score))
}
#n=125,mu0=420,sample_mean=418,alfa=0.01,sigma=2.75
T_test('left',0.01,125,418,420,2.75)


#E.2

#ipoteza nula : concentratia nivelului de monoxid este limita admisa
#ipoteza alternativa : ca este mai mare decat limita admisa

Z_test=function(tip,n,mu0,sample_mean,alfa,sigma)
{
  #population mean=mu0
  z_score=(sample_mean-mu0)/(sigma/sqrt(n))
  if (tip=='left')
  {
    z_critic=qnorm(alfa,0,1)
    if (z_score<z_critic) 
      cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
    else 
        cat("Nu avem suficiente dovezi. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
  }
  else 
    if (tip=='right')
    {
      z_critic=qnorm(1-alfa,0,1)
      if (z_score>z_critic) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
      else cat("Nu avem suficiente dovezi. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
    }
  else
  {
    if (tip=='simetric')
    {
      z_critic=qnorm(1-alfa/2,0,1)
      if (abs(z_score)>abs(z_critic)) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)
      else 
        cat("Nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)
    }
  }
}

Z_test('right',25,4.9,5.17,0.01,0.35)
Z_test('right',25,4.9,5.17,0.05,0.35)




#E.3

z_test_means=function(tip,alfa,sigma1,sigma2,n1,n2,sample1_mean,sample2_mean,m0)
{
  combined_sigma = sqrt(sigma1^2/n1 + sigma2^2/n2)
  z_score = (sample1_mean - sample2_mean - m0)/combined_sigma
  if (tip=='left')
  {
    z_critic=qnorm(alfa,0,1)
    if (z_score<z_critic) 
      cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
    else 
      cat("Nu avem suficiente dovezi. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
  }
  else 
    if (tip=='right')
    {
      z_critic=qnorm(1-alfa,0,1)
      if (z_score>z_critic) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
      else 
        cat("Nu avem suficiente dovezi. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
    }
  else
  {
    if (tip=='simetric')
    {
      z_critic=qnorm(1-alfa/2,0,1)
      if (abs(z_score)>abs(z_critic)) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)
      else 
        cat("Nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)
    }
  }
}

z_test_means('simetric',0.01,1.31,0.93,25,28,5.48,6.12,0)
z_test_means('left',0.01,1.31,0.93,25,28,5.48,6.12,0)





#E.4

F_test_means=function(tip,alfa,n1,n2,sigma1,sigma2)
{
  z_score = sigma1^2/sigma2^2
  if (tip=='right')
  {
    z_critic=qf(1-alfa,n1-1,n2-1)
    if (z_score>z_critic) 
      cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
    else 
      cat("Nu avem suficiente dovezi. Z_score este ", z_score,"\nsi z_critic este ",z_critic)
  }
  else
  {
    if (tip=='simetric')
    {
      zs_critic=qf(alfa/2,n1-1,n2-1)
      zd_critic=qf(1-alfa/2,n1-1,n2-1)
      if (z_score>zd_critic|z_score<zs_critic) 
        cat("Ipoteza nula respinsa, se accepta ip alternativa. Z_score este ", z_score,"si z_critic este ",z_critic)
      else 
        cat("Nu avem suficiente dovezi. Z_score este ", z_score,"si z_critic este ",z_critic)
    }
  }
}
F_test_means('right',0.01,25,28,1.24,0.87)
