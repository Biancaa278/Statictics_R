 #B1

paraboloid=function(N,a,b,c,h)
{
  ct=0
  for (i in 1:N)
  {
    x=runif(1,(-1)*a*sqrt(h/c),a*sqrt(h/c))
    y=runif(1,(-1)*b*sqrt(h/c),b*sqrt(h/c))
    z=runif(1,0,h)
    if ((x^2)/(a^2)+(y^2)/(b^2)<=z/c) 
    {
      ct=ct+1
    }
  }
  v1=2*a*sqrt(h/c)
  v2=2*b*sqrt(h/c)
  #vol paralelipipedului * ct / N
  return (v1*v2*h*ct/N) 
}

erori=function(N)
{
  val_act=pi*4*3*4*4/(2*4)
  print(val_act)
  alphaMC=paraboloid(N,4,3,4,4)
  print(alphaMC)
  er_abs=abs(alphaMC-val_act)
  er_rel=er_abs/abs(val_act)
  er_proc=er_rel*100
  cat("Eroare absoluta :", er_abs, "\n") 
  cat("Eroare relativa :", er_rel, "\n")
  cat("Eroare procentuala :", er_proc, "%")
}

erori(20000)
erori(50000)
erori(100000)

print(pi*4*3*4*4/(2*4))

#B2

arie=function(N,a,b,c,d)
{
  ct=0
  for (i in 1:N)
  {
    x=runif(1,a,b)
    y=runif(1,c,d)
    if ((x>=1)&(y<=2)&(y<=x-1)&(y<=7-x)&(y>=0))
    {
      ct=ct+1
    }
  }
  return(abs((a-b)*(c-d))*ct/N)
}

arie(20000,1,7,0,2)
print((6+2)*2/2) #(B+b)*h/2


#B3

#a
punctul_a=function(N,a,b)
{
  s=0
  for (i in 1:N)
  {
    x=runif(1,a,b)
    s=s+(x/(x^2+2)^3)
  }
  #return lungimea integralei * suma / N
  return((b-a)*s/N)
} 

erori_integrala=function(N, alphaMC, val_act)
{
  er_abs=abs(alphaMC-val_act)
  er_rel=er_abs/abs(val_act)
  er_proc=er_rel*100
  cat("Eroare absoluta :", er_abs, "\n") 
  cat("Eroare relativa :", er_rel, "\n")
  cat("Eroare procentuala :", er_proc, "%")
}

A=punctul_a(10000,1,2)
erori_integrala(10000, A, 1/48)

#b

punctul_b=function(N,a,b)
{
  s=0
  for (i in 1:N)
  {
    x=runif(1,a,b)
    s=s+(1/(x^2+9))
  }
  #return lungimea integralei * suma / N
  return((b-a)*s/N)
} 

B=punctul_b(10000,-3,3)
erori_integrala(10000, B, pi/6)


#c

punctul_c=function(N,a,b)
{
  s=0
  for (i in 1:N)
  {
    x=runif(1,a,b)
    s=s+(x*exp((-1)*x^2))
  }
  #return lungimea integralei * suma / N
  return((b-a)*s/N)
} 

C=punctul_c(10000,0,50)
erori_integrala(10000, C, 1/2)


#B4

server=function()
{
  x=runif(1,0,1) #generez o probabilitate
  t=0
  if (x<=0.25)
  {
    print("Proceseaza server 1")
    t=t+rexp(1,4)+pgamma(4,3)
  }
  else if ((x>0.25)&(x<=0.5))
  {
    print("Proceseaza server 2")
    t=t+rexp(1,4)+pgamma(4,2)
  }
  else if ((x>0.5)&(x<=0.8))
  {
    print("Proceseaza server 3")
    t=t+rexp(1,4)+pgamma(5,2)
  }
  else
  {
    print("Proceseaza server 4")
    t=t+rexp(1,4)+pgamma(5,3)
  }
  return(t)
}

Media=function(N)
{
  s=0
  for (i in 1:N)
  {
    srv=server()
    s=s+srv
  }
  return (s/N)
}

Media(1000)


#B5

#B5
#a
pct_a=function(N,p)
{
  suma=0
  for(i in 1:N)
  {
    mail=vector(mode="logical",length = 50) #vectorul initial care are un singur cont infectat
    pozitie=sample(1:50,1) #indexul mailului infectat
    mail[pozitie]=1
    
    infectat=vector(mode="logical", length = 50) #vectorul celor infectati
    infectat[pozitie]=1
    
    for(j in 1:50) #infectam cu probabilitate p
    {
      if(mail[j]==1)
      {
        for(k in 1:50)
        {
          if(mail[k]==0)
          {
            x=runif(1,0,1)
            if(x<=p)
            {
              mail[k]=1
              infectat[k]=1
            }
          }
        }
      }
    }
   
    #vom sterge 8 conturi infectate sau mai putine daca nu sunt 8
    ct=0
    for(j in 1:length(mail))
    {
      if(mail[j]==0 && ct<8)
      {
        mail[j]=0
        ct=ct+1
      }
    }
    #verificam cerinta de la punctul a (daca fiecare cont a fost infectat cel putin o data)
    ok=1
    for(j in 1:50)
    {
      if(infectat[j]==0)
      {
        ok=0
      }
    }
    suma=suma+ok
  }
  return(suma/N)
}

pct_a(10000,0.05)
pct_a(10000,0.1)
pct_a(10000,0.2)

#b
pct_b=function(N,p,nr_de_Zile)
{
  suma=0
  for(i in 1:N)
  {
    mail=vector(mode="logical",length = 50) #vectorul initial care are un singur cont infectat
    pozitie=sample(1:50,1) #indexul mailului infectat
    mail[pozitie]=1
    
    #infectam cu probabilitate p in ziua 1
    for(j in 1:50)
    {
      if(mail[j]==1)
      {
        for(k in 1:50)
        {
          if(mail[k]==0)
          {
            x=runif(1,0,1)
            if(x<=p)
            {
              mail[k]=1
            }
          }
        }
      }
    }
    
    #pentru zilele de la 2 la 7
    for(j in 1:(nr_de_Zile-1))
    {
      #vom sterge 8 conturi care sunt infectate
      ct=0
      for(k in 1:length(mail))
      {
        if(mail[k]==1 && ct<8)
        {
          mail[k]=0
          ct=ct+1
        }
      }
      #infectam cu probabilitate p zilele 2 pana la 7
      for(k in 1:50)
      {
        if(mail[k]==1)
        {
          for(f in 1:50)
          {
            if(mail[f]==0)
            {
              x=runif(1,0,1)
              if(x<=p)
              {
                mail[f]=1
              }
            }
          }
        }
      }
    }
    ok=0
    for(j in 1:50){
      if(mail[j]==1){
        ok=1
      }
    }
    suma=suma+ok
  }
  return(suma/N)
}

pct_b(10000,0.05,7)
pct_b(10000,0.1,7)
pct_b(10000,0.2,7)

#c
alfa=1-0.99
z=qnorm(alfa/2)
epsilon=0.01
p=pct_b(10000,0.2,7)
N_min=p*(1-p)*(z/epsilon)^2
N_min
