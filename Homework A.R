#Ex A1

reprezentare=function(lmd, p, n, k)
{
  x=vector()
  for (i in 0:n)
    x[i+1]=k+i
  y=dpois(x, lmd)
  barplot(y, col='green')
  z=dgeom(x,p)
  barplot(z, add=T, col='blue', density=30)
  t=dbinom(x,n,p)
  barplot(t, add=T, col='yellow', density=5)
}

reprezentare (3, 0.5, 5, 1)


#Ex A2

#(a)
punctul_a=function()
{
  x=scan("NotePS.txt")
  v=vector()
  v[1]=mean(x)
  v[2]=median(x)
  v[3]=sd(x)
  v[4]=as.vector(quantile(x))[1+1]
  v[5]=as.vector(quantile(x))[2+1]
  v[6]=as.vector(quantile(x))[3+1]
  return (v)
}

punctul_a()


#(b)

valori_aberante=function()
{
  #in afara intervalului (M-2s, M+2s)
  x=scan("NotePS.txt")
  y=vector()
  n=length(x)
  for (i in 1:n)
  {
    y[i]=x[i]
  }
  M=mean(x)
  s=sd(x)
  j=0
  abr=vector()
  #cautam valorile aberante si le pastram in vectorul abr
  for (i in 1:length(x))
  {
    if ((x[i]<=(M-2*s))|(x[i]>=(M+2*s))) 
    {
      j=j+1
      abr[j]=x[i]
    }
  }
  #stergem vlorile aberante din vectorul copie lui x ( y )
  i=1
  m=length(abr)
  while (i<=m)
  {
    j=1
    while (j<=n)
    {
      if (abr[i] == y[j])
      {
        k=j
        while (k<=(n-1))
          {
             y[k]=y[k+1]
             k=k+1
          }
        n=n-1
        j=j-1
      }
      j=j+1
    }
    i=i+1
  }
  
  
  #copiem elementele vectorului y inapoi in vectorul initial x
  x=vector()
  for(i in 1:n)
  {
    x[i] = y[i]
  }
  return (x)
}

valori_aberante()



#(c)

intervale=function()
{
  x=valori_aberante()
  interv=c(0,10,20,30,40,50,60,70,80,90,100)
  hist(x, breaks = interv, main = "Note PS", freq = T, col='yellow')
}

intervale()
