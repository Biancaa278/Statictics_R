
#C1

leaves = c(1, 0, 0, 1, 1, 1, 1, 0, 0,1, 0, 0, 1, 1, 1, 1, 0, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0,1, 0, 0, 1, 1, 1, 0, 1, 0) #vectorul frunzelor

tree_eval=function(i, leaves)
{
  a=runif(1,0,1) #generam o probabilitate
  x=0 # de la pozitia x+1 va fi primul nod de pe penultimul nivel (fii sunt frunze)
  y=0 # la pozitia y se va afla ultimul nod de pe penultimul nivel (fii sunt frunze)
  
  len=length(leaves)
  
  for (j in 0:(log(len,3)-2))
  {
    x=x+3^j
  }
  for (j in 0:(log(len,3)-1))
  {
    y=y+3^j
  }
  
  
  if ((i>x) & (i<=y)) #daca suntem pe penultimjul nivel
  { # copiii nodului i sunt frunze
      if(a<=0.333)
      {
        if(leaves[3*i-y-1]==0) 
        {
          if(leaves[3*i-y]==0)
            return(leaves[3*i-y+1])
          else return(1)
        }
        return(1)
      }
      else if (a>0.333 & a<=0.666)
      {
        if(leaves[3*i-y]==0) 
        {
          if(leaves[3*i-y+1]==0)
            return(leaves[3*i-y-1])
          else return(1)
        }
        return(1)
      }
      else
      {
        if(leaves[3*i-y+1]==0) 
        {
          if(leaves[3*i-y-1]==0)
            return(leaves[3*i-y])
          else return(1)
        }
        return(1)
      }
  }
  
  #aflam nivelul nodului i
  nivel=-1 #nivelul nodului i
  s=0 #cate noduri sunt de la radacina pana la nivelul i
  
  for (j in 0:log(len,3)-1)
  {
     if(s+3^j<=i) #daca inca nu am ajuns pe nivelul nodului i
     {
        s=s+3^j #atunci crestem numarul de noduri de pana acum
        nivel=nivel+1 #iar nivelul creste cu 1
     }
  }

  #nodul este de tip AND - nivel par
  if(nivel%%2==0)
  {
    {
      if(a <= 0.333)
      {
        if(tree_eval(3*i-1,leaves)==1) 
        {
          if(tree_eval(3*i,leaves)==1)
            return(tree_eval(3*i+1,leaves))
          return(0)
        }
        return(0)
      }
      else if (a>0.333 & a<=0.666)
      {
        if(tree_eval(3*i,leaves)==1) 
        {
          if(tree_eval(3*i+1,leaves)==1)
            return(tree_eval(3*i-1,leaves))
          return(0)
        }
        return(0)
      }
      else
      {
        if(tree_eval(3*i+1,leaves)==1) 
        {
          if(tree_eval(3*i-1,leaves)==1)
            return(tree_eval(3*i,leaves))
          return(0)
        }
        return(0)
      }
    
  }
  
  }
  #nodul este de tip OR - nivel impar
  if(nivel%%2==1)
  {
    {
      if(a<=0.333)
      {
        if(tree_eval(3*i-1,leaves)==0) 
        {
          if(tree_eval(3*i,leaves)==0)
            return(tree_eval(3*i+1,leaves))
          return(1)
        }
        return(1)
      }
      else if (a>0.333 & a<=0.666)
      {
        if(tree_eval(3*i,leaves)==0) 
        {
          if(tree_eval(3*i+1,leaves)==0)
            return(tree_eval(3*i-1,leaves))
          return(1)
        }
        return(1)
      }
      else
      {
        if(tree_eval(3*i+1,leaves)==0) 
        {
          if(tree_eval(3*i-1,leaves)==0)
            return(tree_eval(3*i,leaves))
          return(1)
        }
        return(1)
      }
    }
  }
}

tree_eval(1, leaves) 






#C2

C2=function(n)
{
  M=matrix(data=NA, nrow=n, ncol=n) #am creat o matrice in care se vor afla preferintele fiecarei femei
  W=matrix(data=NA, nrow=n, ncol=n) #am creat o matrice in care se vor afla preferintele fiecarui barbat
  for (i in 1:n)
    M[i,]=sample(1:n, n, replace=F) #generam preferintele femeilor
  for (i in 1:n)
    W[i,]=sample(1:n, n, replace=F) #generam preferintele barbatilor
  f=vector() #vectorul barbatilor cuplati
  f1=vector() #vectorul femeilor cuplate
  
  for (i in 1:n)
  {
    f[i]=-1 #initial niciun barbat nu este cuplat
    f1[i]=-1 #initial nicio femeie nu este cuplata
  }
  ct=n #cati barbati nu sunt cuplati
  while(ct>0) #cat timp mai avem barbati care nu sunt cuplati
  {
    i=1
    while(f[i]!=-1)
      i=i+1
    man=i #alegem primul barbat singur
    
    j=sample(1:n, 1) #alegem o femeie pe care sa o cuplam cu barbatul singur
    a=runif(1,0,1) #generam o probabilitate sa vedem daca femeia accepta sau nu propunerea barbatului singur
    if (a>=0.5) #daca ii accepta propunerea
    {
      i=1
      while(W[j,i]!=man)
        i=i+1
      poz_new_man=i #aflam pozitia pe care sa afla barbatul singur in preferintele femeii j
      poz_old_man=-1 
      if (f1[j]!=-1) #daca femaia j este singura
      {
        i=1
        while(W[j,i]!=f1[j])
          i=i+1
        poz_old_man=i #aflam pozitia pe care sa afla barbatul cu care este cuplat femeia j in preferintele acesteia
      }
      if (f1[j]!=-1 & poz_new_man>poz_old_man)  #daca femaia j nu este singura, dar il prefera mai mult pe barbatul singur decat pe barbatul ei
      { 
        i=1
        f[W[j, poz_old_man]]=-1 #barbatul femeii j devine singur
        f[W[j, poz_new_man]]=j #barbatul singur se cupleaza cu femeia j
        f1[j]=W[j, poz_new_man] #femeia j se cupleaza cu barbatul singur (care acum nu mai este singur)
      }
      if(f1[j]==-1) #daca femaia j este singura
      {
        f[W[j, poz_new_man]]=j #barbatul singur se cupleaza cu femeia j
        f1[j]=W[j, poz_new_man] #femeia j se cupleaza cu barbatul singur
        ct=ct-1 #am mai cuplat un barbat
      }
    }
  }
  print(f)
  print(f1)
}

C2(5)





#C3

number=function(x)
{
  sum=0
  for(i in 1:length(x))
  {
    sum=sum+(x[i]*2^(i-1))
  }
  return(sum)
}

functie_C3=function(u,n,m)
{
  library("numbers")
  rj=vector()
  p=sample(Primes(n^2),1)
  r=number(u)%%p
  for(j in 1:m)
  {
    rj[j]=number(u[j])&&p
  }
  for(j in 1:m)
  {
    if(r==rj[j]) return("u apartine lui U")
  }
  return("u nu apartine lui U")
}
functie_C3(c(0,1,1,0,1,1,1,0,0,1,1),11,3)
