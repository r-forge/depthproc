
#include <R.h> 
#include <Rdefines.h> 
#include <Rinternals.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> 
#include <math.h>
 
double qs(double *t,long int k,long int n, long int x,long int l);// deklaracja wlasciwej funkcji quickselect 
/* 
t-tablica na ktorej wszystko sie dzieje 
k-poczatek od ktorego interesuja nas liczby 
n-koniec fragmentu tablicy ktory nas teraz interesuje 
x-ilosc liczb mniejszych od mediany 
l-ile aktualnie mamy liczb mniejszych od piv'a 
*/ 
 
//funkcja do losowania piva 
long int pivr(long int k,long int n); 
double medianqs(double *tab, long int n) ;
 
 SEXP colMedians(SEXP matrix, SEXP nn, SEXP nd)
 {
	int pnd = *INTEGER(nd);
	int pnn = *INTEGER(nn);
	
	SEXP medians;
	PROTECT(medians=NEW_NUMERIC(pnd));
	double *pm=REAL(medians);
	double *pmatrix;
	pmatrix = REAL(matrix);
	
	int i;
	for(i = 0; i < pnd; i++)
	{
	pm[i] = medianqs(pmatrix,pnn);
	pmatrix = pmatrix+pnn;
	}
	UNPROTECT(1);
	return(medians);
 }
 
 SEXP colMediansMads(SEXP matrix, SEXP nn, SEXP nd)
 {
	int pnd = *INTEGER(nd);
	int pnn = *INTEGER(nn);
	
	SEXP results;
	PROTECT(results=NEW_NUMERIC(2*pnd));
	double *presults=REAL(results);
	

	
	double *pmatrix;
	pmatrix = REAL(matrix);
	
	int i;
	for(i = 0; i < pnd; i++)
	{
	presults[i] = medianqs(pmatrix,pnn);
	pmatrix = pmatrix+pnn;
	}
	
	int j;
	double tmed;
	pmatrix = REAL(matrix);
	
	for(i = 0; i < pnd; i++)
	{
		tmed = presults[i];
		for(j = 0; j < pnn; j++)
		 {
			pmatrix[j + i*pnn] = pmatrix[j + i*pnn] - tmed;
			pmatrix[j + i*pnn] = fabs(pmatrix[j + i*pnn]);
		 }
	}
	
	for(i = 0; i < pnd; i++)
	{
	presults[i+pnd] = medianqs(pmatrix,pnn);
	pmatrix = pmatrix+pnn;
	}
	
	UNPROTECT(1);
	return(results);
 }
 
/*Wlasciwa funkcja - do wyznaczania mediany
ona jest wywo?ywana z poziomu R*/
double medianqs(double *tab, long int n) 
{ 
 
srand(time(NULL));

double median;
long int x=n/2; 
if(n&1) median=qs(tab,0,n,x,0); 
else { 
//int r=rand(); 
//if(r&1) pm[0]=; 
//else 
median=(qs(tab,0,n,x,0)+qs(tab,0,n,x-1,0))/2; 
} 
 
return(median); 
} 



double qs(double *t, long int k,long int n,long int x,long int l) 
{ 
long int i; 
long int pivn=pivr(k,n); 
 
double tmp; 
double *p;
p = t;
 
double piv=p[pivn]; 
int flag=1; //flaga zeby dzialalo jak jest kilka takich samych median 
 
for(i=k;i<n;i++) 
{ 
    if(p[i]!=piv) flag=0; 
    if(p[i]<piv)  
    {tmp=p[i]; 
    p[i]=p[l]; 
    p[l]=tmp; 
    l++; 
    }     
} 
 
 
if ((l==x) || flag) return(piv);//jezeli ilosc liczb mniejszych od mediany = ilosc aktualnie liczb 
//mniejszych od piv'a (l) - piv jest mediana 
//albo wszystkie liczby w danym zakresie w korym szukamy mediany sa takie same - wtedy piv to tez 
//mediana 
else if (l>x) return(qs(t,k,l,x,k));//jezeli aktualnie mamy wiecej liczb wiekszych od piv'a wtedy szukamy  
//w tablicy od poczatku przedzialu w ktorym szukalismy, do aktualnej ilosci liczb wiekszych od piv'a  
//a liczba aktualnie mniejszych od piva - zmienia sie na k (czyli poczatek przedzialu) poniewaz ponizej 
//tego przedzialu jest na wlasnie k liczb 
else return(qs(t,l,n,x,l));//mamy za malo za mala liczbe akualnie mniejszych od piva, dlatego mediana 
//powinna sie znajdowac gdzies 
//w przedziale liczb wiekszych od terazniejszego piv'a 
} 
 
 
/************************************************************************/ 
//funkcja losujaca liczbe z przedzialu <k,n) 
long int pivr(long int k,long int n) 
{ 
long int r; 
r=rand()%(n-k)+k; 
return(r); 
} 
 
