#include<cstdio>
#include<iostream>
#include<map>
#include<vector>
#include<algorithm>
#define Lim 100000000
using namespace std;
typedef long long ll;
typedef pair<ll,ll> PI;

multimap<ll,ll> M;
int main()
	{
		ll sum=0;
		for(int i=1;i<=Lim;i++)  
		{
			sum+=(Lim/i)*i;
			
		}
		




	      for(int a=1;a*a<Lim;a++)
		  for(int b=1;b<=a && (a*a+b*b)<=Lim;b++) 
			if(__gcd(a,b)==1)
		{	
			int i=(a*a+b*b),val=(a==b)?(a+b):2*(a+b);
			for(int j=1;i*j<=Lim;j++) 
			 {
				sum+=(j*val*(Lim/(i*j)));
				//for(int k=i*j;k<=Lim;k+=i*j) sum+=(j*val);
			}
		}
		
		
		cout<<sum<<endl;
	}
