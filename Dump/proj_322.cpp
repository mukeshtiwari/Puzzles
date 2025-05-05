#include<cstdio>
#include<algorithm>
#include<iostream>
using namespace std;
#define N 9999990
#define M 1000000000

int BN5[40],BI5[40],BN2[40],BI2[40];
int  main()
	{
		int cnt=0,p=N,cnt_2=0,sum_2,sum_5;
		bool f_2=0,f_5=0,f=0;
		while(p)
		 {
			BI5[cnt]=BN5[cnt]=p%5;
			p/=5;cnt++;
		 }
		for(int i=0;i<cnt;i++) cout<<BI5[i]<<" ";cout<<" = "<<N<<endl;
		p=N;cnt=0;
		while(p)
		{
			BI2[cnt]=BN2[cnt]=p%2;
			p/=2;cnt++;
		}
		//for(int i=0;i<cnt;i++) cout<<BI2[i]<<" ";cout<<endl;
		for(int i=N;i<M;i++)
		 {
			/*if(!f_5)
			{
			cnt=39;
			while(BI5[cnt]==0) cnt--;
			for(int j=0;j<=cnt;j++) cout<<BI5[j]<<" ";cout<<" = "<<i+1<<endl;
			}*/
			if( !f_2 || !f_5) cnt_2++/*,printf("%d\n",i)*/;
			
			sum_2=sum_5=1;
			for(int j=0;j<40 && sum_2;j++)
			{
				BI2[j]+=sum_2;
				sum_2=BI2[j]/2;
				BI2[j]%=2;
				
				
			}
			for(int j=0;j<40 && sum_5;j++)
			{
				BI5[j]+=sum_5;
				sum_5=BI5[j]/5;
				BI5[j]%=5;
				
			}
			f_2=f_5=0;
			for(int j=0;j<40;j++) 
			{
				if (BN2[j]>BI2[j]) f_2=1;
				if (BN5[j]>BI5[j]) f_5=1;
			}
			
		 }		
		cout<<cnt_2<<endl;
	}
