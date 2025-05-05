#include<cstdio>
#include<iostream>
#include<vector>
#include<algorithm>
#include<cstdlib>
using namespace std;
vector<int> v(10) , ret , finP , finN ;

void recur(int sum , int lev)
	{
		if(lev == 10 )
		 {
			if( sum -100 >= 0 ) finP.push_back(sum - 100 );
			else finN.push_back(  sum - 100 );
		 }
		else 
		 {
			recur(sum + v[lev+1] , lev+1);
			recur(sum , lev + 1);
		 }
		return ;
	}
int main()
	{
		for(int i=0 ; i < 10 ; i++) cin>>v[i];
		int sum = 0;
		
		//he may chose or not  
		finP.clear();
		finN.clear();
		recur(v[0] , 0 );
		recur(0 , 0) ; //decided not to chose 
		sort( finP.begin() , finP.end() ) ;
		sort( finN.rbegin() , finN.rbegin() );
		if(finN.empty() ) cout<<100 + finP[0]<<endl;
		else if(finP.empty() ) cout<<100 + finN[0]<<endl;
		else if ( finP[0] <= abs ( finN[0] ) ) cout<<100 + finP[0]<<endl;
		else  cout<<100 + finN[0]<<endl; 
		  
		 
	}
