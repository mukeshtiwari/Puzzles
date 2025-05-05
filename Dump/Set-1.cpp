#include<cstdio>
#include<iostream>
#include<algorithm>
#include<set>
using namespace std;

struct frac
	{
		int num,denum;
		frac(int num,int denum)
		{
		
			this->num=num;
			this->denum=denum;
		}
		bool operator<(struct frac f) const 
		{
			return ( ( this->num ) * f.denum ) < ( ( this->denum ) * f.num );
		}
		bool operator==(struct frac f) const
		{
                        return ( ( this->num ) * f.denum ) == ( ( this->denum ) * f.num );
                }

	};
set<struct frac> s[20],q;
int main()
	{
		int n;
		cin>>n;
		s[1].insert((struct frac) {1,1});
		q.insert((struct frac) {1,1});
		for(int i=1;i<n;i++)
		 {
			int n_1,d_1;
			for(set<struct frac> :: iterator it_1=s[i].begin();it_1!=s[i].end();it_1++)
			{
				n_1=it_1->num,d_1=it_1->denum;
				for(int j=1;i+j<=n;j++)
			 	{
					int n_2,d_2;
					for(set<struct frac> :: iterator it_2=s[j].begin();it_2!=s[j].end();it_2++) 
					 {
						n_2=it_2->num,d_2=it_2->denum;
						s[i+j].insert((struct frac) {n_1*d_2+n_2*d_1,d_1*d_2});
						s[i+j].insert((struct frac) {n_1*n_2,n_1*d_2+n_2*d_1});
						q.insert((struct frac) {n_1*d_2+n_2*d_1,d_1*d_2});
						q.insert((struct frac) {n_1*n_2,n_1*d_2+n_2*d_1});
					 }
				}
			}
		}
		
		cout<<q.size()<<endl;		
		//for(set<struct frac> :: iterator it=s[1].begin();it!=s[1].end();it++) cout<<(*it).num<<" "<<(*it).denum<<endl;
	}
