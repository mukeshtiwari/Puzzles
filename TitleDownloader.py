import urllib2
import socket
import Queue
import thread
import re
from bs4 import BeautifulSoup
from random import randrange

class Downloader():

	def __init__( self ):
			self.q = Queue.Queue( 100 )
			socket.setdefaulttimeout( 10 ) 

	def downloadurl( self ) :	
			while True :
				try :
					url = self.q.get( )
					data = urllib2.urlopen ( url )
					regex = re.compile('<title>(.*?)</title>' , re.IGNORECASE)
					title = regex.search(data.read())
					print ', '.join ( [ url , title.group(1) ] )
					#parsed = BeautifulSoup ( data.read() , 'lxml' )
					#print  ', '.join ( [ url , parsed.find( 'title' ).renderContents() ] )
					data.close()
				except :
					print 'error occured in download url' 


	def createurl ( self  ) : 
			#This reading line by line is good but it's slow.
			#with open('top-1m.csv', 'r') as file:
				#for line in iter(file.readline, ''):
				#	self.q.put (  ''.join ( [ 'http://www.' , line.split(',')[1] ] ) )

			#Reading data in chunks is fast but we can miss some sites due to reading the data in chunks( It's worth missing because reading is very fast)
			with open('top-1m.csv', 'r') as file:
				prefix = ''
				for lines in iter(lambda:file.read(1024) , ''):
					l = lines.split('\n')
					n = len ( l )
					l[0] = ''.join( [ prefix , l[0] ] )
					for i in xrange ( n - 1 ) : self.q.put ( ''.join ( [ 'http://www.', l[i].split(',')[1] ] )  )
					prefix = l[n-1]

			


if __name__== '__main__':
	u = Downloader()
	thread.start_new_thread ( u.createurl , () )
	thread.start_new_thread ( u.downloadurl , () )
	while True : pass

