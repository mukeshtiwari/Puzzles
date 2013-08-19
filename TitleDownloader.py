import urllib2
import socket
import Queue
import thread
from bs4 import BeautifulSoup

class Downloader():
	socket.setdefaulttimeout ( 10 )

	def __init__( self ):
			self.q = Queue.Queue( 100 )

	def downloadurl( self ) :	
			while True :
				try :
					url = self.q.get( )
					data = urllib2.urlopen ( url ) 
					parsed = BeautifulSoup ( data.read() , 'lxml' )
					print parsed.find( 'title' )
				except  :
					print 'error occured in download url' 


	def createurl ( self  ) : 
			#create the url here
			while True :
				try :
					self.q.put('http://www.aol.com' )
		
				except :
					print 'error occured in create url'

if __name__== '__main__':
	u = Downloader()
	thread.start_new_thread ( u.createurl , () )
	thread.start_new_thread ( u.downloadurl , () )
	while True : pass

