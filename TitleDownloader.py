import urllib2
import socket
import Queue
import thread
from bs4 import BeautifulSoup
from random import randrange

class Downloader():

	def __init__( self ):
			self.q = Queue.Queue( 100 )
			self.notvalid = [ 10, 127, 169, 172, 192]
			socket.setdefaulttimeout( 10 ) 

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
					self.q.put('http://www.facebook.com')
					self.q.put('http://www.yahoo.com')
					self.q.put('http://www.icicibank.com')
					self.q.put('http://www.apple.com')
					self.q.put('http://173.200.40.143')
					first = randrange( 1, 256 )
					while first in self.notvalid : 
						first = randrange( 1, 256 ) 
					ip = '.'.join([ str( first ), str( randrange( 1, 256 ) ), str( randrange( 1, 256 ) ), str( randrange( 1, 256 ) ) ] )
					#print ip 
					self.q.put('http://ip')
				except :
					print 'error occured in create url'

if __name__== '__main__':
	u = Downloader()
	thread.start_new_thread ( u.createurl , () )
	thread.start_new_thread ( u.downloadurl , () )
	while True : pass

