import urllib2, socket, Queue, thread, signal, sys, re

class Downloader():

	def __init__( self ):
		self.q = Queue.Queue( 100 )
		socket.setdefaulttimeout( 10 )

	def downloadurl( self ) :
		while True :
			try :
				url = self.q.get( )
				data = urllib2.urlopen ( url )
				regex = re.compile('<title.*>(.*?)</title>' , re.IGNORECASE)
				title = regex.search(data.read())
				result =  ', '.join ( [ url , title.group(1) ] )
				print result
				data.close()
			except urllib2.URLError, e :
				print e.code
				#print e.read()
			except socket.error:
				print 'Could not open socket'
			except :
				print 'some thing wrong'
				

	def createurl ( self ) :

		#Reading data in chunks is fast but we can miss some sites due to reading the data in chunks( It's worth missing because reading is very fast)
		with open('top-1m.csv', 'r') as file:
			prefix = ''
			for lines in iter(lambda:file.read(1024) , ''):
				l = lines.split('\n')
				n = len ( l )
				l[0] = ''.join( [ prefix , l[0] ] )
				for i in xrange ( n - 1 ) : self.q.put ( ''.join ( [ 'http://www.', l[i].split(',')[1] ] ) )
				prefix = l[n-1]

	def handleexception ( self , signal , frame) :
		print 'i am here to handle exception'
		sys.exit(0)

if __name__== '__main__':
	u = Downloader()
	signal.signal( signal.SIGINT , u.handleexception)
	thread.start_new_thread ( u.createurl , () )
	for i in xrange ( 5 ) :
		thread.start_new_thread ( u.downloadurl , () )
	while True : pass
			

