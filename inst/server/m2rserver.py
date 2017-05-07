import socket
import sys
import time
import random
import string

from subprocess import Popen
from subprocess import call
from contextlib import closing

MAX_PORTS = 20
FIRST_PORT = 27436


def generateLogFileName(N = 10):
	randpart = ''.join(random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(N))
	
	ret = "session-"
	ret = ret + time.strftime("%Y-%m-%d-%H-%M-%S-", time.gmtime())
	ret = ret + randpart + ".txt"
	
	return ret
	

with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as serversocket:
	print(socket.gethostname())
	serversocket.bind((socket.gethostname(), 27435))
	serversocket.listen(5)


	while True:
		# Wait for a connection
		print("Waiting for a connection")
		connection, client_address = serversocket.accept()
		
		try:
			print("Connection from " + str(client_address))
			
			openport = 0
			containername = ""
			
			portrange = range(FIRST_PORT, FIRST_PORT+MAX_PORTS)
			
			# find the first open port
			for port in portrange:
				with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as sock:
					if sock.connect_ex(("localhost",port)) != 0:
						# create M2 docker instance
						containername = "m2rserver" + str(port)
						retval = call(["docker", "create", "-it", "--expose=" + str(port), "--publish=" + str(port) + ":" + str(port), "--name=" + containername, "sommars/m2r"])
						if retval == 0:
							openport = port
							break
			
			if openport != 0:
				logfilename = generateLogFileName()
				
				# launch M2 docker instance
				cmd = ["docker", "start", containername, "&>/dev/null"]
				cmd = cmd + [";", "docker", "exec", containername, "M2", "--script", "/m2rserverscript.m2", str(openport), "&>" + logfilename]
				cmd = cmd + [";", "docker", "stop", containername, "&>/dev/null"]
				cmd = cmd + [";", "docker", "rm", containername, "&>/dev/null"]
				cmd = cmd + [";", "aws", "s3", "cp", logfilename, "s3://m2r-dev-logs/logfiles/"]
				Popen(" ".join(cmd), shell=True)
				# p.terminate()
				
				print("Session logging to " + logfilename)
			else:
				print("No session started, all ports taken")
			
			# send open port back to the client
			connection.sendall(str(openport) + "\n")
		
		finally:
			# Clean up the connection
			connection.close()
		
		print("")





