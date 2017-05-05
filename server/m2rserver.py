import socket
import sys

from subprocess import Popen
from contextlib import closing

MAX_PORTS = 20
FIRST_PORT = 27436



serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

serversocket.bind((socket.gethostname(), 27545))
serversocket.listen(5)


while True:
	# Wait for a connection
	print("Waiting for a connection")
	connection, client_address = serversocket.accept()
	
	try:
		print("connection from", client_address)
		
		openport = 0
		
		portrange = range(FIRST_PORT, FIRST_PORT+MAX_PORTS)
		
		# find the first open port
		for port in portrange:
			with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as sock:
				if sock.connect_ex(("localhost",port)) != 0:
					openport = port
					break
		
		# launch M2 docker instance
		containername = "m2rserver" + str(openport)
		
		cmd = ["docker", "create", "-it", "--name=" + containername, "fhinkel/macaulay2:latest"]
		cmd = cmd + ["&&", "docker", "start", containername]
		cmd = cmd + ["&&", "docker", "exec", containername, "M2", "-e", "sleep 30", "--script", "FILENAME"]
		cmd = cmd + ["&&", "docker", "stop", containername]
		cmd = cmd + ["&&", "docker", "rm", containername]
		
		Popen(cmd)
		# p.terminate()
		
		# send open port back to the client
		connection.sendall(str(openport))
	
	finally:
		# Clean up the connection
		connection.close()
	





