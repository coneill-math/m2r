import socket
import sys
import time

from subprocess import Popen
from subprocess import call
from contextlib import closing

MAX_PORTS = 20
FIRST_PORT = 27436


def randomString(N = 10):
	randpart = ''.join(random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(N))
	
	ret = "/m2_logs/session-"
	ret = ret + time.strftime("%Y-%m-%d-%H-%M-%S-", time.gmtime())
	ret = randpart + ".txt"
	
	return ret
	

serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

print(socket.gethostname())
serversocket.bind((socket.gethostname(), 27434))
serversocket.listen(5)


while True:
	# Wait for a connection
	print("Waiting for a connection")
	connection, client_address = serversocket.accept()
	
	try:
		print("Connection from ", client_address)
		
		openport = 0
		containername = ""
		
		portrange = range(FIRST_PORT, FIRST_PORT+MAX_PORTS)
		
		# find the first open port
		for port in portrange:
			with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as sock:
				if sock.connect_ex(("localhost",port)) != 0:
					# create M2 docker instance
					containername = "m2rserver" + str(openport)
					retval = call(["docker", "create", "-it", "--expose=" + str(port), "--publish=" + str(port) + ":" + str(port), "--name=" + containername, "sommars/m2r"])
					if retval == 0:
						openport = port
						break
		
		logfilename = logFileName()
		
		outredir = ["2&>1", ">" + logfilename]
		# launch M2 docker instance
		cmd = ["docker", "start", containername] + outredir
		cmd = cmd + [";", "docker", "exec", containername, "M2", "--script", "/m2r_server_script.m2", str(openport)] + outredir
		cmd = cmd + [";", "docker", "stop", containername] + outredir
		cmd = cmd + [";", "docker", "rm", containername] + outredir
		# cmd = cmd + [";", "aws", "s3", "cp", logfilename, "s3://my_bucket/my_folder/my_file.ext"]
		Popen(" ".join(cmd), shell=True)
		# p.terminate()
		
		print("Session logging to " + logfilename)
		
		# send open port back to the client
		connection.sendall(str(openport) + "\n")
	
	finally:
		# Clean up the connection
		connection.close()
	





