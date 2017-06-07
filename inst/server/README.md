Creating your own **m2r** cloud server
======================================

EC2
---

Begin by getting your own [Amazon EC2](https://aws.amazon.com/ec2/) instance running. For an operating system, any Unix-alike should work; we used the Amazon Linux AMI. If you prefer not to use a cloud server and would rather use a physical machine, in principal all should work in the same way, though this has not been tested.

### Setting up the server

Once you have started your machine, log into it and run the following commands that download the m2rserver python script:

``` bash
mkdir m2rserver
cd m2rserver
curl https://raw.githubusercontent.com/coneill-math/m2r/master/inst/server/m2rserver.py > m2rserver.py
```

Using your favorite text editor, in the file /etc/rc.local, add the following line

``` bash
cd /home/ec2-user/m2rserver; python m2rserver.py
```

which will cause the server to start when the system boots. Reboot your machine now, and the **Python** script will start listening for an attempted connection.

Next, install docker on your machine. Your method of doing this will depend on what distribution you use. For the Amazon Linux AMI, yum worked when we used it.

Once docker has been installed, run

``` bash
docker pull sommars/m2r
```

which will install our docker image on your machine. At the point this README was first written, it had **Macaulay2**, **PHCpack**, **Bertini**, **4ti2**, and **LattE** installed on it.

### Local changes

When you use **m2r**, after loading the package, each time you must run the command

``` bash
start_m2(cloud=TRUE, hostname=absolute_path_to_your_host)
```

to initialize a connection to your new host.

Questions?
----------

If you have any issues setting up a server, we're happy to try to help. File an issue and we'll get back to you.
