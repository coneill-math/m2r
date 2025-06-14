# Creating your own **m2r** Amazon cloud server

## EC2

Begin by getting your own [Amazon EC2](https://aws.amazon.com/ec2/)
instance running. For an operating system, any Unix-alike should work;
we used the [Amazon Linux
AMI](https://aws.amazon.com/amazon-linux-ami/). If you prefer not to use
a cloud server and would rather use a physical machine, in principle all
that follows should work in the same way, though this has not been
tested.

### Setting up the server

Once you have spun your EC2 instance, log into it and run the following
commands that download the m2rserver python script:

``` bash
mkdir m2rserver
cd m2rserver
curl https://raw.githubusercontent.com/coneill-math/m2r/master/inst/server/m2rserver.py > m2rserver.py
echo 'cd /home/ec2-user/m2rserver; python m2rserver.py' >> /etc/rc.local
```

<!-- Using your favorite text editor, in the file `/etc/rc.local`, add the -->
<!-- following line -->
<!-- ```{bash eval=FALSE} -->
<!-- cd /home/ec2-user/m2rserver; python m2rserver.py -->
<!-- ``` -->

The last line will cause the server to start when the system boots.
Reboot the instance, and the python script will start listening for a
connection.

Next, install [docker](https://www.docker.com) on your machine. Your
method of doing this will depend on what distribution you use. For the
Amazon Linux AMI,
[yum](https://en.wikipedia.org/wiki/Yellowdog_Updater,_Modified) should
work, see instructions
[here](https://docs.docker.com/engine/installation/linux/centos/). Once
docker has been installed, run

``` bash
docker pull sommars/m2r
```

to install our docker image on your machine. As of June 2017, the image
comes provisioned with [Macaulay2](https://www.macaulay2.com),
[PHCpack](http://homepages.math.uic.edu/~jan/download.html),
[Bertini](https://bertini.nd.edu), [4ti2](http://www.4ti2.de), and
[LattE](https://www.math.ucdavis.edu/~latte/).

### Local changes

When you use **m2r**, after loading the package, each time you must run
the command

``` r
start_m2(cloud = TRUE, hostname = "host.uri.com")
```

to initialize a connection to your new host.

## Questions?

If you have any issues setting up a server, we’re happy to try to help.
File an issue and we’ll get back to you.
