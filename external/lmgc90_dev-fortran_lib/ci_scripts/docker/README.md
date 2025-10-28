
Use and installation of GitLab CI/CD
====================================

Here is an explanation on our policy/use of
GitLab CI/CD tools.

As explained in [GitLab's documentation](https://docs.gitlab.com/ee/ci/quick_start/)
to run, build and test automatically a code
sheltered on the GitLab server only need two things:

- a *.gitlab-ci.yml* file in the source code
  defining the actions to run
- a *gitlab-runner*  on a separate server which
  will execute the actions defined above


There is an extensive documentation on the web and there
is no interest in writing another one here. So only some
points which were not understood explicitly at first
reading of those forums/documentation will be written down
here.


Desired behaviour:
------------------

The YAML configuration file defines the pipelines of
test and deployment step.

The aim is that at each push on a merge request:

- The code is build (cmake and make)
- The code is tested (make tests)
- The reference results are generated
- The non regression tests are run

When tagging:

- A new version is delivered
  (currently only windows binary are generated)

Furthermore, it is wanted that the build and test
must be done on several types of Linux distribution
(and maybe macOS) with different types of build (python,
HDF5, standalone...).

To ensure a reasonable testing time, we must have one runner
per distribution to test (and or build of LMGC90). Otherwise
for each push, a single runner will get the task and execute
them one after another (and deploying the image and building
on each distros is quite time consuming).

If there is a lot of activity on the project, we may need
to duplicate the list of initial runners.

Thus several runners will be registered, each with a specific
Linux distribution and/or type of build of LMGC90, and the YAML
will define all the test to run. The drawback is that the registered
runners will be very specific to LMGC90. But it was intended to
not use shared nor group runners for this project.


What to do on GitLab:
---------------------

On LMGC90 project on the GitLab, on the left hand side menu, select
*Settings -> CI/CD*. Explore the *General pipelines* section to
do some settings (like doing only fetch or cloning each time and so on),
then explore the *Runner* section to check that *Shared* and *Group Runners*
are inactive; the section *Setting a specific runner manually* is nice
to keep at hand when registering the runners later.


YAML file writing:
------------------

The pipeline currently holds five stages:

- build
- test
- reference results generation
- non regression tests
- deploy


The deployement currently only consist on running
cross-compiling script to generate windows binary.
A full deployement may be worked on further later in time.

What is currently wanted is to run these four first stages for:

- Ubuntu 16 with python 2.7
- Ubuntu 18 with python 3.6 and HDF5 library support

The trick is that, if nothing is done, even if there are several
runners available, only one runner will take charge of every stages
of the pipeline. Thus each build/test of a specific distribution
must be matched to a specific runner. To this end **tags** must be
used in the YAML file and when registering the runner (these tags
have nothing to do with *git* tags).

List of tags to use:

- ub16
- ub18
- py27
- py36
- hdf5

Thus to build on Ubuntu 16 with python 2.7 the file must contains:

```
build:ub16py27:
  stage: build
  [...]
  tags:
    - ub16
    - py27
```

And only if all the tags of the stage match the tags of a runner can
it be used. The runner will use a docker image to build and test it.

Since it is desired to separate the different stages, some information
must be transmitted between each stage, this is done thanks to the *artifacts*.
So to keep the build directory at the end of the *build* stage so that the
*test* stage uses it, the file must have something like:

```
build:ub16py27:
  stage: build
  [...]
  artifacts:
    paths:
      - build
```

In the same way to get access to the tests log, the *test* stage must have
something like:

```
test:ub16py27:
  stage: test
  [...]
  artifacts:
    paths:
      - build/Testing/Temporary/LastTest.log
```

The core of each stage is defined by the *script* section. Thus to build
there should be something like:

```
build:ub16py27:
  [...]
  script:
    - mkdir build
    - cd build
    - cmake ..
    - make -j12
```

It is possible, using the *image* keyword, to specify what docker image
to use. If nothing is provided, there is a default image defined when
registering the runner. To reduce the size of the YAML file, it is assumed
that the runners will always have the good default docker image associated
with its tags! This is a strong assumption but since, for the build/test to
work, this consistency must be ensured. But adding the docker image name in
the YAML file will in no way provide this insurance, but only add cryptic
names...

To shorten the syntax of each job submitted, variables are used.
But these variables are only replaced in the job submissions,
so *tags* cannot be evaluated as variables in the same way.
In this case it is wanted to replace the *ub16* and *py27* in the
examples above by variables. So a template job is created:

```
.build_template: &build_def
  stage: build
  script:
    - mkdir build_${linux}_${py}
    - cd build_${linux}_${py}
    - cmake .. {h5f} 
  [...]
```

And then a real job is created:

```
build:ub16py27:
  variables:
    linux: ub16
    py: py27
  <<: *build_def
  tags:
    - ub18
    - py36
```

Be carefull when using `variables` keyword. Only one keyboard is allowed
per job. Thus if, for example, a reference sha1 number is needed, it cannot
be added in a template job ! It must be added to all real job, or defined
as a global variable (outside of any job).

If artifacts become too big, the maximum size of artifacts can be changed
in the `Admin->Settings->CI/CD` menu, as well as the duration of time
the artifacts will be accessible.

TODO : explain dependencies to reduce artifacts transmissions
       and naming to avoir conflicts.


Runner registration:
--------------------

Again everything has be done following [GitLab CI documentation](https://docs.gitlab.com/runner/install/)


### Docker installation and image creation ###


On the server/computer that will run the pipeline, the first thing
to do is to install docker since it is the executor chosen for us.
This is explained [here](https://docs.docker.com/install/linux/docker-ce/ubuntu/).

Each time the gitlab-runner is called, an image can be downloaded and deployed.
Before running the script, some packages can be installed and configured.
But since this step is not relevant for us, it is preferred to generate
a new docker image with everything installed beforehand in order to not
repeat this step at each push.

The images are build thanks to the dockerfiles:

- *Dockerfile_ub16py27*
- *Dockerfile_ub18py36hdf5*

To generate the image locally on your server, run for each image needed
(replace *[image_name]* by the desired string and do not miss the last dot character):

```shell
docker build -t [image_name] -f Dockerfile_[image_name] .
```
If by any chance you run into a "permission denied" error you may need
to be part of group `docker`:
```shell
sudo usermod -a -G docker $USER
```
Then log out/in.

For the record in the above command line, the `-t` option providing the
image name does not need to be the same as the name of the input file.
But for consistency's sake and to be able to follow tracks of tags, runner,
docker image and so on, names will be redundant.

The list of images present on the server can be displayed with:
```shell
docker images
```

And to remove an image run:
```shell
docker rmi [image_name]
```

Which may need to run before:
```shell
docker container prune
```


And in case of uncertainty:

```shell
docker help
```

### gitlab-runner installation ###

It is recommended to install the *gitlab-runner* package using GitLab's repository.
In this case a *gitlab-runner* user will be automatically created and the runner
will be added as a service so that if the server must be rebooted, the runners
will be automatically available.

Once *gitlab-runner* is installed, which is basically done by doing:

```shell
curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-runner/script.deb.sh | sudo bash
sudo apt-get install gitlab-runner
```

Runners can be created and registered. To do this the information to provide
are at least:

- the token available in *Settings/ CI/CD / Runners* section of the GitLab project
- the tags accepted by the runner
- the default docker image name to use

So to create and register the run, run **as a sudoer**:

```shell
sudo gitlab-runner register
```

The input to provide are:

- https://git-xen.lmgc.univ-montp2.fr
- the Gitlab-CI token from the *Settings/ CI/CD / Runners* section of the GitLab project
- a runner name, the convention is \[server\_name\]\[tags\_list\] (for example: *picsou_ub18py36hdf5*)
- the list of tags separated by comma (for example: *ub18,py36,hdf5*)
- docker
- the name of the docker image (for example *ub18py36hdf5*)

Always run *gitlab-runner* commands as a sudoer, it will run in a system-mode sense
which makes it automatically run as a service. Running the command without the *sudo*
will run it in user-mod which ask many more operations to run. Be careful of this
feature which is kind of schizophrenic (even if multiple personality is not a systematic
symptom of this mental trouble).

To verify the list of runner:

```shell
sudo gitlab-runner list
sudo gitlab-runner verify
```

The *list* command provide the location of the configuration file
(probably: */etc/gitlab-runner/config.toml*). In this file you
can specify a set of option, globally or for each runner. An important
one is the pull policy of docker. By default, docker will always try
to pull a docker image from its repository. Since it is wanted to use
some local images, the policy must be change by adding to the *.toml*
configuration file the line:

```
pull_policy: "if-not-present"
```

In the *[runners.docker]* blocks. Putting this parameter to *never* does
not work because when invoking the runner, GitLab will try to pull another
docker image containing some tools it needs; so downloading it must be
allowed.

Furthermore, it is a global option of this file which define the maximum
concurrent jobs that all the runner can run on this server. To change this
value change the *concurrent* value of this file.

Finally, instead of typing everything each time and changing the configuration
file afterward, a one-liner command can be used:

```shell
sudo gitlab-runner register \
  --non-interactive \
  --url "https://git-xen.lmgc.univ-montp2.fr/" \
  --registration-token "PROJECT_REGISTRATION_TOKEN" \
  --name "picsou_ub18py36hdf5" \
  --executor "docker" \
  --docker-image ub18py36hdf5 \
  --docker-pull-policy "if-not-present"
  --tag-list "ub18,py36,hdf5" \
  --locked="false" \
```

Register as many runner as needed (have a look to the *register_runner.sh* script
which take the gitlab token as an argument and fix variables).

In case of uncertainty run:
```shell
sudo gitlab-runner help
```

For example, currently there are some problem with wine
configuration with the dockerfile. So to generate the 
`ub18cross` docker image one must generate and start a
base image first:
```shell
docker build -t ub18cross-base -f Dockerfile_ub18cross .
docker run --name ub18cross-tmp -d -i -t ub18cross-base
```

This returns a `CONTAINER_ID`, that must be used to connect
to this running container:
```shell
docker exec -it CONTAINER_ID /bin/bash
```

Then it connects to the container where the change to the
container can be made before exiting:
```shell
wine64 regedit
sed -i -e 's/\("PATH"=str(2):".*\)"/\1;\/usr\/lib\/gcc\/x86_64-w64-mingw32\/7.3-win32"/g' /root/.wine/system.reg
exit
```

And the final image with these last modifications can
be save into the final image:
```shell
docker commit CONTAINER_ID ub18cross
```

