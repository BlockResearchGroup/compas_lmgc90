To package a new User version
=============================

Run the script *user_version.sh* which
uses a tag as an argument. It will:

* download the git branch
* export it as a regular directory with the name *lmgc90_user_tag*
* then it will execute the following:
```cmd
mv README_FOR_USER.md README.md
rm -rf src/obsolete
rm -rf src/Sandbox/Astro
rm -rf src/Sandbox/BindingPeligriff
rm -rf src/Sandbox/BindingProjection
rm -rf src/Sandbox/Cellule
rm -rf src/Sandbox/GGC
rm -rf src/Sandbox/Post
rm -rf src/Sandbox/Standalone_MPI
```
* a build and test will be run
* the doc is build in duser directory and outside
* the build directory content is then removed
* the source directory is zipped


Upload zip file and docs directory on LMGC90_USER library of seafile and on puech ~/public_html/...

Share the correct link in lmgc90_user.git/wikis 

