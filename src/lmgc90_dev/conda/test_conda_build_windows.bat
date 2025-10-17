@echo off
setlocal ENABLEDELAYEDEXPANSION ENABLEEXPENSION

SET DIR=%~dp0

@REM Create temporary directory
SET str_date=%DATE:/=_%
SET str_time=%TIME::=_%
SET str_time=%str_time: =0%
SET str_time=%str_time:,=_%
SET DOCKER_FOLDER=%TEMP%^\%str_date%_%str_time%
echo Temporary directory is %DOCKER_FOLDER%
MKDIR %DOCKER_FOLDER%

@REM Copy Dockerfile and conda files in temporary directory
COPY %DIR%\docker_test\Dockerfile_win_conda_build %DOCKER_FOLDER%
MKDIR %DOCKER_FOLDER%\conda
COPY %DIR%\meta.yaml %DOCKER_FOLDER%\conda\meta.yaml.origin
COPY %DIR%\bld.bat %DOCKER_FOLDER%\conda

@REM Replace username with Administrator in meta.yaml file, for the path of future archive
for /f "delims=" %%x in (%DOCKER_FOLDER%\conda\meta.yaml.origin) do (
	SET LINE=%%x
	ECHO !LINE! | FIND "url: https://seafile.lmgc.univ-montp2.fr" > nul && SET LINE=  url: C:\Users\Administrator\lmgc90_dev.zip
	ECHO !LINE! >> %DOCKER_FOLDER%\conda\meta.yaml
)
DEL %DOCKER_FOLDER%\conda\meta.yaml.origin

@REM create archive, using powershell.
ECHO Creating archive...
powershell -Command "& {Compress-Archive -Path %DIR%\..\* -DestinationPath %DOCKER_FOLDER%\lmgc90_dev.zip;}"
powershell -Command "& {Compress-Archive -Path %DIR%\..\.git -Update -DestinationPath %DOCKER_FOLDER%\lmgc90_dev.zip;}"

@REM Building dockerfile
CD %DOCKER_FOLDER%
docker build -f Dockerfile_win_conda_build -t conda_build:test_package --no-cache .

@REM removing temporary folder
CD %TEMP%
RMDIR /S %DOCKER_FOLDER%
