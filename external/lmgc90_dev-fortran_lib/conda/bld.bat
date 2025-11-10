
IF NOT EXIST build MKDIR build
CD build

cmake ^
    -DCMAKE_PREFIX_PATH="%LIBRARY_PREFIX%" ^
    -DCMAKE_INSTALL_PREFIX="%LIBRARY_PREFIX%" ^
    -DVENV_PATH=%PREFIX% ^
    -DCONDA_BUILD=%CONDA_BUILD% ^
    -DCMAKE_SH="CMAKE_SH-NOTFOUND" ^
    -DWINDOWS_COMPILATION=ON ^
    -DBUILD_rTree_BINDING=OFF ^
	-DBUILD_MATLIB=ON ^
    -G "MinGW Makefiles" ^
    ..

mingw32-make install

@REM liste de dlls à copier si elles existent
SET DLLS=libdmumps.dll ^
         libmatlib.dll ^
         libmpiseq.dll ^
         libmumps_common.dll ^
         libpord.dll

@REM recherche et copie des dlls dans le bon repertoire
FOR /F "delims=" %%f IN ('DIR /b %PREFIX%\Lib\*.dll') DO (
    FOR %%d IN (%DLLS%) DO ( 
        IF %%d == %%f (
            echo %%f :
            COPY %PREFIX%\Lib\%%f %PREFIX%\Lib\site-packages\pylmgc90\chipy\
        )
    )
)

