@echo off

@REM liste de dlls à copier si elles existent
SET DLLS=libdmumps.dll ^
		 libmatlib.dll ^
		 libmpiseq.dll ^
		 libmumps_common.dll ^
		 libpord.dll

@REM recherche et copie des dlls dans le bon repertoire
FOR /F "delims=" %%f IN ('DIR /b %CONDA_PREFIX%\Lib\*.dll') DO (
	FOR %%d IN (%DLLS%) DO ( 
		IF %%d == %%f (
			echo %%f :
			COPY %CONDA_PREFIX%\Lib\%%f %CONDA_PREFIX%\Lib\site-packages\pylmgc90\chipy\
			)
))

@REM renommage du dossier pylmgc90 du dossier build pour éviter les erreurs d'import
@REM SET DIR=%~dp0
@REM echo %DIR%
@REM IF exist %DIR%\build\pylmgc90 (MOVE %DIR%\build\pylmgc90 %DIR%\build\pylmgc90.build)
