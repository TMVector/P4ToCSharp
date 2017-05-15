@echo off
REM Install csc for C#6
REM C:\temp\nuget.exe install Microsoft.Net.Compilers

REM Set up variables
set csc="C:\working\part-ii-project\P4ToCSharp\HandConverted\P4lang\P4c\P4_16_samples\bmv2\Microsoft.Net.Compilers.2.1.0\tools\csc.exe"
set P4TOCS="C:\working\part-ii-project\P4ToCSharp\App\bin\Debug\p4tocs.exe"
set p4tocslib="P4ToCSharp.Lib.dll"
set v1model="v1model.dll"

FOR %%F in (*.stf) DO (
  ECHO Processing %%F
  REM Transpile P4 json
  %p4tocs% generate-program %%~nF.p4.json --architecture-library %v1model%
  
  REM Compile generated C#
  IF EXIST "%%~nF.p4.json.gen.cs" ( %csc% /t:exe /out:%%~nF.exe "%%~nF.p4.json.gen.cs" /r:%v1model% /r:%p4tocslib% )
  
  REM Run compiled exe
  IF EXIST "%%~nF.exe" (
    ECHO Running %%~nF.exe...
	"%%~nF.exe" %%~nF.stf > %%~nF.log 2>&1
  )
)