REM Set up variables
set csc="C:\working\part-ii-project\P4ToCSharp\HandConverted\P4lang\P4c\P4_16_samples\bmv2\Microsoft.Net.Compilers.2.1.0\tools\csc.exe"
set P4TOCS="C:\working\part-ii-project\P4ToCSharp\App\bin\Debug\p4tocs.exe"
set p4tocslib="C:\working\part-ii-project\P4ToCSharp\P4ToCSharp.Lib\bin\Debug\P4ToCSharp.Lib.dll"
set v1model="C:\working\part-ii-project\P4ToCSharp\v1model\bin\Debug\v1model.dll"

REM Transpile P4 json
FOR %F in (*bmv2.p4.json) DO %p4tocs% generate-program %~nxF --architecture-library %v1model

REM Compile generated C#
FOR /R %F in (*.cs) DO %csc% /t:exe /out:%~nF.exe %~nxF /r:%v1model% /r:%p4tocslib%