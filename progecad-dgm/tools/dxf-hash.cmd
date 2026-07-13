@echo off
rem Ypologismos SHA512 hash arxeiou DXF (ektos CAD).
rem Xrisi: syrete to arxeio DXF pano sto dxf-hash.cmd, i:
rem        dxf-hash.cmd "C:\path\to\diagramma.dxf"
if "%~1"=="" (
  echo Xrisi: dxf-hash.cmd arxeio.dxf
  pause
  exit /b 1
)
echo =========== SHA512 ===========
certutil -hashfile "%~1" SHA512
pause
