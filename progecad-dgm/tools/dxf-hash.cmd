@echo off
rem Ypologismos hash arxeiou DXF (ektos CAD).
rem Xrisi: syrete to arxeio DXF pano sto dxf-hash.cmd, i:
rem        dxf-hash.cmd "C:\path\to\diagramma.dxf"
if "%~1"=="" (
  echo Xrisi: dxf-hash.cmd arxeio.dxf
  pause
  exit /b 1
)
echo ============ MD5 ============
certutil -hashfile "%~1" MD5
echo ========== SHA256 ===========
certutil -hashfile "%~1" SHA256
pause
