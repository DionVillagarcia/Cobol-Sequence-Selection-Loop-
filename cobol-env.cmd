@echo off

set "COB_MAIN_DIR=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\"
set "COB_CONFIG_DIR=%COB_MAIN_DIR%config"
set "COB_COPY_DIR=%COB_MAIN_DIR%copy"
set "COB_CFLAGS=-I"%COB_MAIN_DIR%include""
set "COB_LDFLAGS=-L"%COB_MAIN_DIR%lib""
set "COB_LIBRARY_PATH=%COB_MAIN_DIR%extras"
set "PATH=%COB_MAIN_DIR%bin;%PATH%"

cmd /k