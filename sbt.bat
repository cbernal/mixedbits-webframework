set SCRIPT_DIR=%~dp0
java -Xmx512M -jar "%SCRIPT_DIR%\tools\sbt-launcher-0.5.6.jar" %*
