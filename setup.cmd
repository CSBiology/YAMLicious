@echo off
TITLE YAMLicious Setup

ECHO Restore .NET tools
CALL dotnet tool restore

ECHO Install JavaScript Dependencies
CALL npm i

ECHO Install Python Dependencies
Call uv python install
CALL uv pip install
ECHO DONE!