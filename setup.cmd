@echo off
TITLE YAMLicious Setup

ECHO Restore .NET tools
CALL dotnet tool restore

ECHO Install JavaScript Dependencies
CALL npm ci

ECHO Install Python Dependencies
Call uv python install
CALL uv sync --frozen
ECHO DONE!