@echo off
cd %~dp0
e:\apache-ant-1.7.1-bin\apache-ant-1.7.1\bin\ant.bat -logger org.apache.tools.ant.listener.MailLogger
pause
