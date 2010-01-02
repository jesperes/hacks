@echo off
cd %~dp0
d:\apache-ant-1.7.1\bin\ant.bat -logger org.apache.tools.ant.listener.MailLogger
