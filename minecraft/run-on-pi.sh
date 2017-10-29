#!/bin/bash

PI_HOST=pi@192.168.0.64
scp *.py $PI_HOST:/home/pi/minecraft-python/
ssh $PI_HOST "python ./minecraft-python/test.py"
