#!/bin/bash
erl -name attach_pm1@172.18.230.118 -setcookie moongame -pa ebin deps/ebin config config/app setting -remsh pm1@172.18.230.118
