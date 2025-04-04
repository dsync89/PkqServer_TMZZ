#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name pm_platform@172.18.110.224 -setcookie moongame -pa ebin deps/ebin config config/app setting -s inets start -s user_default s -detached
