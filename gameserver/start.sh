#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name pm1@172.18.110.224 -setcookie moongame -pa ebin deps/ebin config config/app setting -s tk start -detached
