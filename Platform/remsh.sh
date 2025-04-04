#!/bin/bash
/usr/local/erlang/bin/erl -name attach_pm_platform@172.18.230.118 -setcookie moongame -pa ebin deps/ebin config config/app setting -remsh pm_platform@172.18.230.118
