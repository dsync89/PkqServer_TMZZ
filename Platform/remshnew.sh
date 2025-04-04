#!/bin/bash
source /home/cmaction/.bash_profile
/usr/local/erlang/bin/erl -name attach_pm_platform@172.18.110.224 -setcookie moongame -pa /home/cmaction/Platform/ebin /home/cmaction/Platform/deps/ebin /home/cmaction/Platform/config /home/cmaction/Platform/config/app /home/cmaction/Platform/setting -remsh pm_platform@172.18.110.224
