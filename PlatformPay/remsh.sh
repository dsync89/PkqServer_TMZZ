#!/bin/bash
erl -name attach_pm_pay@127.0.0.1 -setcookie moongame -pa ebin deps/ebin config config/app setting -remsh pm_pay@172.18.230.118
