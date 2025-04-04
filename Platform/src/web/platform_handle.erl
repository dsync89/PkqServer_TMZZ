%% @author admin
%% @doc 逻辑路由
%% Created 2013-2-25


-module(platform_handle).
-include("common.hrl").
%% API functions
-export([handle/3]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

handle('GET', "loginyj"++_, Req) ->
    mod_login_yijie:handle(Req);
handle('GET', "loginquickios"++_, Req) ->
    mod_login_quick_ios:handle(Req);
handle('GET', "loginquick"++_, Req) ->
    mod_login_quick:handle(Req);
handle('GET', "all_sdk"++_, Req) ->
    mod_all_sdk:handle(Req);
handle('GET', "change_password"++_, Req) ->
    mod_change_password:handle(Req);
handle('POST', "create"++_, Req) ->
	mod_account_create:handle(Req);
handle('GET', "giftcreate"++_, Req) ->
	mod_gift_code:handle(Req);
handle('POST', "gift"++_, Req) ->
	mod_gift:handle(Req);
handle('POST', "version"++_,Req) ->
	mod_version_check:handle(Req);
handle('POST', "login_again"++_,Req) ->
    mod_login_again:handle(Req);
handle('GET', "baseinfo"++_, Req) ->
	mod_baseinfo:handle(Req);
handle('GET', "server_list"++_, Req) ->
	mod_server_list:handle(Req);
handle('GET', "select"++_, Req) ->
	mod_select:handle(Req);
handle('GET', "state"++_, Req) ->
	mod_server_state:handle(Req);
handle('GET', "guest"++_, Req) ->
	mod_guest:handle(Req);
handle('GET', "bind"++_, Req) ->
	mod_bind:handle(Req);
handle('GET', "loginzz"++_, Req) ->
    mod_login_zz:handle(Req);
handle('GET', "loginzr"++_, Req) ->
    mod_login_zr:handle(Req);
handle('GET', "login_txhd_ard_ext"++_, Req) ->
    mod_login_txhd_ard_ext:handle(Req);
handle('GET', "login_txhd_ard"++_, Req) ->
    mod_login_txhd_ard:handle(Req);
handle('GET', "login_txhd_ios"++_, Req) ->
    mod_login_txhd_ios:handle(Req);
handle('GET', "login91"++_, Req) ->
    mod_login91:handle(Req);
handle('GET', "loginbd"++_, Req) ->
    mod_loginbd:handle(Req);
handle('GET', "loginyw"++_, Req) ->
    mod_login_yw:handle(Req);
handle('GET', "loginuc"++_, Req) ->
    mod_loginuc:handle(Req);
handle('GET', "login360"++_, Req) ->
    mod_login360:handle(Req);
handle('GET', "logindk"++_, Req) ->
    mod_login_dk:handle(Req);
handle('GET', "logintbt"++_, Req) ->
    mod_login_tbt:handle(Req);
handle('GET', "login_coolpad"++_, Req) ->
    mod_login_coolpad:handle(Req);
handle('GET', "loginit"++_, Req) ->
    mod_login_it:handle(Req);
handle('GET', "loginpp"++_, Req) ->
    mod_login_pp:handle(Req);
handle('GET', "loginky"++_, Req) ->
    mod_login_ky:handle(Req);
handle('GET', "loginhw"++_, Req) ->
    mod_login_hw:handle(Req);
handle('GET', "loginqq"++_, Req) ->
    mod_login_qq:handle(Req);
handle('GET', "loginwx"++_, Req) ->
    mod_login_wx:handle(Req);
handle('GET', "logindj"++_, Req) ->
    mod_login_dj:handle(Req);
handle('GET', "loginwdj"++_, Req) ->
    mod_loginwdj:handle(Req);
handle('GET', "logindl"++_, Req) ->
    mod_logindl:handle(Req);
handle('GET', "loginmi"++_, Req) ->
    mod_login_mi:handle(Req);
handle('GET', "loginaz"++_, Req) ->
    mod_login_az:handle(Req);
handle('GET', "loginsina"++_, Req) ->
    mod_login_sina:handle(Req);
handle('GET', "loginmzw"++_, Req) ->
    mod_login_mzw:handle(Req);
handle('GET', "loginmz"++_, Req) ->
    mod_login_mz:handle(Req);
handle('GET', "login37wan"++_, Req) ->
    mod_login_37wan:handle(Req);
handle('GET', "loginks"++_, Req) ->
    mod_login_ks:handle(Req);
handle('GET', "logini4"++_, Req) ->
    mod_login_i4:handle(Req);
handle('GET', "loginhjr"++_, Req) ->
    mod_login_hjr:handle(Req);
handle('GET', "loginiiapple"++_, Req) ->
    mod_login_iiapple:handle(Req);
handle('GET', "loginyk"++_, Req) ->
    mod_login_yk:handle(Req);
handle('GET', "login_pps"++_, Req) ->
    mod_login_pps:handle(Req);
handle('GET', "loginlenovo"++_, Req) ->
    mod_login_lenovo:handle(Req);
handle('GET', "loginoppo"++_, Req) ->
    mod_login_oppo:handle(Req);
handle('GET', "loginsogou"++_, Req) ->
    mod_login_sogou:handle(Req);
handle('GET', "login_dianjoy"++_, Req) ->
    mod_login_dianjoy:handle(Req);
handle('GET', "logingfan"++_, Req) ->
    mod_login_jf:handle(Req);
handle('GET', "loginyyh"++_, Req) ->
    mod_login_yyh:handle(Req);
handle('GET', "loginyyb"++_, Req) ->
    mod_login_yyb:handle(Req);
handle('GET', "login_yy"++_, Req) ->
    mod_login_yy:handle(Req);
handle('GET', "loginvivo"++_, Req) ->
    mod_login_vivo:handle(Req);
handle('GET', "logintecent"++_, Req) ->
    mod_login_tecent:handle(Req);
handle('GET', "loginouw"++_, Req) ->
    mod_login_ouw:handle(Req);
handle('GET', "loginjl"++_, Req) ->
    mod_login_jl:handle(Req);
handle('GET', "loginkw"++_, Req) ->
    mod_login_kw:handle(Req);
handle('GET', "login4399"++_, Req) ->
    mod_login_4399:handle(Req);
handle('GET', "login51cm"++_, Req) ->
	mod_login_51cm:handle(Req);
handle('GET', "loginxy"++_, Req) ->
    mod_login_xy:handle(Req);
handle('GET', "logincw"++_, Req) ->
    mod_login_cw:handle(Req);
handle('GET', "loginuu"++_, Req) ->
    mod_login_uu:handle(Req);
handle('GET', "loginld"++_, Req) ->
    mod_login_ld:handle(Req);
handle('GET', "login_gg"++_, Req) ->
    mod_login_gg:handle(Req);
handle('GET', "login_qtld_ard"++_, Req) ->
    mod_login_qtld_ard:handle(Req);
handle('GET', "login_qtld_ios"++_, Req) ->
    mod_login_qtld_ios:handle(Req);
handle('GET', "login_mmy"++_, Req) ->
    mod_login_mmy:handle(Req);
handle('GET', "login_hm"++_, Req) ->
    mod_login_hm:handle(Req);
handle('POST', "process_create"++_,Req) ->
    mod_create_process:handle(Req);
handle(Method, Path, Req) ->
	?ERR("error message:Method=~w,Path=~s,Req=~w",[Method, Path, Req]).


%% ====================================================================
%% Internal functions
%% ====================================================================


