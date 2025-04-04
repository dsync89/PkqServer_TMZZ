%% @author admin
%% @doc 逻辑路由
%% Created 2013-2-25


-module(platform_pay_handle).
-include("common.hrl").
%% API functions
-export([handle/3]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

handle('GET', "pay_yijie"++_, Req) ->
    mod_pay_yijie:handle(Req);
handle('POST', "pay_quickios"++_, Req) ->
    mod_pay_ios_quick:handle(Req);	
handle('POST', "pay_quick"++_, Req) ->
    mod_pay_quick:handle(Req);
handle('GET', "payall"++_, Req) ->
    mod_pay_all:handle(Req);
handle('GET', "pay360"++_, Req) ->
    mod_pay_360:handle(Req);
handle('GET', "paytbt"++_, Req) ->
    mod_pay_tbt:handle(Req);
handle('POST', "paywdj"++_, Req) ->
    mod_pay_wdj:handle(Req);
handle('POST', "payuc"++_, Req) ->
    mod_pay_uc:handle(Req);
handle('POST', "payxy"++_, Req) ->
    mod_pay_xy:handle(Req);
handle('POST',"payzz"++_, Req) ->
    mod_pay_zz:pay_gold(Req);
handle('POST',"payzr"++_, Req) ->
    mod_pay_zr:pay_gold(Req);
handle('POST',"pay_txhd_ard_ext"++_, Req) ->
    mod_pay_txhd_ard_ext:pay_gold(Req);
handle('POST',"pay_txhd_ard"++_, Req) ->
    mod_pay_txhd_ard:pay_gold(Req);
handle('POST',"pay_txhd_ios"++_, Req) ->
    mod_pay_txhd_ios:pay_gold(Req);
handle('GET',"paydk"++_, Req) ->
    mod_pay_dk:handle(Req);
handle('POST',"paypp"++_, Req) ->
    mod_pay_pp:handle(Req);
handle('POST',"payky"++_, Req) ->
    mod_pay_ky:handle(Req);
handle('POST',"payit"++_, Req) ->
    mod_pay_it:handle(Req);
handle('GET', "pay91_ios"++_, Req)->
    mod_pay_91_sgz15_ios:handle(Req);
handle('POST',"paybd"++_, Req) ->
    mod_pay_bd:handle(Req);
handle('POST',"payyw"++_, Req) ->
    mod_pay_yw:handle(Req);
handle('POST',"payhw"++_, Req) ->
    mod_pay_hw:handle(Req);
handle('GET',"paydl"++_, Req) ->
    mod_pay_dl:handle(Req);
handle('GET',"paymi"++_, Req) ->
    mod_pay_mi:handle(Req);
handle('POST',"payaz"++_, Req) ->
    mod_pay_az:handle(Req);
handle('POST',"paydj"++_, Req) ->
    mod_pay_dj:handle(Req);
handle('GET',"paysina"++_, Req) ->
    mod_pay_sina:handle(Req);
handle('GET',"pay_mzw"++_, Req) ->
    mod_pay_mzw:handle(Req);
handle('POST',"paymz"++_, Req) ->
    mod_pay_mz:handle(Req);
handle('POST',"pay37wan"++_, Req) ->
    mod_pay_37wan:handle(Req);
handle('GET',"payks"++_, Req) ->
    mod_pay_ks:handle(Req);
handle('POST',"payi4"++_, Req) ->
    mod_pay_i4:handle(Req);
handle('POST', "payhjr"++_, Req) ->
    mod_pay_hjr:handle(Req);
handle('POST',"payiiapple"++_, Req) ->
    mod_pay_iiapple:handle(Req);
handle('POST',"payyk"++_, Req) ->
    mod_pay_yk:handle(Req);
handle('POST',"payhm"++_, Req) ->
    mod_pay_hm:handle(Req);
handle('GET',"pay_pps"++_, Req) ->
    mod_pay_pps:handle(Req);
handle('POST',"paylenovo"++_, Req) ->
    mod_pay_lenovo:handle(Req);
handle('POST',"pay_lenovo_push"++_, Req) ->
    mod_pay_lenovo_push:handle(Req);
handle('POST',"payoppo"++_, Req) ->
    mod_pay_oppo:handle(Req);
handle('POST',"paysogou"++_, Req) ->
    mod_pay_sogou:handle(Req);
handle('POST',"paygfan"++_, Req) ->
    mod_pay_jf:handle(Req);
handle('POST',"payyyh"++_, Req) ->
    mod_pay_yyh:handle(Req);
handle('POST',"payvivo"++_, Req) ->
    mod_pay_vivo:handle(Req);
handle('POST', "pay_dianjoy"++_, Req) ->
    mod_pay_dianjoy:handle(Req);
handle('POST', "pay_coolpad"++_, Req) ->
    mod_pay_coolpad:handle(Req);
handle('GET',"payouw"++_, Req) ->
    mod_pay_ouw:handle(Req);
handle('POST',"payjl"++_, Req) ->
    mod_pay_jl:handle(Req);
handle('GET',"paykw"++_, Req) ->
    mod_pay_kw:handle(Req);
handle('GET',"pay4399"++_, Req) ->
    mod_pay_4399:handle(Req);
handle('GET',"paycw"++_, Req) ->
    mod_pay_cw:handle(Req);
handle('GET',"payld"++_, Req) ->
    mod_pay_ld:handle(Req);
handle('GET', "pay_yy"++_, Req) ->
    mod_pay_yy:handle(Req);
handle('GET',"payyyb"++_, Req) ->
    mod_pay_yyb:handle(Req,'GET',"/payyyb");
handle('GET',"payqq"++_, Req) ->
    mod_pay_qq:handle(Req,'GET',"/payqq");
handle('GET',"pay51cm"++_, Req) ->
	mod_pay_51cm:handle(Req);
handle('POST',"payuu"++_, Req) ->
    mod_pay_uu:handle(Req);
handle('POST',"paygg"++_, Req) ->
    mod_pay_gg:handle(Req);
handle('POST',"pay_qtld_chk_ard"++_, Req) ->
    mod_pay_qtld_chk_ard:handle(Req);
handle('POST',"pay_qtld_ard"++_, Req) ->
    mod_pay_qtld_ard:handle(Req);
handle('POST',"pay_qtld_chk_ios"++_, Req) ->
    mod_pay_qtld_chk_ios:handle(Req);
handle('POST',"pay_qtld_ios"++_, Req) ->
    mod_pay_qtld_ios:handle(Req);
handle('POST',"pay_mmy"++_, Req) ->
    mod_pay_mmy:handle(Req);
handle('POST',"checkiosreicept"++_,Req) ->
    mod_check_receipt:handle(Req);
handle('GET', "pay"++_, Req) ->
    mod_pay:handle(Req);
handle(Method, Path, Req)->
	?ERR("error message:Method=~w,Path=~s,Req=~w",[Method, Path, Req]).


%% ====================================================================
%% Internal functions
%% ====================================================================


