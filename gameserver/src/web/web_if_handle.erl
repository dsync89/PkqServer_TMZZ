%% @author admin
%% @doc 按功能分发http请求，为保证礼包与支付功能走不同的处理，可以为支付定义接口handle2来与礼包的handle相区分

-module(web_if_handle).
-export([handle/4]).
-include("common.hrl").

handle('POST', "activity_list", Req, _DocRoot) ->
    activity_list_mod:get_list(Req);
handle('POST', "clear_world_talk", Req, _DocRoot) ->
    clear_world_talk_mod:get_list(Req);
handle('GET', "payall"++_, Req, _DocRoot) ->
    pay_mod_all:pay_gold(Req);
handle('POST', "broadcast"++_, Req, _DocRoot)	->
	post_bc_mod:post_bc(Req);
handle('POST', "ban"++_, Req, _DocRoot)	->
	ban_roleID_mod:ban_roleID(Req);
handle('POST', "gag"++_, Req, _DocRoot)	->
	gag_roleID_mod:gag_roleID(Req);
handle('POST', "vip"++_, Req, _DocRoot) ->
    change_vip_mod:change_vip(Req);
handle('POST', "change_pay_list"++_, Req, _DocRoot) ->
    change_pay_list_mod:change_pay_list(Req);
handle('POST', "change_activity_list"++_, Req, _DocRoot) ->
    change_activity_list_mod:change_activity_list(Req);
handle('POST', "mailgift"++_, Req, DocRoot) ->
	mail_gift_mod:mail_gift(Req, DocRoot);
handle('POST', "payzz"++_, Req, _DocRoot) ->
    pay_mod_zz:pay_gold(Req);
handle('POST', "payzr"++_, Req, _DocRoot) ->
    pay_mod_zr:pay_gold(Req);
handle('POST', "pay_txhd_ard_ext"++_, Req, _DocRoot) ->
    pay_mod_txhd_ard_ext:pay_gold(Req);
handle('POST', "pay_txhd_ard"++_, Req, _DocRoot) ->
    pay_mod_txhd_ard:pay_gold(Req);
handle('POST', "pay_txhd_ios"++_, Req, _DocRoot) ->
    pay_mod_txhd_ios:pay_gold(Req);
handle('GET', "pay360"++_, Req, _DocRoot) ->
    pay_mod_360:pay_gold(Req);
handle('GET', "paytbt"++_, Req, _DocRoot) ->
    pay_mod_tbt:pay_gold(Req);
handle('POST', "paywdj"++_, Req, _DocRoot) ->
    pay_mod_wdj:pay_gold(Req);
handle('POST', "paypp"++_, Req, _DocRoot) ->
    pay_mod_pp:pay_gold(Req);
handle('POST', "payky"++_, Req, _DocRoot) ->
    pay_mod_ky:pay_gold(Req);
handle('POST', "payhw"++_, Req, _DocRoot) ->
    pay_mod_hw:pay_gold(Req);
handle('POST', "payit"++_, Req, _DocRoot) ->
    pay_mod_it:pay_gold(Req);
handle('GET', "paybd"++_, Req, _DocRoot) ->
    pay_mod_bd:pay_gold(Req);
handle('GET', "payyw"++_, Req, _DocRoot) ->
    pay_mod_yw:pay_gold(Req);
handle('GET', "pay_yijie"++_, Req, _DocRoot) ->
    ?DEBUG("in gameserver[web_if_handle], debug yijie pay,  ~p~n", ["laochen"]),
    pay_mod_yijie:pay_gold(Req);
handle('GET', "pay_quick"++_, Req, _DocRoot) ->
    ?DEBUG("in gameserver[web_if_handle], debug quic pay,  ~p~n", ["laochen"]),
    pay_mod_quick:pay_gold(Req);
handle('GET', "pay91_ios"++_, Req, _DocRoot)->
    pay_mod_91_sgz15_ios:pay_gold(Req);
handle('POST',"payuc"++_,Req,_DocRoot) ->
    pay_mod_uc:pay_gold(Req);
handle('GET',"paydk"++_,Req,_DocRoot) ->
    pay_mod_dk:pay_gold(Req);
handle('GET',"paydl"++_,Req, _DocRoot) ->
    pay_mod_dl:pay_gold(Req);
handle('GET',"paymi"++_,Req, _DocRoot) ->
    pay_mod_mi:pay_gold(Req);
handle('GET',"paysina"++_,Req, _DocRoot) ->
    pay_mod_sina:pay_gold(Req);
handle('GET',"pay_mzw"++_,Req, _DocRoot) ->
    pay_mod_mzw:pay_gold(Req);
handle('GET', "paymz"++_, Req, _DocRoot) ->
    pay_mod_mz:pay_gold(Req);
handle('GET', "pay37wan"++_, Req, _DocRoot) ->
    pay_mod_37wan:pay_gold(Req);
handle('GET', "payks"++_, Req, _DocRoot) ->
    pay_mod_ks:pay_gold(Req);
handle('GET', "payi4"++_, Req, _DocRoot) ->
    pay_mod_i4:pay_gold(Req);
handle('GET', "payhjr"++_, Req, _DocRoot) ->
    pay_mod_hjr:pay_gold(Req);
handle('GET', "payiiapple"++_, Req, _DocRoot) ->
    pay_mod_iiapple:pay_gold(Req);
handle('GET', "payhm"++_, Req, _DocRoot) ->
    pay_mod_hm:pay_gold(Req);
handle('GET', "payyk"++_, Req, _DocRoot) ->
    pay_mod_yk:pay_gold(Req);
handle('GET', "pay_pps"++_, Req, _DocRoot) ->
    pay_mod_pps:pay_gold(Req);
handle('GET', "pay_4399"++_, Req, _DocRoot) ->
    pay_mod_4399:pay_gold(Req);
handle('GET', "pay_cw"++_, Req, _DocRoot) ->
    pay_mod_cw:pay_gold(Req);
handle('GET', "pay_ld"++_, Req, _DocRoot) ->
    pay_mod_ld:pay_gold(Req);
handle('GET',"paylenovo"++_,Req, _DocRoot) ->
    pay_mod_lenovo:pay_gold(Req);
handle('GET',"pay_lenovo_push"++_,Req, _DocRoot) ->
    pay_mod_lenovo_push:pay_gold(Req);
handle('POST',"payoppo"++_,Req, _DocRoot) ->
    pay_mod_oppo:pay_gold(Req);
handle('POST',"paysogou"++_,Req, _DocRoot) ->
    pay_mod_sogou:pay_gold(Req);
handle('POST',"payjf"++_,Req, _DocRoot) ->
    pay_mod_jf:pay_gold(Req);
handle('GET', "pay_yy"++_, Req, _DocRoot) ->
    pay_mod_yy:pay_gold(Req);
handle('GET',"payyyh"++_,Req, _DocRoot) ->
    pay_mod_yyh:pay_gold(Req);
handle('POST',"payvivo"++_,Req, _DocRoot) ->
    pay_mod_vivo:pay_gold(Req);
handle('GET', "pay_dianjoy"++_, Req, _DocRoot) ->
    pay_mod_dianjoy:pay_gold(Req);
handle('GET', "pay_coolpad"++_, Req, _DocRoot) ->
    pay_mod_coolpad:pay_gold(Req);
handle('POST',"payaz"++_,Req, _DocRoot) ->
    pay_mod_az:pay_gold(Req);
handle('POST', "paydj"++_, Req, _DocRoot) ->
    pay_mod_dj:pay_gold(Req);
handle('GET',"payouw"++_,Req, _DocRoot) ->
    pay_mod_ouw:pay_gold(Req);
handle('POST', "payjl"++_, Req, _DocRoot) ->
    pay_mod_jl:pay_gold(Req);
handle('POST', "payxy"++_, Req, _DocRoot) ->
    pay_mod_xy:pay_gold(Req);
handle('POST', "paygg"++_, Req, _DocRoot) ->
    pay_mod_gg:pay_gold(Req);
handle('GET',"paykw"++_,Req,_DocRoot) ->
    pay_mod_kw:pay_gold(Req);
handle('GET',"payyyb"++_,Req, _DocRoot) ->
    pay_mod_yyb:pay_gold(Req,'GET',"/payyyb");
handle('GET',"payqq"++_,Req, _DocRoot) ->
    pay_mod_qq:pay_gold(Req,'GET',"/payqq");
handle('GET', "pay51cm"++_, Req, _DocRoot) ->
	pay_mod_51cm:pay_gold(Req);
handle('GET', "payuu"++_, Req, _DocRoot) ->
    pay_mod_uu:pay_gold(Req);
handle('GET', "pay_qtld_ard"++_, Req, _DocRoot) ->
    pay_mod_qtld_ard:pay_gold(Req);
handle('GET', "pay_qtld_chk_ard"++_, Req, _DocRoot) ->
    pay_mod_qtld_chk_ard:pay_gold(Req);
handle('GET', "pay_qtld_ios"++_, Req, _DocRoot) ->
    pay_mod_qtld_ios:pay_gold(Req);
handle('GET', "paymmy"++_, Req, _DocRoot) ->
	pay_mod_mmy:pay_gold(Req);
handle('GET', Path, Req, DocRoot)   ->
    Req:serve_file(Path, DocRoot).
