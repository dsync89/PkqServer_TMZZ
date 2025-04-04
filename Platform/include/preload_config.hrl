-define(PRELOAD_CONFIG, [ 
%% 服务器基础配置
{"config/common.config",data_common, key_value, original},

%% 服务器列表配置
{"config/server_list.config",data_server_list, key_value, original},

%% 礼品码奖励配置
{"config/data_gift.config", data_gift, key_value, original},
                        
%% 棱镜渠道代码对应渠道ID,ProductCode，ProductSecret配置
{"config/data_sdk_id.config", data_sdk_id, key_value, original}
						
						]).