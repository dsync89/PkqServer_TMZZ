-record(p_account, {
		account
		,password
		,latestServerList
		}).

-record(server,
        {
         id             % 游戏服务器区号
        ,serverID       % 游戏服务器实际ID
        ,serverName     % 游戏服务器名称
        ,serverIP       % 游戏服务器IP
        ,serverPort     % 游戏服务器游戏端口
        ,serverHttpPort  % 游戏服务器开放的http端口
        ,serverKey      % 游戏服务器连接帐号服务器的key
        ,state=1        %服务器状态
        }).
		 