%%%-----------------------------------
%%% @Module  : pt
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.04.29
%%% @Description: 协议公共函数
%%%-----------------------------------
-module(pt).
-export([
            read_string/1,
			write_id_list/3,
            pack/2,
            write_string/1,
			write_id_byte/2,
			binary_to_id_list/4,
			write_equip_wing_horse/1	
        ]).
-include("common.hrl").

%% 将一id集合的列表打包，其中BitSize指明了每个id的位大小，如32位或是16位
write_id_list(0, [], _BitSize) -> <<>>;
write_id_list(Size, [Id | Rest], BitSize) ->
	RestData = write_id_list(Size - 1, Rest, BitSize),
	<<Id:BitSize, RestData/binary>>.

%% 把列表写入bit
write_id_byte(List,Len) ->
	write_id_byte(List,1,<<>>,Len).

write_id_byte([],N,Bin,Len) ->
	if
		N =:= Len ->
			FillL = 0;
		true ->
			FillL = Len - (N -1 )	
	end,
	<<Bin/bitstring,0:FillL>>;

write_id_byte([H|T],N,Bin,Len) ->
	if 
		H =:= N ->
			write_id_byte(T,N+1,<<Bin/bitstring,1:1>>,Len);
		true ->
			write_id_byte([H|T],N+1,<<Bin/bitstring,0:1>>,Len)
	end.

%% 读取整型的id二进制数据，Size为整型id的个数，BitSize整型的位大小
binary_to_id_list(0, _BitSize, <<>>, IdList) -> IdList; 
binary_to_id_list(Size, BitSize, BinData, IdList) ->
	<<Id:BitSize, Rest/binary>> = BinData,
	binary_to_id_list(Size - 1, BitSize, Rest, [Id | IdList]).

%% 打包信息，添加消息头
pack(Cmd, Data) ->
	if (Cmd =:= 20107 orelse Cmd =:= 11401 orelse 
	    Cmd =:= 11402 orelse Cmd =:= 11400) ->
		ok;
	true ->
		?INFO(pack, "packing data ~w, ~w", [Cmd, Data])
	end,
    L = byte_size(Data) + 4,
    <<L:16, Cmd:16, Data/binary>>.

%% 打包一个字符串
-spec write_string(list()) -> binary().
write_string(Str) ->
	BinStr = list_to_binary(Str),
    Len = byte_size(BinStr),
    <<Len:16, BinStr/binary>>.

%% 读取字符串
%% 参数：Bin -> 为由经过write_string格式打包的字符串的list的二进制
%% 返回值：{读出的第一个string, 剩余的二进制数据}
-spec read_string(binary()) -> {list(), binary()}.
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.

%% 类别定义：
%%    坐骑：1，翅膀：2，武器：3，铠甲：4，披风：5，鞋子：6，戒指：7
write_equip_wing_horse(DataList) ->
	?INFO(scene, "DataList = ~w", [DataList]),
	write_equip_wing_horse(DataList, <<(length(DataList)):16>>).
write_equip_wing_horse([{wing, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 2:8, Datat:32>>);
write_equip_wing_horse([{horse, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 1:8, Datat:32>>);	
write_equip_wing_horse([{weapon, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 3:8, Datat:32>>);
write_equip_wing_horse([{kaijia, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 4:8, Datat:32>>);
write_equip_wing_horse([{pifeng, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 5:8, Datat:32>>);
write_equip_wing_horse([{shoes, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 6:8, Datat:32>>);
write_equip_wing_horse([{ring, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 7:8, Datat:32>>);
write_equip_wing_horse([{ability, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 8:8, Datat:32>>);
write_equip_wing_horse([{fashion, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 9:8, Datat:32>>);
write_equip_wing_horse([{king, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 10:8, Datat:32>>);
write_equip_wing_horse([{chief_disciple, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 11:8, Datat:32>>);
write_equip_wing_horse([{speed, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 12:8, Datat:32>>);
write_equip_wing_horse([{change, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 13:8, Datat:32>>);
write_equip_wing_horse([{role, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 20:8, Datat:32>>);
write_equip_wing_horse([{supremacy_title, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 30:8, Datat:32>>);
write_equip_wing_horse([{legend_title, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 31:8, Datat:32>>);
write_equip_wing_horse([{achievement_title, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 32:8, Datat:32>>);
write_equip_wing_horse([{honour_title, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 33:8, Datat:32>>);
write_equip_wing_horse([{special_title, Datat} | Rest], Packet) ->
	write_equip_wing_horse(Rest, <<Packet/binary, 34:8, Datat:32>>);
write_equip_wing_horse([], Packet) -> Packet.

