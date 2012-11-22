
-module(data_skill_table).
-compile(export_all).

-include("common.hrl").

-spec get(SkillId :: integer(), Level :: integer()) -> #battle_skill{} | ?UNDEFINED.
	
get(100, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 100,	
		level  = 1,
		target = enemy,
		type   = other,
		param  = {1}    
	};

get(101, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 101,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.3}    
	};

get(102, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 102,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.3}    
	};

get(103, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 103,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.1}    
	};

get(104, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.4}    
	};

get(104, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {0.45}    
	};

get(104, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {0.5}    
	};

get(104, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {0.55}    
	};

get(104, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {0.6}    
	};

get(104, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {0.65}    
	};

get(104, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {0.7}    
	};

get(104, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {0.75}    
	};

get(104, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {0.82}    
	};

get(104, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 104,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {0.9}    
	};

get(105, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 1,
		target = friend,
		type   = common,
		param  = {0.2}    
	};

get(105, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 2,
		target = friend,
		type   = common,
		param  = {0.23}    
	};

get(105, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 3,
		target = friend,
		type   = common,
		param  = {0.26}    
	};

get(105, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 4,
		target = friend,
		type   = common,
		param  = {0.29}    
	};

get(105, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 5,
		target = friend,
		type   = common,
		param  = {0.32}    
	};

get(105, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 6,
		target = friend,
		type   = common,
		param  = {0.35}    
	};

get(105, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 7,
		target = friend,
		type   = common,
		param  = {0.38}    
	};

get(105, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 8,
		target = friend,
		type   = common,
		param  = {0.42}    
	};

get(105, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 9,
		target = friend,
		type   = common,
		param  = {0.46}    
	};

get(105, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 105,	
		level  = 10,
		target = friend,
		type   = common,
		param  = {0.5}    
	};

get(106, 1) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1.1}    
	};

get(106, 2) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1.13}    
	};

get(106, 3) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1.15}    
	};

get(106, 4) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1.18}    
	};

get(106, 5) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1.22}    
	};

get(106, 6) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1.26}    
	};

get(106, 7) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1.31}    
	};

get(106, 8) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1.36}    
	};

get(106, 9) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1.42}    
	};

get(106, 10) -> 
	#battle_skill {
		hp     = 0.1,       
		mp     = 0,
		cd     = 1,     
		id     = 106,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1.5}    
	};

get(107, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.2}    
	};

get(107, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {1,0.23}    
	};

get(107, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {1,0.26}    
	};

get(107, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {1,0.29}    
	};

get(107, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {1,0.32}    
	};

get(107, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {1,0.35}    
	};

get(107, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {1,0.38}    
	};

get(107, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {1,0.42}    
	};

get(107, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {1,0.46}    
	};

get(107, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 107,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {1,0.5}    
	};

get(108, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 1,
		target = self,
		type   = common,
		param  = {0.05}    
	};

get(108, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 2,
		target = self,
		type   = common,
		param  = {0.06}    
	};

get(108, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 3,
		target = self,
		type   = common,
		param  = {0.07}    
	};

get(108, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 4,
		target = self,
		type   = common,
		param  = {0.08}    
	};

get(108, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 5,
		target = self,
		type   = common,
		param  = {0.09}    
	};

get(108, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 6,
		target = self,
		type   = common,
		param  = {0.1}    
	};

get(108, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 7,
		target = self,
		type   = common,
		param  = {0.11}    
	};

get(108, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 8,
		target = self,
		type   = common,
		param  = {0.12}    
	};

get(108, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 9,
		target = self,
		type   = common,
		param  = {0.13}    
	};

get(108, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 108,	
		level  = 10,
		target = self,
		type   = common,
		param  = {0.15}    
	};

get(109, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.8,0.7,0,0}    
	};

get(109, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0,0}    
	};

get(109, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.4,0}    
	};

get(109, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.45,0}    
	};

get(109, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0}    
	};

get(109, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0.25}    
	};

get(109, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0.3}    
	};

get(109, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0.35}    
	};

get(109, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0.4}    
	};

get(109, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 109,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {0.8,0.75,0.5,0.45}    
	};

get(110, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.6}    
	};

get(110, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {0.62}    
	};

get(110, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {0.64}    
	};

get(110, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {0.66}    
	};

get(110, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {0.68}    
	};

get(110, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {0.7}    
	};

get(110, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {0.72}    
	};

get(110, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {0.74}    
	};

get(110, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {0.77}    
	};

get(110, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 110,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {0.80}    
	};

get(111, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1,0.1,2}    
	};

get(111, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1,0.12,2}    
	};

get(111, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1,0.14,2}    
	};

get(111, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1,0.16,2}    
	};

get(111, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1,0.18,2}    
	};

get(111, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1,0.2,3}    
	};

get(111, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1,0.22,3}    
	};

get(111, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1,0.24,3}    
	};

get(111, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1,0.27,3}    
	};

get(111, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 111,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1,0.3,3}    
	};

get(112, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1,0.3}    
	};

get(112, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1,0.35}    
	};

get(112, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1,0.4}    
	};

get(112, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1,0.45}    
	};

get(112, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1,0.5}    
	};

get(112, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1,0.55}    
	};

get(112, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1,0.6}    
	};

get(112, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1,0.65}    
	};

get(112, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1,0.72}    
	};

get(112, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 112,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1,0.8}    
	};

get(113, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1.00,1}    
	};

get(113, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1.05,1}    
	};

get(113, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1.1,1}    
	};

get(113, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1.15,1}    
	};

get(113, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1.2,1}    
	};

get(113, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1.25,1}    
	};

get(113, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1.3,1}    
	};

get(113, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1.35,1}    
	};

get(113, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1.42,1}    
	};

get(113, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 113,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1.5,1}    
	};

get(114, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.9}    
	};

get(114, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {0.95}    
	};

get(114, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {1}    
	};

get(114, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {1.03}    
	};

get(114, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {1.06}    
	};

get(114, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {1.09}    
	};

get(114, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {1.12}    
	};

get(114, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {1.15}    
	};

get(114, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {1.18}    
	};

get(114, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 114,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {1.21}    
	};

get(115, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.6,0.3,100}    
	};

get(115, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 2,
		target = enemy,
		type   = super,
		param  = {0.62,0.3,200}    
	};

get(115, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 3,
		target = enemy,
		type   = super,
		param  = {0.64,0.3,350}    
	};

get(115, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 4,
		target = enemy,
		type   = super,
		param  = {0.66,0.3,500}    
	};

get(115, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 5,
		target = enemy,
		type   = super,
		param  = {0.68,0.3,650}    
	};

get(115, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 6,
		target = enemy,
		type   = super,
		param  = {0.7,0.3,800}    
	};

get(115, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 7,
		target = enemy,
		type   = super,
		param  = {0.72,0.3,1000}    
	};

get(115, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 8,
		target = enemy,
		type   = super,
		param  = {0.74,0.3,1200}    
	};

get(115, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 9,
		target = enemy,
		type   = super,
		param  = {0.77,0.3,1400}    
	};

get(115, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 60,
		cd     = 0,     
		id     = 115,	
		level  = 10,
		target = enemy,
		type   = super,
		param  = {0.8,0.3,1600}    
	};

get(116, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1,0.1,2}    
	};

get(116, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1,0.12,2}    
	};

get(116, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1,0.14,2}    
	};

get(116, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1,0.16,2}    
	};

get(116, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1,0.18,2}    
	};

get(116, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1,0.2,2}    
	};

get(116, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1,0.22,2}    
	};

get(116, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1,0.24,2}    
	};

get(116, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1,0.27,2}    
	};

get(116, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 1,     
		id     = 116,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1,0.3,2}    
	};

get(117, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1,0.1}    
	};

get(117, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1.02,0.15}    
	};

get(117, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1.04,0.2}    
	};

get(117, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1.06,0.25}    
	};

get(117, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1.08,0.3}    
	};

get(117, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1.1,0.35}    
	};

get(117, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1.12,0.4}    
	};

get(117, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1.14,0.45}    
	};

get(117, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1.17,0.5}    
	};

get(117, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 117,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1.2,0.55}    
	};

get(118, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {0.75}    
	};

get(118, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {0.8}    
	};

get(118, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {0.85}    
	};

get(118, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {0.9}    
	};

get(118, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {0.95}    
	};

get(118, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1}    
	};

get(118, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1.05}    
	};

get(118, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1.1}    
	};

get(118, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1.15}    
	};

get(118, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 3,     
		id     = 118,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1.2}    
	};

get(119, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1,0.1}    
	};

get(119, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 2,
		target = enemy,
		type   = common,
		param  = {1.02,0.1}    
	};

get(119, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 3,
		target = enemy,
		type   = common,
		param  = {1.04,0.1}    
	};

get(119, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 4,
		target = enemy,
		type   = common,
		param  = {1.08,0.1}    
	};

get(119, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 5,
		target = enemy,
		type   = common,
		param  = {1.1,0.1}    
	};

get(119, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 6,
		target = enemy,
		type   = common,
		param  = {1.12,0.1}    
	};

get(119, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 7,
		target = enemy,
		type   = common,
		param  = {1.14,0.1}    
	};

get(119, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 8,
		target = enemy,
		type   = common,
		param  = {1.16,0.1}    
	};

get(119, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 9,
		target = enemy,
		type   = common,
		param  = {1.18,0.1}    
	};

get(119, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 2,     
		id     = 119,	
		level  = 10,
		target = enemy,
		type   = common,
		param  = {1.2,0.1}    
	};

get(201, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.2,0.4}    
	};

get(201, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.24,0.4}    
	};

get(201, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.28,0.4}    
	};

get(201, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.32,0.4}    
	};

get(201, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.36,0.4}    
	};

get(201, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.4,0.4}    
	};

get(201, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.45,0.4}    
	};

get(201, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.5,0.4}    
	};

get(201, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.55,0.4}    
	};

get(201, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 201,	
		level  = 10,
		target = self,
		type   = other,
		param  = {0.6,0.4}    
	};

get(202, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.05,0.08}    
	};

get(202, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.065,0.08}    
	};

get(202, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.08,0.08}    
	};

get(202, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.095,0.08}    
	};

get(202, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.11,0.08}    
	};

get(202, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.125,0.08}    
	};

get(202, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.14,0.08}    
	};

get(202, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.16,0.08}    
	};

get(202, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.18,0.08}    
	};

get(202, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 202,	
		level  = 10,
		target = self,
		type   = other,
		param  = {0.2,0.08}    
	};

get(203, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.052,0.9,0.4}    
	};

get(203, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.074,0.9,0.4}    
	};

get(203, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.096,0.9,0.4}    
	};

get(203, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.118,0.9,0.4}    
	};

get(203, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.14,0.9,0.4}    
	};

get(203, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.162,0.9,0.4}    
	};

get(203, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.184,0.9,0.4}    
	};

get(203, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.206,0.9,0.4}    
	};

get(203, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.228,0.9,0.4}    
	};

get(203, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 203,	
		level  = 10,
		target = self,
		type   = other,
		param  = {0.25,0.9,0.4}    
	};

get(204, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(204, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 204,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(205, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.08,0.06}    
	};

get(205, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.1,0.08}    
	};

get(205, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.12,0.10}    
	};

get(205, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.14,0.12}    
	};

get(205, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.16,0.14}    
	};

get(205, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.18,0.16}    
	};

get(205, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.2,0.18}    
	};

get(205, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.22,0.21}    
	};

get(205, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.24,0.23}    
	};

get(205, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 205,	
		level  = 10,
		target = self,
		type   = other,
		param  = {0.26,0.25}    
	};

get(206, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.06,0.06}    
	};

get(206, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.08,0.08}    
	};

get(206, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.1,0.10}    
	};

get(206, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.12,0.12}    
	};

get(206, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.14,0.14}    
	};

get(206, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.16,0.16}    
	};

get(206, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.18,0.18}    
	};

get(206, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.2,0.20}    
	};

get(206, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.22,0.22}    
	};

get(206, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 206,	
		level  = 10,
		target = self,
		type   = other,
		param  = {0.25,0.25}    
	};

get(207, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 1,
		target = self,
		type   = other,
		param  = {10,0.08}    
	};

get(207, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 2,
		target = self,
		type   = other,
		param  = {12,0.10}    
	};

get(207, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 3,
		target = self,
		type   = other,
		param  = {14,0.12}    
	};

get(207, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 4,
		target = self,
		type   = other,
		param  = {16,0.14}    
	};

get(207, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 5,
		target = self,
		type   = other,
		param  = {18,0.16}    
	};

get(207, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 6,
		target = self,
		type   = other,
		param  = {20,0.18}    
	};

get(207, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 7,
		target = self,
		type   = other,
		param  = {22,0.21}    
	};

get(207, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 8,
		target = self,
		type   = other,
		param  = {24,0.24}    
	};

get(207, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 9,
		target = self,
		type   = other,
		param  = {27,0.27}    
	};

get(207, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 207,	
		level  = 10,
		target = self,
		type   = other,
		param  = {30,0.3}    
	};

get(208, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(208, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 208,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(209, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 209,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(210, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 210,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(211, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 211,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(212, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 212,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(213, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 213,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(214, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 1,
		target = self,
		type   = other,
		param  = {5}    
	};

get(214, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 2,
		target = self,
		type   = other,
		param  = {10}    
	};

get(214, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 3,
		target = self,
		type   = other,
		param  = {15}    
	};

get(214, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 4,
		target = self,
		type   = other,
		param  = {20}    
	};

get(214, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 5,
		target = self,
		type   = other,
		param  = {25}    
	};

get(214, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 6,
		target = self,
		type   = other,
		param  = {30}    
	};

get(214, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 7,
		target = self,
		type   = other,
		param  = {35}    
	};

get(214, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 8,
		target = self,
		type   = other,
		param  = {40}    
	};

get(214, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 9,
		target = self,
		type   = other,
		param  = {45}    
	};

get(214, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 214,	
		level  = 10,
		target = self,
		type   = other,
		param  = {50}    
	};

get(215, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 1,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 2,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 3,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 4,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 5,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 6,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 7,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 8,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 9,
		target = self,
		type   = other,
		param  = {0.2}    
	};

get(215, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 215,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(216, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 216,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(217, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 217,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(218, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 218,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(219, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 219,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(220, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 220,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(221, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 221,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(222, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 222,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(223, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 223,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.3}    
	};

get(224, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 224,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.4}    
	};

get(225, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 225,	
		level  = 1,
		target = self,
		type   = super,
		param  = {0.5}    
	};

get(226, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 226,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.9,0.25}    
	};

get(227, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 227,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.9,1}    
	};

get(228, 1) -> 
	#battle_skill {
		hp     = 0.15,       
		mp     = 100,
		cd     = 0,     
		id     = 228,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.5}    
	};

get(229, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 229,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.4}    
	};

get(230, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 230,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.6}    
	};

get(231, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 231,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.8}    
	};

get(232, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 232,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1}    
	};

get(233, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 233,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {}    
	};

get(234, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 234,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.3,1.1}    
	};

get(235, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 235,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.7}    
	};

get(236, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 236,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.7}    
	};

get(237, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 237,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.5,0.3,25}    
	};

get(238, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 238,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.9,0.7}    
	};

get(239, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 239,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.15,0.9}    
	};

get(240, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 240,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.3,1,1}    
	};

get(241, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 241,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.6,0.3,0.5}    
	};

get(242, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 242,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {20}    
	};

get(243, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 243,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {0.4,0.2,0.2}    
	};

get(244, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 244,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {}    
	};

get(245, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 245,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {0.3,0.3}    
	};

get(246, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 246,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {}    
	};

get(247, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 247,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {}    
	};

get(248, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 248,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {0.17}    
	};

get(249, 1) -> 
	#battle_skill {
		hp     = 0.05,       
		mp     = 100,
		cd     = 1,     
		id     = 249,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.25}    
	};

get(250, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 3,     
		id     = 250,	
		level  = 1,
		target = self,
		type   = super,
		param  = {0.09}    
	};

get(251, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 251,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.8,0.6,0.28}    
	};

get(252, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 252,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.68}    
	};

get(253, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 253,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.35,3}    
	};

get(254, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 254,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.75}    
	};

get(255, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 255,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.2,0.21}    
	};

get(256, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 256,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.21}    
	};

get(257, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 257,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.17}    
	};

get(258, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 258,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.1,0.22}    
	};

get(259, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 259,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.85}    
	};

get(260, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 260,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.5}    
	};

get(261, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 1,     
		id     = 261,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {2}    
	};

get(262, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 262,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1}    
	};

get(263, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 263,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.5,1}    
	};

get(264, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 264,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.9,0.5,0.5}    
	};

get(265, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 265,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.99,1}    
	};

get(266, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 266,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1}    
	};

get(267, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 267,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.5}    
	};

get(268, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 268,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,0.2}    
	};

get(269, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 269,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {}    
	};

get(270, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 270,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1,2}    
	};

get(271, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 271,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {0.5}    
	};

get(272, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 272,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1}    
	};

get(273, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 273,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {3}    
	};

get(274, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 274,	
		level  = 1,
		target = self,
		type   = super,
		param  = {0.5}    
	};

get(275, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 275,	
		level  = 1,
		target = friend,
		type   = super,
		param  = {0.5,2}    
	};

get(276, 1) -> 
	#battle_skill {
		hp     = 0.2,       
		mp     = 100,
		cd     = 4,     
		id     = 276,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1.5}    
	};

get(277, 1) -> 
	#battle_skill {
		hp     = 0.2,       
		mp     = 100,
		cd     = 0,     
		id     = 277,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.3}    
	};

get(278, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 4,     
		id     = 278,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {0.5,2}    
	};

get(279, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 4,     
		id     = 279,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {2}    
	};

get(280, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 4,     
		id     = 280,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1.2}    
	};

get(281, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 4,     
		id     = 281,	
		level  = 1,
		target = enemy,
		type   = common,
		param  = {1.2}    
	};

get(282, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 282,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.3,2}    
	};

get(283, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 283,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.1}    
	};

get(284, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 284,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.1}    
	};

get(285, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 285,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.5}    
	};

get(286, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 286,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {0.5,2}    
	};

get(287, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 287,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {2}    
	};

get(288, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 288,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.2}    
	};

get(289, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 100,
		cd     = 0,     
		id     = 289,	
		level  = 1,
		target = enemy,
		type   = super,
		param  = {1.2}    
	};

get(301, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(301, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 301,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(302, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 302,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(303, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 303,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(304, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 304,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(305, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 305,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(306, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 306,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(307, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 307,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(308, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 308,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(309, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 309,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(310, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 310,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(311, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 311,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(312, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 312,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(313, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 313,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 1) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 1,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 2) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 2,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 3) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 3,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 4) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 4,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 5) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 5,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 6) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 6,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 7) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 7,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 8) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 8,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 9) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 9,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 10) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 10,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 11) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 11,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 12) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 12,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 13) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 13,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 14) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 14,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 15) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 15,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 16) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 16,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 17) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 17,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 18) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 18,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 19) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 19,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 20) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 20,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 21) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 21,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 22) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 22,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 23) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 23,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 24) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 24,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 25) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 25,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 26) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 26,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 27) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 27,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 28) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 28,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 29) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 29,
		target = self,
		type   = other,
		param  = {}    
	};

get(314, 30) -> 
	#battle_skill {
		hp     = 0,       
		mp     = 0,
		cd     = 0,     
		id     = 314,	
		level  = 30,
		target = self,
		type   = other,
		param  = {}    
	};


get(_, _) ->
	#battle_skill {
		hp     = 0,			   
		mp     = 0,   
		cd     = 0,
		id     = 1,
		level  = 0,
		type   = other,
		target = enemy   
	}.