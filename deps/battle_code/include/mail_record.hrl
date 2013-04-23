-record(mail, {
		mail_id,
		sender_name,
		sender_id,
		mail_type,
		mail_title,
		mail_status = 1,
		timestamp,
		attachment_status = 2,
		goods_list = [],
		mail_content,
		silver_status,
		silver = 0,
		gold_status,
		gold = 0,
		bind_gold_status,
		bind_gold = 0,
		jungong_status,
		jungong = 0,
		reputation_status,
		reputation = 0,
		junliang_status,
		junliang = 0,
		exp_status,
		exp = 0,
		goods_isbind = 0,
		from = 0
			   }).
			   
-record(mail_state, {
		max_mail_id = 0
	}).
	
-record(ets_global_mail, {
		gd_mail_id = 0, 
		type = 10001, 
		gd_sender_name, 
		gd_title, 
		gd_content, 
		gd_MailSendTime = 0,
		gd_mail_expired_time = 0, 
		gd_Attachment = [], 
		gd_Attachment_isbind = 1, 
		gd_mail_gold = 0, 
		gd_mail_bind_gold = 0, 
		gd_mail_bind_silver = 0, 
		gd_mail_jungong = 0,
		gd_mail_reputation = 0,
		gd_mail_junliang = 0,
		gd_mail_exp = 0
	}).