-record(usr, {msisdn, % SIM card phone number
	id, % the customer ID
	status = enabled, % atom(), enabled | disabled
	plan, % atom(), prepay | postpay
	services = []}
). 