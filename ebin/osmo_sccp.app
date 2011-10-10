{application, osmo_sccp,
	[{description, "Osmocom SCCP Server"},
	 {vsn, "1"},
	 {modules, [	osmo_sccp,
			osmo_sccp_app,
			osmo_sccp_sup,
			sccp_links,
			sccp_link_ipa_client,
			sccp_link_m3ua,
			sccp_routing,
			sccp_user
		]},
	 {registered, [osmo_sccp_app]},
	 {mod, {osmo_sccp_app, []}},
	 {applications, []},
	 {env, [
	  ]}
]}.
