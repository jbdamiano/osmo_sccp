{application, osmo_sccp,
	[{description, "Osmocom SCCP Server"},
	 {vsn, "1"},
	 {modules, [	osmo_sccp,
			osmo_sccp_app,
			osmo_sccp_sup,
			ss7_links,
			ss7_link_ipa_client,
			ss7_link_m3ua,
			sccp_routing,
			sccp_scrc,
			sccp_scoc,
			sccp_user
		]},
	 {registered, [osmo_sccp_app]},
	 {mod, {osmo_sccp_app, []}},
	 {applications, []},
	 {env, [
	  ]}
]}.
