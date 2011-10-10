{application, osmo_sccp,
	[{description, "Osmocom SCCP Server"},
	 {vsn, "1"},
	 {modules, [	osmo_sccp,
			osmo_sccp_app,
			osmo_sccp_sup,
			sccp_routing,
			sccp_scrc,
			sccp_scoc,
			sccp_user
		]},
	 {registered, [osmo_sccp_app]},
	 {mod, {osmo_sccp_app, []}},
	 {applications, [osmo_ss7]},
	 {env, [
	  ]}
]}.
