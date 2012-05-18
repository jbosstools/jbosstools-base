package org.jboss.tools.runtime.as.ui.bot.test.server.epp4;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectServerTemplate;

public class DetectEPP4 extends DetectServerTemplate {

	public static final String SERVER_ID = "jboss-epp-4.3";
	
	@Override
	protected String getServerID() {
		return SERVER_ID;
	}

	@Override
	protected Server getExpectedServer() {
		Server server = new Server();
		server.setName(getServerID());
		server.setType("EPP");
		server.setVersion("4.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerID()));
		return server;
	}
}
