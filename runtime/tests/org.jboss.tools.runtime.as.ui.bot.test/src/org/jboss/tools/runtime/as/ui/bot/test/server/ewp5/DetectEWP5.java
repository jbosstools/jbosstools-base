package org.jboss.tools.runtime.as.ui.bot.test.server.ewp5;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectServerTemplate;

public class DetectEWP5 extends DetectServerTemplate {

	public static final String SERVER_ID = "jboss-ewp-5.1";
	
	@Override
	protected String getServerID() {
		return SERVER_ID;
	}

	@Override
	protected Server getExpectedServer() {
		Server server = new Server();
		server.setName(getServerID());
		server.setType("EWP");
		server.setVersion("5.1");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerID()));
		return server;
	}
}
