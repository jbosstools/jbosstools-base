package org.jboss.tools.runtime.as.ui.bot.test.server.ewp5;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEWP5 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-ewp-5.1";
	
	@Override
	protected String getServerRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedServerRuntime() {
		Runtime server = new Runtime();
		server.setName(getServerRuntimeID());
		server.setType("EWP");
		server.setVersion("5.1");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		return server;
	}
}
