package org.jboss.tools.runtime.as.ui.bot.test.server.eap4;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEAP4 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-eap-4.3";
	
	@Override
	protected String getServerRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedServerRuntime() {
		Runtime server = new Runtime();
		server.setName(getServerRuntimeID());
		server.setType("EAP");
		server.setVersion("4.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		return server;
	}
}
