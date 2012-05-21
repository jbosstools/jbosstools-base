package org.jboss.tools.runtime.as.ui.bot.test.server.epp4;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEPP4 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-epp-4.3";
	
	@Override
	protected String getServerRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedServerRuntime() {
		Runtime server = new Runtime();
		server.setName(getServerRuntimeID());
		server.setType("EPP");
		server.setVersion("4.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		return server;
	}
}
