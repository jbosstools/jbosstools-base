package org.jboss.tools.runtime.as.ui.bot.test.server.epp5;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEPP5 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-epp-5.2";
	
	@Override
	protected String getServerRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedServerRuntime() {
		Runtime server = new Runtime();
		server.setName(getServerRuntimeID());
		server.setType("EPP");
		server.setVersion("5.2");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		return server;
	}
}
