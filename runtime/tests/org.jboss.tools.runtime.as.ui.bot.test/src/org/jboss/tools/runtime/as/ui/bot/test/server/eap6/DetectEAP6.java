package org.jboss.tools.runtime.as.ui.bot.test.server.eap6;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEAP6 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-eap-6.0";
	
	@Override
	protected String getRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName(getRuntimeID());
		server.setType("EAP");
		server.setVersion("6.0");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getRuntimeID()));
		return server;
	}
}
