package org.jboss.tools.runtime.as.ui.bot.test.server.eap5;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEAP5 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-eap-5.1";
	
	@Override
	protected String getRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName(getRuntimeID());
		server.setType("EAP");
		server.setVersion("5.1");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getRuntimeID()));
		return server;
	}
}
