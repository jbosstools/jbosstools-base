package org.jboss.tools.runtime.as.ui.bot.test.server.epp4;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEPP4 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-epp-4.3";
	
	@Override
	protected String getRuntimeID() {
		return SERVER_ID;
	}

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName(getRuntimeID());
		server.setType("EPP");
		server.setVersion("4.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getRuntimeID()));
		return server;
	}
}
