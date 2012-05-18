package org.jboss.tools.runtime.as.ui.bot.test.server.jboss7;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectJBoss7 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-as-7.1.1.Final";
	
	@Override
	protected String getRuntimeID() {
		return SERVER_ID;
	}
	
	@Override
	protected Runtime getExpectedRuntime() {
		Runtime expectedServer = new Runtime();
		expectedServer.setName(SERVER_ID);
		expectedServer.setVersion("7.1");
		expectedServer.setType("AS");
		expectedServer.setLocation(RuntimeProperties.getInstance().getRuntimePath(SERVER_ID));
		return expectedServer;
	}
}
