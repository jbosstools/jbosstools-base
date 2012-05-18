package org.jboss.tools.runtime.as.ui.bot.test.server.jboss7;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectServerTemplate;

public class DetectJBoss7 extends DetectServerTemplate {

	public static final String SERVER_ID = "jboss-as-7.1.1.Final";
	
	@Override
	protected String getServerID() {
		return SERVER_ID;
	}
	
	@Override
	protected Server getExpectedServer() {
		Server expectedServer = new Server();
		expectedServer.setName(SERVER_ID);
		expectedServer.setVersion("7.1");
		expectedServer.setType("AS");
		expectedServer.setLocation(RuntimeProperties.getInstance().getRuntimePath(SERVER_ID));
		return expectedServer;
	}
}
