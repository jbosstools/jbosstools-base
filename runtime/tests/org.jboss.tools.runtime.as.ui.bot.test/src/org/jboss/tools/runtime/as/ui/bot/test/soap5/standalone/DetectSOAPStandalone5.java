package org.jboss.tools.runtime.as.ui.bot.test.soap5.standalone;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectServerTemplate;

public class DetectSOAPStandalone5 extends DetectServerTemplate {

	public static final String SERVER_ID = "jboss-soa-p-standalone-5";
	
	@Override
	protected String getServerID() {
		return SERVER_ID;
	}
	
	@Override
	protected Server getExpectedServer() {
		Server expectedServer = new Server();
		expectedServer.setName(SERVER_ID);
		expectedServer.setVersion("5.2");
		expectedServer.setType("SOA-P-STD");
		expectedServer.setLocation(RuntimeProperties.getInstance().getRuntimePath(SERVER_ID));
		return expectedServer;
	}
}
