package org.jboss.tools.runtime.as.ui.bot.test.jboss71;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectServerTemplate;

public class DetectJBoss71 extends DetectServerTemplate {

	public static final String JBOSS_7_1 = "jboss-as-7.1.0.Final";
	
	@Override
	protected String getServerID() {
		return JBOSS_7_1;
	}
	
	@Override
	protected Server getExpectedServer() {
		Server expectedServer = new Server();
		expectedServer.setName(JBOSS_7_1);
		expectedServer.setVersion("7.1");
		expectedServer.setType("AS");
		expectedServer.setLocation(RuntimeProperties.getInstance().getRuntimePath(JBOSS_7_1));
		return expectedServer;
	}
}
