package org.jboss.tools.runtime.as.ui.bot.test.seam.seam22;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectSeam22 extends DetectRuntimeTemplate {

	public static final String SEAM_ID = "jboss-seam-2.2.2.Final";
	
	public static final String VERSION = "2.2.2.Final";
	
	@Override
	protected String getServerRuntimeID() {
		return SEAM_ID;
	}

	@Override
	protected Runtime getExpectedServerRuntime() {
		Runtime server = new Runtime();
		server.setName(getServerRuntimeID());
		server.setType("SEAM");
		server.setVersion(VERSION);
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		return server;
	}
}
