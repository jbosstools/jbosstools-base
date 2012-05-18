package org.jboss.tools.runtime.as.ui.bot.test.seam.seam23;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectSeam23 extends DetectRuntimeTemplate {

	public static final String SEAM_ID = "jboss-seam-2.3.0.Beta1";
	
	@Override
	protected String getRuntimeID() {
		return SEAM_ID;
	}

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName(getRuntimeID());
		server.setType("SEAM");
		server.setVersion("2.3.0.Beta1");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getRuntimeID()));
		return server;
	}
}
