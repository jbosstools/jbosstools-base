package org.jboss.tools.runtime.as.ui.bot.test.seam.seam23;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.CheckSeamRuntimeTemplate;

public class CheckSeam23 extends CheckSeamRuntimeTemplate {

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName("Seam " + DetectSeam23.SEAM_ID + " 2.3");
		server.setVersion("2.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(DetectSeam23.SEAM_ID));
		return server;
	}
}
