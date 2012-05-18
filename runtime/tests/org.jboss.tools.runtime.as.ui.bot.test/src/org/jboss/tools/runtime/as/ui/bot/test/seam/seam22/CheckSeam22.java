package org.jboss.tools.runtime.as.ui.bot.test.seam.seam22;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.CheckSeamRuntimeTemplate;

public class CheckSeam22 extends CheckSeamRuntimeTemplate {

	@Override
	protected Runtime getExpectedRuntime() {
		Runtime server = new Runtime();
		server.setName("Seam " + DetectSeam22.SEAM_ID + " 2.2");
		server.setVersion("2.2");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(DetectSeam22.SEAM_ID));
		return server;
	}
}
