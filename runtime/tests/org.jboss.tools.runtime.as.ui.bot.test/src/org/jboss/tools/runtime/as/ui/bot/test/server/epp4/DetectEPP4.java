package org.jboss.tools.runtime.as.ui.bot.test.server.epp4;

import java.util.Arrays;
import java.util.List;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectEPP4 extends DetectRuntimeTemplate {

	public static final String SERVER_ID = "jboss-epp-4.3";
	
	@Override
	protected String getPathID() {
		return SERVER_ID;
	}

	@Override
	protected List<Runtime> getExpectedRuntimes() {
		Runtime server = new Runtime();
		server.setName(getPathID());
		server.setType("EPP");
		server.setVersion("4.3");
		server.setLocation(RuntimeProperties.getInstance().getRuntimePath(getPathID()));

		Runtime seam = new Runtime();
		seam.setName("seam2");
		seam.setType("SEAM");
		seam.setVersion("2.0.2-FP_SEC1");
		seam.setLocation(RuntimeProperties.getInstance().getRuntimePath(getPathID()) + "/seam2");
		
		return Arrays.asList(server, seam);
	}
}
