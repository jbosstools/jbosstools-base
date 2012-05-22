package org.jboss.tools.runtime.as.ui.bot.test.seam.seam22;

import java.util.Arrays;
import java.util.List;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.DetectRuntimeTemplate;

public class DetectSeam22 extends DetectRuntimeTemplate {

	public static final String SEAM_ID = "jboss-seam-2.2.2.Final";
	
	public static final String VERSION = "2.2.2.Final";
	
	@Override
	protected String getPathID() {
		return SEAM_ID;
	}

	@Override
	protected List<Runtime> getExpectedRuntimes() {
		Runtime seam = new Runtime();
		seam.setName(getPathID());
		seam.setType("SEAM");
		seam.setVersion(VERSION);
		seam.setLocation(RuntimeProperties.getInstance().getRuntimePath(getPathID()));
		return Arrays.asList(seam);
	}
}
