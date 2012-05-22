package org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap4;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEAP4 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEAP4.SERVER_ID;
	}
}
