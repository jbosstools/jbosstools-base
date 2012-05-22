package org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap6;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEAP6 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEAP6.SERVER_ID;
	}
}
