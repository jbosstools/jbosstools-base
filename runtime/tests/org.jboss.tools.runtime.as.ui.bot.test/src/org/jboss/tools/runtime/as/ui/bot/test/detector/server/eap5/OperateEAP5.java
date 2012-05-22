package org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap5;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEAP5 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEAP5.SERVER_ID;
	}
}
