package org.jboss.tools.runtime.as.ui.bot.test.detector.server.soap5;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateSOAP5 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectSOAP5.SERVER_ID;
	}
}
