package org.jboss.tools.runtime.as.ui.bot.test.detector.server.jboss7;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateJBoss7 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectJBoss7.SERVER_ID;
	}
}
