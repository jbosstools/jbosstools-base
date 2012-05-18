package org.jboss.tools.runtime.as.ui.bot.test.server.soap5.standalone;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateSOAPStandalone5 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectSOAPStandalone5.SERVER_ID;
	}
}
