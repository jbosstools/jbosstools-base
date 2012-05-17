package org.jboss.tools.runtime.as.ui.bot.test.ewp5;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEWP5 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEWP5.SERVER_ID;
	}
}
