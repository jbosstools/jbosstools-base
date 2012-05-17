package org.jboss.tools.runtime.as.ui.bot.test.epp5;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEPP5 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEPP5.SERVER_ID;
	}
}
