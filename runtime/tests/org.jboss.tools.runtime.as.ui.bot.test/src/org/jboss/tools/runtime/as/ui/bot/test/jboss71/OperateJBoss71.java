package org.jboss.tools.runtime.as.ui.bot.test.jboss71;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateJBoss71 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectJBoss71.JBOSS_7_1;
	}
}
