package org.jboss.tools.runtime.as.ui.bot.test.epp4;

import org.jboss.tools.runtime.as.ui.bot.test.template.OperateServerTemplate;

public class OperateEPP4 extends OperateServerTemplate {

	@Override
	protected String getServerName() {
		return DetectEPP4.SERVER_ID;
	}
	
	@Override
	protected void assertNoException(String message) {
		// do not check the exception - it will be there
	}
}
