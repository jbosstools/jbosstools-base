package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

public class AddJBPM extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getJBPM().isConfigured;
	}

	@Override
	public void handle() {
		String jbpmName = "JBPM-"+TestConfigurator.currentConfig.getJBPM().version;
		SWTTestExt.eclipse.addJBPMRuntime(jbpmName, TestConfigurator.currentConfig.getJBPM().version, TestConfigurator.currentConfig.getJBPM().jbpmHome);
		SWTTestExt.configuredState.getJBPM().isConfigured=true;
		SWTTestExt.configuredState.getJBPM().name=jbpmName;
		SWTTestExt.configuredState.getJBPM().version=TestConfigurator.currentConfig.getJBPM().version;	

	}

}
