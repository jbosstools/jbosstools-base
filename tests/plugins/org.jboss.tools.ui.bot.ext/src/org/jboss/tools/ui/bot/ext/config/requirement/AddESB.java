package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

public class AddESB extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getEsb().isConfiured;
	}

	@Override
	public void handle() {
		String esbName = "ESB-"+TestConfigurator.currentConfig.getEsb().version;
		SWTTestExt.eclipse.addESBRuntime(esbName,TestConfigurator.currentConfig.getEsb().version,TestConfigurator.currentConfig.getEsb().runtimeHome);
		SWTTestExt.configuredState.getEsb().isConfiured=true;
		SWTTestExt.configuredState.getEsb().name=esbName;
		SWTTestExt.configuredState.getEsb().version=TestConfigurator.currentConfig.getEsb().version;	

	}

}
