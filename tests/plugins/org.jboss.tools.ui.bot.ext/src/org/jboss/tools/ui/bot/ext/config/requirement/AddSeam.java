package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

public class AddSeam extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getSeam().isConfiured 
				&& SWTTestExt.configuredState.getSeam().version.equals(TestConfigurator.currentConfig.getSeam().version);
	}

	@Override
	public void handle() {
		String seamName = "Seam-"+TestConfigurator.currentConfig.getSeam().version;
		SWTTestExt.eclipse.addSeamRuntime(seamName, TestConfigurator.currentConfig.getSeam().version, TestConfigurator.currentConfig.getSeam().runtimeHome);
		SWTTestExt.configuredState.getSeam().isConfiured=true;
		SWTTestExt.configuredState.getSeam().name=seamName;
		SWTTestExt.configuredState.getSeam().version=TestConfigurator.currentConfig.getSeam().version;		
	}

}
