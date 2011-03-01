package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

/**
 * special type of requirement. Does nothing, just copies several properties and its values from {@link TestConfigurator#currentConfig}
 * to {@link SWTTestExt#configuredState}. Such properties are only propagated from config to config state 
 * @author lzoubek
 *
 */
public class SetProperties extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		SWTTestExt.configuredState.setSecureStoragePassword(TestConfigurator.currentConfig.getSecureStorage().password);
		return true;
	}

	@Override
	public void handle() {

	}

}
