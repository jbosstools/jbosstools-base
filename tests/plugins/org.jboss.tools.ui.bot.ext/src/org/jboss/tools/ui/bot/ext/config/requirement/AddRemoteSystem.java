package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

public class AddRemoteSystem extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getRemoteSystem().isConfigured && 
		SWTTestExt.configuredState.getRemoteSystem().remoteHost.equals(TestConfigurator.currentConfig.getServer().remoteSystem);
	}

	@Override
	public void handle() {
		SWTTestExt.eclipse.setSSHKey(TestConfigurator.currentConfig.getRemoteSystem().key);
		String hostname = TestConfigurator.currentConfig.getRemoteSystem().host;
		SWTTestExt.eclipse.addRemoteSystem(hostname, hostname);
		SWTTestExt.configuredState.getRemoteSystem().isConfigured=true;
		SWTTestExt.configuredState.getRemoteSystem().remoteHost=hostname;
		SWTTestExt.configuredState.getRemoteSystem().remoteUser = TestConfigurator.currentConfig.getRemoteSystem().user;
	}

}
