package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.view.RemoteSystems;

public class RemoveRemoteSystem extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return !SWTTestExt.configuredState.getRemoteSystem().isConfigured;
	}

	@Override
	public void handle() {
		String connection = SWTTestExt.configuredState.getRemoteSystem().remoteHost;
		String user = SWTTestExt.configuredState.getRemoteSystem().remoteUser;
		SWTTestExt.eclipse.removeRemoteSystem(connection);
	}

}
