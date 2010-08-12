package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;

/**
 * removes server and server runtime
 * @author lzoubek@redhat.com
 *
 */
public class RemoveServer extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return !SWTTestExt.configuredState.getServer().isConfigured;
	}

	@Override
	public void handle() {
		if (!checkFulfilled()) {
			SWTTestExt.servers.deleteServer(SWTTestExt.configuredState.getServer().name);
			SWTTestExt.eclipse.removeServerRuntime(SWTTestExt.configuredState.getServer().name);
			SWTTestExt.configuredState.getServer().isConfigured=false;
			SWTTestExt.configuredState.getServer().name=null;
			SWTTestExt.configuredState.getServer().version=null;
			SWTTestExt.configuredState.getServer().type=null;
			SWTTestExt.configuredState.getServer().withJavaVersion=null;
			SWTTestExt.configuredState.getServer().bundledESBVersion=null;
		}
	}
}
