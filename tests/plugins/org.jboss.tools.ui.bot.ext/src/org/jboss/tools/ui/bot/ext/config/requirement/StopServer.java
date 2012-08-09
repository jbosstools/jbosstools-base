package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
/**
 * Stops server
 * @author Vladimir Pakan
 *
 */
public class StopServer extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return !SWTTestExt.configuredState.getServer().isRunning;
	}

	@Override
	public void handle(){
		if (SWTTestExt.configuredState.getServer().isRunning){
			if (SWTTestExt.servers.serverExists(SWTTestExt.configuredState.getServer().name)){
				SWTTestExt.servers.stopServer(SWTTestExt.configuredState.getServer().name);	  
			}
			SWTTestExt.configuredState.getServer().isRunning = false;
		}
	}
}
