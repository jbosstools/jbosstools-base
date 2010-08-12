package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
/**
 * Starts server (as dependent requirement has {@link AddServer}
 * @author lzoubek
 *
 */
public class StartServer extends RequirementBase {

	public StartServer() {
		// define dependency				
		getDependsOn().add(createAddServer());
	}
	
	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getServer().isRunning;
	}

	@Override
	public void handle(){
	  if (!checkFulfilled()){
	    SWTTestExt.servers.startServer(SWTTestExt.configuredState.getServer().name);
	    SWTTestExt.configuredState.getServer().isRunning = true;
	  }
	}
}
