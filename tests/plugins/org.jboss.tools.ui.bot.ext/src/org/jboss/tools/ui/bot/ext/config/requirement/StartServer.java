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
		  String server = SWTTestExt.configuredState.getServer().name;
		  String user = SWTTestExt.configuredState.getRemoteSystem().remoteUser;		  
		  SWTTestExt.servers.startServer(server,user,null);
		  SWTTestExt.configuredState.getServer().isRunning = true;
	  }
	}
}
