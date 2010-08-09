package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
/**
 * undeploys and deletes all projects
 * @author lzoubek@redhat.com
 *
 */
public class ClearProjects extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		try {		
			return SWTTestExt.projectExplorer.show().bot().tree().getAllItems().length==0;						
		}
		catch (Exception ex) {
			log.error("Unable to determine count of projects",ex);
			return false;
		}
	}

	@Override
	public void handle() {
		SWTTestExt.servers.removeAllProjectsFromServer();
		SWTTestExt.projectExplorer.deleteAllProjects();

	}

}
