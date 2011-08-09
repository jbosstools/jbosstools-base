package org.jboss.tools.ui.bot.ext.view;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.View.ServerServers;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

public class ServersView extends ViewBase {

	public ServersView() {
		viewObject = ServerServers.LABEL;
	}
	/**
	 * removes all projects from server with given name
	 * @param serverName
	 */
	public void removeAllProjectsFromServer(String serverName) {
		if (serverName==null) {
			return;
		}
		SWTBotTree tree = show().bot().tree();
		SWTBotTreeItem server = findServerByName(tree,serverName);
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.ADD_AND_REMOVE, false)).click();
	        try {
	        	SWTBotShell shell = bot.shell(IDELabel.Menu.ADD_AND_REMOVE);
	        	shell.activate();
		        SWTBotButton btRemoveAll=shell.bot().button("<< Remove All");		        
		        if (btRemoveAll.isEnabled()) {
		        	btRemoveAll.click();
		        	log.info("Removing all projects from server '"+serverName+"'");
		        }
		        open.finish(shell.bot(), IDELabel.Button.FINISH);
		        util.waitForNonIgnoredJobs();
		        util.waitForAll(Timing.time3S());
	        } catch (WidgetNotFoundException ex) {
	        	ex.printStackTrace();
	        	bot.shell("Server").activate();
	        	bot.button(IDELabel.Button.OK).click();
	        }
	        
		}
	}
	/**
	 * stops application server of given name
	 * @param serverName
	 */
	public void stopServer(String serverName) {
		SWTBot bot = show().bot();
		SWTBotTree tree = bot.tree();
		SWTBotTreeItem server = findServerByName(tree,serverName);
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.STOP, false)).click();
		    util.waitForNonIgnoredJobs();
		    util.waitForAll(Timing.time10S());
	        
		}
	}
	/**
	 * deletes server with given name
	 * @param serverName
	 */
	public void deleteServer(String serverName) {
		SWTBot bot = show().bot();
		SWTBotTree tree = bot.tree();
		SWTBotTreeItem server = findServerByName(tree,serverName);
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.DELETE, false)).click();
	        SWTBotShell shell = bot.shell("Delete Server");
	          shell.activate();
	          open.finish(shell.bot(), IDELabel.Button.OK);
	          log.info("Removed  server: " + serverName);
	        
		}
	}
	/**
	 * starts application server by given name
	 * @param serverName
	 */
	public void startServer(String serverName) {
		startServer(serverName, null, null);
	}
	/**
	 * start server with given name, username and password are used in case server is remote type
	 * @param serverName
	 * @param username remote user, can be null (server is then treated as local)
	 * @param password should be null when SSH keys are properly set
	 */
	public void startServer(String serverName, String username, String password) {
		show();
		SWTBot bot = show().bot();
		SWTBotTree tree = bot.tree();
		SWTBotTreeItem server = findServerByName(tree,serverName);
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.START, false)).click();
	        if (username!=null) {
	        	try {
	        		SWTBot shell = bot.shell("Enter Password").bot();
	       			shell.textWithLabel("User ID:").setText(username);
	        		if (password!=null)
	        			shell.textWithLabel("Password (optional):").setText(password);
	        		shell.button(IDELabel.Button.OK).click();
	        	}
	        	catch (Exception ex){
	        		log.error(ex);
	        	}
	        }
		    util.waitForNonIgnoredJobs(Timing.time(600 * 1000));
		    util.waitForAll(Timing.time3S());
		}
		else{
		  throw new RuntimeException("Unable to start server witn name: " + serverName +
		    "\nThis server is not defined within Servers view");
		}
	}
	public SWTBotTreeItem findServerByName(SWTBotTree tree, String name) {
		
		for (SWTBotTreeItem i : tree.getAllItems()) {
			if (i.getText().startsWith(name)) {
				return i;
			}
		}
		return null;
	}
	/**
	 * removes project with given name from all servers
	 * @param projectName
	 */
	public void removeProjectFromServers(String projectName){
		   	    
	    SWTBotTree serverTree = show().bot().tree();
	    // Expand All
	    for (SWTBotTreeItem serverTreeItem : serverTree.getAllItems()){
	      serverTreeItem.expand();
	      // if JSF Test Project is deployed to server remove it
	      SWTBotTreeItem[] serverTreeItemChildren = serverTreeItem.getItems();
	      if (serverTreeItemChildren != null && serverTreeItemChildren.length > 0){
	        int itemIndex = 0;
	        boolean found = false;
	        do{
	          String treeItemlabel = serverTreeItemChildren[itemIndex].getText();
	          found = treeItemlabel.startsWith(projectName);
	        } while (!found && ++itemIndex < serverTreeItemChildren.length);
	        // Server Tree Item has Child with Text equal to JSF TEst Project
	        if (found){
	          log.info("Found project to be removed from server: " + serverTreeItemChildren[itemIndex].getText());
	          ContextMenuHelper.prepareTreeItemForContextMenu(serverTree,serverTreeItemChildren[itemIndex]);
	          new SWTBotMenu(ContextMenuHelper.getContextMenu(serverTree, IDELabel.Menu.REMOVE, false)).click();
	          SWTBotShell shell = bot.shell("Server");
	          shell.activate();
	          open.finish(shell.bot(), IDELabel.Button.OK);
	          log.info("Removed project from server: " + serverTreeItemChildren[itemIndex].getText());
	          util.waitForNonIgnoredJobs();	          
	        } else {
	        	log.info("Project '"+projectName+"' not found on any server");
	        }
	      }
	    }
	  }
	/**
	 * removes projects from pre-configured server
	 * @param projectName
	 */
	public void removeAllProjectsFromServer() {
		removeAllProjectsFromServer(SWTTestExt.configuredState.getServer().name);
		
	}

}
