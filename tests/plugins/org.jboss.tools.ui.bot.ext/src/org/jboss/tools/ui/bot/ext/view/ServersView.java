package org.jboss.tools.ui.bot.ext.view;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withMnemonic;
import static org.hamcrest.Matchers.allOf;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.ContextMenuFinder;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.Matcher;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.condition.NonSystemJobRunsCondition;
import org.jboss.tools.ui.bot.ext.condition.TaskDuration;
import org.jboss.tools.ui.bot.ext.entity.XMLConfiguration;
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
	 * restarts server with given name
	 * @param serverName
	 */
	public void restartServer(final String serverName) {
		SWTBot bot = show().bot();
		SWTBotTree tree = bot.tree();
		SWTBotTreeItem server = findServerByName(tree,serverName);

		ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
		new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, "Restart", false)).click();
		handleServerAlreadyRunning(bot);

		bot.waitWhile(new NonSystemJobRunsCondition(), TaskDuration.VERY_LONG.getTimeout());
		bot.waitUntil(new ICondition() {

			@Override
			public boolean test() throws Exception {
				return "Started".equals(getServerStatus(serverName));
			}

			@Override
			public void init(SWTBot bot) {
			}

			@Override
			public String getFailureMessage() {
				return "The server does not have status 'Started'";
			}
		}, TaskDuration.LONG.getTimeout());
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
			handleServerAlreadyRunning(bot);
			util.waitForNonIgnoredJobs(Timing.time(600 * 1000));
			util.waitForAll(Timing.time3S());
		}
		else{
			throw new RuntimeException("Unable to start server witn name: " + serverName +
					"\nThis server is not defined within Servers view");
		}
	}
	private void handleServerAlreadyRunning(SWTBot bot) {
		try {
			bot.shell("Server already running on localhost");
			throw new RuntimeException("Another server is running on localhost");
		} catch (WidgetNotFoundException e) {
			// ok, nothing to do
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

	public SWTBotTreeItem findServerByName(String name) {

		SWTBot bot = show().bot();
		return findServerByName(bot.tree(), name);
	}

	public boolean serverExists(String serverName){
		SWTBot bot = show().bot();

		try {
			// if there are no servers the following text appears
			bot.link("No servers available. Define a new server from the <a>new server wizard</a>...");
			return false;
		} catch (WidgetNotFoundException e){
			// ok, there are some servers, let's check the name
		}

		SWTBotTreeItem server = findServerByName(bot.tree(), serverName);
		return server != null;
	}

	public String getServerStatus(String serverName){
		SWTBot bot = show().bot();
		SWTBotTreeItem server = findServerByName(bot.tree(), serverName);

		String label = server.getText();
		int startIndex = label.indexOf('[') + 1;
		int endIndex = label.indexOf(',');
		return label.substring(startIndex, endIndex);
	}
	
	public String getServerPublishStatus(String serverName){
		SWTBot bot = show().bot();
		SWTBotTreeItem server = findServerByName(bot.tree(), serverName);

		String label = server.getText();
		int startIndex = label.indexOf(',') + 2;
		int endIndex = label.indexOf(']');
		return label.substring(startIndex, endIndex);
	}

	public boolean containsProject(String serverName, String project){
		SWTBot bot = show().bot();
		SWTBotTreeItem server = findServerByName(bot.tree(), serverName);
		server.expand();
		
		try {
			getProjectNode(server, project);
			return true;
		} catch (WidgetNotFoundException e){
			return false;
		}
	}
	
	private SWTBotTreeItem getProjectNode(SWTBotTreeItem server, String projectName){
		for (SWTBotTreeItem child : server.getItems()){
			String name = child.getText();
			if (name.contains("[")){
				name = name.substring(0, name.indexOf("["));
			}
			if (name.trim().equals(projectName)){
				return child;
			}
		}
		throw new WidgetNotFoundException("Project " + projectName + " was not found within server");
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

	public void addProjectToServer(String projectName, String serverName){
		SWTBot bot = show().bot();
		SWTBotTree serversTree = bot.tree();
		SWTBotTreeItem server = findServerByName(serversTree, serverName); 

		ContextMenuHelper.prepareTreeItemForContextMenu(serversTree,server);
		new SWTBotMenu(ContextMenuHelper.getContextMenu(serversTree, IDELabel.Menu.ADD_AND_REMOVE, false)).click();

		SWTBotShell shell = bot.shell("Add and Remove...");
		SWTBot shellBot = shell.bot();

		shellBot.tree(0).getTreeItem(projectName).select();
		shellBot.button("Add >").click();
		shellBot.button("Finish").click();
		shellBot.waitWhile(new NonSystemJobRunsCondition(), TaskDuration.VERY_LONG.getTimeout());
		shellBot.waitUntil(Conditions.shellCloses(shell), TaskDuration.NORMAL.getTimeout());
	}
	
	public void removeProjectFromServer(String projectName, String serverName){
		SWTBot bot = show().bot();
		SWTBotTree serversTree = bot.tree();
		SWTBotTreeItem server = findServerByName(serversTree, serverName); 

		ContextMenuHelper.prepareTreeItemForContextMenu(serversTree,server);
		new SWTBotMenu(ContextMenuHelper.getContextMenu(serversTree, IDELabel.Menu.ADD_AND_REMOVE, false)).click();

		SWTBot shellBot = bot.shell("Add and Remove...").bot();

		shellBot.tree(1).getTreeItem(projectName).select();
		shellBot.button("< Remove").click();
		shellBot.button("Finish").click();
		shellBot.waitWhile(new NonSystemJobRunsCondition(), TaskDuration.VERY_LONG.getTimeout());
	}

	public void openServerEditor(String serverName){
		SWTBot bot = show().bot();
		SWTBotTreeItem server = findServerByName(bot.tree(), serverName);
		server.doubleClick();
	}
	
	public void openWebPage(String serverName){
		SWTBot bot = show().bot();
		SWTBotTree serversTree = bot.tree();
		SWTBotTreeItem server = new SWTBotTreeItemWithContextMenu(findServerByName(serversTree, serverName));
		
		server.contextMenu("Web Browser").click();
	}
	
	public void openWebPage(String serverName, String projectName){
		SWTBot bot = show().bot();
		SWTBotTree serversTree = bot.tree();
		SWTBotTreeItem server = findServerByName(serversTree, serverName);
		SWTBotTreeItem project = new SWTBotTreeItemWithContextMenu(getProjectNode(server, projectName));
		project.contextMenu("Web Browser").click();
	}
	
	public List<XMLConfiguration> getXMLConfiguration(String serverName, String categoryName){
		SWTBotTreeItem server = findServerByName(serverName);
		server.expand();
		final SWTBotTreeItem category = server.expandNode("XML Configuration", categoryName);

		String separator = "   ";
		SWTBotFactory.getBot().waitUntil(new TreeItemLabelDecorated(category.getNode(0), separator));

		List<XMLConfiguration> configurations = new ArrayList<XMLConfiguration>();
		for (final SWTBotTreeItem item : category.getItems()){
			String[] columns = item.getText().split(separator);
			if (columns.length < 2){
				// it is nested node, we should process it recursively in the future
				// but for now not crucial, let's skip it
				continue;
			}
			configurations.add(new XMLConfiguration(columns[0].trim(), columns[1].trim()));
		}
		return configurations;
	}

	private static class TreeItemLabelDecorated implements ICondition {

		private String separator;

		private SWTBotTreeItem item;
		
		public TreeItemLabelDecorated(SWTBotTreeItem item, String separator) {
			super();
			this.item = item;
			this.separator = separator;
		}

		@Override
		public void init(SWTBot bot) {
		}

		@Override
		public boolean test() throws Exception {
			return item.getText().contains(separator);
		}

		@Override
		public String getFailureMessage() {
			return "Expected the tree item to be decorated with separator '" + separator + "'";
		}
	}
	
	class SWTBotTreeItemWithContextMenu extends SWTBotTreeItem {

		private Tree tree;
		
		public SWTBotTreeItemWithContextMenu(final SWTBotTreeItem treeItem)
				throws WidgetNotFoundException {
			super(treeItem.widget);
			
			this.tree = syncExec(new WidgetResult<Tree>() {
				public Tree run() {
					return treeItem.widget.getParent();
				}
			});
		}
		
		@Override
		public SWTBotMenu contextMenu(String text) {
			select();
			notifyTree(SWT.MouseDown, createMouseEvent(0, 0, 3, 0, 1));
			notifyTree(SWT.MouseUp, createMouseEvent(0, 0, 3, 0, 1));
			notifyTree(SWT.MenuDetect);
			return contextMenu(tree, text);
		}
		
		@SuppressWarnings("unchecked")
		protected SWTBotMenu contextMenu(final Control control, final String text) {
			Matcher<MenuItem> withMnemonic = withMnemonic(text);
			final Matcher<MenuItem> matcher = allOf(widgetOfType(MenuItem.class), withMnemonic);
			final ContextMenuFinder menuFinder = new ContextMenuFinder(control);
			return new SWTBotMenu(getMenu(menuFinder.findMenus(matcher)), matcher);
		}
		
		private void notifyTree(int eventType) {
			notify(eventType, createEvent(), tree);
		}

		private void notifyTree(int eventType, Event event) {
			notify(eventType, event, tree);
		}
		
		private MenuItem getMenu(List<MenuItem> items){
			for (MenuItem item : items){
				if (!item.isDisposed()){
					return item;
				}
			}
			
			if (items.isEmpty()){
				throw new WidgetNotFoundException("Widget menuItem has not been found");				
			} else {
				throw new IllegalStateException("All menu items have been disposed");
			}
		}
	}
}
