/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.hamcrest.Matchers.allOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.matchers.WidgetMatcherFactory;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCTabItem;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.Matcher;
import org.jboss.tools.ui.bot.ext.condition.ButtonIsDisabled;
import org.jboss.tools.ui.bot.ext.entity.JavaClassEntity;
import org.jboss.tools.ui.bot.ext.entity.JavaProjectEntity;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.NewObject;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.NewObject.ServerServer;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Preference;
import org.jboss.tools.ui.bot.ext.gen.IServer;
import org.jboss.tools.ui.bot.ext.gen.IServerRuntime;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.helper.MenuBarHelper;
import org.jboss.tools.ui.bot.ext.types.EntityType;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.IDELabel.PreferencesDialog;
import org.jboss.tools.ui.bot.ext.types.PerspectiveType;
import org.jboss.tools.ui.bot.ext.types.ViewType;
import org.jboss.tools.ui.bot.ext.view.PackageExplorer;
import org.jboss.tools.ui.bot.ext.view.RemoteSystems;
import org.junit.Assert;

/**
 * Provides Eclipse common operation based on SWTBot element operations
 * 
 * @author jpeterka
 * 
 */
public class SWTEclipseExt {

	private final SWTOpenExt open;
	private SWTUtilExt util;
	private SWTBotExt bot;
	// private SWTUtilExt swtUtilExt;
	private static Logger log = Logger.getLogger(SWTEclipseExt.class);
	public static final long DEFAULT_UI_TIMEOUT = 1000L;

	public enum StringConditionType {
	  EQUALS, CONTAINS, STARTS_WITH;
	}
	
	public SWTWorkbenchBot getBot() {
		return bot;
	}

	public static Logger getLog() {
		return log;
	}

	public SWTEclipseExt(SWTBotExt bot) {
		this.bot = bot;
		this.util = new SWTUtilExt(bot);
		this.open = new SWTOpenExt(bot);
	}

	public SWTEclipseExt() {
		this.bot = new SWTBotExt();
		this.util = new SWTUtilExt(bot);
		this.open = new SWTOpenExt(bot);
	}

	
	// ------------------------------------------------------------
	// Check methods
	// ------------------------------------------------------------

    /**
     * Checks if view is opened.
     * 
     * @return <code>true</code> if view with given <code>viewTitle</code>
     *         is opened, <code>false</code> otherwise.
     */
    public boolean isViewOpened(final String viewTitle) {
        if (viewTitle == null) {
            throw new IllegalArgumentException("viewTitle cannot be null");
        }
        for (SWTBotView view : bot.views()) {
            if (viewTitle.equals(view.getTitle())) {
                return true;
            }
        }
        return false;
    }
	
	// ------------------------------------------------------------
	// View related methods
	// ------------------------------------------------------------

    /**
     * Close view with given title.
     * If view is closed nothing happens.
     * 
     * @param viewTitle Title of view which should be closed.
     */
	public void closeView(final String viewTitle) {
		if (viewTitle == null) {
			throw new IllegalArgumentException("viewTitle cannot be null");
		}
        for (SWTBotView view : bot.views()) {
            if (viewTitle.equals(view.getTitle())) {
                view.close();
                break;
            }
        }		
	}

	/**
	 * Show view
	 * 
	 * @param type
	 */
	public SWTBot showView(ViewType type) {
		return SWTEclipseExt.showView(bot, type);
	}

	/**
	 * Show view static version
	 * 
	 * @param type
	 */
	public static SWTBot showView(SWTBotExt bot, ViewType type) {
		
		SWTBotMenu menu1 = bot.menu(IDELabel.Menu.WINDOW);
		SWTBotMenu menu2 = menu1.menu(IDELabel.Menu.SHOW_VIEW);
		menu2.menu(IDELabel.Menu.OTHER).click();
			
		SWTBotTreeItem[] theItems = bot.tree().getAllItems();
		for (SWTBotTreeItem i : theItems) {
			log.info("SWTBotTreeItem = item = " + i);
		}

		/* ldimaggi - Dec 2011 - List of views that includes "Problems" selection is slow to appear 
		 * when ESB tests are run via mvn - https://issues.jboss.org/browse/JBQA-5813
		 */
		waitForViewInList (bot, type.getGroupLabel(), 2 );

		/* ldimaggi - Dec 2011 - And - the "Problems" view is also slow to appear when ESB 
		 * tests are run via mvn - https://issues.jboss.org/browse/JBQA-5813
		 */
		waitForView (bot, type.getGroupLabel(), type.getViewLabel(), 3 );
		
		bot.button(IDELabel.Button.OK).click();
//		bot.sleep(30000l);		

		SWTBot viewBot = bot.viewByTitle(type.getViewLabel()).bot();
//		bot.sleep(30000l);
		return viewBot;		
	}

	// ------------------------------------------------------------
	// Perspective related methods
	// ------------------------------------------------------------
	/**
	 * Open Perspective
	 * 
	 * @param type
	 */
	public void openPerspective(PerspectiveType type) {
		String perspectiveLabel = "";

		switch (type) {
		case JAVA:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.JAVA;
			break;	
		case CDI:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.CDI;
			break;
		case HIBERNATE:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.HIBERNATE;
			break;
		case SEAM:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.SEAM;
			break;
		case WEB_DEVELOPMENT:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.WEB_DEVELOPMENT;
			break;
		case DB_DEVELOPMENT:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.DB_DEVELOPMENT;
			break;
		case JPA:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.JPA;
			break;
		case DEBUG:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.DEBUG;
			break;
		case GUVNOR_REPOSITORY_EXPLORING:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.GUVNOR_REPOSITORY_EXPLORING;
			break;
		case DROOLS:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.DROOLS;
			break;
		case JBPM3:
			perspectiveLabel = IDELabel.SelectPerspectiveDialog.JBPM3;
			break;
		default:
			fail("Unknown perspective to open");
		}
    bot.sleep(Timing.time1S()); 
		SWTBotMenu menu1 = bot.menu(IDELabel.Menu.WINDOW);
    bot.sleep(Timing.time1S());		
		SWTBotMenu menu2 = menu1.menu(IDELabel.Menu.OPEN_PERSPECTIVE);
    bot.sleep(Timing.time1S());		
		menu2.menu(IDELabel.Menu.OTHER).click();
    bot.sleep(Timing.time1S());
    SWTBotTable table = bot.table();
    if (isItemInTableColumn(table, perspectiveLabel, 0)){
      table.select(perspectiveLabel);
    }
    else{
      log.warn("WARN - Perspecive with label '" + perspectiveLabel + "' was not found, trying label '" 
          + perspectiveLabel + " (default)'");
      table.select(perspectiveLabel + (" (default)"));
    }
    bot.sleep(Timing.time1S());
		// Another approach
		SWTBotShell openPerpectiveShell = bot.shell("Open Perspective");
		openPerpectiveShell.activate();

		bot.button(IDELabel.Button.OK).click();
	}

	// ------------------------------------------------------------
	// Create related methods
	// ------------------------------------------------------------
	/**
	 * Create Java Project desribed with propriate entity
	 * 
	 * @param entity
	 */
	public void createJavaProject(JavaProjectEntity entity) {
		// NewWizard
		createNew(EntityType.JAVA_PROJECT);
		waitForShell(IDELabel.Shell.NEW_JAVA_PROJECT);

		// JavaProjectWizard
		SWTBotShell shell = bot.activeShell();
		bot.textWithLabel(IDELabel.JavaProjectWizard.PROJECT_NAME).setText(
				entity.getProjectName());

		bot.waitWhile(new ButtonIsDisabled(IDELabel.Button.FINISH));			
		bot.button(IDELabel.Button.FINISH).click();

		// Wait for shell closing JavaProjectWizard
		waitForClosedShell(shell);
		util.waitForNonIgnoredJobs();
	}

	// ------------------------------------------------------------
	// Create related methods
	// ------------------------------------------------------------
	/**
	 * Create new Java Class described with JavaClassEntity
	 * 
	 * @param entity
	 */
	public void createJavaClass(JavaClassEntity entity) {
		createNew(EntityType.JAVA_CLASS);
		waitForShell(IDELabel.Shell.NEW_JAVA_CLASS);

		bot.textWithLabel(IDELabel.NewClassCreationWizard.PACKAGE_NAME)
				.setText(entity.getPackageName());
		bot.textWithLabel(IDELabel.NewClassCreationWizard.CLASS_NAME).setText(
				entity.getClassName());
		bot.button(IDELabel.Button.FINISH).click();

		waitForClosedShell(IDELabel.Shell.NEW_JAVA_CLASS);
		util.waitForNonIgnoredJobs();
	}

	/**
	 * Remove entity from package explorer
	 * 
	 * @param projectName
	 * @param path
	 */
	public void removeFile(String projectName, String... path) {
		// Open Package Explorer and aim the Project
		SWTBot viewBot = bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).bot();
		SWTBotTreeItem item = viewBot.tree().expandNode(projectName);

		// Go through path
		for (String nodeName : path) {
			item = item.expandNode(nodeName);
		}

		// Delete File
		item.select().contextMenu("Delete").click();
		bot.button(IDELabel.Button.OK).click();
	}

	/**
	 * Open File in Package Explorer
	 * 
	 * @param projectName
	 * @param path
	 */
	public SWTBotEditor openFile(String projectName, String... path) {
		return SWTEclipseExt.openFile(bot, projectName, path);
	}
	
	 /**
   * Delete File in Package Explorer
   * 
   * @param projectName
   * @param path
   */
  public void deleteFile(String projectName, String... path) {
    SWTEclipseExt.deleteFile(bot, projectName, path);
  }

	// ------------------------------------------------------------
	// Navigation related
	// ------------------------------------------------------------
	/**
	 * Select element in tree
	 * 
	 * @return
	 */
	public SWTBotTreeItem selectTreeLocation(String... path) {
		return selectTreeLocation(bot, path);
	}

	/**
	 * Select element in tree with given bot
	 * 
	 * @return
	 */
	public static SWTBotTreeItem selectTreeLocation(SWTBot bot, String... path) {

		SWTBot viewBot = bot;

		SWTBotTreeItem item = null;
		// Go through path
		for (String nodeName : path) {
			if (item == null) {
				item = viewBot.tree().expandNode(nodeName);
			} else {
				item = item.expandNode(nodeName);
			}
			log.info(nodeName);
		}
		return item.select();
	}	

	// ------------------------------------------------------------
	// Subroutines
	// ------------------------------------------------------------

	/* ldimaggi - Dec 2011 - For dealing with view refresh issues - https://issues.jboss.org/browse/JBQA-5813 */
	public static void waitForViewInList(SWTBotExt theBot, String theGroupLabel, int counter) {
		int itemsCount = theBot.tree().expandNode(theGroupLabel).getItems().length;
		while (itemsCount < counter) {
			SWTBotTreeItem treeItem = theBot.tree().expandNode(theGroupLabel);
			treeItem.collapse().expand();
			SWTBotTreeItem[] theItems = treeItem.getItems();
			itemsCount = theItems.length;
			for (SWTBotTreeItem i : theItems) {
				log.info("SWTBotTreeItem = item = " + i);
			}
			theBot.sleep(Timing.time3S());
		}
	}
	
	/* ldimaggi - Dec 2011 - For dealing with view refresh issues https://issues.jboss.org/browse/JBQA-5813 */
	public static void waitForView (SWTBotExt theBot, String groupLabel, String viewLabel, int counterLimit ) {
		SWTBotTreeItem tempItem = null;
		int counter = 0;
//		System.out.println ("DEBUG - got it - " + theBot.tree().expandNode(groupLabel).expandNode(viewLabel).getText() );	
		
		while ((tempItem == null) && (counter < counterLimit) ){
			theBot.shell(IDELabel.Shell.SHOW_VIEW).activate();	
			
			/* Re-expand the tree - commented out as this does not seem to help */
			//theBot.tree().collapseNode(groupLabel);
			//theBot.tree().expandNode(groupLabel);
			//theBot.tree().getTreeItem(groupLabel).click();
			//theBot.tree().setFocus();
			//theBot.tree().getTreeItem(groupLabel).expand();
			
			log.info ("Located view - " + theBot.tree().expandNode(groupLabel).expandNode(viewLabel).getText() );
			tempItem = theBot.tree().expandNode(groupLabel).expandNode(viewLabel).select();
			if (tempItem == null) {
				theBot.sleep(Timing.time3S());
			}
			counter++;
		}		
	}
	
	
	/**
	 * Invoke Create new entity dialog (it means File -> New -> EntityType ->
	 * Next )
	 */
	public void createNew(EntityType entityType) {

		bot.menu(IDELabel.Menu.FILE).menu(IDELabel.Menu.NEW).menu(
				IDELabel.Menu.OTHER).click();
		waitForShell(IDELabel.Shell.NEW);

		String entityLabel = entityType.getEntityLabel();
		SWTBotTree tree = bot.tree();
		Iterator<String> itGroupsNodes = entityType.getGroupsLabels()
				.iterator();
		// if there are group labels defined expand groups nodes
		if (itGroupsNodes.hasNext()) {
			SWTBotTreeItem groupTreeItem = tree
					.expandNode(itGroupsNodes.next());
			while (itGroupsNodes.hasNext()) {
				groupTreeItem = groupTreeItem.expandNode(itGroupsNodes.next());
			}
			groupTreeItem.collapse().expand();
			groupTreeItem.select(entityLabel);
		} else {
			tree.select(entityLabel);
		}
		bot.button(IDELabel.Button.NEXT).click();
	}

	/**
	 * Wait for appearance shell of given name
	 * 
	 * @param shellName
	 */
	public void waitForShell(String shellName) {
		Matcher<Shell> matcher = WidgetMatcherFactory.withText(shellName);
		bot.waitUntil(Conditions.waitForShell(matcher));

	}

	/**
	 * Waits for closed shell with shell name
	 * 
	 * @param shellName
	 */
	public void waitForClosedShell(String shellName) {
		bot.waitWhile(Conditions.shellIsActive(shellName));
	}

	public void waitForClosedShell(SWTBotShell shell) {
		while (shell.isActive()) {
			bot.sleep(SWTEclipseExt.DEFAULT_UI_TIMEOUT);
			log.info("Waiting for closing shell...");
		}
	}

	/**
	 * Assert same content of file in project explorer with content of file in
	 * resources
	 * 
	 * @param pluginId
	 * @param projectName
	 * @param path
	 */
	public void assertSameContent(String pluginId, String projectName,
			String... path) {
		File file = SWTUtilExt.getResourceFile(pluginId, path);
		String resourceContent = SWTTestExt.util.readTextFile(file);

		openAsText(projectName, path);
		SWTBotEclipseEditor editor = bot.editorByTitle(path[path.length - 1])
				.toTextEditor();
		String fileContent = editor.getText();

		assertEquals(resourceContent, fileContent);
	}

	/**
	 * Open file from Package Explorer as text
	 * 
	 * @param projectName
	 * @param path
	 */
	public void openAsText(String projectName, String... path) {
		SWTBot viewBot = bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).bot();
		SWTBotTreeItem item = viewBot.tree().expandNode(projectName);

		// Go through path
		for (String nodeName : path) {
			log.info(nodeName);
			item = item.expandNode(nodeName);
		}

		item.select();
	}

	/**
	 * Replace class content from content of class from resource
	 * 
	 * @param pluginId
	 * @param path
	 */
	public void setClassContentFromResource(boolean save, String pluginId,
			String... path) {
		SWTBotEclipseEditor editor;
		editor = bot.editorByTitle(path[path.length - 1]).toTextEditor();
		editor.selectRange(0, 0, editor.getLineCount());
		File file = SWTUtilExt.getResourceFile(pluginId, path);
		String content = util.readTextFile(file);
		editor.setText(content);
		if (save)
			editor.save();
	}

	/**
	 * Replace editor content by content from resource
	 * 
	 * @param editor
	 * @param pluginId
	 * @param path
	 */
	public void setClassContentFromResource(SWTBotEditor editor, boolean save,
			String pluginId, String... path) {
		SWTBotEclipseEditor edit = editor.toTextEditor();
		edit.selectRange(0, 0, edit.getText().length());
		File file = SWTUtilExt.getResourceFile(pluginId, path);
		String content = util.readTextFile(file);
		edit.setText(content);
		if (save)
			edit.save();
	}

	/**
	 * adds server (Server runtime must be defined, the default selected runtime
	 * is used)
	 * 
	 * @param server
	 *            to add ( for example
	 *            {@link ActionItem.Server.JBossCommunityJBossAS50#LABEL} class)
	 * @param serverName
	 */
	public void addServer(IServer server, String serverName) {
	  addServer(server, serverName, null,null);
	}
	/**
	 * adds server (Server runtime must be defined, the default selected runtime
	 * is used)
	 * 
	 * @param server
	 *            to add ( for example
	 *            {@link ActionItem.Server.JBossCommunityJBossAS50#LABEL} class)
	 * @param serverName
	 */
	public void addServer(IServer server, String serverName, String remoteSystem,String remoteHome) {
	  log.info("Adding server: " + serverName);
		SWTBot wiz = open.newObject(ActionItem.NewObject.ServerServer.LABEL);
		open.selectTreeNode(server);
		wiz.textWithLabel(ServerServer.TEXT_SERVER_NAME).setText(serverName);
		wiz.button(IDELabel.Button.NEXT).click();
		if (remoteSystem!=null && remoteHome!=null) {
			wiz.comboBoxInGroup("Server Behaviour", 0).setSelection("Remote System Deployment");
			wiz.comboBoxWithLabel("Host").setSelection(remoteSystem);
			wiz.textInGroup("Server Behaviour", 0).setText(remoteHome);
		}
		open.finish(wiz);
	}
	/**
	 * adds seam runtime
	 * @param name of newly added runtime
	 * @param version seam version
	 * @param seamHome path to seam home directory
	 */
	public void addSeamRuntime(String name, String version, String seamHome) {
	  log.info("Adding Seam Runtime: " + name +
	    "\nVersion: " + version +
	    "\nHome: " + seamHome);
		SWTBot wiz = open.preferenceOpen(ActionItem.Preference.JBossToolsWebSeam.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		boolean createRuntime = true;
		// first check if Environment doesn't exist
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (createRuntime && currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						name)) {
					createRuntime = false;
				} else {
					currentRow++;
				}
			}
		}
		if (createRuntime) {
			wiz.button("Add").click();
			bot.shell(IDELabel.Shell.NEW_SEAM_RUNTIME).activate();
			bot.text(0).setText(seamHome);
			bot.text(1).setText(name);
			// find and select version
			String[] versions = bot.comboBox().items();
			int myIndex =0;
			for (int index=0;index<versions.length;index++) {
				if (version.equals(versions[index])) {
					myIndex=index;
					break;
				}
			}
			bot.comboBox().setSelection(myIndex);
			open.finish(bot.activeShell().bot());
		}
		open.finish(wiz, IDELabel.Button.OK);
	}
	
	/**
	 * Adds jBPM runtime via SWTBot
	 * @param name
	 * @param runtimeHome
	 */
	public void addJBPMRuntime(String name, String runtimeHome) {
		SWTBot wiz = open.preferenceOpen(ActionItem.Preference.JBossjBPMRuntimeLocations.LABEL);
		
		boolean createRuntime = true;
		// first check if Environment doesn't exist
		SWTBotTable tbRuntimeEnvironments = bot.table();
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (createRuntime && currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						name)) {
					createRuntime = false;
				} else {
					currentRow++;
				}
			}
		}
		if (createRuntime) {
			wiz.button("Add...").click();
			log.info("jBPM Runtime '" + name +"' added.");
			
			bot.shell(IDELabel.Shell.NEW_JBPM_RUNTIME).activate();
			bot.text(0).setText(name);
			bot.text(1).setText(runtimeHome);
			bot.clickButton(IDELabel.Button.OK);

		}
		open.finish(wiz, IDELabel.Button.OK);		
	}
	
	/**
	 * Removes jBPM Runtime via SWTBot
	 * @param name
	 */
	public void removeJBPMRuntime(String name) {
		SWTBot wiz = open.preferenceOpen(ActionItem.Preference.JBossjBPMRuntimeLocations.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						name)) {
					tbRuntimeEnvironments.click(currentRow, 1);
					wiz.button(IDELabel.Button.REMOVE).click();
					SWTBotShell shell = bot.shell("Confirm Runtime Delete");
					shell.activate();
					shell.bot().button(IDELabel.Button.OK).click();
					log.info("jBPM Runtime '" + name +"' removed.");
					break;
				} else {
					currentRow++;
				}
			}
		}
		open.finish(wiz,IDELabel.Button.OK);
	}
	
	public void addESBRuntime(String name, String version, String runtimeHome ) {
		SWTBot wiz = open.preferenceOpen(ActionItem.Preference.JBossToolsJBossESBRuntimes.LABEL);
		
		boolean createRuntime = true;
		// first check if Environment doesn't exist
		SWTBotTable tbRuntimeEnvironments = bot.table();
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (createRuntime && currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						name)) {
					createRuntime = false;
				} else {
					currentRow++;
				}
			}
		}
		if (createRuntime) {
			wiz.button("Add").click();
			bot.shell(IDELabel.Shell.NEW_ESB_RUNTIME).activate();
			bot.text(0).setText(name);
			bot.text(1).setText(runtimeHome);
			String[] versions = bot.comboBox().items();
			int myIndex =0;
			for (int index=0;index<versions.length;index++) {
				if (version.equals(versions[index])) {
					myIndex=index;
					break;
				}
			}
			bot.comboBox().setSelection(myIndex);
			open.finish(bot.activeShell().bot());
		}
		open.finish(wiz, IDELabel.Button.OK);
	}
	public void removeESBRuntime(String name) {
		SWTBot wiz = open.preferenceOpen(ActionItem.Preference.JBossToolsJBossESBRuntimes.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						name)) {
					tbRuntimeEnvironments.click(currentRow, 1);
					wiz.button(IDELabel.Button.REMOVE).click();
					SWTBotShell shell = bot.shell("Confirm Runtime Delete");
					shell.activate();
					shell.bot().button(IDELabel.Button.OK).click();
					log.info("ESB Runtime '" + name +"' removed.");
					break;
				} else {
					currentRow++;
				}
			}
		}
		open.finish(wiz,IDELabel.Button.OK);
	}
	/**
	 * adds jboss server runtime only if it's not specified yet
	 * 
	 * @param runtime type of runtime
	 * @param runtimeHome homedir of runtime
	 * @param name for new runtime
	 * @param jreToUse if null default jre for runtime is used, otherwise selects given jre name
	 */
	public void addJbossServerRuntime(IServerRuntime runtime,
			String runtimeHome, String name, String jreToUse) {
    log.info("Adding JBoss Server Runtime: " + name +
      "\nHome: " + runtimeHome +
      "\nJRE: " + jreToUse);	  
		Properties props = new Properties();
		props
				.put(IDELabel.JBossServerRuntimeDialog.HOME_DIRECTORY,
						runtimeHome);
		props.put(IDELabel.JBossServerRuntimeDialog.NAME, name);
		addServerRuntime(runtime, props, jreToUse);
	}
	/**
	 * adds remote system to RSE, does not verify hostname 
	 * @param connectionName
	 * @param hostname
	 */
	public void addRemoteSystem(String connectionName, String hostname) {
		log.info(String.format("Adding remote system connection name=%s hostname=%s",connectionName,hostname));
		RemoteSystems remote = new RemoteSystems();
		if (remote.existsConnection(connectionName)) {
			log.warn(String.format("Connection called '%' already exits, skipping",connectionName));
			return;
		}
		SWTBot wiz = open.newObject(NewObject.create("Remote System Explorer","Connection"));
		open.selectTreeNode(ActionItem.create("General","SSH Only"));
		wiz.button(IDELabel.Button.NEXT).click();
		wiz.textWithLabel("Connection name:").setText(connectionName);
		wiz.comboBoxWithLabel("Host name:").setText(hostname);
		wiz.checkBox("Verify host name").deselect();
		open.finish(wiz);
		log.info("Connection was added");		
	}
	/**
	 * adds path to private key into SSH2 Preference page
	 * @param pathToKey
	 */
	public void setSSHKey(String pathToKey) {
		SWTBot wiz = open.preferenceOpen(Preference.create("General","Network Connections","SSH2"));
		wiz.tabItem("General").activate();
		wiz.textWithLabel("Private keys:").setText(pathToKey);
		open.finish(wiz, IDELabel.Button.OK);
	}
	public void removeRemoteSystem(String connectionName) {
		log.info(String.format("Removing remote system connection name=%s",connectionName));
		RemoteSystems remote = new RemoteSystems();
		if (remote.existsConnection(connectionName)) {
			log.warn(String.format("Connection called '%' does not exist!",connectionName));
			return;
		}
		remote.bot().tree().select(connectionName);
		if (!remote.bot().tree().contextMenu(IDELabel.Menu.DELETE).isEnabled()) {
			remote.bot().tree().contextMenu("Disconnect").click();
		}
		remote.bot().tree().contextMenu(IDELabel.Menu.DELETE).click();
		open.finish(bot.shell("Delete Confirmation").bot(), true);
		log.info("Connection was removed");
	}
	/**
	 * adds server runtime only if it's not specified yet
	 * 
	 * @param runtime type of runtime
	 * @param properties properties in form (text with label->value), will be filled in runtime form
	 * @param jreToUse if null default jre for runtime is used, otherwise selects given jre name
	 */
	public void addServerRuntime(IServerRuntime runtime, Properties properties,
			String jreToUse) {
	  log.info("Addding Server Runtime: " + runtime.getName() +
	    "\nJRE: " + jreToUse);
		SWTBot wiz = open
				.preferenceOpen(ActionItem.Preference.ServerRuntimeEnvironments.LABEL);
		
		String runtimeName = properties.getProperty(
				IDELabel.JBossServerRuntimeDialog.NAME, "eap");
		SWTBotTable tbRuntimeEnvironments = bot.table();
		boolean createRuntime = true;
		// first check if Environment doesn't exist
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (createRuntime && currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 0).equalsIgnoreCase(
						runtimeName)) {
					createRuntime = false;
				} else {
					currentRow++;
				}
			}
		}
		if (createRuntime) {
			wiz.button(IDELabel.Button.ADD).click();
			bot.shell(IDELabel.Shell.NEW_SERVER_RUNTIME_ENVIRONMENT).activate();
			open.selectTreeNode(runtime);
			bot.button(IDELabel.Button.NEXT).click();
			for (Object key : properties.keySet()) {
				bot.textWithLabel(key.toString()).setText(
						properties.getProperty((String) (key).toString()));
			}
			if (jreToUse != null) {
				bot.comboBox(0).setSelection(jreToUse);
			}
			open.finish(bot.activeShell().bot());
	  }
		else{
		  log.info("Server Runtime: " + runtime.getName() +
		      " already exists.\nNo Server Runtime were added.");
		}
		open.finish(wiz, IDELabel.Button.OK);
	}
	public void removeServerRuntime(String runtimeName) {
		  log.info("Removing Server Runtime: " + runtimeName );
					SWTBot wiz = open
							.preferenceOpen(ActionItem.Preference.ServerRuntimeEnvironments.LABEL);
					SWTBotTable tbRuntimeEnvironments = bot.table();

					// first check if Environment doesn't exist
					int numRows = tbRuntimeEnvironments.rowCount();
					if (numRows > 0) {
						int currentRow = 0;
						while (currentRow < numRows) {
							if (tbRuntimeEnvironments.cell(currentRow, 0).equalsIgnoreCase(
									runtimeName)) {
								tbRuntimeEnvironments.select(currentRow);
								wiz.button(IDELabel.Button.REMOVE).click();
								open.finish(wiz,IDELabel.Button.OK);
								log.info("Server Runtime '" + runtimeName +"' removed.");
								return;
							} else {
								currentRow++;
							}
						}
					}
	}
	/**
	 * adds given java to Installed JRE's
	 * 
	 * @param vmName
	 *            name of newly added java runtime
	 * @param jreHome
	 *            path to jre
	 */
	public void addJavaVM(String vmName, String jreHome) {
	  log.info("Adding Java Virtual Machine: " + vmName + "\nHome: " + jreHome);
		SWTBot pref = open
				.preferenceOpen(ActionItem.Preference.JavaInstalledJREs.LABEL);
		if (!pref.table().containsItem(vmName)){
	    pref.button(IDELabel.Button.ADD).click();
	    bot.shell("Add JRE").activate();
	    SWTBot add = bot.shell("Add JRE").bot();
	    add.button(IDELabel.Button.NEXT).click();
	    add.text(0).setText(jreHome);
	    add.text(1).setText(vmName);
	    open.finish(add);
		}
		else{
		  log.info("Java Virtual Machine: " + vmName + " already exists\nNo JVM were added");
		}
		open.finish(pref,IDELabel.Button.OK);
	}

	/**
	 * Define new Server Runtime only if it's not specified yet
	 * 
	 * @param runtimeName
	 * @param runtimeGroup
	 * @param runtimeType
	 * @param runtimeHomeDir
	 */
	public void addServerRuntime(String runtimeName, String runtimeGroup,
			String runtimeType, String runtimeHomeDir) {
	  log.info("Adding Server Runtime: " + runtimeName +
	    "\nRuntime Group: " + runtimeGroup +
	    "\nRuntime Type: " + runtimeType +
	    "\nHome: " + runtimeHomeDir);
		bot.menu(IDELabel.Menu.WINDOW).menu(IDELabel.Menu.PREFERENCES).click();
		bot.shell(IDELabel.Shell.PREFERENCES).activate();
		bot.tree().expandNode(IDELabel.PreferencesDialog.SERVER_GROUP).select(
				PreferencesDialog.RUNTIME_ENVIRONMENTS);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		boolean createRuntime = true;
		// first check if Environment doesn't exist
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (createRuntime && currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 0).equalsIgnoreCase(
						runtimeName)) {
					createRuntime = false;
				} else {
					currentRow++;
				}
			}
		}
		// create Server Runtime
		if (createRuntime) {
			bot.button(IDELabel.Button.ADD).click();
			bot.shell(IDELabel.Shell.NEW_SERVER_RUNTIME_ENVIRONMENT).activate();
			bot.tree().expandNode(runtimeGroup).select(runtimeType);
			bot.button("Next >").click();
			bot.textWithLabel(IDELabel.JBossServerRuntimeDialog.NAME).setText(
					runtimeName);
			bot.textWithLabel(IDELabel.JBossServerRuntimeDialog.HOME_DIRECTORY)
					.setText(runtimeHomeDir);
			bot.button(IDELabel.Button.FINISH).click();
		}
		bot.button(IDELabel.Button.OK).click();
	}

	/**
	 * Test if tree contains item with itemLabel
	 * 
	 * @param tree
	 * @param itemLabel
	 * @return
	 */
	public static boolean treeContainsItemWithLabel(SWTBotTree tree,
			String itemLabel) {
		boolean containsItem = false;
		try {
			tree.getTreeItem(itemLabel);
			containsItem = true;
		} catch (WidgetNotFoundException e) {
		}
		return containsItem;
	}

	/**
	 * if Open Associated Perspective Shell is opened close it and depend on
	 * switchPerspective parameter change current perspective
	 * 
	 * @param switchPerspective
	 */
	public void closeOpenAssociatedPerspectiveShellIfOpened(
			boolean switchPerspective) {
		try {
			bot.shell(IDELabel.Shell.OPEN_ASSOCIATED_PERSPECTIVE).activate();
			bot.button(
					switchPerspective ? IDELabel.Button.YES
							: IDELabel.Button.NO).click();
		} catch (WidgetNotFoundException wnfe) {
			// do nothing
		}
	}

	/**
	 * Close Warning Window if is opened. Press Continue or Cancel button
	 * dependent on pressContinueButton
	 * 
	 * @param pressContinueButton
	 */
	public void closeWarningWindowIfOpened(boolean pressContinueButton) {
		try {
			bot.shell(IDELabel.Shell.WARNING).activate();
			bot.button(
					pressContinueButton ? IDELabel.Button.CONTINUE
							: IDELabel.Button.CANCEL).click();
		} catch (WidgetNotFoundException wnfe) {
			// do nothing
		}
	}

	public static void closeWarningWindowIfOpened(SWTWorkbenchBot bot,
			boolean pressContinueButton) {
		closeWarningWindowIfOpened(bot, pressContinueButton);
	}

    /**
     * if Confirm Perspective Switch Shell is opened close it and depend on
     * switchPerspective parameter change current perspective
     * 
     * @param switchPerspective
     */
    public void closeConfirmPerspectiveSwitchShellIfOpened(final boolean switchPerspective) {
        closeConfirmPerspectiveSwitchShellIfOpened(switchPerspective, false);
    }

    /**
     * This method closes Confirm Perspective Switch dialog if it is opened.
     * Otherwise nothing happens.
     * 
     * @param switchPerspective If <code>true</code> perspective switches,
     *                          if <code>false</code> perspective does not switch.
     * @param rememberMyDecision If <code>true</code> it checks "Remember my decision" option.
     */
    public void closeConfirmPerspectiveSwitchShellIfOpened(
            final boolean switchPerspective, final boolean rememberMyDecision) {
        while (bot.waitForShell(IDELabel.Shell.PROGRESS_INFORMATION, 0) != null) {
            bot.sleep(Timing.time2S());
        }

        // Finds confirmation shell and waits for it
        SWTBotShell confirmationShell = bot.waitForShell(IDELabel.Shell.CONFIRM_PERSPECTIVE_SWITCH);

        // confirmation dialog is not shown, let it be
        if (confirmationShell == null) {
            return;
        }

        // close confirmation dialog
        confirmationShell.activate();
        if (rememberMyDecision) {
            bot.checkBox(IDELabel.Shell.REMEMBER_MY_DECISION).click();
        }
        bot.button(switchPerspective ? IDELabel.Button.YES : IDELabel.Button.NO).click();
    }

	/**
	 * Returns true if table column specified by column parameter contains item
	 * 
	 * @param table
	 * @param item
	 * @param column
	 * @return
	 */
	public static boolean isItemInTableColumn(SWTBotTable table, String item,
			int column) {
		boolean found = false;

		int rowIndex = 0;
		while (!found && rowIndex < table.rowCount()) {
			if (table.cell(rowIndex, column).trim().equals(item.trim())) {
				found = true;
			} else {
				rowIndex++;
			}
		}
		return found;
	}

	/**
	 * Hide Warning Message if displayed static version
	 */
	public static void hideWarningIfDisplayed(SWTBot bot) {
		try {
		  SWTBot warningBot = bot.shell(IDELabel.Shell.WARNING).activate().bot();
		  warningBot.sleep(Timing.time3S());
		  warningBot.button(IDELabel.Button.OK).click();
		} catch (WidgetNotFoundException wnfe) {
			// do nothing
		}
	}

	/**
	 * Hide Warning Message if displayed
	 */
	public void hideWarningIfDisplayed() {
		SWTEclipseExt.hideWarningIfDisplayed(bot);
	}

	/**
	 * Returns true when projectName is present in Package Explorer
	 * 
	 * @param projectName
	 * @return
	 */
	public boolean isProjectInPackageExplorer(String projectName) {
		return SWTEclipseExt.isProjectInPackageExplorer(bot, projectName);
	}

    /**
     * Returns <code>true</code> when projectName is present in Package Explorer static
     * version, <code>false</code> otherwise.
     * 
     * @param bot
     * @param projectName
     * @return
     */
    public static boolean isProjectInPackageExplorer(SWTBotExt bot, String projectName) {
        if (bot == null || projectName == null) {
            throw new NullPointerException();
        }

        SWTBot innerBot = SWTEclipseExt.showView(bot, ViewType.PACKAGE_EXPLORER);
        SWTBotTreeItem[] treeItems = innerBot.tree().getAllItems();
        for (int i = 0; i < treeItems.length; i++) {
            if (projectName.equals(treeItems[i].getText())) {
                return true;
            }
        }
        return false;
    }

	/**
	 * Returns Tree Item with specified label and located on path
	 * @param bot
	 * @param tree
	 * @param timeOut
	 * @param treeItemText
	 * @param path
	 * @return
	 */
	public static SWTBotTreeItem getTreeItemOnPath(SWTBot bot,SWTBotTree tree, int timeOut,
			String treeItemText, String[] path) {
		SWTBotTreeItem parentTreeItem = null;
		SWTBotTreeItem treeItem = null;
		if (path != null && path.length > 0) {
			parentTreeItem = tree.expandNode(path[0]);
			bot.sleep(timeOut);
			for (int index = 1; index < path.length; index++) {
				parentTreeItem = parentTreeItem.expandNode(path[index]);
				bot.sleep(timeOut);
			}
			treeItem = parentTreeItem.getNode(treeItemText);
		} else {
			treeItem = tree.getTreeItem(treeItemText);
		}
		return treeItem;
	}
	/**
	 * Returns first Tree Item with label starting with treeItemTextStartsWith and located on path
	 * @param bot
	 * @param tree
	 * @param timeOut
	 * @param treeItemTextStartsWith
	 * @param path
	 * @return
	 */
	public static SWTBotTreeItem getTreeItemOnPathStartsWith(SWTBot bot,SWTBotTree tree, int timeOut,
      String treeItemTextStartsWith, String[] path) {
    SWTBotTreeItem[] items = null;
	  if (path.length > 0){
	    String[] parentPath = new String[path.length - 1];
      System.arraycopy(path, 0, parentPath, 0, parentPath.length);
      SWTBotTreeItem parentTreeItem  = getTreeItemOnPath(bot, tree, timeOut, path[path.length - 1], parentPath);
      parentTreeItem.expand();
      bot.sleep(timeOut);
      items = parentTreeItem.getItems();
	  }else{
	    items = tree.getAllItems();
	  }
	  SWTBotTreeItem treeItem = null;
	  for (int index = 0 ; index < items.length && treeItem == null; index++){
	    if (items[index].getText().trim()
	          .startsWith(treeItemTextStartsWith)){
	      treeItem = items[index];
	    }
	  }

	  if (treeItem == null){
	    throw new WidgetNotFoundException("Unable to find Tree Item with label starting with " + treeItemTextStartsWith);
	  }
	  return treeItem;
	}
	/**
	 * Choose Run As Java Application menu for specified Tree Item
	 * 
	 * @param treeItem
	 */
	public void runTreeItemAsJavaApplication(SWTBotTreeItem treeItem) {		
		treeItem.select();
		treeItem.click();		
		
		//SWTEclipseExt.getMenuFromSubmenu(
		//		bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.RUN_AS),
		//		IDELabel.Menu.RUN_AS_JAVA_APPLICATION).click();
		
		//System.out.println("runJavaApplication!!!");
		//bot.sleep(30000l);
		//System.out.println (treeItem.contextMenu("Run As").menu("2 Java Application").getText());

		bot.sleep(Timing.time30S());
		treeItem.contextMenu("Run As").menu("2 Java Application").click();		
		//runJavaApplication("helloworld_testclient","org.jboss.soa.esb.samples.quickstart.helloworld.test.SendJMSMessage"," ");
		bot.sleep(Timing.time30S());	// This is needed to enable the test to run successfully to completion
		
	}

	/**
	 * runs java application (run configuration) 
	 * NOTE that Run->Run Configurations.. must be available to work (use appropriate perspective)
	 * @param project which should run
	 * @param className with main class to run
	 * @param arguments that will be passed to main class (can be null)
	 */
	@SuppressWarnings("unchecked")
	public void runJavaApplication(String project, String className, String arguments) {
		bot.menu(IDELabel.Menu.RUN).menu("Run Configurations...").click();
		SWTBotShell shell = bot.shell("Run Configurations");
		shell.activate();
		open.selectTreeNode(shell.bot(),ActionItem.create("Java Application"));		
		shell.bot().toolbarButton(0).click();
		SWTBotCTabItem tab =  shell.bot().cTabItem("Main").activate();
		List<Text> list = (List<Text>) shell.bot().widgets(allOf(widgetOfType(Text.class)), tab.widget);
		assertTrue(list.size()>=2);
		new SWTBotText(list.get(0)).setText(project);
		new SWTBotText(list.get(1)).setText(className);
		if (arguments!=null) {
			tab =  shell.bot().cTabItem("Arguments").activate();
			list = (List<Text>) shell.bot().widgets(allOf(widgetOfType(Text.class)), tab.widget);
			assertTrue(list.size()>=2);
			new SWTBotText(list.get(0)).setText(arguments);
		}
		bot.button(IDELabel.Button.RUN).click();
	}
	/**
	 * Search for Menu Item with Run on Server substring within label
	 * 
	 * @param subMenu
	 * @return
	 */
	public static SWTBotMenu getMenuFromSubmenu(SWTBotMenu subMenu,
			String menuLabelTextToContain) {
		final SWTBotMenu subMenuToSearch = subMenu;
		final String stringToSearchFor = menuLabelTextToContain;
		final MenuItem menuItem = UIThreadRunnable
				.syncExec(new WidgetResult<MenuItem>() {
					public MenuItem run() {
						int menuItemIndex = 0;
						MenuItem menuItem = null;
						final MenuItem[] menuItems = subMenuToSearch.widget
								.getMenu().getItems();
						while (menuItem == null
								&& menuItemIndex < menuItems.length) {
							if (menuItems[menuItemIndex].getText().indexOf(
									stringToSearchFor) > -1) {
								menuItem = menuItems[menuItemIndex];
							} else {
								menuItemIndex++;
							}
						}
						return menuItem;
					}
				});
		return new SWTBotMenu(menuItem);
	}
  /**
   * Opens file from Package Explorer static version
   * @param bot
   * @param path
   * @return SWTBotEditor
   */
  public static SWTBotEditor openFile(SWTBotExt bot, String projectName, String... path) {
    SWTBot viewBot = bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).bot();
    bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).show();
    bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).setFocus();
    SWTBotTree tree = viewBot.tree();
    SWTBotTreeItem item = tree.expandNode(projectName);
    StringBuilder builder = new StringBuilder(projectName);

    // Go through path
    for (String nodeName : path) {
      item = item.expandNode(nodeName);
      builder.append("/" + nodeName);
    }

    item.select().doubleClick();
    log.info("File Opened:" + builder.toString());

    SWTBotEditor editor = bot.activeEditor();
    return editor;
  }

  /**
   * Collapse all expanded tree items
   * @param tree
   */
  public static void collapseAll (SWTBotTree tree,SWTBotExt bot){
    for (SWTBotTreeItem treeItem : tree.getAllItems()){
      if (treeItem.isExpanded()){
        treeItem.collapse();
      }
    }
    bot.sleep(SWTEclipseExt.DEFAULT_UI_TIMEOUT);
  }
  /**
   * Returns all tree items which are children of parent tree item
   * @param bot
   * @param tree
   * @param parent
   * @param expand
   * @return
   */
  public static List<SWTBotTreeItem> getAllTreeItemsRecursive (SWTBotExt bot, SWTBotTree tree , SWTBotTreeItem parent, boolean expand){
    
    LinkedList<SWTBotTreeItem> treeItems = new LinkedList<SWTBotTreeItem>();
    
    if (parent != null){
      if (expand 
          && parent.getItems() != null 
          && parent.getItems().length > 0) {
        parent.expand();
        bot.sleep(SWTEclipseExt.DEFAULT_UI_TIMEOUT);
      }
      
      SWTBotTreeItem[] nodeChildren = parent.getItems();
       
      if (nodeChildren != null){
        for (SWTBotTreeItem child : nodeChildren){
          if (child.getText().length() > 0){
            treeItems.add(child);
            treeItems.addAll(SWTEclipseExt.getAllTreeItemsRecursive(bot,tree,child,expand));
          }
        }
      }
    }
    else{
      treeItems = (LinkedList<SWTBotTreeItem>)SWTEclipseExt.getAllTreeItemsRecursive(bot, tree, expand);
    }
    
    return treeItems;
  }
  /**
   * Returns all tree items recursive
   * @param bot
   * @param tree
   * @param expand
   * @return
   */
  public static List<SWTBotTreeItem> getAllTreeItemsRecursive (SWTBotExt bot, SWTBotTree tree, boolean expand){

    LinkedList<SWTBotTreeItem> treeItems = new LinkedList<SWTBotTreeItem>();
    
    SWTBotTreeItem[] nodeChildren = tree.getAllItems();
    if (nodeChildren != null){
      for (SWTBotTreeItem child : nodeChildren){
        if (child.getText().length() > 0){
          treeItems.add(child);
          treeItems.addAll(SWTEclipseExt.getAllTreeItemsRecursive(bot,tree,child,expand));
        }
      }
    }  
    
    return treeItems;
  }
  public static String getFormattedTreeNode(SWTBotTreeItem item) {
	  if (item==null) {
		  return "<item is null>";
	  }
	  if (item.row()==null) {
		  return item.getText();
	  }
	  StringBuilder sb = new StringBuilder("");	  
	  for (int i=0;i<item.row().columnCount();i++) {
		  String text = item.row().get(i);
		  if (text==null) {
			  sb.append("\"<null>\"");
		  }
		  else {
			  //sb.append(String.format("\"{0}\"", text));
			  sb.append(String.format("\"{%s}\"", text));  /* https://issues.jboss.org/browse/JBQA-5837 - ldimaggi */
		  }
	  }
	  return sb.toString();
  }
  public static String getFormattedTreeNodeText (SWTBotTreeItem item){
    StringBuilder stringBuilder = new StringBuilder("");
    
    if (item != null){
      SWTBotTree tree = 
        new SWTBotTree((Tree)SWTUtilExt.invokeMethodReturnObject(item.widget, "getParent"));
      for (int column = 0 ; column < tree.columnCount(); column++){
        if (column > 0){
          stringBuilder.append(" - ");
        }
        String columnText = item.cell(column);
        if (columnText == null){
          columnText = "<null>";
        } else if (columnText.length( )== 0){
          columnText = "<empty>";
        }
        stringBuilder.append(columnText);
      }
    }
    
    return stringBuilder.toString();
    
  }

  public static String getFormattedTreeNodesText (SWTBotTreeItem[] items){
    StringBuilder stringBuilder = new StringBuilder("");
    
    if (items != null){
      for (SWTBotTreeItem item : items){
        if (stringBuilder.length() > 0){
          stringBuilder.append("\n");
        }
        stringBuilder.append(SWTEclipseExt.getFormattedTreeNodeText(item));
      }
    }
    
    return stringBuilder.toString();
    
  }
  /**
   * Choose Debug As Drools Application menu for specified Tree Item
   * 
   * @param treeItem
   */
  public void debugTreeItemAsDroolsApplication(SWTBotTreeItem treeItem) {
    treeItem.select();
    treeItem.click();
    SWTEclipseExt.getMenuFromSubmenu(
        bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.DEBUG_AS),
        IDELabel.Menu.DEBUG_AS_DROOLS_APPLICATION).click();
  }
  
  
  /**
   * Returns true if parentNode containss Tree Item with nodeLabel
   * @param parentNode
   * @param nodeLabel
   * @return
   */
  public static boolean containsTreeItemWithLabel(SWTBotTreeItem parentNode, String nodeLabel){
    boolean containsItem = true;
    parentNode.expand();
    try{
      parentNode.getNode(nodeLabel);
    } catch (WidgetNotFoundException wnfe){
      containsItem = false;  
    }
    return containsItem;
  }
  /**
   * returns true if path composed from items exists in tree
   * @param tree
   * @param items
   * @return
   */
  public static boolean containstInTree(SWTBotTree tree, String...items) {
		try {			
			SWTBotTreeItem ancestor = tree.getTreeItem(items[0]);
			tree.expandNode(items[0]);
			for (int i=1;i<items.length;i++) {
				try {
					ancestor = ancestor.expandNode(items[i]);
				}
				catch (WidgetNotFoundException ex) {
					return false;
				}				
			}
			return true;
			}
			catch (WidgetNotFoundException ex) {
				return false;
			}	
  }

    /**
     * Returns <code>true</code> if editor with <code>editorLabel</code> exists within bot.
     * static version
     * 
     * @param bot Bot for finding editor.
     * @param editorLabel Title of tested editor.
     * @return <code>true</code> if editor with <code>editorLabel</code> exists,
     *         <code>false</code> otherwise.
     */
    public static boolean existEditorWithLabel(final SWTBotExt bot, final String editorLabel) {
        if (bot == null) {
            throw new NullPointerException("bot cannot be null");
        }
        if (editorLabel == null) {
            throw new NullPointerException("editorLabel cannot be null");
        }
        for (SWTBotEditor editor : bot.editors()) {
            if (editorLabel.equals(editor.getTitle())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns <code>true</code> if editor with <code>editorLabel</code> exists.
     *
     * @param editorLabel Title of tested editor.
     * @return <code>true</code> if editor with <code>editorLabel</code> exists,
     *         <code>false</code> otherwise.
     */
    public boolean existEditorWithLabel(final String editorLabel) {
        return SWTEclipseExt.existEditorWithLabel(bot, editorLabel);
    }

  /**
   * Maximizes active shell
   */
  public void maximizeActiveShell() {
	final Shell shell =  (Shell)(bot.activeShell().widget);
	bot.getDisplay().syncExec(new Runnable() {

		public void run() {
			shell.setMaximized(true);
			
	    }		
    });
  }
  /**
   * Returns SWTBotEditor with title starting with prefix 
   * @param prefix
   * @return
   */
  public SWTBotEditor editorStartsWith (String prefix){
    SWTBotEditor editor = null;
    List<?> editors = bot.editors();
    Iterator<?> iterator = editors.iterator();
    boolean found = false;
    while (iterator.hasNext() && !found){
      editor = ((SWTBotEditor)iterator.next()); 
      found = editor.getTitle().startsWith(prefix);
    }
    if (found){
      return editor;
    }
    else{
      return null;
    }
  }

    /**
     * Closes all opened editors.
     */
    public void closeAllEditors() {
        SWTBotMenu closeAllMenu = bot.menu(IDELabel.Menu.FILE).menu(IDELabel.Menu.CLOSE_ALL);
        if (closeAllMenu.isEnabled()) {
            closeAllMenu.click();
            log.info("All editors were closed");
        } else {
            log.info("No editors to close");
        }
    }

  /**
   * Opens properties dialog of project with projectName
   * @param projectName
   */
  public SWTBotExt openPropertiesOfProject(String projectName){
    PackageExplorer packageExplorer = new PackageExplorer();
    // Open Project Properties
    packageExplorer.show();
    bot.sleep(Timing.time2S());
    packageExplorer.selectProject(projectName);
    Assert.assertTrue(isProjectInPackageExplorer(projectName));
    bot.sleep(Timing.time2S());
    util.waitForNonIgnoredJobs();
    MenuBarHelper
      .getMenu(IDELabel.Menu.PROJECT)
      .menu(IDELabel.Menu.PROPERTIES)
      .click();
    waitForShell(IDELabel.Shell.PROPERTIES_FOR + " " + projectName);
    return bot;
  }
  /**
   * Cleans All Projects
   */
  public void cleanAllProjects(){
    MenuBarHelper.getMenu(IDELabel.Menu.PROJECT).menu(IDELabel.Menu.CLEAN).click();
    bot.shell(IDELabel.Shell.CLEAN).activate();
    bot.radio(IDELabel.CleanProjectDialog.CLEAN_ALL_PROJECTS_RADIO).click();
    bot.button(IDELabel.Button.OK).click();
    util.waitForNonIgnoredJobs();
  }

  /**
   * Deletes file from Package Explorer static version
   * @param bot
   * @param path
   * @return SWTBotEditor
   */
  public static void deleteFile(SWTBotExt bot, String projectName, String... path) {
    SWTBot viewBot = bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).bot();
    bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).show();
    bot.viewByTitle(IDELabel.View.PACKAGE_EXPLORER).setFocus();
    SWTBotTree tree = viewBot.tree();
    SWTBotTreeItem item = tree.expandNode(projectName);
    StringBuilder builder = new StringBuilder(projectName);
  
    // Go through path
    for (String nodeName : path) {
      item = item.expandNode(nodeName);
      builder.append("/" + nodeName);
    }
    
    ContextMenuHelper.prepareTreeItemForContextMenu(tree, item);
    new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, 
      IDELabel.Menu.DELETE, false)).click();
  
    bot.shell(IDELabel.Shell.CONFIRM_DELETE).activate();
    bot.button(IDELabel.Button.OK).click();
    new SWTUtilExt(bot).waitForNonIgnoredJobs();
    
    log.info("File Deleted: " + builder.toString());
  
  }

    // ------------------------------------------------------------
    // Debugging related methods
    // ------------------------------------------------------------

    /**
     * @return <code>true</code> if debug is running, <code>false</code> otherwise
     */
    public boolean isDebugging() {
        return bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.TERMINATE).isEnabled();
    }

    /**
     * Goes through all breakpoints while debug is finished.
     */
    public void finishDebug() {
        SWTBotMenu runMenu = bot.menu(IDELabel.Menu.RUN);
        while (isDebugging()) {
            if (runMenu.menu(IDELabel.Menu.RESUME).isEnabled()) {
                runMenu.menu(IDELabel.Menu.RESUME).click();
            }
            bot.sleep(Timing.time2S());
        }
    }

    /**
     * @return <code>true</code> if debug is suspended at breakpoint,
     *         <code>false</code> otherwise
     */
    public boolean isSuspendedAtBreakpoint() {
        return bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.MENU_STEP_OVER).isEnabled();
    }

    /**
     * Does one step in debugging after stopping at breakpoint.
     */
    public void stepOver() {
        SWTBotMenu stepOverMenu = bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.MENU_STEP_OVER);
        if (stepOverMenu.isEnabled()) {
            stepOverMenu.click();
        } else {
            log.warn("It is not possible to step over. (Step Over menu is disabled)");
        }
    }

    /**
     * Resumes debug (F8).
     */
    public void resumeDebug() {
        SWTBotMenu resumeMenu = bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.MENU_RESUME);
        if (resumeMenu.isEnabled()) {
            resumeMenu.click();
        } else {
            log.info("It is not possible to resume debugging. (Resume menu is disabled)");
        }
    }

    /**
     * Sets breakpoints at desired lines.
     * Before settings new breakpoints all old ones are removed.
     * 
     * @param breakpointsLineNumbers Line numbers of desired breakpoints
     */
    public void setNewBreakpoints(final SWTBotEclipseEditor editor, final int... breakpointsLineNumbers) {
        removeBreakpoints(editor);
        setBreakpoints(editor, breakpointsLineNumbers);
    }

    /**
     * Remove all breakpoints in active editor.
     */
    public void removeBreakpoints(final SWTBotEclipseEditor editor) {
    	if (editor == null) {
            throw new NullPointerException("editor cannot be null");
    	}
    	editor.show();
        SWTBotMenu removeBreakpointMenu = bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.REMOVE_ALL_BREAKPOINTS);
        if (removeBreakpointMenu.isEnabled()) {
            removeBreakpointMenu.click();
            bot.button(IDELabel.Button.YES).click();
        }
    }

    /**
     * This methods sets breakpoints at given lines.
     * 
     * @param breakpointsLineNumbers Line numbers of wanted breakpoints.
     */
    public void setBreakpoints(final SWTBotEclipseEditor editor, final int... breakpointsLineNumbers) {
        if (editor == null) {
            throw new NullPointerException("editor cannot be null");
        }
        editor.show();
        for (int i = 0; i < breakpointsLineNumbers.length; i++) {
            editor.selectRange(breakpointsLineNumbers[i], 0, 0);
            bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.TOGGLE_BREAKPOINT).click();
        }
    }

}