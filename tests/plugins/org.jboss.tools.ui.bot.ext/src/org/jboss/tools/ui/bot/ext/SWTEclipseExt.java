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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.matchers.WidgetMatcherFactory;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.Matcher;
import org.jboss.tools.ui.bot.ext.condition.ButtonIsDisabled;
import org.jboss.tools.ui.bot.ext.entity.JavaClassEntity;
import org.jboss.tools.ui.bot.ext.entity.JavaProjectEntity;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.NewObject.ServerServer;
import org.jboss.tools.ui.bot.ext.gen.IServer;
import org.jboss.tools.ui.bot.ext.gen.IServerRuntime;
import org.jboss.tools.ui.bot.ext.types.EntityType;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.IDELabel.PreferencesDialog;
import org.jboss.tools.ui.bot.ext.types.PerspectiveType;
import org.jboss.tools.ui.bot.ext.types.ViewType;

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
	// View related methods
	// ------------------------------------------------------------
	/**
	 * Close view by text
	 */
	public void closeView(String view) {
		try {
			bot.viewByTitle(view).close();
		} catch (WidgetNotFoundException ex) {
			log.info("WARN - Can't close the view \"" + view + "\"");
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
    bot.shell(IDELabel.Shell.SHOW_VIEW).activate();
		bot.tree().expandNode(type.getGroupLabel()).expandNode(
				type.getViewLabel()).select();
		bot.button(IDELabel.Button.OK).click();

		SWTBot viewBot = bot.viewByTitle(type.getViewLabel()).bot();
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
    try {
		bot.table().select(perspectiveLabel);
    } catch (WidgetNotFoundException e) {
    	log.warn("WARN - Perspecive with label " + perspectiveLabel + " not found, try + (default)");
		bot.table().select(perspectiveLabel + (" (default)"));
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
		File file = util.getResourceFile(pluginId, path);
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
		File file = util.getResourceFile(pluginId, path);
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
		File file = util.getResourceFile(pluginId, path);
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
	  log.info("Adding server: " + serverName);
		SWTBot wiz = open.newObject(ActionItem.NewObject.ServerServer.LABEL);
		open.selectTreeNode(server);
		wiz.textWithLabel(ServerServer.TEXT_SERVER_NAME).setText(serverName);
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
			open.finish(wiz, IDELabel.Button.OK);
		}
	}
	
	public void addJBPMRuntime(String name, String version, String runtimeHome) {
		// TODO - needs to be impl.
		log.info("WARN - Adding JBPM Runtime, needs to be impl.");
	}
	
	public void removeJBPMRuntime(String name) {
		// TODO - needs to be impl
		log.info("WARN - Removing JBPM Runtime, needs to be impl.");	
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
			open.finish(wiz, IDELabel.Button.OK);
		}

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
		pref.button(IDELabel.Button.ADD).click();
		bot.shell("Add JRE").activate();
		SWTBot add = bot.shell("Add JRE").bot();
		add.button(IDELabel.Button.NEXT).click();
		add.text(0).setText(jreHome);
		add.text(1).setText(vmName);
		open.finish(add);
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
  public void closeConfirmPerspectiveSwitchShellIfOpened(
      boolean switchPerspective) {
    try {
      bot.shell(IDELabel.Shell.CONFIRM_PERSPECTIVE_SWITCH).activate();
      bot.button(
          switchPerspective ? IDELabel.Button.YES
              : IDELabel.Button.NO).click();
    } catch (WidgetNotFoundException wnfe) {
      // do nothing
    }
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
	 * Returns true when projectName is present in Package Explorer static
	 * version
	 * 
	 * @param bot
	 * @param projectName
	 * @return
	 */
	public static boolean isProjectInPackageExplorer(SWTBotExt bot,
			String projectName) {
		boolean found = false;

		SWTBot innerBot = SWTEclipseExt
				.showView(bot, ViewType.PACKAGE_EXPLORER);
		SWTBotTree tree = innerBot.tree();
		try {
			tree.getTreeItem(projectName);
			found = true;
		} catch (WidgetNotFoundException e) {
		}
		return found;
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
		SWTEclipseExt.getMenuFromSubmenu(
				bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.RUN_AS),
				IDELabel.Menu.RUN_AS_JAVA_APPLICATION).click();
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
      if (expand) {
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
  
  public static String getFormattedTreeNodeText (SWTBotTree tree, SWTBotTreeItem item){
    StringBuilder stringBuilder = new StringBuilder("");
    
    if (item != null){
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

  public static String getFormattedTreeNodesText (SWTBotTree tree, SWTBotTreeItem[] items){
    StringBuilder stringBuilder = new StringBuilder("");
    
    if (items != null){
      for (SWTBotTreeItem item : items){
        if (stringBuilder.length() > 0){
          stringBuilder.append("\n");
        }
        stringBuilder.append(SWTEclipseExt.getFormattedTreeNodeText(tree,item));
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
   * Returns true if parentNode contains Tree Item with nodeLabel
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
   * Returns true if editor with editorLabel exists within bot
   * static version
   * @param bot
   * @param editorLabel
   * @return
   */
  public static boolean existEditorWithLabel(SWTBotExt bot, String editorLabel){
    boolean editorExists = true;
    try{
      bot.editorByTitle(editorLabel);
    } catch (WidgetNotFoundException wnfe){
      editorExists = false;  
    }
    return editorExists;
  }
  /**
   * Returns true if editor with editorLabel exists
   * @param editorLabel
   * @return
   */
  public boolean existEditorWithLabel(String editorLabel){
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
}