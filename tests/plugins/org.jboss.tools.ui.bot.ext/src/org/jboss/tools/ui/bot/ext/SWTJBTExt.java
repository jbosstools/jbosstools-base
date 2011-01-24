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

import static org.jboss.tools.ui.bot.ext.SWTTestExt.eclipse;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.ViewType;
import org.jboss.tools.ui.bot.ext.types.IDELabel.PreferencesDialog;
import org.osgi.framework.Version;
/**
 * Provides JBoss Tools common operations based on SWTBot element operations
 * @author Vladimir Pakan
 *
 */
public class SWTJBTExt {
  
  private static final long DEFAULT_UI_TIMEOUT = 1000L;
  
	SWTWorkbenchBot bot;
	Logger log = Logger.getLogger(SWTJBTExt.class);
	
	public SWTJBTExt(SWTWorkbenchBot bot) {
		this.bot = bot;
	}
	
	/**
	 * Check if JBoss Developer Studio Is Running
	 * Dynamic version of isJBDSRun Method
	 * @return
	 */
  public static boolean isJBDSRun (){
    IProduct prod = Platform.getProduct();
    return prod != null;
  }
  /*
   * 
   * check if JBoss Developer Studio is running 
   */
  @Deprecated
  public static boolean isJBDSRun(SWTBot bot) {
	  return isJBDSRun();
  }

	/**
	 * Returns JBT version (taken from version of org.jboss.tools.common plugin version)
	 * @return
	 */
	public Version getJBTVersion() {
		return Platform.getBundle("org.jboss.tools.common").getVersion();
	}
	/**
	 * Returns true when in Web Page of Wizard is defined at least one Server Runtime Instance
	 * @param bot
	 * @return
	 */
  public static boolean isServerDefinedInWebWizardPage(SWTWorkbenchBot bot){
    boolean isServerDefined = false;
    try{
      bot.label(IDELabel.ImportJSFProjectDialog.CHOICE_LIST_IS_EMPTY);
    } catch (WidgetNotFoundException wnfe){
      isServerDefined = true;
    }
    return isServerDefined;
  }
  /**
   * Return true when in Web Page of Wizard is defined at least one Server Runtime Instance
   * Dynamic version of isServerDefinedInWebWizardPage
   * @return
   */
  public boolean isServerDefinedInWebWizardPage(){
    return SWTJBTExt.isServerDefinedInWebWizardPage(bot);
  }
  /**
   * Starts Application Server in Server View on position specified by index
   * Dynamic version of startApplicationServer
   * @param index - zero based Position of Server within Server Tree
   * @param uiTimeOut
   */
  public void startApplicationServer(int index, long uiTimeOut){
    SWTJBTExt.startApplicationServer(bot, index, uiTimeOut);
  }
  /**
   * Starts Application Server in Server View on position specified by index
   * @param bot
   * @param index - zero based Position of Server within Server Tree
   * @param uiTimeOut
   */
  public static void startApplicationServer(SWTWorkbenchBot bot , int index, long uiTimeOut){
    SWTJBTExt.chooseServerPopupMenu(bot,index, IDELabel.Menu.START,120*1000L,uiTimeOut);
    bot.sleep(10*1000L);
  }
  /**
   * Starts Application Server in Server View on position specified by index
   * Dynamic version of startApplicationServer with default UI TimeOut
   * @param index - zero based Position of Server within Server Tree
   */
  public void startApplicationServer(int index){
    SWTJBTExt.startApplicationServer(bot, index, SWTJBTExt.DEFAULT_UI_TIMEOUT);
  }
  /**
   * Starts Application Server in Server View on position specified by index
   * with default UI TimeOut
   * @param bot
   * @param index - zero based Position of Server within Server Tree
   */
  public static void startApplicationServer(SWTWorkbenchBot bot , int index){
    startApplicationServer(bot , index, SWTJBTExt.DEFAULT_UI_TIMEOUT);
  }
  /**
   * Stops Application Server in Server View on position specified by index
   * with default UI TimeOut
   * Dynamic version of stopApplicationServer
   * @param index - zero based Position of Server within Server Tree
   */
  public void stopApplicationServer(int index){
    SWTJBTExt.stopApplicationServer(bot, index, SWTJBTExt.DEFAULT_UI_TIMEOUT);
  }
  /**
   * Stops Application Server in Server View on position specified by index
   * Dynamic version of stopApplicationServer
   * @param index - zero based Position of Server within Server Tree
   * @param uiTimeOut
   */
  public void stopApplicationServer(int index,long uiTimeOut){
    SWTJBTExt.stopApplicationServer(bot, index);
  }
  /**
   * Stops Application Server in Server View on position specified by index
   * with default UI TimeOut
   * @param bot
   * @param index - zero based Position of Server within Server Tree
   */
  public static void stopApplicationServer(SWTWorkbenchBot bot , int index){
    SWTJBTExt.chooseServerPopupMenu(bot,index, IDELabel.Menu.STOP,20*1000L);
  }
  /**
   * Stops Application Server in Server View on position specified by index
   * @param bot
   * @param index - zero based Position of Server within Server Tree
   * @param iuTimeOut
   */
  public static void stopApplicationServer(SWTWorkbenchBot bot , int index,long uiTimeOut){
    SWTJBTExt.chooseServerPopupMenu(bot,index, IDELabel.Menu.STOP,20*1000L,uiTimeOut);
  }
  /**
   * Choose Server Popup Menu with specified label on Server with position specified by index 
   * @param bot
   * @param index
   * @param menuLabel
   * @param timeOut
   * @param uiTimeOut
   */
  public static void chooseServerPopupMenu(SWTWorkbenchBot bot , int index, String menuLabel, long timeOut, long uiTimeOut){
    SWTEclipseExt swtEclipseExt = new SWTEclipseExt();
    SWTBot servers = swtEclipseExt.showView(ViewType.SERVERS);
    SWTBotTree serverTree = servers.tree();
    ContextMenuHelper.prepareTreeItemForContextMenu(serverTree, index);
    SWTTestExt.util.waitForAll(uiTimeOut); 
    SWTBotMenu menu = new SWTBotMenu(ContextMenuHelper.getContextMenu(serverTree,
        menuLabel, false));
    SWTTestExt.util.waitForAll(uiTimeOut);
    menu.click();
    SWTTestExt.util.waitForAll(timeOut);    
  }
  /**
   * Choose Server Popup Menu with specified label on Server with position specified by index
   * with defaul UI TimeOut  
   * @param bot
   * @param index
   * @param menuLabel
   * @param timeOut
   */
  public static void chooseServerPopupMenu(SWTWorkbenchBot bot , int index, String menuLabel, long timeOut){
    chooseServerPopupMenu(bot,index,menuLabel,timeOut,SWTJBTExt.DEFAULT_UI_TIMEOUT);
  }
  /**
   * Deletes Application Server in Server View on position specified by index
   * Dynamic version of deleteApplicationServer
   * @param index - zero based Position of Server within Server Tree
   */
  public void deleteApplicationServer(int index){
    SWTJBTExt.deleteApplicationServer(bot, index);
  }
  /**
   * Deletes Application Server in Server View on position specified by index
   * @param bot
   * @param index - zero based Position of Server within Server Tree
   */
  public static void deleteApplicationServer(SWTWorkbenchBot bot , int index){
    SWTJBTExt.chooseServerPopupMenu(bot,index, IDELabel.Menu.DELETE,10*1000L);
    bot.shell(IDELabel.Shell.DELETE_SERVER).activate();
    bot.button(IDELabel.Button.OK).click();
  }
  /**
   * Remove Project from all Servers
   * @param projectName 
   */
  public void removeProjectFromServers(String projectName){
    removeProjectFromServers(projectName, null);
  }
  /**
   * Remove Project from all Servers
   * @param projectName
   * @param stringToContain
   */
  public void removeProjectFromServers(String projectName , String stringToContain){
    
    eclipse.showView(ViewType.SERVERS);
    
    delay();
    
    SWTBotTree serverTree = bot.viewByTitle(IDELabel.View.SERVERS).bot().tree();
    
    delay();
    
    // Expand All
    for (SWTBotTreeItem serverTreeItem : serverTree.getAllItems()){
      serverTreeItem.expand();
      // if JSF Test Project is deployed to server remove it
      SWTBotTreeItem[] serverTreeItemChildren = serverTreeItem.getItems();
      if (serverTreeItemChildren != null && serverTreeItemChildren.length > 0){
        int itemIndex = 0;
        boolean found = false;
        String treeItemlabel = null;
        do{
          treeItemlabel = serverTreeItemChildren[itemIndex].getText();
          found = treeItemlabel.startsWith(projectName)
                  && (stringToContain == null || treeItemlabel.indexOf(stringToContain) >= 0);
        } while (!found && ++itemIndex < serverTreeItemChildren.length);
        // Server Tree Item has Child with Text equal to JSF TEst Project
        if (found){
          log.info("Found project to be removed from server: " + treeItemlabel);
          ContextMenuHelper.prepareTreeItemForContextMenu(serverTree,serverTreeItemChildren[itemIndex]);
          new SWTBotMenu(ContextMenuHelper.getContextMenu(serverTree, IDELabel.Menu.REMOVE, false)).click();
          bot.shell("Server").activate();
          bot.button(IDELabel.Button.OK).click();
          log.info("Removed project from server: " + treeItemlabel);
          bot.sleep(10*1000L);
        }  
      }
    }
    delay();
  }
  
  public void delay() {
    bot.sleep(500);
  }
  /**
   * Delete Project from workspace
   * @param projectName
   */
  public void deleteProject(String projectName) {

    removeProjectFromServers(projectName);
    
    SWTBot packageExplorer = eclipse.showView(ViewType.PACKAGE_EXPLORER);
    delay();
    SWTBotTree tree = packageExplorer.tree();
    delay();
    
    ContextMenuHelper.prepareTreeItemForContextMenu(tree,
      tree.getTreeItem(projectName));
    new SWTBotMenu(ContextMenuHelper.getContextMenu(tree,
      IDELabel.Menu.DELETE, false)).click();
    bot.shell(IDELabel.Shell.DELETE_RESOURCES).activate();
    bot.button(IDELabel.Button.OK).click();
    
    new SWTUtilExt(bot).waitForNonIgnoredJobs();
    
  }
  /**
   * Choose Run On Server menu for specified project
   * @param bot
   * @param projectName
   */
  public static void runProjectOnServer(SWTWorkbenchBot bot, String projectName){
 
    SWTBotTree packageExplorerTree = eclipse.showView(ViewType.PACKAGE_EXPLORER).tree();

    packageExplorerTree.setFocus();
    SWTBotTreeItem packageExplorerTreeItem = packageExplorerTree
        .getTreeItem(projectName);
    
    packageExplorerTreeItem.select();
    packageExplorerTreeItem.click();
    // Search for Menu Item with Run on Server substring within label
    final SWTBotMenu menuRunAs = bot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.RUN_AS);
    final MenuItem menuItem = UIThreadRunnable
      .syncExec(new WidgetResult<MenuItem>() {
        public MenuItem run() {
          int menuItemIndex = 0;
          MenuItem menuItem = null;
          final MenuItem[] menuItems = menuRunAs.widget.getMenu().getItems();
          while (menuItem == null && menuItemIndex < menuItems.length){
            if (menuItems[menuItemIndex].getText().indexOf("Run on Server") > - 1){
              menuItem = menuItems[menuItemIndex];
            }
            else{
              menuItemIndex++;
            }
          }
        return menuItem;
        }
      });
    if (menuItem != null){
      new SWTBotMenu(menuItem).click();
      bot.shell(IDELabel.Shell.RUN_ON_SERVER).activate();
      bot.button(IDELabel.Button.FINISH).click();
      SWTUtilExt swtUtil = new SWTUtilExt(bot);      
      swtUtil.waitForAll(10*1000L);
    }
    else{
      throw new WidgetNotFoundException("Unable to find Menu Item with Label 'Run on Server'");
    }
  }
  /**
   * Choose Run On Server menu for specified project
   * @param projectName
   */
  public void runProjectOnServer(String projectName){
    runProjectOnServer(bot,projectName);
  }
  /**
   * Creates new Server within Server View when Wizard for new Project is called
   * @param bot
   * @param serverGroup
   * @param serverType
   */
  public static void addServerToServerViewOnWizardPage (SWTWorkbenchBot bot,String serverGroup , String serverType){
    // Check if there is defined Application Server if not create one
    if (!SWTJBTExt.isServerDefinedInWebWizardPage(bot)){
      // Specify Application Server for Deployment
      bot.button(IDELabel.Button.NEW, 1).click();
      bot.shell(IDELabel.Shell.NEW_SERVER).activate();
      bot.tree().select(serverGroup);
      bot.tree().expandNode(serverGroup)
        .select(serverType);
      bot.button(IDELabel.Button.FINISH).click();
    }  
    
  }
  /**
   * Creates new Server within Server View when Wizard for new Project is called
   * @param serverGroup
   * @param serverType
   */
  public void addServerToServerViewOnWizardPage (String serverGroup , String serverType){
    addServerToServerViewOnWizardPage (bot,serverGroup , serverType);
  }
  /**
   * Returns true if runtimeName Server Runtime is defined
   * @param runtimeName
   * @return
   */
  public boolean isServerRuntimeDefined(String runtimeName){
    return SWTJBTExt.isServerRuntimeDefined(bot,runtimeName);
  }

  /**
   * Returns true if runtimeName Server Runtime is defined
   * @param bot
   * @param runtimeName
   * @return
   */
  public static boolean isServerRuntimeDefined(SWTWorkbenchBot bot,String runtimeName){
    
    boolean serverRuntimeNotDefined = true;
    
    bot.menu(IDELabel.Menu.WINDOW).menu(IDELabel.Menu.PREFERENCES).click();
    bot.shell(IDELabel.Shell.PREFERENCES).activate();
    bot.tree().expandNode(IDELabel.PreferencesDialog.SERVER_GROUP).select(
      PreferencesDialog.RUNTIME_ENVIRONMENTS);
    
    SWTBotTable tbRuntimeEnvironments = bot.table();
    int numRows = tbRuntimeEnvironments.rowCount();
    if (numRows > 0) {
      int currentRow = 0;
      while (serverRuntimeNotDefined && currentRow < numRows) {
        if (tbRuntimeEnvironments.cell(currentRow, 0).equalsIgnoreCase(
            runtimeName)) {
          serverRuntimeNotDefined = false;
        } else {
          currentRow++;
        }
      }
    }  
   
    bot.button(IDELabel.Button.OK).click();
    
    return !serverRuntimeNotDefined;
      
  }
  /**
   * Returns true if any Server Runtime is defined
   * @param bot
   * @return
   */
  public static boolean isServerRuntimeDefined(SWTWorkbenchBot bot){
    
    bot.menu(IDELabel.Menu.WINDOW).menu(IDELabel.Menu.PREFERENCES).click();
    bot.shell(IDELabel.Shell.PREFERENCES).activate();
    bot.tree().expandNode(IDELabel.PreferencesDialog.SERVER_GROUP).select(
      PreferencesDialog.RUNTIME_ENVIRONMENTS);
    boolean isServerRuntimeDefined = bot.table().rowCount() > 0;
    
    bot.button(IDELabel.Button.OK).click();
    
    return isServerRuntimeDefined;
      
  }

  /**
   * Returns true if any Server Runtime is defined
   * @param bot
   * @return
   */
  public boolean isServerRuntimeDefined(){
    
    return SWTJBTExt.isServerRuntimeDefined(bot);
      
  }

  public void removeSeamProjectFromServers(String projectName){
    removeProjectFromServers(projectName);
    removeProjectFromServers("/" + projectName,"-ds.xml");
  }
  /**
   * Returns string representing version of defined Server Runtime on rowIndex position in Defined Server Runtime table
   * @param bot
   * @param rowIndex
   * @return null when no server runtime is specified, "unknown when not possible to determine server runtime version" or server runtime version
   */
  public static String getDefinedServerRuntimeVersion(SWTWorkbenchBot bot , int rowIndex){
    
    String result = null;
    
    bot.menu(IDELabel.Menu.WINDOW).menu(IDELabel.Menu.PREFERENCES).click();
    bot.shell(IDELabel.Shell.PREFERENCES).activate();
    bot.tree().expandNode(IDELabel.PreferencesDialog.SERVER_GROUP).select(
      PreferencesDialog.RUNTIME_ENVIRONMENTS);
    
    SWTBotTable serverRuntimesTable = bot.table(); 
    if (serverRuntimesTable.rowCount() > rowIndex){
      String[] splitServerRuntimeType = serverRuntimesTable.cell(rowIndex, 1).split(" ");
      int index = 0;
      while (index < splitServerRuntimeType.length && result == null){
        if (splitServerRuntimeType[index].length() > 0 &&
            splitServerRuntimeType[index].charAt(0) >= '0' &&
            splitServerRuntimeType[index].charAt(0) <= '9'){
          result = splitServerRuntimeType[index].trim();
        }
        else{
          index++;
        }
      }
    }
    
    bot.button(IDELabel.Button.OK).click();
    
    return result;
      
  }

  /**
   * Returns string representing version of defined Server Runtime on index position in Defined Server Runtime table
   * @param rowIndex
   * @return null when no server runtime is specified, "unknown when not possible to determine server runtime version" or server runtime version
   */
  public String getDefinedServerRuntimeVersion(int index){
    
    return SWTJBTExt.getDefinedServerRuntimeVersion(bot,index);
      
  }

	/**
	 * Closes RH Report Usage Dialog if found
	 * 
	 * @param cancel
	 *            select if ok or cancel button is used
	 */
	public void closeReportUsageWindowIfOpened(boolean cancel) {

		String title;
		if (isJBDSRun()) {
			title = "JBoss Developer Studio Usage";
		} else {
			title = "JBoss Tools Usage";
		}
		try {

			bot.shell(title).activate();
			bot.button(cancel ? IDELabel.Button.NO : IDELabel.Button.YES)
					.click();
			log.info("Report usage window closed");
		} catch (WidgetNotFoundException wnfe) {
			log.info("Report usage window didn't appear");
		}
	}
}
