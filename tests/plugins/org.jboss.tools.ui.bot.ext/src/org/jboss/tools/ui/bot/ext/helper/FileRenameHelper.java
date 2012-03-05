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
package org.jboss.tools.ui.bot.ext.helper;

import static org.jboss.tools.ui.bot.ext.SWTTestExt.eclipse;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.ViewType;

/**
 * Check Renaming Functionality within WebProjects View
 * Tests if file was properly renamed in WebProjects View
 * and Title of file in Editor was renamed also. 
 * @author Vladimir Pakan
 *
 */
public class FileRenameHelper {
  private static final int sleepTime = Timing.time2S();
  /**
   * Check File Renaming
   * @param bot
   * @param oldFileName
   * @param newFileName
   * @param treePathItems
   * @param fileTreeItemSuffix
   * @return
   */
  public static String checkFileRenamingWithinWebProjects(SWTWorkbenchBot bot , String oldFileName, String newFileName, 
      String[] treePathItems , String fileTreeItemSuffix){
    
    bot.sleep(sleepTime);    
    SWTBot webProjects = eclipse.showView(ViewType.WEB_PROJECTS);
    SWTBotTree tree = webProjects.tree();

    tree.setFocus();
    
    if (treePathItems != null && treePathItems.length > 0){
      SWTBotTreeItem parentTreeItem = tree.getTreeItem(treePathItems[0]);
      parentTreeItem.expand();
      bot.sleep(Timing.time1S());  
      parentTreeItem.select();
      bot.sleep(Timing.time1S());
      // Do not remove this part of code otherwise tree view is not populated properly
      parentTreeItem.collapse();
      bot.sleep(Timing.time1S());  
      parentTreeItem.expand();
      bot.sleep(Timing.time1S());
      int index = 1;
      while (treePathItems.length > index){
        parentTreeItem = parentTreeItem.getNode(treePathItems[index]);
        parentTreeItem.expand();
        index++;
      }
      // Open File
      ContextMenuHelper.prepareTreeItemForContextMenu(tree , parentTreeItem.getNode(oldFileName + fileTreeItemSuffix));
      new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.OPEN, true)).click();
      bot.sleep(sleepTime); 
      // Rename file
      new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.RENAME, true)).click();
      bot.sleep(sleepTime); 
      bot.shell(IDELabel.Shell.RENAME_RESOURCE).activate();
      bot.textWithLabel(IDELabel.RenameResourceDialog.NEW_NAME)
        .setText(newFileName);
      bot.button(IDELabel.Button.OK).click();
      new SWTUtilExt(bot).waitForAll(60 * 1000L);
      bot.sleep(Timing.time5S());
      // Check Results
      // File with Old Name doesn't exists within WebProjects View
      try{
        parentTreeItem.getNode(oldFileName + fileTreeItemSuffix);
        return "File " + oldFileName + " was not renamed to " + newFileName + ".";
      }catch (WidgetNotFoundException wnfe) {
        // do nothing 
      }
      // File with New Name exists within WebProjects View
      try{
        parentTreeItem.getNode(newFileName + fileTreeItemSuffix);
      }catch (WidgetNotFoundException wnfe) {
        return "Renamed File " + newFileName + " was not found."; 
      }
      // Editor Title was renamed
      bot.sleep(Timing.time5S());
      try{
        bot.editorByTitle(newFileName);
      }catch (WidgetNotFoundException wnfe) {
        return "Editor Title was not changed to " + newFileName + " after renaming."; 
      }
    }
    else{
      return "Unable to find file for renaming.";
    }
    
    return null;
    
  }
  /**
   * Check File Renaming
   * @param bot
   * @param oldFileName
   * @param newFileName
   * @param treePathItems
   * @return
   */
  public static String checkFileRenamingWithinWebProjects(SWTWorkbenchBot bot , String oldFileName, String newFileName, 
      String[] treePathItems){
    return checkFileRenamingWithinWebProjects(bot, oldFileName, newFileName, treePathItems, "");
  }
  /**
   * Check Project Renaming within Package Explorer
   * @param bot
   * @param oldProjectName
   * @param newProjectName
   * @return
   */
  public static String checkProjectRenamingWithinPackageExplorer(SWTWorkbenchBot bot , 
    String oldProjectName, 
    String newProjectName,
    String renameShellTitle){
    
    bot.sleep(sleepTime);    
    SWTBotTree tree = eclipse.showView(ViewType.PACKAGE_EXPLORER).tree();

    tree.setFocus();
    tree.getTreeItem(oldProjectName).select();
    bot.sleep(Timing.time1S());  
    // Rename project
    bot.menu(IDELabel.Menu.FILE).
      menu("Rename...")
      .click();
    bot.sleep(Timing.time1S()); 
    bot.shell(renameShellTitle).activate();
    bot.textWithLabel(IDELabel.RenameResourceDialog.NEW_NAME)
      .setText(newProjectName);
    bot.button(IDELabel.Button.OK).click();
    new SWTUtilExt(bot).waitForAll(Timing.time30S());
    // Check Results
    // Project with Old Name doesn't exists within Package explorer
    try{
      tree.getTreeItem(oldProjectName);
      return "Project " + oldProjectName + " was not renamed to " + newProjectName + ".";
    }catch (WidgetNotFoundException wnfe) {
      // do nothing 
    }
    // Project with New Name exists within Package Explorer
    try{
      tree.getTreeItem(newProjectName);
    }catch (WidgetNotFoundException wnfe) {
        return "Renamed Project " + newProjectName + " was not found."; 
    }
    
  return null;
    
  }

}
