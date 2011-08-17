 /*******************************************************************************
  * Copyright (c) 2007-2010 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.view;

import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt.StringConditionType;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.ViewType;
/**
 * Manage Problems View related tasks
 * @author Vlado Pakan
 *
 */
public class ProblemsView extends ViewBase {

  public static final int PROBLEMS_DESCRIPTION_COLUMN_INDEX = 0;
  public static final int PROBLEMS_RESOURCE_COLUMN_INDEX = 1;
  public static final int PROBLEMS_PATH_COLUMN_INDEX = 2;
  public static final int PROBLEMS_TYPE_COLUMN_INDEX = 4;
	Logger log = Logger.getLogger(ProblemsView.class);
	public ProblemsView() {
		viewObject = ActionItem.View.GeneralProblems.LABEL;
	}

  /**
   * Returns all warnings nodes filtered by input parameters
   * @param bot
   * @param descriptionContains
   * @param pathStartsWith
   * @param resourceText
   * @param typeText
   * @return
   */
  public static SWTBotTreeItem[] getFilteredWarningsTreeItems(SWTBotExt bot, String descriptionContains, String pathStartsWith,
      String resourceText,String typeText){
    
    SWTBotTreeItem[] warningsTreeItems = new SWTBotTreeItem[0];
    SWTBotTreeItem warningsNode = ProblemsView.getWarningsNode(bot);
    SWTBotTree tree = bot.tree();
    if (warningsNode != null){
      warningsNode.expand();
      bot.sleep(SWTEclipseExt.DEFAULT_UI_TIMEOUT);
      warningsTreeItems = ProblemsView.getProblemsTreeItemsContainingText(bot,tree,warningsNode, 
        descriptionContains, pathStartsWith, resourceText, typeText,
        false);
    }
    
    return warningsTreeItems;
  }
  
  /**
   * Returns all errors nodes filtered by input parameters
   * @param bot
   * @param descriptionContains
   * @param pathStartsWith
   * @param resourceText
   * @param typeText
   * @return
   */
  public static SWTBotTreeItem[] getFilteredErrorsTreeItems(SWTBotExt bot, String descriptionContains, String pathStartsWith,
      String resourceText,String typeText){
    
    SWTBotTreeItem[] errorsTreeItems = new SWTBotTreeItem[0];
    SWTBotTreeItem errorsNode = ProblemsView.getErrorsNode(bot);
    SWTBotTree tree = bot.tree();
    if (errorsNode != null){
      errorsNode.expand();
      bot.sleep(SWTEclipseExt.DEFAULT_UI_TIMEOUT);
      errorsTreeItems = ProblemsView.getProblemsTreeItemsContainingText(bot,tree,errorsNode, 
        descriptionContains, pathStartsWith, resourceText, typeText,
        false);
    }
    
    return errorsTreeItems;
    
  }

  
  /**
   * Returns Errors Node from Problems View static version
   * @param bot
   * @return
   */
  public static SWTBotTreeItem getErrorsNode (SWTBotExt bot){
    SWTBotTreeItem errorsNode = null;
    SWTBot problemsBot = SWTEclipseExt.showView(bot,ViewType.PROBLEMS);
    bot.sleep(Timing.time3S());
    try{
      SWTBotTreeItem[] filteredTreeItems = ProblemsView.getProblemsTreeItemsContainingText(bot,problemsBot.tree(),null,
        IDELabel.ProblemsTree.ERRORS,"","","",
        false);
      if (filteredTreeItems != null && filteredTreeItems.length > 0){
        errorsNode = filteredTreeItems[0];
      }
    } catch (WidgetNotFoundException wnfe){
      // do nothing
    }
    return errorsNode;
  }
  /**
   * Returns Warnings Node from Problems View static version
   * @param bot
   * @return
   */
  public static SWTBotTreeItem getWarningsNode (SWTBotExt bot){
    SWTBotTreeItem warningsNode = null;
    SWTBot problemsBot = SWTEclipseExt.showView(bot,ViewType.PROBLEMS);
    bot.sleep(Timing.time3S());
    try{
      SWTBotTreeItem[] filteredTreeItems = ProblemsView.getProblemsTreeItemsContainingText(bot,problemsBot.tree(),null,
        IDELabel.ProblemsTree.WARNINGS,"","","",
        false);
      if (filteredTreeItems != null && filteredTreeItems.length > 0){
        warningsNode = filteredTreeItems[0];
      }
    } catch (WidgetNotFoundException wnfe){
      // do nothing
    }
    return warningsNode;
  }
  /**
   * Returns Tree Items from specified tree with specified attributes
   * When attribute is null then is ignored
   * @param bot
   * @param tree
   * @param parent - when null go thorough all tree items
   * @param descriptionContains
   * @param pathStartsWith
   * @param resourceText
   * @param typeText
   * @param expand
   * @return
   */
  public static SWTBotTreeItem[] getProblemsTreeItemsContainingText (SWTBotExt bot,SWTBotTree tree, SWTBotTreeItem parent,
    String descriptionContains, String pathStartsWith,
    String resourceText,String typeText,
    boolean expand){

    LinkedList<SWTBotTreeItem> treeItems = new LinkedList<SWTBotTreeItem>();
    for (SWTBotTreeItem treeItem : SWTEclipseExt.getAllTreeItemsRecursive(bot, tree, parent, expand)){
      if (ProblemsView.testProblemsTreeItemForStringCondition(treeItem, descriptionContains, 
            ProblemsView.PROBLEMS_DESCRIPTION_COLUMN_INDEX, StringConditionType.CONTAINS)
          && ProblemsView.testProblemsTreeItemForStringCondition(treeItem, pathStartsWith, 
              ProblemsView.PROBLEMS_PATH_COLUMN_INDEX, StringConditionType.STARTS_WITH)
          && ProblemsView.testProblemsTreeItemForStringCondition(treeItem, resourceText, 
              ProblemsView.PROBLEMS_RESOURCE_COLUMN_INDEX, StringConditionType.EQUALS)
          && ProblemsView.testProblemsTreeItemForStringCondition(treeItem, typeText, 
              ProblemsView.PROBLEMS_TYPE_COLUMN_INDEX, StringConditionType.EQUALS)){
        treeItems.add(treeItem);
      }
    }
    
    return treeItems.toArray(new SWTBotTreeItem[treeItems.size()]);
    
  }
  /**
   * Tests Tree Item for condition dependent on specified parameters
   * @param treeItem
   * @param testString - when null returns true
   * @param column - tested column index 
   * @param conditionType
   * @return
   */
  public static boolean testProblemsTreeItemForStringCondition (SWTBotTreeItem treeItem, 
    String testString, int column, StringConditionType conditionType){
    
    boolean isOK = false;
    
    if (testString != null){
      String itemString = treeItem.cell(column);
      if (itemString != null){
        if (conditionType.equals(StringConditionType.STARTS_WITH)){
          isOK = itemString.startsWith(testString);
        }else if (conditionType.equals(StringConditionType.EQUALS)){
          isOK = itemString.equals(testString);
        }else if (conditionType.equals(StringConditionType.CONTAINS)){
          isOK = itemString.indexOf(testString) > -1;
        }
        
      }
      else{
        isOK = false;
      }
    }
    else{
      isOK = true;
    }
    
    return isOK;
    
  }
  
}
