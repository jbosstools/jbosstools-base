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

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withMnemonic;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.instanceOf;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.Matcher;

/**
 * Helper to find context menu of widget
 * @author Vladimir Pakan
 *
 */
public class ContextMenuHelper {
  protected static final Logger log = Logger.getLogger(ContextMenuHelper.class);
  /**
   * Clicks the context menu matching the text.
   *
   * @param bot bot containing context menu.
   * @param text the text on the context menu.
   * @param hideAfterwards hide menu when method is finished.
   * @throws WidgetNotFoundException if the widget is not found.
   */
  public static MenuItem getContextMenu(final AbstractSWTBot<?> bot,
      final String text, final boolean hideAfterwards) {
	  final List<String> foundMenuItems = new Vector<String>();
    final MenuItem menuItem = UIThreadRunnable
        .syncExec(new WidgetResult<MenuItem>() {
          @SuppressWarnings("unchecked")
		public MenuItem run() {
            MenuItem menuItem = null;
            Menu menu = getWidgetMenu(bot.widget);
            Matcher<?> matcher = allOf(instanceOf(MenuItem.class),withMnemonic(text));
            menuItem = show(menu, matcher, hideAfterwards, foundMenuItems);
            if (menuItem != null) {
              menu = menuItem.getMenu();
            } else {
              hide(menu);
            }
            return menuItem;
          }
        });
    if (menuItem == null) {
      throw new WidgetNotFoundException("Could not find menu: '" + text+"', found items :"+Arrays.toString(foundMenuItems.toArray()));
    }
    else{
      if (hideAfterwards){
        // hide
        UIThreadRunnable.syncExec(new VoidResult() {
          public void run() {
            hide(menuItem.getParent());
          }
        });
      }

      return menuItem;
    }  
  }
  /**
   * Simulate Show event to menu and returns MenuItem matching to matcher
   * @param menu
   * @param matcher
   * @param hideAfterwards
   * @param foundMenuItems list of menuItems found (useful for debugging), items found in menu are appended into given list, can be null
   * @return
   */
  private static MenuItem show(final Menu menu, final Matcher<?> matcher, final boolean hideAfterwards, List<String> foundMenuItems) {
    if (menu != null) {
      menu.notifyListeners(SWT.Show, new Event());
      MenuItem[] items = menu.getItems();
      for (final MenuItem menuItem : items) {
    	  if (foundMenuItems!=null) {
    		  foundMenuItems.add(menuItem.getText());
    	  }
    	  if (matcher.matches(menuItem)) {
          return menuItem;
        }
      }
      if (hideAfterwards){
        menu.notifyListeners(SWT.Hide, new Event());
      }
    }
    return null;
  }
  /**
   * Recursively hide menus
   * @param menu bottom menu to start hiding from
   */
  private static void hide(final Menu menu) {
    menu.notifyListeners(SWT.Hide, new Event());
    if (menu.getParentMenu() != null) {
      hide(menu.getParentMenu());
    }
  }
  /**
   * Returns menu of input widget
   * @param widget
   * @return
   */
  private static Menu getWidgetMenu (Widget widget){
    
    Menu result = null;
    
    if (widget instanceof MenuItem){
      result = ((MenuItem)widget).getMenu();
    }
    else{
      result = ((Control)widget).getMenu();
    }
    
    return result;
    
  }
  /**
   * Executes proper steps to be able call getContextMenu on input treeItem
   * @param tree
   * @param treeItem
   */
  public static void prepareTreeItemForContextMenu(SWTBotTree tree , SWTBotTreeItem treeItem){
    tree.setFocus();
    treeItem.select();
    treeItem.click();
  }
  /**
   * Executes proper steps to be able call getContextMenu on first Tree Item within tree
   * @param tree
   */
  public static void prepareTreeItemForContextMenu(SWTBotTree tree){
    
    tree.setFocus();
    if (tree.getAllItems().length > 0){
      tree.select(0);
    }
    
  }
  /**
   * Executes proper steps to be able call getContextMenu on Tree Item within tree on specified position
   * @param tree
   * @param index - zero based index of Tree Item to be selected
   */
  public static void prepareTreeItemForContextMenu(SWTBotTree tree, int index){
    
    tree.setFocus();
    SWTBotTreeItem[] treeItems = tree.getAllItems(); 
    if (treeItems.length > 0){
      ContextMenuHelper.prepareTreeItemForContextMenu(tree,treeItems[index]);
    }
    
  }
  /**
   * Clicks the context menu matching the text.
   * @param topMenu first menu in submenu path to requested menu item 
   * @param text
   *          the text on the context menu.
   * @throws WidgetNotFoundException
   *           if the widget is not found.
   */
  public static void clickContextMenu(final Menu topMenu, final String... texts) {
    final MenuItem menuItem = UIThreadRunnable.syncExec(new WidgetResult<MenuItem>() {
      @SuppressWarnings("unchecked")
      public MenuItem run() {
        MenuItem menuItem = null;
        Menu menu = topMenu;
        for (String text : texts) {
          Matcher<?> matcher = allOf(instanceOf(MenuItem.class),withMnemonic(text));
          menuItem = show(menu, matcher, false, null);
          if (menuItem != null) {
            menu = menuItem.getMenu();
          } else {
            hide(menu);
            break;
          }
        }
        return menuItem;
      }
    });
    if (menuItem == null) {
      throw new WidgetNotFoundException("Could not find menu: "
          + Arrays.asList(texts));
    }
    // click
    click(menuItem);
    // hide
    UIThreadRunnable.syncExec(new VoidResult() {
      public void run() {
        hide(menuItem.getParent());
      }
    });

  }

  /**
   * Clicks the context menu matching the text.
   * @param control Control containing Menu 
   * @param text
   *          the text on the context menu.
   * @throws WidgetNotFoundException
   *           if the widget is not found.
   */
  public static void clickContextMenu(final Control control,
      final String... texts) {

    final Menu topMenu = UIThreadRunnable.syncExec(new WidgetResult<Menu>() {
      public Menu run() {
        return control.getMenu();
        }
      });
    if (topMenu == null) {
      throw new WidgetNotFoundException("Could not find menu: "
          + Arrays.asList(texts));
    }
    clickContextMenu(topMenu, texts);
  }
  
  /**
   * Clicks the context menu matching the text.
   * @param bot bot containing Menu 
   * @param text
   *          the text on the context menu.
   * @throws WidgetNotFoundException
   *           if the widget is not found.
   */
  public static void clickContextMenu(final AbstractSWTBot<?> bot,
      final String... texts) {

    final Control parentControl = UIThreadRunnable
        .syncExec(new WidgetResult<Control>() {
    	public Control run() {
            return (Control) bot.widget;
      }
    });
    
    clickContextMenu(parentControl, texts);
    
  }
  
  private static void click(final MenuItem menuItem) {
	    final Event event = new Event();
	    event.time = (int) System.currentTimeMillis();
	    event.widget = menuItem;
	    event.display = menuItem.getDisplay();
	    event.type = SWT.Selection;

	    UIThreadRunnable.asyncExec(menuItem.getDisplay(), new VoidResult() {
	      public void run() {
	        log.info("Click on menu item: " + menuItem.getText());
	        menuItem.notifyListeners(SWT.Selection, event);
	      }
	    });
	  }


} 
