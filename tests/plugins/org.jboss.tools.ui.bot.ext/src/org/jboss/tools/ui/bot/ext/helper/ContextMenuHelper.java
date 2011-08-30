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

import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Decorations;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.Matcher;
import org.jboss.tools.ui.bot.ext.SWTJBTExt;

/**
 * Helper to find context menu of widget
 * @author Vladimir Pakan
 *
 */
public class ContextMenuHelper {
  protected static final Logger log = Logger.getLogger(ContextMenuHelper.class);
  /**
   * Returns the context menu item matching the text.
   *
   * @param bot bot containing context menu.
   * @param text the text on the context menu.
   * @param hideAfterwards hide menu when method is finished.
   * @throws WidgetNotFoundException if the widget is not found.
   */
  public static MenuItem getContextMenu(final AbstractSWTBot<?> bot,
      final String text, final boolean hideAfterwards) {
    final Menu menu = UIThreadRunnable.syncExec(new WidgetResult<Menu>() {
  		public Menu run() {
        return getWidgetMenu(bot.widget);
      }
    });
    if (menu == null) {
      throw new WidgetNotFoundException("Could not find menu of active bot: " + bot);
    }
    else{
      return ContextMenuHelper.getContextMenu(menu, text, hideAfterwards);
    }

  }
  /**
   * Returns the context menu item matching the text.
   *
   * @param menu top parent menu containing menu item.
   * @param text the text on the context menu.
   * @param hideAfterwards hide menu when method is finished.
   * @throws WidgetNotFoundException if the widget is not found.
   */
  public static MenuItem getContextMenu(final Menu menu,
      final String text, final boolean hideAfterwards) {
    final List<String> foundMenuItems = new Vector<String>();
    final MenuItem menuItem = UIThreadRunnable
        .syncExec(new WidgetResult<MenuItem>() {
          @SuppressWarnings("unchecked")
    public MenuItem run() {
            MenuItem menuItem = null;
            Matcher<?> matcher = allOf(instanceOf(MenuItem.class),withMnemonic(text));
            menuItem = show(menu, matcher, hideAfterwards, foundMenuItems);
            Menu currentMenu = menu;
            if (menuItem != null) {
              currentMenu = menuItem.getMenu();
            } else {
              hide(currentMenu);
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
   * Hides menus recursively dependent on hideRecursively parameter
   * @param menu bottom menu to start hiding from
   * @param hideRecursively
   */
  private static void hide(final Menu menu , boolean hideRecursively) {
    if (menu != null){
      menu.notifyListeners(SWT.Hide, new Event());
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
      // Hide recursively
      if (hideRecursively) {
        if (SWTJBTExt.isRunningOnMacOs()){
          KeyboardHelper.typeKeyCodeUsingAWT(KeyEvent.VK_ESCAPE);
          try {
            Thread.sleep(100);
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        }
        else{
          if (menu.getParentMenu() != null) {
            hide(menu.getParentMenu(),hideRecursively);
          }
        }
      }
    }
  }
  /**
   * Hides menus recursively dependent on hideRecursively parameter
   * @param menu bottom menu to start hiding from
   */
  private static void hide(final Menu menu) {
    ContextMenuHelper.hide (menu,true);
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

  /**
   * Returns tree menu which contains menuLabel
   * 
   * @param tree
   * @param menuLabel
   * @return
   */
  public static Menu getTreeMenuViaReflections(final Tree tree,
      final String menuLabel) {
    // Menu has to be already created
    return UIThreadRunnable.syncExec(new WidgetResult<Menu>() {
      public Menu run() {
        Menu result = null;
        Composite parent = tree.getParent();
        while (!(parent instanceof Decorations)) {
          parent = parent.getParent();
        }
        try {
          Menu[] menus = ReflectionsHelper.getPrivateFieldValue(
              Decorations.class, "menus", parent, Menu[].class);
          if (menus != null) {
            MenuItem topMenuItem = null;
            int index = menus.length - 1;
            while (topMenuItem == null && index >= 0) {
              if (menus[index] != null) {
                MenuItem[] menuItems = menus[index].getItems();
                int menuItemIndex = 0;
                while (topMenuItem == null && menuItemIndex < menuItems.length) {
                  if (menuItems[menuItemIndex].getText().equals(menuLabel)) {
                    topMenuItem = menuItems[menuItemIndex];
                  }
                  menuItemIndex++;
                }
              }
              index--;
            }
            if (topMenuItem != null) {
              result = topMenuItem.getParent();
            }
          } else {
            throw new WidgetNotFoundException(
                "Unable to find MenuItem with label " + menuLabel);
          }
        } catch (SecurityException se) {
          throw new WidgetNotFoundException(
              "Unable to find MenuItem with label " + menuLabel, se);
        } catch (NoSuchFieldException nsfe) {
          throw new WidgetNotFoundException(
              "Unable to find MenuItem with label " + menuLabel, nsfe);
        } catch (IllegalArgumentException iae) {
          throw new WidgetNotFoundException(
              "Unable to find MenuItem with label " + menuLabel, iae);
        } catch (IllegalAccessException iace) {
          throw new WidgetNotFoundException(
              "Unable to find MenuItem with label " + menuLabel, iace);
        }
        return result;
      }
    });
  }

  /**
   * Simulate Right Click on treeItem
   * 
   * @param tree
   * @param treeItem
   */
  public static void treeRightClick(final Tree tree, final TreeItem treeItem) {
    Rectangle cellBounds = UIThreadRunnable.syncExec(new Result<Rectangle>() {
      public Rectangle run() {
        return treeItem.getBounds();
      }
    });
    clickXY(cellBounds.x + (cellBounds.width / 2), cellBounds.y
        + (cellBounds.height / 2), tree, treeItem);
  }

  /**
   * Simulate Right Click on treeItem on specified position
   * 
   * @param x
   * @param y
   * @param tree
   * @param treeItem
   */
  protected static void clickXY(int x, int y, final Tree tree,
      final TreeItem treeItem) {
    notifyTree(SWT.MouseEnter, tree, treeItem);
    notifyTree(SWT.MouseMove, tree, treeItem);
    notifyTree(SWT.Activate, tree, treeItem);
    notifyTree(SWT.FocusIn, tree, treeItem);
    notifyTree(SWT.MouseDown,
        createMouseEvent(x, y, 3, SWT.BUTTON3, 1, tree, treeItem), tree);
    notifyTree(SWT.MouseUp,
        createMouseEvent(x, y, 3, SWT.BUTTON3, 1, tree, treeItem), tree);
    notifyTree(SWT.Selection, createEvent(tree, treeItem), tree);
    notifyTree(SWT.MouseHover, tree, treeItem);
    notifyTree(SWT.MouseMove, tree, treeItem);
    notifyTree(SWT.MouseExit, tree, treeItem);
    notifyTree(SWT.Deactivate, tree, treeItem);
    notifyTree(SWT.FocusOut, tree, treeItem);
  }

  /**
   * Notify tree with Event of specified eventType
   * 
   * @param eventType
   * @param tree
   * @param treeItem
   */
  private static void notifyTree(int eventType, Tree tree, TreeItem treeItem) {
    Event event = new Event();
    event.time = (int) System.currentTimeMillis();
    event.widget = tree;
    event.display = tree.getDisplay();
    event.item = treeItem;

    notify(eventType, event, tree);
  }

  /**
   * Notify tree with Event of specified eventType
   * 
   * @param eventType
   * @param event
   * @param tree
   */
  private static void notifyTree(int eventType, Event event, Tree tree) {
    notify(eventType, event, tree);
  }

  /**
   * Sends a non-blocking notification of the specified type to the widget.
   * 
   * @param eventType
   *          the type of event.
   * @param createEvent
   *          the event to be sent to the {@link #widget}.
   * @param widget
   *          the widget to send the event to.
   */
  protected static void notify(final int eventType, final Event createEvent,
      final Widget widget) {
    createEvent.type = eventType;
    widget.getDisplay().asyncExec(new Runnable() {
      public void run() {
        if ((widget == null) || widget.isDisposed()) {
          return;
        }
        widget.notifyListeners(eventType, createEvent);
      }
    });

    widget.getDisplay().syncExec(new Runnable() {
      public void run() {
        // do nothing, just wait for sync.
      }
    });
  }

  /**
   * Create a mouse event
   * 
   * @param x
   *          the x co-ordinate of the mouse event.
   * @param y
   *          the y co-ordinate of the mouse event.
   * @param button
   *          the mouse button that was clicked.
   * @param stateMask
   *          the state of the keyboard modifier keys.
   * @param count
   *          the number of times the mouse was clicked.
   * @return an event that encapsulates {@link #widget} and {@link #display}
   * @since 1.2
   */
  protected static Event createMouseEvent(int x, int y, int button,
      int stateMask, int count, Tree tree, TreeItem treeItem) {
    Event event = new Event();
    event.time = (int) System.currentTimeMillis();
    event.widget = tree;
    event.display = tree.getDisplay();
    event.x = x;
    event.y = y;
    event.button = button;
    event.stateMask = stateMask;
    event.count = count;
    event.item = treeItem;
    return event;
  }

  /**
   * Create default Event for tree and treeItem
   * 
   * @param tree
   * @param treeItem
   * @return
   */
  protected static Event createEvent(Tree tree, TreeItem treeItem) {
    Event event = new Event();
    event.time = (int) System.currentTimeMillis();
    event.widget = tree;
    event.display = tree.getDisplay();
    event.item = treeItem;
    return event;
  }

  /**
   * Hide menu and all his parent menus and explicitly call
   * menu.setVisible(false)
   * 
   * @param menu
   */
  public static void hideMenuRecursively(final Menu menu) {
    menu.getDisplay().syncExec(new Runnable() {
      public void run() {
        ContextMenuHelper.hide(menu, true);
        menu.setVisible(false);
      }
    });
    try {
      Thread.sleep(50);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  /**
   * Hides one particular menu and explicitly call menu.setVisible(false)
   * 
   * @param menu
   */
  public static void hideMenuNonRecursively(final Menu menu) {
    menu.getDisplay().syncExec(new Runnable() {
      public void run() {
        ContextMenuHelper.hide(menu, false);
        menu.setVisible(false);
      }
    });
    try {
      Thread.sleep(50);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  /**
   * Returns Menu Items Labels of menu
   * 
   * @param menu
   * @return
   */
  public static String[] getMenuItemLabels(final Menu menu) {

    String[] result = null;

    result = UIThreadRunnable.syncExec(new Result<String[]>() {
      public String[] run() {
        MenuItem[] items = menu.getItems();
        String[] itemsLabels = new String[items.length];
        for (int index = 0; index < items.length; index++) {
          itemsLabels[index] = items[index].getText();
        }
        return itemsLabels;
      }
    });

    ContextMenuHelper.hideMenuRecursively(menu);

    return result;

  }

  /**
   * Shows menu of menuItem
   * 
   * @param menuItem
   * @return
   */
  public static Menu showMenuOfMenuItem(final MenuItem menuItem) {
    Menu result = null;

    result = UIThreadRunnable.syncExec(new Result<Menu>() {
      public Menu run() {
        Menu menu = menuItem.getMenu();
        if (menu != null) {
          menu.notifyListeners(SWT.Show, new Event());
          menu.setVisible(true);
        }
        return menu;
      }
    });

    return result;
  }
  /**
   * Trim special characters from menu item label used before comparing menu item label
   * @param menuItemLabel
   * @return
   */
  public static String trimMenuItemLabel (String menuItemLabel){
    String result = menuItemLabel.trim().replaceAll("\\&", "").split("\t")[0];

    return result;
    
  }
}