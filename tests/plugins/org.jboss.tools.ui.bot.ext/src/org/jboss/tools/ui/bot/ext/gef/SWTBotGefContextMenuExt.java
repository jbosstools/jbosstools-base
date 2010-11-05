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
package org.jboss.tools.ui.bot.ext.gef;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withRegex;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.instanceOf;

import org.apache.log4j.Logger;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.hamcrest.Matcher;

/**
 * Context menu bot for GEF widgets. Improved version updated to work for GEF
 * environment
 * 
 * @author jpeterka
 * 
 */
public class SWTBotGefContextMenuExt {

	/**
	 * Default Constructor
	 * 
	 * @param control
	 */

	Menu menu;
	Logger log = Logger.getLogger(SWTBotGefContextMenuExt.class);

	public SWTBotGefContextMenuExt(Menu menu) {
		this.menu = menu;
	}

	/**
	 * Invokes context menu sequence
	 * 
	 * @param texts
	 */
	public void clickMenu(final String... texts) {
		log.debug("Clicking graph menu: "); //$NON-NLS-1$

		// Run in UI Thread
		final MenuItem menuItem = UIThreadRunnable
				.syncExec(new WidgetResult<MenuItem>() {
					public MenuItem run() {
						// Get menu from control

						MenuItem lastMenuItem = null;
						lastMenuItem = getLastMenuItem(texts, menu);

						// if last menu found, click and hide
						if (lastMenuItem != null) {
							log.debug("Final menu returned"); //$NON-NLS-1$
							// Click
							// clickOnMenuItem(lastMenuItem);
							// Hide
							// hideMenu(lastMenuItem.getParent());
						} else
							log.debug("Final menu not found"); //$NON-NLS-1$

						return lastMenuItem;
					}
				});

		// Async block to avoid blocking by raised dlgs, etc.
		if (menuItem != null)
			UIThreadRunnable.asyncExec(new VoidResult() {

				public void run() {
					clickOnMenuItem(menuItem);
					// Hide
					hideMenu(menuItem.getParent());

				}
			});

		if (menuItem == null)
			throw new WidgetNotFoundException("Unable to find menuitem"); //$NON-NLS-1$
		log.debug("GEF Context menu " + texts[texts.length - 1] + "click finished"); //$NON-NLS-1$

	}

	/**
	 * Gets last menitem on menuitem hierary. UIThread methods
	 * 
	 * @param text
	 */
	private MenuItem getLastMenuItem(String[] texts, Menu menu) {

		if (menu == null)
			throw new WidgetNotFoundException("No context menu found"); //$NON-NLS-1$

		Menu nextMenu = menu;
		MenuItem nextMenuItem = null;
		for (String text : texts) {
			// Get next menu
			nextMenuItem = null;

			nextMenuItem = getNextMenuItem(text, nextMenu);
			// Set menu as next menu item menu
			if (nextMenuItem == null) {
				hideMenu(nextMenu);
				return null;
			} else {
				log.debug("next menu for menu item " + nextMenuItem.getText() //$NON-NLS-1$
						+ " found"); //$NON-NLS-1$
				nextMenu = nextMenuItem.getMenu();
			}
		}
		return nextMenuItem;
	}

	/**
	 * Gets next menuitem of given text. UIThread method
	 * 
	 * @param text
	 * @param menu
	 * @return
	 */
	@SuppressWarnings("unchecked")
	private MenuItem getNextMenuItem(String text, Menu menu) {

		MenuItem nextMenuItem = null;

		Matcher<?> matcher = allOf(instanceOf(MenuItem.class), withRegex("\\s*"
				+ text + "\\s*"));

		showMenu(menu);
		MenuItem[] items = menu.getItems();
		log.debug("Menu items found:" + items.length + " items"); //$NON-NLS-1$ //$NON-NLS-2$
		// menu item loops
		for (MenuItem i : items) {
			log.debug("Trying to match " + text + " with " + i.getText()); //$NON-NLS-1$ //$NON-NLS-2$
			if (matcher.matches(i)) {
				nextMenuItem = i;
				log.debug("Next menu item with text:" + text + " found"); //$NON-NLS-1$ //$NON-NLS-2$
				break;
			}
		}

		return nextMenuItem;
	}

	/**
	 * Clicks on given menuitem. UIThread method
	 * 
	 * @param menuItem
	 */
	private void clickOnMenuItem(final MenuItem menuItem) {
		System.out.println("Menuitem:" + menuItem); //$NON-NLS-1$
		final Event event = new Event();
		event.time = (int) System.currentTimeMillis();
		event.widget = menuItem;
		event.display = menuItem.getDisplay();
		event.type = SWT.Selection;

		log.debug("Click event generated"); //$NON-NLS-1$

		menuItem.notifyListeners(SWT.Selection, event);
		log.debug("Event sent"); //$NON-NLS-1$
	}

	/**
	 * Hides menu including all predecessor. UIThread method
	 * 
	 * @param menu
	 */
	private static void hideMenu(final Menu menu) {
		menu.notifyListeners(SWT.Hide, new Event());
		if (menu.getParentMenu() != null) {
			hideMenu(menu.getParentMenu());
		}
	}

	/**
	 * Shows menu. UIThread method
	 */
	private void showMenu(final Menu menu) {
		menu.notifyListeners(SWT.Show, new Event());
	}
}
