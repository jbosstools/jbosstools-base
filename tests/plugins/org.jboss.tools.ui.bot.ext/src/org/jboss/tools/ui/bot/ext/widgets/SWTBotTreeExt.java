package org.jboss.tools.ui.bot.ext.widgets;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

public class SWTBotTreeExt {
	SWTBotTree tree;
	
	/**
	 * SWTBotTreeExt contstructor
	 * @param tree
	 */
	public SWTBotTreeExt(SWTBotTree tree) {
		this.tree = tree;
	}

	/**
	 * Expands whole tree
	 */
	public void expandAll() {
		SWTBotTreeItem[] items = tree.getAllItems();
		for (SWTBotTreeItem i : items ) {
			if (!i.isExpanded()) 
				i.expand();
		}
	}
}
