package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

public class TreeItemContainsNode implements ICondition {

	private SWTBotTreeItem treeItem;

	private String node;

	public TreeItemContainsNode(SWTBotTreeItem treeItem, String node) {
		this.treeItem = treeItem;
		this.node = node;
	}

	@Override
	public void init(SWTBot bot) {

	}

	@Override
	public boolean test() throws Exception {
		try {
			treeItem.expandNode(node);			
			return true;
		} catch (WidgetNotFoundException e){
			return false;
		}
	}

	@Override
	public String getFailureMessage() {
		return "Tree item " + node + " is not visible " +
				"under tree item " + treeItem.getText();
	}
}
