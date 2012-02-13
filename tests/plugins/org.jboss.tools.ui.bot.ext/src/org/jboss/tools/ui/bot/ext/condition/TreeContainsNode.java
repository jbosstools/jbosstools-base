package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;

public class TreeContainsNode implements ICondition {

	private SWTBotTree tree;

	private String[] path;

	public TreeContainsNode(SWTBotTree tree, String... path) {
		this.tree = tree;
		this.path = path;
	}

	@Override
	public void init(SWTBot bot) {

	}

	@Override
	public boolean test() throws Exception {
		try {
			tree.expandNode(path);
			System.out.println("TRUE");
			return true;
		} catch (WidgetNotFoundException e){
			System.out.println("FALSE");
			return false;
		}
	}

	@Override
	public String getFailureMessage() {
		return "Expected tree node " + path[path.length -1] + " to become visible ";
	}
}
