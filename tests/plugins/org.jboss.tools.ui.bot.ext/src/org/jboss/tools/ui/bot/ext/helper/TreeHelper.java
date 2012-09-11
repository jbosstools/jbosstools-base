package org.jboss.tools.ui.bot.ext.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.eclipse.swtbot.swt.finder.widgets.TimeoutException;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.condition.TreeContainsNode;
import org.jboss.tools.ui.bot.ext.condition.TreeItemContainsNode;

/**
 * 
 * @author psrna
 *
 */
public class TreeHelper {
	
	
	public static SWTBotTreeItem expandNode(final SWTBot bot, final String... nodes){

		List<String> asList = new ArrayList<String>(Arrays.asList(nodes));
		SWTBotTree tree = bot.tree(0);
		try {
			bot.waitUntil(new TreeContainsNode(tree,asList.get(0)), Timing.time5S());
		} catch (TimeoutException exc) {
			// do nothing, WidgetNotFoundException will be thrown just after this
		}
		SWTBotTreeItem item = tree.getTreeItem(asList.get(0));
		asList.remove(0);
		
		for(String node : asList){
			item.expand();
			while(item.getNode(0).getText().equals("Pending...") || 
				  item.getNode(0).getText().equals("Loading...")) {
				bot.sleep(Timing.time1S());
			}
			try {
				bot.waitUntil(new TreeItemContainsNode(item, node), Timing.time5S());
			} catch (TimeoutException exc) {
				// do nothing, WidgetNotFoundException will be thrown just after this
			}
			item = item.getNode(node);
		}			
		return item;
	}
}
