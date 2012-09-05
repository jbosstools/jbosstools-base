package org.jboss.tools.ui.bot.ext.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.Timing;

/**
 * 
 * @author psrna
 *
 */
public class TreeHelper {
	
	
	public static SWTBotTreeItem expandNode(final SWTBot bot, final String... nodes){

		List<String> asList = new ArrayList<String>(Arrays.asList(nodes));
		SWTBotTreeItem item = bot.tree(0).getTreeItem(asList.get(0));
		asList.remove(0);
		
		for(String node : asList){
			if(item == null)
				throw new WidgetNotFoundException("Could not find node with text:" + node);
			
			item.expand();
			while(item.getNode(0).getText().equals("Pending...") || 
				  item.getNode(0).getText().equals("Loading...")) {
				bot.sleep(Timing.time1S());
			}
			item = item.getNode(node);
		}			
		return item;
	}
}
