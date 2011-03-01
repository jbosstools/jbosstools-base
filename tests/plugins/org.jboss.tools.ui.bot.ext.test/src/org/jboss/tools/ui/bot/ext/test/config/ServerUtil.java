package org.jboss.tools.ui.bot.ext.test.config;

import static org.junit.Assert.*;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

public class ServerUtil  {

	public static void serverExists() {
		boolean found=false;
		for (SWTBotTreeItem item : SWTTestExt.servers.show().bot().tree().getAllItems()) {
			if (item.getText().startsWith(SWTTestExt.configuredState.getServer().name)) {
				found = true;
				break;
			}
		}
		assertTrue(found);
	}
	
	public static void serverRunning() {
		SWTBotTreeItem server =null;
		SWTBotTree tree = SWTTestExt.servers.show().bot().tree();
		for (SWTBotTreeItem item : tree.getAllItems()) {
			if (item.getText().startsWith(SWTTestExt.configuredState.getServer().name)) {
				server = item;
				break;
			}
		}
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        assertTrue(new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.STOP, false)).isEnabled());
		}
	}
}
