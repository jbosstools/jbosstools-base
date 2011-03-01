package org.jboss.tools.ui.bot.ext.test.config;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerState;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.junit.Test;


@SWTBotTestRequires(server=@Server(state=ServerState.NotRunning),perspective="Java EE")
public class AnnotatedNotRunningServer extends SWTTestExt {

	@Test
	public void configuredState() {
		assertTrue(!configuredState.getServer().isRunning);
		assertTrue(configuredState.getServer().isConfigured);
		assertNotNull(configuredState.getServer().version);
		assertNotNull(configuredState.getServer().type);
		assertNotNull(configuredState.getServer().name);
		assertNotNull(configuredState.getServer().withJavaVersion);
	}
	
	@Test
	public void serverExists() {
		ServerUtil.serverExists();
	}
	
	@Test
	public void serverNotRunning() {
		SWTBotTreeItem server =null;
		SWTBotTree tree = servers.show().bot().tree();
		for (SWTBotTreeItem item : tree.getAllItems()) {
			if (item.getText().startsWith(configuredState.getServer().name)) {
				server = item;
				break;
			}
		}
		if (server!=null) {
			ContextMenuHelper.prepareTreeItemForContextMenu(tree, server);
	        assertFalse(new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.STOP, false)).isEnabled());
	        assertTrue(new SWTBotMenu(ContextMenuHelper.getContextMenu(tree, IDELabel.Menu.START, false)).isEnabled());
		}
	}
	

}
