package org.jboss.tools.ui.bot.ext.test.config;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerState;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.junit.Test;

@Require(server=@Server(state=ServerState.Disabled))
public class AnnotatedDisabledServer extends SWTTestExt {

	@Test
	public void configuredState() {
		assertTrue(!configuredState.getServer().isRunning);
		assertTrue(!configuredState.getServer().isConfigured);
		assertNull(configuredState.getServer().version);
		assertNull(configuredState.getServer().type);
		assertNull(configuredState.getServer().name);
		assertNull(configuredState.getServer().withJavaVersion);
	}
	
	@Test
	public void serverDoesNotExist() {
		assertTrue(!servers.show().bot().tree().hasItems());
	}
	
	@Test
	public void runtimeDoesNotExist() {
		SWTBot wiz = open
		.preferenceOpen(ActionItem.Preference.ServerRuntimeEnvironments.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		assertTrue(tbRuntimeEnvironments.rowCount()==0);
		open.closeCancel(wiz);
	}

}
