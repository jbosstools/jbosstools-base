package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerLocation;
import org.junit.Test;
@SWTBotTestRequires(server=@Server(location=ServerLocation.Remote),perspective="Java EE")
public class AnnotatedRemoteServer extends SWTTestExt{
	@Test
	public void configuredState() {
		assertTrue(configuredState.getServer().isRunning);
		assertTrue(configuredState.getServer().isConfigured);
		assertNotNull(configuredState.getServer().version);
		assertNotNull(configuredState.getServer().type);
		assertNotNull(configuredState.getServer().name);
		assertNotNull(configuredState.getServer().withJavaVersion);
		assertFalse(configuredState.getServer().isLocal);
	}
	
	@Test
	public void serverExists() {
		ServerUtil.serverExists();
	}
}
