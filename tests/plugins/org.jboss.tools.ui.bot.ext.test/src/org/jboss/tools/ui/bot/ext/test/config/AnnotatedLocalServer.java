package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerLocation;
import org.junit.Test;
@SWTBotTestRequires(server=@Server(location=ServerLocation.Local),perspective="Java EE")
public class AnnotatedLocalServer extends SWTTestExt{
	@Test
	public void configuredState() {
		assertTrue(configuredState.getServer().isRunning);
		assertTrue(configuredState.getServer().isConfigured);
		assertNotNull(configuredState.getServer().version);
		assertNotNull(configuredState.getServer().type);
		assertNotNull(configuredState.getServer().name);
		assertNotNull(configuredState.getServer().withJavaVersion);
		assertTrue(configuredState.getServer().isLocal);
	}
	
	@Test
	public void serverExists() {
		ServerUtil.serverExists();
	}
	
	@Test
	public void serverRunning() {
		ServerUtil.serverRunning();
	}
}
