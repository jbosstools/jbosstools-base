package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerType;
import org.junit.Test;


@SWTBotTestRequires(server=@Server(type=ServerType.JbossAS),perspective="Java EE")
public class AnnotatedJbossASServer extends SWTTestExt {

	@Test
	public void configuredState() {
		assertTrue(configuredState.getServer().isRunning);
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
	public void serverRunning() {
		ServerUtil.serverRunning();
	}
	
}
