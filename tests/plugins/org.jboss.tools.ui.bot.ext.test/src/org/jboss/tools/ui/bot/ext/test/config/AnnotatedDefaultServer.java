package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.junit.Test;


@Require(server=@Server(),perspective="Java EE")
public class AnnotatedDefaultServer extends SWTTestExt {

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
