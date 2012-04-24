 /*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.test.config;
/**
 * Parent test for tests using local server in default state.
 * @author lzoubek
 *
 */
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerLocation;
import org.junit.Test;
@Require(server=@Server(location=ServerLocation.Local),perspective="Java EE")
public class AnnotatedLocalServer extends SWTTestExt{
  /**
   * Checks server state
   */
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
	 /**
   * Checks server existence
   */
	@Test
	public void serverExists() {
		ServerUtil.serverExists();
	}
	 /**
   * Checks if server is running
   */
	@Test
	public void serverRunning() {
		ServerUtil.serverRunning();
	}
}
